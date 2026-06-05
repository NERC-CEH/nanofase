!> Module containing definition of `SoilLayer` class.
module SoilLayerModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
    use UtilModule
    use AbstractSoilLayerModule
    use DataInputModule, only: DATASET
    use PFASEConstantsModule, only: PFAS_AQ, PFAS_SOL, PFAS_AWI
    use BiotaSoilModule
    use datetime_module
    use ContaminantModule, only: Contaminant
    implicit none

    !> `SoilLayer` is responsible for routing percolated water through
    !! the `SoilProfile` in which it is contained.

   type, public, extends(AbstractSoilLayer) :: SoilLayer
      contains
        procedure :: create => createSoilLayer
        procedure :: update => updateSoilLayer
        procedure :: update_contaminant_state => updateContaminantStateSoilLayer
        procedure :: addPooledWater => addPooledWaterSoilLayer
        procedure :: erode => erodeSoilLayer
        procedure :: calculateAttachmentRate => calculateAttachmentRateSoilLayer
        procedure :: calculateBioturbationRate => calculateBioturbationRateSoilLayer
        procedure :: parseInputData => parseInputDataSoilLayer
        procedure :: parseNewBatchData => parseNewBatchDataSoilLayer
    end type

  contains
    !> Create this `SoilLayer` and call the input data parsing procedure
    function createSoilLayer(me, x, y, p, l, WC_sat, WC_FC, K_s, area, bulkDensity, d_grain, porosity, earthwormDensity) result(r)
        class(SoilLayer) :: me                  !! This `SoilLayer` instance
        integer, intent(in) :: x                  !! Containing `GridCell` x index
        integer, intent(in) :: y                  !! Containing `GridCell` y index
        integer, intent(in) :: p                  !! Containing `SoilProfile` index
        integer, intent(in) :: l                  !! Layer index
        real(dp), intent(in) :: WC_sat            !! Water content at saturation [m3/m3]
        real(dp), intent(in) :: WC_FC             !! Water content at field capacity [m3/m3]
        real(dp), intent(in) :: K_s               !! Saturated hydraulic conductivity [m/s]
        real(dp), intent(in) :: area              !! Area of the containing SoilProfile [m2]
        real(dp), intent(in) :: bulkDensity       !! Bulk density [kg/m3]
        real(dp), intent(in) :: d_grain           !! Average grain diameter [m]
        real(dp), intent(in) :: porosity          !! Porosity [-]
        real(dp), intent(in) :: earthwormDensity  !! Earthworm density [individuals/m2]
        integer :: i                              ! Iterator
        type(Result) :: r                         !! The `Result` object to return, with any errors from parsing input data.
        integer :: allocStat                      ! Allocation status
        real(dp) :: T_water_t                     ! Water temperature for initialization [deg C]
        type(datetime) :: currentDate             ! Current date for water temperature

        ! Set the metadata and area
        me%x = x
        me%y = y 
        me%p = p
        me%l = l
        me%ref = ref("SoilLayer", x, y, p, l)
        me%area = area
        me%depth = C%soilLayerDepth(l)
        me%volume = me%area * me%depth
        me%bulkDensity = bulkDensity
        me%d_grain = d_grain
        me%porosity = porosity
        me%earthwormDensity = earthwormDensity

        ! Get water temperature for initialization (use first day of simulation)
        currentDate = C%startDate
        T_water_t = DATASET%waterTemperature(currentDate%yearday())

        ! Initialize Contaminant objects
        ! FIX: Removed the invalid DATASET%nc argument and switched to keyword-based passing.
        call r%addErrors(.errors. me%m_contaminant%create_from_data( &
            compartment='soil', &
            contaminantDensity=DATASET%contaminantDensity, &
            soilAttachmentEfficiency=DATASET%soilConstantAttachmentEfficiency, &
            riverAttachmentEfficiency=DATASET%riverAttachmentEfficiency, &
            estuaryAttachmentEfficiency=DATASET%estuaryAttachmentEfficiency, &
            k_diss_pristine=DATASET%contaminant_k_diss_pristine, &
            k_diss_transformed=DATASET%contaminant_k_diss_transformed, &
            k_transform_pristine=DATASET%contaminant_k_transform_pristine, &
            waterTemperature=T_water_t &
        ))

        ! Initialize other Contaminant objects
        call r%addErrors(.errors. me%j_contaminant_in%create())
        call r%addErrors(.errors. me%j_contaminant_perc%create())
        call r%addErrors(.errors. me%j_contaminant_eroded%create())

        ! Set initial contaminant concentrations from DATASET
        ! PFAS initial condition: DATASET field is expected to be already mapped
        ! as c(species, form, phase). Legacy dissolved scalar is not duplicated
        ! into m_dissolved; m_dissolved is an alias of PFAS_AQ mass.
        if (allocated(DATASET%initialContaminantConcsSoil)) then
            me%m_contaminant%c = DATASET%initialContaminantConcsSoil(me%x, me%y, :, :, :)
            me%m_contaminant%m_dissolved = sum(me%m_contaminant%c(:,:,PFAS_AQ))
        end if

        ! Allocate and initialize k_att
        allocate(me%k_att(C%contaminantDim(1)), stat=allocStat)
        if (allocStat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate k_att"))
            return
        end if
        me%k_att = 0.0_dp

        call r%addErrors(.errors. me%parseInputData())

        allocate(me%biotaIndices(0))
        if (DATASET%hasBiota) then
            do i = 1, DATASET%nBiota
                if (trim(DATASET%biotaCompartment(i)) == 'soil') then
                    me%nBiota = me%nBiota + 1
                    me%biotaIndices = [me%biotaIndices, i]
                end if
            end do
        end if

        allocate(me%biota(me%nBiota), stat=allocStat)
        if (allocStat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate soil biota"))
            return
        end if

        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%create(me%biotaIndices(i)))
        end do

        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Update the `SoilLayer` on a given time step, based on specified inflow.
    !! Calculate percolation to next layer and, if saturated, the amount
    !! to pool to the above layer (or surface runoff, if this is the top layer)
    function updateSoilLayer(me, t, q_in, j_contaminant_in) result(r)
        class(SoilLayer) :: me                          !! This `SoilLayer` instance
        integer :: t                                    !! The current time step [s]
        real(dp) :: q_in                                !! Water into the layer on this time step, from percolation and pooling [m/timestep]
        type(Contaminant), intent(in) :: j_contaminant_in
        type(Result) :: r                               !! The Result object to return any errors in
        real(dp) :: initial_V_w                         ! Initial V_w used for checking whether all water removed
        integer :: i                                    ! Iterators
        type(datetime) :: currentDate                   ! Current date
        real(dp) :: T_water_t                           ! Water temperature on the current timestep [deg C]
        real(dp) :: leach_fraction

        ! NEW: zero SPM arrays with correct model dimension
        real(dp) :: C_spm_zero(C%nSizeClassesSpm)
        real(dp) :: W_settle_zero(C%nSizeClassesSpm)

        C_spm_zero    = 0.0_dp
        W_settle_zero = 0.0_dp

        ! Get the current date to use to get the water temperature
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = DATASET%waterTemperature(currentDate%yearday())

        call me%j_contaminant_perc%empty()
        call me%j_contaminant_in%empty()
        call me%j_contaminant_in%add(j_contaminant_in)

        me%q_in = max(0.0_dp, q_in)
        initial_V_w = me%V_w
        call me%m_contaminant%add(j_contaminant_in)

        if (me%V_w + me%q_in < me%V_sat) then
            me%V_pool = 0.0_dp
            me%V_w = me%V_w + me%q_in
            me%V_excess = max(me%V_w - me%V_FC, 0.0_dp)
        else
            me%V_pool = max(0.0_dp, me%V_w + me%q_in - me%V_sat)
            me%V_w = me%V_sat
            me%V_excess = max(0.0_dp, me%V_w - me%V_FC)
        end if

        if (me%V_sat > me%V_FC + C%epsilon) then
            me%V_perc = min(me%V_excess * (1.0_dp - exp(-real(C%timeStep, dp) * me%K_s / &
                        max(C%epsilon, me%V_sat - me%V_FC))), me%V_w)
        else
            me%V_perc = 0.0_dp
        end if

        if (me%V_perc > C%epsilon .and. me%V_w > C%epsilon) then
            leach_fraction = min(1.0_dp, max(0.0_dp, me%V_perc / me%V_w))
            call me%m_contaminant%leach(leach_fraction, me%j_contaminant_perc)
            ! leach() already removes AQ mass from m_contaminant in the supplied ContaminantModule.
            me%V_w = max(0.0_dp, me%V_w - me%V_perc)
        else
            me%V_perc = 0.0_dp
        end if

        ! Update PFAS phase equilibrium/kinetics after hydrological movement.
        call r%addErrors(.errors. me%m_contaminant%update( &
            real(C%timeStep, dp), T_water_t, C_spm_zero, W_settle_zero, 0.0_dp, &
            max(C%epsilon, me%V_w * me%area), 'soil'))

        if (me%V_w <= C%epsilon .and. initial_V_w > C%epsilon) then
            call r%addError(ErrorInstance(600, isCritical=.false., &
                message="Soil layer drained completely during PFAS leaching step"))
        end if

        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%update(t, me%m_contaminant))
        end do

        me%m_contaminant%m_dissolved = sum(me%m_contaminant%c(:,:,PFAS_AQ))
        me%j_contaminant_perc%m_dissolved = sum(me%j_contaminant_perc%c(:,:,PFAS_AQ))

        call r%addToTrace("Updating " // trim(me%ref) // " on time step #" // trim(str(t)))
    end function

    !> Update the internal contaminant state (attachment, etc.) without handling water fluxes.
    subroutine updateContaminantStateSoilLayer(me, T_water_t)
        class(SoilLayer), intent(inout) :: me
        real(dp), intent(in) :: T_water_t
        type(Result) :: r
        ! These are zero because we are only updating internal state, not adding fluxes
        real(dp) :: C_spm_zero(C%nSizeClassesSpm)
        real(dp) :: W_settle_zero(C%nSizeClassesSpm)
        C_spm_zero = 0.0_dp
        W_settle_zero = 0.0_dp

        ! Call the generic contaminant update routine to perform attachment etc.
        call r%addErrors(.errors. me%m_contaminant%update( &
            real(C%timeStep, dp), T_water_t, C_spm_zero, W_settle_zero, 0.0_dp, &
            max(C%epsilon, me%V_w * me%area), 'soil'))

        me%m_contaminant%m_dissolved = sum(me%m_contaminant%c(:,:,PFAS_AQ))

        if (r%hasCriticalError()) then
            call r%addToTrace("Updating PFAS contaminant state in " // trim(me%ref))
            call ERROR_HANDLER%trigger(errors=.errors.r)
        end if
    end subroutine

    !> Add a volume \( V_{\text{pool}} \) of pooled water to the layer.
    !! No percolation occurs as pooled water never really leaves the `SoilLayer`.
    function addPooledWaterSoilLayer(me, V_pool) result(r)
        class(SoilLayer) :: me                         !! This SoilLayer instance
        real(dp) :: V_pool                              !! Volume of pooled water to add, \( V_{\text{pool}} \) [m3/m2]
        type(Result) :: r                               !! The Result object to return, with no data

        me%V_pool = max(me%V_w + V_pool - me%V_sat, 0.0)  ! Will the input pooled water result in pooled water for this layer?
        me%V_w = min(me%V_w + V_pool, me%V_sat)         ! Add pooled water, up to a maximum of V_sat
    end function

    !> Erode NM from this soil layer
    !! TODO bulk density could be stored in this object, not passed, probably same with area
    function erodeSoilLayer(me, erodedSediment, bulkDensity, area) result(r)
        class(SoilLayer) :: me
        real(dp) :: erodedSediment(:)
        real(dp) :: bulkDensity
        real(dp) :: area
        type(Result) :: r
        real(dp) :: m_soil_layer, propEroded

        ! Calculate the mass of the soil in this layer
        m_soil_layer = max(C%epsilon, bulkDensity * area * me%depth)
        propEroded = min(1.0_dp, max(0.0_dp, sum(erodedSediment) * area / m_soil_layer))

        ! Initialize the eroded contaminant object
        call r%addErrors(.errors. me%j_contaminant_eroded%create())

        ! Calculate eroded contaminant (only attached contaminant is eroded)
        call me%m_contaminant%erosion_export(propEroded, me%j_contaminant_eroded)
        me%j_contaminant_eroded%m_dissolved = sum(me%j_contaminant_eroded%c(:,:,PFAS_AQ))

        call r%addToTrace("Eroding PFAS from " // trim(me%ref))
    end function

    function calculateBioturbationRateSoilLayer(me) result(bioturbationRate)
        class(SoilLayer) :: me
        real(dp) :: bioturbationRate
        real(dp) :: earthwormDensity_perVolume
        real(dp) :: bioturb_alpha = 3.56e-9
        earthwormDensity_perVolume = max(0.0_dp, me%earthwormDensity * me%depth)
        if (me%depth > C%epsilon) then
            bioturbationRate = (earthwormDensity_perVolume * bioturb_alpha) / me%depth
        else
            bioturbationRate = 0.0_dp
        end if
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data
    function parseInputDataSoilLayer(me) result(r)
        class(SoilLayer) :: me
        type(Result)     :: r
        logical :: have2D
        integer :: nx, ny

        have2D = .false.
        if (allocated(DATASET%soilAttachmentEfficiency)) then
            nx = size(DATASET%soilAttachmentEfficiency,1)
            ny = size(DATASET%soilAttachmentEfficiency,2)
            have2D = (me%x>=1 .and. me%y>=1 .and. me%x<=nx .and. me%y<=ny)
        end if

        if (have2D) then
            me%alpha_att = DATASET%soilAttachmentEfficiency(me%x, me%y)
        else
            me%alpha_att = DATASET%soilConstantAttachmentEfficiency
        end if

        call r%addToTrace("Parsing input data (P-FASE soil layer)")
    end function

    subroutine parseNewBatchDataSoilLayer(me)
        class(SoilLayer), intent(inout) :: me
        type(Result) :: r
        r = me%parseInputData()
    end subroutine

end module
!> Module containing definition of `SoilLayer` class.
module SoilLayerModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
    use UtilModule
    use AbstractSoilLayerModule
    use DataInputModule, only: DATASET
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
        procedure :: addPooledWater => addPooledWaterSoilLayer
        procedure :: erode => erodeSoilLayer
        procedure :: attachment => attachmentSoilLayer
        procedure :: calculateAttachmentRate => calculateAttachmentRateSoilLayer
        procedure :: calculateBioturbationRate => calculateBioturbationRateSoilLayer
        procedure :: parseInputData => parseInputDataSoilLayer
        procedure :: parseNewBatchData => parseNewBatchDataSoilLayer
    end type

  contains
    !> Create this `SoilLayer` and call the input data parsing procedure
    !> Create this `SoilLayer` and call the input data parsing procedure
    !> Create this `SoilLayer` and call the input data parsing procedure
    function createSoilLayer(me, x, y, p, l, WC_sat, WC_FC, K_s, area, bulkDensity, d_grain, porosity, earthwormDensity) result(r)
        class(SoilLayer) :: me                          !! This `SoilLayer` instance
        integer, intent(in) :: x                        !! Containing `GridCell` x index
        integer, intent(in) :: y                        !! Containing `GridCell` y index
        integer, intent(in) :: p                        !! Containing `SoilProfile` index
        integer, intent(in) :: l                        !! Layer index
        real(dp), intent(in) :: WC_sat                  !! Water content at saturation [m3/m3]
        real(dp), intent(in) :: WC_FC                   !! Water content at field capacity [m3/m3]
        real(dp), intent(in) :: K_s                     !! Saturated hydraulic conductivity [m/s]
        real(dp), intent(in) :: area                    !! Area of the containing SoilProfile [m2]
        real(dp), intent(in) :: bulkDensity             !! Bulk density [kg/m3]
        real(dp), intent(in) :: d_grain                 !! Average grain diameter [m]
        real(dp), intent(in) :: porosity                !! Porosity [-]
        real(dp), intent(in) :: earthwormDensity        !! Earthworm density [individuals/m2]
        integer :: i                                    ! Iterator
        type(Result) :: r                               !! The `Result` object to return, with any errors from parsing input data.
        integer :: allocStat                            ! Allocation status
        real(dp) :: T_water_t                           ! Water temperature for initialization [deg C]
        type(datetime) :: currentDate                   ! Current date for water temperature

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
        call r%addErrors(.errors. me%m_contaminant%create_from_data( &
            DATASET%nc, &
            'soil', &
            DATASET%contaminantDensity, &
            DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, &  ! Added missing argument
            DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, &
            DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, &
            T_water_t &
        ))

        ! Initialize other Contaminant objects
        call r%addErrors(.errors. me%j_contaminant_in%create())
        call r%addErrors(.errors. me%j_contaminant_perc%create())
        call r%addErrors(.errors. me%j_contaminant_eroded%create())

        ! Set initial contaminant concentrations from DATASET
        if (allocated(DATASET%initialContaminantConcsSoil)) then
            me%m_contaminant%c = DATASET%initialContaminantConcsSoil(me%x, me%y, :, :, :)
            if (allocated(DATASET%initialDissolvedConcsSoil)) then
                me%m_contaminant%m_dissolved = DATASET%initialDissolvedConcsSoil(me%x, me%y)
            end if
        end if

        ! Allocate and initialize k_att
        allocate(me%k_att(C%contaminantDim(1)), stat=allocStat)
        if (allocStat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate k_att"))
            return
        end if
        me%k_att = 0.0_dp
        me%V_w = 0.0_dp

        ! Parse the input data into the object properties
        call r%addErrors(.errors. me%parseInputData())

        ! Set saturation and field capacity volumes [m3/m2] based on depth of layer
        me%V_sat = WC_sat * me%depth
        me%V_FC = WC_FC * me%depth
        me%K_s = K_s                                    ! Hydraulic conductivity [m/s]

        ! Allocate and create the Biota object
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
            call r%addError(ErrorInstance(code=901, message="Failed to allocate biota"))
            return
        end if
        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%create(me%biotaIndices(i)))
        end do

        ! Add this procedure to error traces
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
        
        ! Get the current date to use to get the water temperature
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = DATASET%waterTemperature(currentDate%yearday())

        ! Set the inflow to this SoilLayer and store initial water in layer
        me%q_in = q_in
        initial_V_w = me%V_w

        call me%m_contaminant%add(j_contaminant_in)

        ! Setting volume of water, pooled water and excess water, based on inflow
        if (me%V_w + me%q_in < me%V_sat) then                   ! If water volume below V_sat after inflow
            me%V_pool = 0.0_dp                                  ! No pooled water
            me%V_w = me%V_w + me%q_in                           ! Update the volume based on inflow
            me%V_excess = max(me%V_w - me%V_FC, 0.0_dp)         ! Volume of water above V_FC
        else if (me%V_w + me%q_in > me%V_sat) then              ! Else, water pooled above V_sat
            me%V_pool = me%V_w + me%q_in - me%V_sat             ! Water pooled above V_sat
            me%V_w = me%V_sat                                   ! Volume of water must be V_sat
            me%V_excess = me%V_w - me%V_FC                      ! Volume must be above FC and so there is excess
        end if

        ! Calculate volume percolated on this timestep [m3 m-2]
        me%V_perc = min(me%V_excess * (1 - exp(-C%timeStep * me%K_s / (me%V_sat - me%V_FC))), me%V_w)
        call r%addErrors(.errors. me%j_contaminant_perc%create())
        if (.not. isZero(me%V_perc) .and. me%V_w > C%epsilon) then
            call me%j_contaminant_perc%multiply_scalar(me%m_contaminant, me%V_perc / me%V_w)
            me%j_contaminant_perc%c(:,:,ATTACHED_CONTAMINANT+1:) = 0.0_dp
            me%j_contaminant_perc%c(:,:,ATTACHED_CONTAMINANT) = 0.0_dp
        end if
        call me%m_contaminant%add_scaled(me%j_contaminant_perc, -1.0_dp)
        me%V_w = me%V_w - me%V_perc
        me%k_att = me%calculateAttachmentRate(T_water_t)
        call r%addErrors(.errors. me%m_contaminant%update(real(C%timeStep, dp), T_water_t, &
            [0.0_dp], [0.0_dp], 0.0_dp, me%volume, 'soil', me%k_att, me%alpha_att))
        if (isZero(me%V_w) .and. initial_V_w > 0) then
            call r%addError(ErrorInstance(600, isCritical=.false.))
        end if
        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%update(t, me%m_contaminant))
        end do
        call r%addToTrace("Updating " // trim(me%ref) // " on time step #" // trim(str(t)))
    end function

    !> Add a volume \( V_{\text{pool}} \) of pooled water to the layer.
    !! No percolation occurs as pooled water never really leaves the `SoilLayer`.
    function addPooledWaterSoilLayer(me, V_pool) result(r)
        class(SoilLayer) :: me                         !! This SoilLayer instance
        real(dp) :: V_pool                              !! Volume of pooled water to add, \( V_{\text{pool}} \) [m3/m2]
        type(Result) :: r                               !! The Result object to return, with no data

        me%V_pool = max(me%V_w + V_pool - me%V_sat, 0.0)  ! Will the input pooled water result in pooled water for this layer?
        me%V_w = min(me%V_w + V_pool, me%V_sat)         ! Add pooled water, up to a maximum of V_sat
    end function


    subroutine attachmentSoilLayer(me, T_water_t)
        class(SoilLayer) :: me
        real(dp) :: T_water_t
        ! Handled by update_soil in ReactorModule
    end subroutine

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
        m_soil_layer = bulkDensity * area * me%depth
        propEroded = sum(erodedSediment) * area / m_soil_layer

        ! Initialize the eroded contaminant object
        call r%addErrors(.errors. me%j_contaminant_eroded%create())

        ! Calculate eroded contaminant (only attached contaminant is eroded)
        me%j_contaminant_eroded%c(:,:,ATTACHED_CONTAMINANT) = &
            me%m_contaminant%c(:,:,ATTACHED_CONTAMINANT) * propEroded

        ! Remove the eroded contaminant from the layer
        call me%m_contaminant%add_scaled(me%j_contaminant_eroded, -1.0_dp)

        ! Add this procedure to the error trace
        call r%addToTrace("Eroding " // trim(me%ref))
    end function

    function calculateAttachmentRateSoilLayer(me, T_water_t) result(k_att)
        class(SoilLayer) :: me
        real(dp) :: T_water_t
        real(dp) :: k_att(C%contaminantDim(1))
        k_att = me%m_contaminant%calculateAttachmentRate(T_water_t, me%porosity, me%d_grain)
    end function

    function calculateBioturbationRateSoilLayer(me) result(bioturbationRate)
        class(SoilLayer) :: me
        real(dp) :: bioturbationRate
        real(dp) :: earthwormDensity_perVolume
        real(dp) :: bioturb_alpha = 3.56e-9
        earthwormDensity_perVolume = me%earthwormDensity * me%depth
        bioturbationRate = (earthwormDensity_perVolume * bioturb_alpha) / me%depth
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data
    function parseInputDataSoilLayer(me) result(r)
       class(SoilLayer) :: me
        type(Result) :: r
        me%alpha_att = DATASET%soilAttachmentEfficiency(me%x, me%y)
    end function

    subroutine parseNewBatchDataSoilLayer(me)
       class(SoilLayer) :: me
        me%alpha_att = DATASET%soilAttachmentEfficiency(me%x, me%y)
    end subroutine

end module
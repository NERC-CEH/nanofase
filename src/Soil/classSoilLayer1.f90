!> Module containing definition of `SoilLayer1` class.
module classSoilLayer1
    use Globals
    use UtilModule
    use spcSoilLayer
    use classDatabase, only: DATASET
    use classBiotaSoil
    use datetime_module
    implicit none

    !> `SoilLayer1` is responsible for routing percolated water through
    !! the `SoilProfile` in which it is contained.
    type, public, extends(SoilLayer) :: SoilLayer1
      contains
        procedure :: create => createSoilLayer1
        procedure :: update => updateSoilLayer1
        procedure :: addPooledWater => addPooledWaterSoilLayer1
        procedure :: erode => erodeSoilLayer1
        procedure :: attachment => attachmentSoilLayer1
        procedure :: calculateAttachmentRate => calculateAttachmentRateSoilLayer1
        procedure :: calculateBioturbationRate => calculateBioturbationRateSoilLayer1
        procedure :: parseInputData => parseInputDataSoilLayer1
        procedure :: parseNewBatchData => parseNewBatchDataSoilLayer1
    end type

  contains
    !> Create this `SoilLayer` and call the input data parsing procedure
    function createSoilLayer1(me, x, y, p, l, WC_sat, WC_FC, K_s, area, bulkDensity, d_grain, porosity, earthwormDensity) result(r)
        class(SoilLayer1) :: me                         !! This `SoilLayer1` instance
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
        type(Result) :: r
            !! The `Result` object to return, with any errors from parsing input data.

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

        ! Allocate and initialise variables
        allocate(me%m_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_np_perc(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_np_eroded(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%C_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_transformed_perc(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_transformed_eroded(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%C_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%k_att(C%npDim(1)))
        me%m_np = 0.0_dp                                ! Set initial NM mass to 0 [kg]
        me%m_np_perc = 0.0_dp                           ! Just to be on the safe side
        me%m_np_eroded = 0.0_dp
        me%m_transformed = 0.0_dp
        me%m_transformed_perc = 0.0_dp
        me%m_transformed_eroded = 0.0_dp
        me%m_dissolved = 0.0_dp
        me%C_np = 0.0_dp
        me%C_transformed = 0.0_dp
        me%C_dissolved = 0.0_dp
        me%V_w = 0.0_dp                                 ! Set initial water content to 0 [m3/m2]

        ! Parse the input data into the object properties
        r = me%parseInputData()

        ! Set saturation and field capacity volumes [m3/m2] based on depth of layer
        me%V_sat = WC_sat*me%depth
        me%V_FC = WC_FC*me%depth
        me%K_s = K_s                                    ! Hydraulic conductivity [m/s]


        ! Allocate and create the Biota object
        ! TODO move all this to database
        allocate(me%biotaIndices(0))
        if (DATASET%hasBiota) then
            do i = 1, DATASET%nBiota
                if (trim(DATASET%biotaCompartment(i)) == 'soil') then
                    me%nBiota = me%nBiota + 1
                    me%biotaIndices = [me%biotaIndices, i]
                end if
            end do
        end if
        allocate(me%biota(me%nBiota))
        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%create(me%biotaIndices(i)))
        end do

        ! Add this procedure to error traces
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Update the `SoilLayer1` on a given time step, based on specified inflow.
    !! Calculate percolation to next layer and, if saturated, the amount
    !! to pool to the above layer (or surface runoff, if this is the top layer)
    function updateSoilLayer1(me, t, q_in, m_np_in, m_transformed_in, m_dissolved_in) result(r)
        class(SoilLayer1) :: me                         !! This `SoilLayer1` instance
        integer :: t                                    !! The current time step [s]
        real(dp) :: q_in                                !! Water into the layer on this time step, from percolation and pooling [m/timestep]
        real(dp) :: m_np_in(:,:,:)                      !! NM into the layer on this time step, from percolation and pooling [kg/timestep]
        real(dp) :: m_transformed_in(:,:,:)             !! Transformed NM into the layer on this time step [kg/timestep]
        real(dp) :: m_dissolved_in                      !! Dissolved species into the layer on this time step [kg/timestep]
        type(Result) :: r                               !! The Result object to return any errors in
        real(dp) :: initial_V_w                         ! Initial V_w used for checking whether all water removed
        integer :: i, j, k                              ! Iterators
        type(datetime)      :: currentDate              ! Current date
        real                :: T_water_t                ! Water temperature on the current timestep [deg C]

        ! Get the current date to use to get the water temperature
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = DATASET%waterTemperature(currentDate%yearday())

        ! Set the inflow to this SoilLayer and store initial water in layer
        me%q_in = q_in
        initial_V_w = me%V_w

        ! Add in the NM from the above layer/source
        me%m_np = me%m_np + m_np_in                             ! [kg]
        me%m_transformed = me%m_transformed + m_transformed_in  ! [kg]
        me%m_dissolved = me%m_dissolved + m_dissolved_in        ! [kg]

        ! Attachment
        call me%attachment(T_water_t)                           ! Transfers free -> attached

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
        me%V_perc = min(me%V_excess * &                          
                        (1-exp(-C%timeStep*me%K_s/(me%V_sat-me%V_FC))), &   ! Up to a maximum of V_w
                        me%V_w)
        ! Use this to calculate the amount of nanomaterial percolated as a fraction of that in layer,
        ! then remove this and the water. Check if (near) zero to avoid FPE.
        me%m_np_perc = 0.0_dp
        me%m_transformed_perc = 0.0_dp
        me%m_dissolved_perc = 0.0_dp
        if (.not. isZero(me%V_perc)) then
            ! Only free (porewater) particles will percolate, otherise m_np_perc is 0 (as set above)
            me%m_np_perc(:,1,1) = (me%V_perc/me%V_w)*me%m_np(:,1,1)     ! V_perc/V_w will be 1 at maximum
            me%m_transformed_perc(:,1,1) = (me%V_perc/me%V_w)*me%m_transformed(:,1,1)
            me%m_dissolved_perc = (me%V_perc/me%V_w)*me%m_dissolved          ! All dissolved percolates
        end if
        me%m_np = me%m_np - me%m_np_perc                        ! Get rid of percolated NM
        me%m_transformed = me%m_transformed - me%m_transformed_perc     ! Get rid of percolated transformed NM
        me%m_dissolved = me%m_dissolved - me%m_dissolved_perc   ! Get rid of dissovled species
        me%V_w = me%V_w - me%V_perc                             ! Get rid of the percolated water
        me%C_np = divideCheckZero(me%m_np, me%depth * me%area * me%bulkDensity)
        me%C_transformed = divideCheckZero(me%m_transformed, (me%depth * me%area * me%bulkDensity))
        me%C_dissolved = divideCheckZero(me%m_dissolved, (me%depth * me%area * me%bulkDensity))

        ! Emit a warning if all water removed
        if (isZero(me%V_w) .and. initial_V_w > 0) then
            call r%addError(ErrorInstance(600, isCritical=.false.))
        end if

        ! Update the biota
        do i = 1, me%nBiota
            call r%addErrors(.errors. me%biota(i)%update( &
                t, &
                me%C_np, &
                me%C_transformed, &
                me%C_dissolved &
            ))
        end do

        ! Add this procedure to the error trace
        call r%addToTrace("Updating " // trim(me%ref) // " on time step #" // trim(str(t)))
    end function

    !> Add a volume \( V_{\text{pool}} \) of pooled water to the layer.
    !! No percolation occurs as pooled water never really leaves the `SoilLayer`.
    function addPooledWaterSoilLayer1(me, V_pool) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        real(dp) :: V_pool                              !! Volume of pooled water to add, \( V_{\text{pool}} \) [m3/m2]
        type(Result) :: r                               !! The Result object to return, with no data

        me%V_pool = max(me%V_w + V_pool - me%V_sat, 0.0)  ! Will the input pooled water result in pooled water for this layer?
        me%V_w = min(me%V_w + V_pool, me%V_sat)         ! Add pooled water, up to a maximum of V_sat
    end function

    !> TODO move this to a Reactor of some sort
    subroutine attachmentSoilLayer1(me, T_water_t)
        class(SoilLayer1)   :: me
        real                :: T_water_t
        integer             :: i
        real(dp)            :: dm_att

        if (C%includeAttachment) then
            do i = 1, C%nSizeClassesNM
                ! Has attachment rate been set by input data? If not, then calculate it from
                ! attachment efficiency. Attachment efficiency will be either spatial (if
                ! provided), or set by default value in constants file. If it is calculated here,
                ! it is temporal as it depends on water temperature
                if (me%k_att(1) == nf90_fill_real) then
                    me%k_att = me%calculateAttachmentRate(T_water_t)
                end if
                ! Pristine NM
                dm_att = min(me%k_att(i)*C%timeStep*me%m_np(i,1,1), me%m_np(i,1,1))     ! Mass to move from free -> attached, max of the current mass
                me%m_np(i,1,1) = me%m_np(i,1,1) - dm_att                                ! Remove from free
                me%m_np(i,1,2) = me%m_np(i,1,2) + dm_att                                ! Add to attached (bound)
                ! Same for transformed NM
                dm_att = min(me%k_att(i)*C%timeStep*me%m_transformed(i,1,1), me%m_transformed(i,1,1))
                me%m_transformed(i,1,1) = me%m_transformed(i,1,1) - dm_att
                me%m_transformed(i,1,2) = me%m_transformed(i,1,2) + dm_att
            end do
        end if
    end subroutine

    !> Erode NM from this soil layer
    !! TODO bulk density could be stored in this object, not passed, probably same with area
    function erodeSoilLayer1(me, erodedSediment, bulkDensity, area) result(r)
        class(SoilLayer1)   :: me
        real(dp)            :: erodedSediment(:)        ! [kg/m2/day] TODO make sure changed to kg/m2/timestep
        real(dp)            :: bulkDensity              ! [kg/m3]
        real(dp)            :: area                     ! [m2]
        type(Result)        :: r
        real(dp)            :: m_soil_l1
        real(dp)            :: propEroded
        real(dp)            :: erodedNP(C%nSizeClassesNM)
        
        m_soil_l1 = bulkDensity * area * me%depth           ! Calculate the mass of the soil in this soil layer
        propEroded = sum(erodedSediment)*area/m_soil_l1     ! Proportion of this that is eroded, convert erodedSediment to kg/gridcell/day
        ! Pristine NM
        erodedNP = me%m_np(:,1,2)*propEroded                ! Only erode attached NM
        me%m_np(:,1,2) = me%m_np(:,1,2) - erodedNP          ! Remove the eroded NM from the layer
        me%m_np_eroded(:,1,2) = erodedNP
        ! Transformed NM
        erodedNP = me%m_transformed(:,1,2)*propEroded
        me%m_transformed(:,1,2) = me%m_transformed(:,1,2) - erodedNP
        me%m_transformed_eroded(:,1,2) = erodedNP
    end function

    function calculateBioturbationRateSoilLayer1(me) result(bioturbationRate)
        class(SoilLayer1) :: me
        real(dp) :: bioturbationRate
        real(dp) :: earthwormDensity_perVolume              ! [individuals/m3]
        real(dp) :: bioturb_alpha = 3.56e-9                ! Bioturbation fitting parameter [m4/s]
        ! Convert from worms/layer to worms/m3 for the bioturbation model
        earthwormDensity_perVolume = me%earthwormDensity * me%depth
        ! Calculate the bioturbation rate based on this worm density
        bioturbationRate = (earthwormDensity_perVolume * bioturb_alpha) / me%depth
    end function

    !> Calculate the attachment rate from the attachment efficiency and soil properties, using
    !! coloid filtration theory. References:
    !!  - Meesters et al. 2014 (SI): https://doi.org/10.1021/es500548h
    !!  - Tufenkji et al. 2004: https://doi.org/10.1021/es034049r
    function calculateAttachmentRateSoilLayer1(me, T_water_t) result(k_att)
        class(SoilLayer1)   :: me
        real                :: T_water_t
        real                :: k_att(C%nSizeClassesNM)
        integer             :: i
        real(dp) :: gamma, r_i, kBT, N_G, N_VDW, N_Pe, N_R, A_s, eta_grav, eta_intercept, &
            eta_0, lambda_filter, D_i, eta_Brownian

        gamma = (1 - me%porosity) ** 0.333
        kBT = C%k_B * (T_water_t + 273.15)
        N_VDW = DATASET%soilHamakerConstant / kBT                                       ! Van der Waals number
        A_s = 2 * (1 - gamma**5) / (2 - 3 * gamma + 3 * gamma**5 - 2 * gamma**6)        ! Porosity dependent param
        ! Loop through NM size classes for the parameters that are dependent on NM size
        do i = 1, C%nSizeClassesNM
            r_i = C%d_nm(i) * 0.5                                                       ! NM radius
            D_i = kBT / (6 * C%pi * C%mu_w(T_water_t) * r_i)             ! Diffusivity of NM particle
            N_Pe = DATASET%soilDarcyVelocity * me%d_grain / D_i                         ! Peclet number
            N_G = 2 * r_i**2 * (DATASET%soilParticleDensity - C%rho_w(T_water_t)) * C%g &
                / (9 * C%mu_w(T_water_t) * DATASET%soilDarcyVelocity)    ! Gravity number
            N_R = r_i / (me%d_grain * 0.5)                                              ! Aspect ratio number
            eta_grav = 2.22 * N_R**(-0.024) * N_G**1.11 * N_VDW**0.053                  ! Gravitational collection efficiency
            eta_intercept = 0.55 * N_R**1.55 * N_Pe**(-0.125) * N_VDW**0.125            ! Interception collection efficiency
            eta_Brownian = 2.4 * A_s**0.33 * N_R**(-0.081) * N_Pe**(-0.715) * N_VDW**0.053 ! Brownian motion collection efficiency
            eta_0 = eta_grav + eta_intercept + eta_Brownian                             ! Total collection efficiency
            lambda_filter = 1.5 * (1 - me%porosity) / (me%d_grain * me%porosity)        ! Filtration
            k_att(i) = me%alpha_att * lambda_filter * eta_0 * DATASET%soilDarcyVelocity ! Attachment rate [/s]
        end do
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data
    function parseInputDataSoilLayer1(me) result(r)
        class(SoilLayer1) :: me         !! This SoilLayer1 instance
        type(Result) :: r               !! The Result object to return any errors relating to the input data file
        me%k_att(:) = DATASET%soilAttachmentRate(me%x, me%y)
        me%alpha_att = DATASET%soilAttachmentEfficiency(me%x, me%y)
    end function

    subroutine parseNewBatchDataSoilLayer1(me)
        class(SoilLayer1) :: me
        me%k_att(:) = DATASET%soilAttachmentRate(me%x, me%y)
        me%alpha_att = DATASET%soilAttachmentEfficiency(me%x, me%y)
    end subroutine

end module
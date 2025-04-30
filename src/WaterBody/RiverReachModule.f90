!> Module containing RiverReach type definition.
module RiverReachModule
    use GlobalsModule
    use ReachModule
    use UtilModule
    use ResultModule
    use BedSedimentModule
    use LoggerModule, only: LOGR
    use DataInputModule, only: DATASET
    use ReactorModule
    use BiotaWaterModule
    use ContaminantModule
    implicit none

    !> The RiverReach type represents a segment of river within a grid cell
    type, public, extends(Reach) :: RiverReach
    contains
        ! Create
        procedure :: create => createRiverReach
        ! Simulators
        procedure :: update => updateRiverReach
        procedure :: updateDisplacement => updateDisplacementRiverReach
        procedure :: setDimensions
        ! Data handlers
        procedure :: parseInputData => parseInputDataRiverReach
        procedure :: updateSources
        ! Calculators
        procedure :: calculateWidth => calculateWidth
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
        procedure :: finalise => finaliseRiverReach
    end type

contains

    !> Create this RiverReach with the provided grid cell and waterbody indices (x, y, w)
    !! and sediment size class distribution
    function createRiverReach(me, x, y, w, distributionSediment) result(rslt)
        class(RiverReach), intent(inout) :: me                     !! This `RiverReach` instance
        integer, intent(in) :: x                                   !! Grid cell x-position index
        integer, intent(in) :: y                                   !! Grid cell y-position index
        integer, intent(in) :: w                                   !! Water body index within the cell
        real(dp), intent(in) :: distributionSediment(C%nSizeClassesSPM) !! Distribution to split sediment across size classes
        type(Result) :: rslt                                       !! Result object to return errors in
        integer :: i                                               ! Iterator

        ! Set reach references (indices set in WaterBody%create) and grid cell area.
        ! Diffuse and point sources are created in WaterBody%create
        call rslt%addErrors(.errors. me%WaterBody%create(x, y, w, distributionSediment))
        me%ref = trim(ref("RiverReach", x, y, w))

        ! Parse input data and allocate/initialise variables. The order here is important:
        ! allocation depends on the input data.
        call rslt%addErrors(.errors. me%parseInputData())
        call rslt%addErrors(.errors. me%m_contaminant%create())
        if (allocated(DATASET%initialContaminantConcsWater)) then
            me%m_contaminant%c = DATASET%initialContaminantConcsWater(me%x, me%y, :, :, :)
            if (allocated(DATASET%initialDissolvedConcsWater)) then
                me%m_contaminant%m_dissolved = DATASET%initialDissolvedConcsWater(me%x, me%y)
            end if
        end if

        ! Create the BedSediment for this RiverReach
        allocate(BedSediment :: me%bedSediment)
        allocate(Reactor :: me%reactor)
        call rslt%addErrors([ &
            .errors. me%bedSediment%create(me%x, me%y, me%w), &
            .errors. me%reactor%create( &
                me%x, me%y, merge('estuary', 'water  ', DATASET%isEstuary(me%x, me%y)), &
                me%m_contaminant, me%volume, &
                DATASET%waterTemperature(C%startDate%yearday()), &
                C_spm=me%C_spm, W_settle_spm=me%W_settle_spm, &
                G=DATASET%shearRate, velocity=me%velocity) &
        ])

        ! Allocate and create the correct number of biota objects for this reach
        allocate(me%biotaIndices(0))
        if (DATASET%hasBiota) then
            do i = 1, DATASET%nBiota
                if (trim(DATASET%biotaCompartment(i)) == 'water') then
                    me%nBiota = me%nBiota + 1
                    me%biotaIndices = [me%biotaIndices, i]
                end if
            end do
        end if

        allocate(me%biota(me%nBiota))
        do i = 1, me%nBiota
            call rslt%addErrors(.errors. me%biota(i)%create(me%biotaIndices(i)))
        end do
        
        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOGR%toFile("Creating " // trim(me%ref) // ": success")
    end function

    subroutine updateSources(me, t)
        class(RiverReach) :: me
        integer :: t
        integer :: p
        type(Result) :: rslt
        type(Contaminant) :: temp_contaminant

        call rslt%addErrors(.errors. me%j_contaminant_pointSources%create())
        do p = 1, DATASET%maxPointSources
            call rslt%addErrors(.errors. temp_contaminant%create())
            if (allocated(DATASET%emissionsPointWaterContaminant)) then
                temp_contaminant%c = DATASET%emissionsPointWaterContaminant(me%x, me%y, t, p, :, :, :)
            else
                temp_contaminant%c = 0.0_dp
            end if
            if (allocated(DATASET%emissionsPointWaterDissolvedContaminant)) then
                temp_contaminant%m_dissolved = DATASET%emissionsPointWaterDissolvedContaminant(me%x, me%y, t)
            else
                temp_contaminant%m_dissolved = 0.0_dp
            end if
            call me%j_contaminant_pointSources%add(temp_contaminant)
            call temp_contaminant%finalise()
        end do

        call rslt%addErrors(.errors. me%j_contaminant_diffuseSources%create())
        call rslt%addErrors(.errors. temp_contaminant%create())
        if (allocated(DATASET%emissionsArealWaterContaminant)) then
            temp_contaminant%c = DATASET%emissionsArealWaterContaminant(me%x, me%y, :, :, :)
        else
            temp_contaminant%c = 0.0_dp
        end if
        if (allocated(DATASET%emissionsArealWaterDissolvedContaminant)) then
            temp_contaminant%m_dissolved = DATASET%emissionsArealWaterDissolvedContaminant(me%x, me%y)
        else
            temp_contaminant%m_dissolved = 0.0_dp
        end if
        call me%j_contaminant_diffuseSources%add(temp_contaminant)
        call temp_contaminant%finalise()

        call rslt%addToTrace("Updating sources for " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    !> Run the river reach simulation for this timestep
    subroutine updateRiverReach(me, t, q_runoff, q_overland, j_spm_runoff, j_contaminant_runoff, &
                                contributingArea, isWarmUp)
        class(RiverReach), intent(inout) :: me                     !! This `RiverReach` instance
        integer, intent(in) :: t                                   !! The current timestep
        real(dp), intent(in) :: q_runoff                           !! Runoff from the hydrological model [m3/m2/timestep]
        real(dp), intent(in) :: q_overland                         !! Overland runoff [m3/m2/timestep]
        real(dp), intent(in) :: j_spm_runoff(:)                    !! Eroded sediment runoff to this reach [kg/timestep]
        type(Contaminant), intent(in) :: j_contaminant_runoff      !! Contaminant runoff to this reach [kg/timestep]
        real(dp), intent(in) :: contributingArea                   !! Area contributing to this reach (e.g. the soil profile) [m2]
        logical, intent(in) :: isWarmUp                            !! Are we in a warm up period?
        type(Result) :: rslt                                       ! Result object to store errors in
        integer :: i                                               ! Iterator
        integer :: nDisp                                           ! Number of displacements to split this time step into
        real(dp) :: dt                                             ! Length of each displacement [s]
        real(dp) :: dQ                                             ! Water flow for each displacement
        real(dp) :: dj_spm(C%nSizeClassesSpm)                      ! SPM inflows for each displacement
        type(Contaminant) :: dj_contaminant_in
        type(datetime) :: currentDate                              ! The current timestep's date
        real(dp) :: T_water_t
        type(Contaminant) :: c_env_contaminant
      
        ! Reset all flows to zero, which is needed as flows are added to iteratively in the displacement loop
        call me%emptyFlows()

        ! Get the current date and use the day of year to get the water temp
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = me%T_water(currentDate%yearday())

        ! Get the inflows from upstream water bodies
        do i = 1, me%nInflows
            me%Q%inflow = me%Q%inflow - me%inflows(i)%item%Q%outflow
            me%j_spm%inflow = me%j_spm%inflow - me%inflows(i)%item%j_spm%outflow
            call me%j_contaminant_inflow%add(me%inflows(i)%item%get_j_contaminant_outflow())
        end do
        ! Get the inflows from runoff and scale to this reach, then use this to set dimensions
        me%Q%runoff = q_runoff * contributingArea
        me%Q_in_total = me%Q%inflow + me%Q%runoff
        call me%setDimensions(t)

        ! Set the erosion yields, with includes scaling the soil erosion by sediment transport
        ! capacity, calculating the bank erosion, and storing these in the flow objects
        call me%setErosionYields(j_spm_runoff, q_overland, contributingArea, j_contaminant_runoff)

        if (.not. C%ignoreContaminant .and. .not. isZero(me%Q_in_total)) then
            call me%updateSources(t)
        end if

        call me%setResuspensionRate(me%Q_in_total / C%timeStep, T_water_t)
        call me%setSettlingRate(T_water_t)

        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = 1
        else
            nDisp = ceiling(me%Q_in_total / me%volume)
        end if
        dt = C%timestep / nDisp
        dQ = me%Q_in_total / nDisp
        dj_spm = (me%j_spm%inflow + me%j_spm%soilErosion + me%j_spm%bankErosion) / nDisp
        call rslt%addErrors(.errors. dj_contaminant_in%create())
        call dj_contaminant_in%add_scaled(me%j_contaminant_inflow, 1.0_dp/nDisp)
        call dj_contaminant_in%add_scaled(me%j_contaminant_soilErosion, 1.0_dp/nDisp)
        call dj_contaminant_in%add_scaled(me%j_contaminant_pointSources, 1.0_dp/nDisp)
        call dj_contaminant_in%add_scaled(me%j_contaminant_diffuseSources, 1.0_dp/nDisp)

        do i = 1, nDisp
            call me%updateDisplacement(t, i, dt, dQ, dj_spm, dj_contaminant_in, T_water_t)
        end do

        call dj_contaminant_in%finalise()

        ! Set the final concentrations based on the calculated masses [kg/m3]
        me%C_spm = divideCheckZero(me%m_spm, me%volume)
        if (.not. C%ignoreContaminant .and. .not. isZero(me%volume)) then
            call rslt%addErrors(.errors. me%reactor%update(dj_contaminant_in, dt))
            me%m_contaminant = me%reactor%contaminant
        end if

        ! Update the biota
        do i = 1, me%nBiota
            c_env_contaminant = me%m_contaminant%divideCheckZero(me%volume)
            call rslt%addErrors(.errors. me%biota(i)%update(t, c_env_contaminant))
            call c_env_contaminant%finalise()
        end do

        ! Add what we're doing here to the error trace and trigger any errors there are
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    !> Run the simulation for an individual time displacement
    subroutine updateDisplacementRiverReach(me, t, d, dt, dQ, dj_spm_in, dj_contaminant_in, T_water_t)
        class(RiverReach)  :: me                                                !! This reach
        integer             :: t                                                !! Current timestep index (used for error output) 
        integer             :: d                                                !! Current time displacement index (used for error output)
        real(dp)            :: dt                                               !! Time displacement [s] 
        real(dp)            :: dQ                                               !! Water flow from runoff and inflows [m3/displacement]
        real(dp)            :: dj_spm_in(C%nSizeClassesSPM)                     !! SPM inflow from erosion and inflows [kg/displacement]
        type(Contaminant)   :: dj_contaminant_in
        real(dp)            :: T_water_t
        real(dp)            :: dj_spm_resus(C%nSizeClassesSPM)
        real(dp)            :: dj_spm_resus_perArea(C%nSizeClassesSpm)
        real(dp)            :: dj_spm_resus_perArea_(C%nSizeClassesSpm)
        type(Result)        :: rslt
        real(dp)            :: dj_spm_deposit(C%nSizeClassesSPM)
        real(dp)            :: dj_spm_outflow(C%nSizeClassesSPM)
        integer             :: i
        real(dp)            :: k_outflow
        type(Contaminant)   :: dj_contaminant_deposit
        type(Contaminant)   :: dj_contaminant_resus
        type(Contaminant)   :: dj_contaminant_outflow
        type(Result0D)      :: res_contaminant
        type(Contaminant)   :: m_contaminant

        call rslt%addErrors(.errors. dj_contaminant_outflow%create())
        call rslt%addErrors(.errors. dj_contaminant_deposit%create())
        call rslt%addErrors(.errors. dj_contaminant_resus%create())

        if (.not. isZero(me%volume)) then
            k_outflow = dQ / me%volume
            dj_spm_outflow = min(flushToZero(me%m_spm * k_outflow), me%m_spm)
            dj_spm_deposit = flushToZero((me%m_spm + dj_spm_in) * me%k_settle * dt)
            dj_spm_deposit = min(dj_spm_deposit, me%m_spm + dj_spm_in)

            dj_spm_resus_perArea = flushToZero(me%k_resus * me%bedSediment%Mf_bed_by_size() * dt)
            dj_spm_resus_perArea_ = dj_spm_resus_perArea
            call rslt%addErrors(.errors. me%bedSediment%resuspend(dj_spm_resus_perArea_))
            dj_spm_resus_perArea = dj_spm_resus_perArea - dj_spm_resus_perArea_
            dj_spm_resus = dj_spm_resus_perArea * me%bedArea

            call rslt%addErrors(.errors. me%depositToBed(dj_spm_deposit))

            me%Q%outflow = me%Q%outflow - dQ
            me%j_spm%resuspension = me%j_spm%resuspension + dj_spm_resus
            me%j_spm%deposition = me%j_spm%deposition - dj_spm_deposit
            me%j_spm%outflow = me%j_spm%outflow - dj_spm_outflow
            me%m_spm = flushToZero(max(me%m_spm + dj_spm_in + dj_spm_resus - dj_spm_deposit - dj_spm_outflow, 0.0_dp))

            if (.not. C%ignoreContaminant) then
                call dj_contaminant_outflow%multiply_scalar(me%m_contaminant, k_outflow)
                call dj_contaminant_deposit%multiply_scalar(me%m_contaminant, sum(me%k_settle * dt))
                res_contaminant = me%bedSediment%get_m_contaminant()
                if (res_contaminant%hasError()) then
                    call rslt%addErrors(res_contaminant%getErrors())
                    call LOGR%toFile(errors = .errors. rslt)
                    call ERROR_HANDLER%trigger(errors = .errors. rslt)
                    return
                end if
                select type (data => res_contaminant%getData())
                    type is (Contaminant)
                        m_contaminant = data
                    class default
                        call rslt%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                        call LOGR%toFile(errors = .errors. rslt)
                        call ERROR_HANDLER%trigger(errors = .errors. rslt)
                        return
                end select
                call dj_contaminant_resus%multiply_scalar(m_contaminant, sum(me%k_resus * dt))

                call me%j_contaminant_deposition%add_scaled(dj_contaminant_deposit, -1.0_dp)
                call me%j_contaminant_resuspension%add(dj_contaminant_resus)
                call me%j_contaminant_outflow%add_scaled(dj_contaminant_outflow, -1.0_dp)
                call me%m_contaminant%add(dj_contaminant_in)
                call me%m_contaminant%add_scaled(dj_contaminant_deposit, -1.0_dp)
                call me%m_contaminant%add(dj_contaminant_resus)
                call me%m_contaminant%add_scaled(dj_contaminant_outflow, -1.0_dp)
                call rslt%addErrors(.errors. me%bedSediment%transferContaminant(dj_contaminant_deposit))
            end if
        else
            me%m_spm = 0.0_dp
            call rslt%addErrors(.errors. me%m_contaminant%create())
            call me%m_contaminant%finalise()
        end if

        call dj_contaminant_outflow%finalise()
        call dj_contaminant_deposit%finalise()
        call dj_contaminant_resus%finalise()

        call rslt%addToTrace("Updating time displacement #" // trim(str(d)))
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    !> Set the dimensions (width, depth, areas, volume) of the reach
    subroutine setDimensions(me, t)
        class(RiverReach)   :: me       !! This reach
        integer             :: t        !! The current timestep index
        ! Calculate the width [m], depth [m], cross-section, bed and surface areas [m2] and volume [m3]
        me%width = me%calculateWidth(me%Q_in_total/C%timeStep)
        me%depth = me%calculateDepth(me%width, me%slope, me%Q_in_total/C%timeStep, t)
        me%xsArea = me%depth*me%width
        me%bedArea = me%width*me%length*me%f_m
        me%surfaceArea = me%bedArea                      ! For river reaches, set surface area equal to bed area [m2]
        me%volume = me%depth*me%width*me%length*me%f_m
        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)
    end subroutine

    !> Parse data from the input file for this river reach
    function parseInputDataRiverReach(me) result(rslt)
        class(RiverReach), intent(inout) :: me
        type(Result) :: rslt

        me%f_m = DATASET%riverMeanderingFactor
        me%alpha_hetero = merge(DATASET%estuaryAttachmentEfficiency, DATASET%riverAttachmentEfficiency, &
                                DATASET%isEstuary(me%x, me%y))
        me%alpha_resus = DATASET%resuspensionAlpha(me%x, me%y)
        me%beta_resus = DATASET%resuspensionBeta(me%x, me%y)
        me%a_stc = DATASET%sedimentTransport_a(me%x, me%y)
        me%b_stc = DATASET%sedimentTransport_b(me%x, me%y)
        me%c_stc = DATASET%sedimentTransport_c(me%x, me%y)
        me%T_water = DATASET%waterTemperature

        call rslt%addErrors(.errors. me%parseInflowsAndOutflow())
        call me%setReachLengthAndSlope()

        call rslt%addToTrace('Parsing input data')
    end function
    
    !> Calculate the width \( W \) of the river based on the discharge:
    !! $$
    !!      W = 1.22Q^{0.557}
    !! $$
    !! References:
    !! <ul>
    !!  <li>[Dumont et al., 2012](https://doi.org/10.1080/02626667.2012.715747)</li>
    !!  <li>[Allen et al., 1994](https://doi.org/10.1111/j.1752-1688.1994.tb03321.x)</li>
    !! </ul>
    function calculateWidth(me, Q) result(width)
        class(RiverReach), intent(in) :: me     !! The `RiverReach` instance
        real(dp), intent(in) :: Q               !! `GridCell` discharge \( Q \) [m3/s]
        real(dp) :: width                       !! The calculated width \( W \) [m]
        width = 1.22*Q**0.557
    end function

    !> Calculate water depth from Manning's roughness coefficient,
    !! using Newton's method:
    !! $$
    !!      D_i = D_{i-1} - \frac{f(D_{i-1})}{f'(D_{i-1})}
    !! $$
    !! where
    !! $$
    !!      f(D) = WD \left( \frac{WD}{W+2D} \right)^{2/3} \frac{\sqrt{S}}{n} - Q = 0
    !! $$
    !! and
    !! $$
    !!      f'(D) = \frac{\sqrt{S}}{n} \frac{(DW)^{5/3}(6D + 5W)}{3D(2D + W)^{5/3}}
    !! $$
    function calculateDepth(me, W, S, Q, t) result(D_i)
        class(RiverReach), intent(in) :: me     !! The `RiverReach` instance.
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        real(dp), intent(in) :: S               !! River slope \( S \) [-].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m3/s].
        integer             :: t                !! Timestep index
        real(dp) :: D_i                         !! The iterative river depth \( D_i \) [m].
        type(Result0D) :: rslt                  ! The Result object to store numerical errors in
        real(dp) :: f                           ! The function to find roots for \( f(D) \).
        real(dp) :: df                          ! The derivative of \( f(D) \) with respect to \( D \).
        real(dp) :: alpha                       ! Constant extracted from f and df
        integer :: i                            ! Loop iterator to make sure loop isn't endless.
        integer :: iMax                         ! Maximum number of iterations before error.
        real(dp) :: epsilon                     ! Proximity to zero allowed.

        ! TODO: Allow user (e.g., data file) to specify max iterations and precision?
        D_i = 1.0_dp                                                            ! Take a guess at D being 1m to begin
        i = 1                                                                   ! Iterator for Newton solver
        iMax = 100000                                                           ! Allow 10000 iterations
        epsilon = 1.0e-9_dp                                                     ! Proximity to zero allowed
        alpha = W**(5.0_dp/3.0_dp) * sqrt(S)/me%n                               ! Extract constant to simplify f and df.
        f = alpha*D_i*((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                    ! First value for f, based on guessed D_i

        ! Loop through and solve until f(D) is within e-9 of zero, or max iterations reached
        do while (abs(f) > epsilon .and. i <= iMax)
            f = alpha * D_i * ((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q            ! f(D) based on D_{m-1}
            df = alpha * ((D_i)**(5.0_dp/3.0_dp) * (6*D_i + 5*W))/(3*D_i * (2*D_i + W)**(5.0_dp/3.0_dp))
            D_i = D_i - f/df                                                    ! Calculate D_i based on D_{m-1}
            i = i + 1
        end do

        ! If method diverged (results in NaN)
        if (isnan(D_i)) then
            call rslt%addError(ErrorInstance( &
                message="Newton's method diverged to NaN after " // trim(str(i)) // " iterations."))
        ! If max number of iterations reached        
        else if (i > iMax) then
            call rslt%addError(ErrorInstance( &
                message="Newton's method failed to converge - maximum number of iterations (" &
                    // trim(str(i)) // ") exceeded. Precision (proximity to zero) required: " &
                    // trim(str(epsilon)) // ". Final value: " // trim(str(f)) // "."))
        ! If we got a negative river depth
        else if (D_i < 0.0_dp) then
            call rslt%addError( &
                ErrorInstance(message="Newton's method gave negative river depth. Depth: " // trim(str(D_i))))
        end if

        ! Add what we're doing here to the error trace and trigger any errors there are
        call rslt%addToTrace("Calculating river depth")
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end function
    
    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    function calculateVelocity(me, D, Q, W) result(v)
        class(RiverReach), intent(in) :: me     !! This `RiverReach` instance
        real(dp), intent(in) :: D               !! River depth \( D \) [m]
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m**3/s]
        real(dp), intent(in) :: W               !! River width \( W \) [m]
        real(dp) :: v                           !! The calculated velocity \( v \) [m/s]
        if (isZero(Q) .or. isZero(W) .or. isZero(D)) then
            v = 0.0_dp
        else
            v = Q/(W*D)
        end if
    end function

    subroutine finaliseRiverReach(me)
        class(RiverReach), intent(inout) :: me
        call me%WaterBody%finalise()
        if (allocated(me%biota)) then
            deallocate(me%biota)
        end if
        if (allocated(me%biotaIndices)) then
            deallocate(me%biotaIndices)
        end if
        if (allocated(me%bedSediment)) then
            call me%bedSediment%finalise()
            deallocate(me%bedSediment)
        end if
        if (allocated(me%reactor)) then
            call me%reactor%finalise()
            deallocate(me%reactor)
        end if
    end subroutine
end module
!> Module containing RiverReach type definition.
module RiverReachModule
    use Globals
    use ReachModule
    use UtilModule
    use ResultModule
    use classBedSediment1
    use classLogger, only: LOGR
    use classDatabase, only: DATASET
    use classReactor1
    use classBiotaWater
    implicit none

    !> The RiverReach type represents a segment of river within a grid cell
    type, public, extends(Reach1) :: RiverReach
      contains
        ! Create
        procedure :: create => createRiverReach
        ! Simulators
        procedure :: update => updateRiverReach
        procedure :: updateDisplacement => updateDisplacementRiverReach
        procedure :: setDimensions
        ! Data handlers
        procedure :: parseInputData => parseInputDataRiverReach
        ! Calculators
        procedure :: calculateWidth => calculateWidth
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
    end type

  contains

    !> Create this RiverReach with the provided grid cell and waterbody indices (x, y, w)
    !! and sediment size class distribution
    function createRiverReach(me, x, y, w, distributionSediment) result(rslt)
        class(RiverReach) :: me                 !! This `RiverReach` instance
        integer :: x                            !! Grid cell x-position index
        integer :: y                            !! Grid cell y-position index
        integer :: w                            !! Water body index within the cell
        real(dp) :: distributionSediment(C%nSizeClassesSPM)     !! Distribution to split sediment across size classes
        type(Result) :: rslt                    !! Result object to return errors in
        integer :: i                            ! Iterator

        ! Set reach references (indices set in WaterBody%create) and grid cell area.
        ! Diffuse and point sources are created in WaterBody%create
        call rslt%addErrors(.errors. me%WaterBody1%create(x, y, w, distributionSediment))
        me%ref = trim(ref("RiverReach", x, y, w))

        ! Parse input data and allocate/initialise variables. The order here is important:
        ! allocation depends on the input data.
        call rslt%addErrors(.errors. me%parseInputData())

        ! Create the BedSediment for this RiverReach
        ! TODO: Get the type of BedSediment from the data file, and check for allst
        allocate(BedSediment1 :: me%bedSediment)
        allocate(Reactor1 :: me%reactor)

        ! Allocate and create the correct number of biota objects for this reach
        ! TODO move all this to database
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

        ! Create the bed sediment and reactor
        call rslt%addErrors([ &
            .errors. me%bedSediment%create(me%x, me%y, me%w), &
            .errors. me%reactor%create(me%x, me%y, me%alpha_hetero) &
        ])

        ! Add what we're doing here to the trace and log that creating the reach was successful
        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOGR%toFile("Creating " // trim(me%ref) // ": success")
    end function

    !> Run the river reach simulation for this timestep
    subroutine updateRiverReach(me, t, q_runoff, q_overland, j_spm_runoff, j_np_runoff, j_transformed_runoff, contributingArea)
        class(RiverReach) :: me                                 !! This `RiverReach` instance
        integer :: t                                            !! The current timestep
        real(dp) :: q_runoff                                    !! Runoff from the hydrological model [m3/m2/timestep]
        real(dp) :: q_overland                                  !! Overland runoff [m3/m2/timestep]
        real(dp) :: j_spm_runoff(:)                             !! Eroded sediment runoff to this reach [kg/timestep]
        real(dp) :: j_np_runoff(:,:,:)                          !! Eroded NP runoff to this reach [kg/timestep]
        real(dp) :: j_transformed_runoff(:,:,:)                 !! Eroded transformed NP runoff to this reach [kg/timestep]
        real(dp) :: contributingArea                            !! Area contributing to this reach (e.g. the soil profile) [m2]
        type(Result) :: rslt                                    ! Result object to store errors in
        integer :: i                                            ! Iterator
        integer :: nDisp                                        ! Number of displacements to split this time step into
        real(dp) :: dt                                          ! Length of each displacement [s]
        real(dp) :: dQ                                          ! Water flow for each displacement
        real(dp) :: dj_spm(C%nSizeClassesSpm)                   ! SPM inflows for each displacement
        real(dp) :: dj_nm(C%npDim(1), C%npDim(2), C%npDim(3))   ! NM inflows for each displacement
        real(dp) :: dj_nm_transformed(C%npDim(1), C%npDim(2), C%npDim(3))   ! Transformed NM inflows for each displacement
        real(dp) :: dj_dissolved                                ! Dissolved species inflows for each displacement
        type(datetime) :: currentDate                           ! The current timestep's date
        real :: T_water_t                                       ! Water temperature on this timestep [deg C]

        ! Reset all flows to zero, which is needed as flows are added to iteratively in the displacement loop
        call me%emptyFlows()

        ! Get the current date and use the day of year to get the water temp
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = me%T_water(currentDate%yearday())

            
        ! Get the inflows from upstream water bodies
        do i = 1, me%nInflows
            me%Q%inflow = me%Q%inflow - me%inflows(i)%item%Q%outflow
            me%j_spm%inflow = me%j_spm%inflow - me%inflows(i)%item%j_spm%outflow
            me%j_nm%inflow = me%j_nm%inflow - me%inflows(i)%item%j_nm%outflow
            me%j_nm_transformed%inflow = me%j_nm_transformed%inflow - me%inflows(i)%item%j_nm_transformed%outflow
            me%j_dissolved%inflow = me%j_dissolved%inflow - me%inflows(i)%item%j_dissolved%outflow
        end do

        ! Get the inflows from runoff and scale to this reach, then use this to set dimensions
        me%Q%runoff = q_runoff * contributingArea
        me%Q_in_total = me%Q%inflow + me%Q%runoff
        call me%setDimensions(t)

        ! Set the erosion yields, with includes scaling the soil erosion by sediment transport
        ! capacity, calculating the bank ersoion, and storing these in the flow objects
        call me%setErosionYields(j_spm_runoff, q_overland, contributingArea, j_np_runoff, j_transformed_runoff)

        ! TODO transfers and demands

        if (.not. C%ignoreNM) then
            ! Inflows from point and diffuse sources, updates the NM flow object
            call me%updateSources(t)
        end if

        ! Set the resuspension and settling rates [/s] (but don't settle until we're looping through displacements) 
        call me%setResuspensionRate(me%Q_in_total / C%timeStep, T_water_t)
        call me%setSettlingRate(T_water_t)

        ! If the total inflow for this timestep is bigger than the current reach volume, 
        ! then we need to split into a number of time displacements
        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = 1
        else
            nDisp = ceiling(me%Q_in_total / me%volume)
        end if
        dt = C%timestep / nDisp
        dQ = me%Q_in_total / nDisp
        dj_SPM = (me%j_spm%inflow + me%j_spm%soilErosion + me%j_spm%bankErosion) / nDisp
        if (.not. C%ignoreNM) then
            dj_NM = (me%j_nm%inflow + me%j_nm%soilErosion + me%j_nm%pointSources & 
                    + me%j_nm%diffuseSources) / nDisp
            dj_NM_transformed = (me%j_nm_transformed%inflow + me%j_nm_transformed%soilErosion &
                                + me%j_nm_transformed%pointSources + me%j_nm_transformed%diffuseSources) / nDisp
            dj_dissolved = (me%j_dissolved%inflow + me%j_dissolved%pointSources &
                            + me%j_dissolved%diffuseSources) / nDisp
        end if

        ! Now we can run the simulation for each time displacement, with calculates SPM and NM outflow,
        ! deposition and resuspension, and updates the flow objects accordingly
        do i = 1, nDisp
            call me%updateDisplacement(t, i, dt, dQ, dj_SPM, dj_NM, dj_nm_transformed, dj_dissolved)
        end do

        ! Set the new concentrations
        me%C_spm = divideCheckZero(me%m_spm, me%volume)
        ! Only if we're not ignoring NM
        if (.not. C%ignoreNM) then
            me%C_np = divideCheckZero(me%m_np, me%volume)
            me%C_transformed = divideCheckZero(me%m_transformed, me%volume)
            me%C_dissolved = divideCheckZero(me%m_dissolved, me%volume)

            ! Now we pass the NM to the reactor to update
            call rslt%addErrors([.errors. &
                me%reactor%update( &
                    t, &
                    me%m_np, &
                    me%m_transformed, &
                    me%m_dissolved, &
                    me%C_spm, &
                    T_water_t, &
                    me%W_settle_np, &
                    me%W_settle_spm, &
                    DATASET%shearRate, &
                    me%volume &
                ) &
            ])
            ! Get the resultant masses from the Reactor
            me%m_np = me%reactor%m_np
            me%m_transformed = me%reactor%m_transformed
            me%m_dissolved = me%reactor%m_dissolved
        end if

        ! Set the final concentrations based on the calculated mases [kg/m3]
        me%C_np = divideCheckZero(me%m_np, me%volume)
        me%C_transformed = divideCheckZero(me%m_transformed, me%volume)
        me%C_dissolved = divideCheckZero(me%m_dissolved, me%volume)

        ! Update the biota
        do i = 1, me%nBiota
            call rslt%addErrors(.errors. me%biota(i)%update( &
                t, &
                me%C_np, &
                me%C_transformed, &
                me%C_dissolved &
            ))
        end do

        ! Add what we're doing here to the error trace and trigger any errors there are
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    !> Run the simulation for an individual time displacement
    subroutine updateDisplacementRiverReach(me, t, d, dt, dQ, dj_spm_in, dj_nm_in, dj_nm_transformed_in, dj_dissolved_in)
        class(RiverReach)  :: me                                                !! This reach
        integer             :: t                                                !! Current timestep index (used for error output) 
        integer             :: d                                                !! Current time displacement index (used for error output)
        real(dp)            :: dt                                               !! Time displacement [s] 
        real(dp)            :: dQ                                               !! Water flow from runoff and inflows [m3/displacement]
        real(dp)            :: dj_spm_in(C%nSizeClassesSPM)                     !! SPM inflow from erosion and inflows [kg/displacement]
        real(dp)            :: dj_nm_in(C%npDim(1), C%npDim(2), C%npDim(3))     !! NM inflow from erosion, inflows and sources [kg/displacement]
        real(dp)            :: dj_nm_transformed_in(C%npDim(1), C%npDim(2), C%npDim(3)) !! Transformed NM inflow from erosion, inflows and sources [kg/displacement]
        real(dp)            :: dj_dissolved_in                                  !! Dissolved species inflow from inflows and sources [kg/displacement]
        real(dp)            :: dj_spm_resus(C%nSizeClassesSPM)
        real(dp)            :: dj_spm_resus_perArea(C%nSizeClassesSpm)
        real(dp)            :: dj_spm_resus_perArea_(C%nSizeClassesSpm)
        type(Result)        :: rslt
        real(dp)            :: dj_spm_deposit(C%nSizeClassesSPM)
        real(dp)            :: dj_spm_outflow(C%nSizeClassesSPM)
        integer             :: i
        real(dp)            :: k_outflow                                        ! The outflow rate [/s]
        real(dp)            :: dj_nm_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_nm_transformed_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_nm_resus(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_nm_transformed_resus(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_nm_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_nm_transformed_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: dj_dissolved_outflow

        ! Check that we've got a volume before going on
        if (.not. isZero(me%volume)) then

            ! Outflow rate [/disp]
            k_outflow = dQ / me%volume

            ! Calculate outflow and deposition first
            dj_spm_outflow = min(flushToZero(me%m_spm * k_outflow), me%m_spm)           ! [kg/disp]
            dj_spm_deposit = flushToZero((me%m_spm + dj_spm_in) * me%k_settle * dt)     ! [kg/disp]
            dj_spm_deposit = min(dj_spm_deposit, me%m_spm + dj_spm_in)      

            ! Calculate resuspension [kg/displacement] and send this to the bed sediment,
            ! which tells us how much sediment can actually be resuspended. Bed sediment resuspend
            ! method *must* be called before deposition
            dj_spm_resus_perArea = flushToZero(me%k_resus * me%bedSediment%Mf_bed_by_size() * dt)           ! kg/m2 = s-1 * kg/m2 * s
            dj_spm_resus_perArea_ = dj_spm_resus_perArea
            call rslt%addErrors(.errors. me%bedSediment%resuspend(dj_spm_resus_perArea_))
            ! The above modifies dj_spm_resus_perArea to be the amount of sediment passed
            ! in that *isn't* resuspended, so the amount actually resuspended is input - output:
            dj_spm_resus_perArea = dj_spm_resus_perArea - dj_spm_resus_perArea_
            dj_spm_resus = dj_spm_resus_perArea * me%bedArea

            ! Pass the deposited SPM to the bed sediment
            call rslt%addErrors(.errors. me%depositToBed(dj_spm_deposit))

            ! Add these to the flow objects and ammend the SPM mass
            me%Q%outflow = me%Q%outflow - dQ 
            me%j_spm%resuspension = me%j_spm%resuspension + dj_spm_resus
            me%j_spm%deposition = me%j_spm%deposition - dj_spm_deposit      ! Deposition is negative
            me%j_spm%outflow = me%j_spm%outflow - dj_spm_outflow            ! Outflow is negative
            ! Mass of SPM = previous mass + inflows (runoff and inflow) + resus - deposition - outflow.
            ! We still need to set minimum bound as 0 in case of FP rounding errors in calculating scaling
            ! factor forces outflow mass to be slightly higher than inflow
            me%m_spm = flushToZero(max(me%m_spm + dj_spm_in + dj_spm_resus - dj_spm_deposit - dj_spm_outflow, 0.0_dp))

            ! Check we're not meant to be ignore NM processes to speed things up
            if (.not. C%ignoreNM) then
                ! Now we can deal with NM, firstly by calculating the deposited and outflowing NM
                dj_nm_outflow = min(flushToZero(me%m_np * k_outflow), me%m_np)                                       ! [kg/disp]
                dj_nm_transformed_outflow = min(flushToZero(me%m_transformed * k_outflow), me%m_transformed)         ! [kg/disp]
                dj_dissolved_outflow = min(flushToZero(me%m_dissolved * k_outflow), me%m_dissolved)                  ! [kg/disp]
                dj_nm_deposit = 0.0_dp              ! Only heteraggregated size classes will be changed, to set others to zero
                dj_nm_transformed_deposit = 0.0_dp
                do i = 1, C%nSizeClassesSpm
                    dj_nm_deposit(:,:,2+i) = min(flushToZero((me%m_np(:,:,2+i) + dj_nm_in(:,:,2+i)) &
                        * me%k_settle(i) * dt), me%m_np(:,:,2+i) + dj_nm_in(:,:,2+i))
                    dj_nm_transformed_deposit(:,:,2+i) = min(flushToZero((me%m_transformed(:,:,2+i) &
                        + dj_nm_transformed_in(:,:,2+i)) * me%k_settle(i) * dt), me%m_transformed(:,:,2+i) &
                        + dj_nm_transformed_in(:,:,2+i))
                end do

            ! Pass the deposited and resuspended SPM to the bed sediment, which will use it
            ! to populate the mass transfer matrix
                call me%bedSediment%getMatrix(divideCheckZero(dj_spm_deposit, me%bedArea), dj_spm_resus_perArea)
                ! Pass the deposited NM to the bed sediment, which apportions it across the sediment layers
                call me%bedSediment%transferNM(divideCheckZero(dj_nm_deposit, me%bedArea))
                ! Now pull the mass of NM resuspended out of the bed sediment (which internally
                ! is calculated using the mass of sediment resuspended)
                dj_nm_resus = flushToZero(me%bedSediment%M_np(2,:,:,:) * me%bedArea)

                ! TODO no resuspended transformed NM for the moment
                dj_nm_transformed_resus = 0.0_dp

                ! Add these NM fluxes to the flow object and ammend the NM mass
                me%j_nm%deposition = me%j_nm%deposition - dj_nm_deposit         ! Deposition is negative
                me%j_nm%resuspension = me%j_nm%resuspension + dj_nm_resus
                me%j_nm%outflow = me%j_nm%outflow - dj_nm_outflow               ! Outflow is negative
                me%j_nm_transformed%deposition = me%j_nm_transformed%deposition - dj_nm_transformed_deposit
                me%j_nm_transformed%resuspension = me%j_nm_transformed%resuspension + dj_nm_transformed_resus
                me%j_nm_transformed%outflow = me%j_nm_transformed%outflow - dj_nm_transformed_outflow
                me%j_dissolved%outflow = me%j_dissolved%outflow - dj_dissolved_outflow
                ! Mass of NM = previous mass + inflows (runoff, inflows, sources) + resus - deposition - outflow
                me%m_np = max(me%m_np + dj_nm_in + dj_nm_resus - dj_nm_deposit - dj_nm_outflow, 0.0_dp)
                me%m_transformed = max(me%m_transformed + dj_nm_transformed_in + dj_nm_transformed_resus &
                                - dj_nm_transformed_deposit - dj_nm_transformed_outflow, 0.0_dp)
                me%m_dissolved = max(me%m_dissolved + dj_dissolved_in - dj_dissolved_outflow, 0.0_dp)
            end if

        else
            ! If there is no volume then there must be no SPM/NM. Concentrations will be
            ! set outside of the displacement loop
            me%m_spm = 0.0_dp
            me%m_np = 0.0_dp 
            me%m_transformed = 0.0_dp
            me%m_dissolved = 0.0_dp
        end if

        ! Trigger any errors that there were
        call rslt%addToTrace("Updating time displacement #" // trim(str(d)))
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine

    !> Set the dimensions (width, depth, areas, volume) of the reach
    subroutine setDimensions(me, t)
        class(RiverReach)  :: me        !! This reach
        integer             :: t        !! The current timestep index
        ! Calculate the width [m], depth [m], cross-section, bed and surface areas [m2] and volume [m3]
        me%width = me%calculateWidth(me%Q_in_total/C%timeStep)
        me%depth = me%calculateDepth(me%width, me%slope, me%Q_in_total/C%timeStep, t)
        me%xsArea = me%depth*me%width
        me%bedArea = me%width*me%length*me%f_m
        me%surfaceArea = me%bedArea                         ! For river reaches, set surface area equal to bed area [m2]
        me%volume = me%depth*me%width*me%length*me%f_m
        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)
    end subroutine

    !> Parse data from the input file for this river reach
    function parseInputDataRiverReach(me) result(rslt)
        class(RiverReach) :: me
        type(Result) :: rslt

        me%f_m = DATASET%riverMeanderingFactor
        me%alpha_hetero = DATASET%riverAttachmentEfficiency
        me%alpha_resus = DATASET%resuspensionAlpha(me%x, me%y)
        me%beta_resus = DATASET%resuspensionBeta(me%x, me%y)
        me%a_stc = DATASET%sedimentTransport_a(me%x, me%y)
        me%b_stc = DATASET%sedimentTransport_b(me%x, me%y)
        me%c_stc = DATASET%sedimentTransport_c(me%x, me%y)
        me%T_water = DATASET%waterTemperature

        ! Parse the input data to get inflows and outflow arrays. Pointers to reaches won't be
        ! set until all reaches created
        call rslt%addErrors( &
            .errors. me%parseInflowsAndOutflow() &
        )

        ! Now we've got inflows and outflows, we can set reach length, assuming one reach per branch
        call rslt%addErrors( &
            .errors. me%setReachLengthAndSlope() &
        )

        call rslt%addToTrace('Parsing input data')             ! Add this procedure to the trace
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
                message="Newton's method diverged to NaN after " // trim(str(i)) // " iterations." &
            ))
        ! If max number of iterations reached
        else if (i > iMax) then
            call rslt%addError(ErrorInstance( &
                message="Newton's method failed to converge - maximum number of iterations (" &
                    // trim(str(i)) // ") exceeded. Precision (proximity to zero) required: " &
                    // trim(str(epsilon)) // ". Final value: " // trim(str(f)) // "." &
            ))
        ! If we got a negative river depth
        else if (D_i < 0.0_dp) then
            call rslt%addError( &
                ErrorInstance(message="Newton's method gave negative river depth. Depth: " // trim(str(D_i))) &
            )
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
        class(RiverReach), intent(in) :: me    !! This `RiverReach` instance
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

end module

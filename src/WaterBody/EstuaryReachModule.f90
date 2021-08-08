module EstuaryReachModule
    use Globals
    use ReachModule
    use UtilModule
    use ResultModule
    use BedSedimentModule
    use LoggerModule, only: LOGR
    use ReactorModule
    implicit none

    type, public, extends(Reach1) :: EstuaryReach
        real(dp) :: meanDepth               !! Mean estuary depth for use in tidal depth calculations [m]
        real(dp) :: distanceToMouth         !! Distance to the mouth of the estuary [m]
        real(dp) :: tidalM2                 !! Tidal harmonic coefficient M2 [-]
        real(dp) :: tidalS2                 !! Tidal harmonic coefficient S2 [-]

      contains
        ! Create/destroy
        procedure :: create => createEstuaryReach
        ! Simulators
        procedure :: update => updateEstuaryReach
        procedure :: setDimensions
        ! Data handlers
        procedure :: parseInputData => parseInputDataEstuaryReach
        ! Calculators
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
        procedure :: calculateDistanceToMouth => calculateDistanceToMouth
        procedure :: changeInVolume => changeInVolume
    end type

  contains

    function createEstuaryReach(me, x, y, w, distributionSediment) result(rslt)
        class(EstuaryReach) :: me               !! This `EstuaryReach` instance
        integer :: x                            !! Grid cell x-position index
        integer :: y                            !! Grid cell y-position index
        integer :: w                            !! Water body index within the cell
        real(dp) :: distributionSediment(C%nSizeClassesSPM)     !! Distribution to split sediment yields with
        type(Result) :: rslt                    !! Result object to return errors in
        integer :: i, j                         ! Iterator

        ! Set reach references (indices set in WaterBody%create) and grid cell area
        call rslt%addErrors(.errors. me%WaterBody%create(x, y, w, distributionSediment))
        me%ref = trim(ref("EstuaryReach", x, y, w))

        ! Parse input data and allocate/initialise variables
        call rslt%addErrors(.errors. me%parseInputData())

        ! Make sure the reach has some dimensions to begin with
        call me%setDimensions(0)

        ! Create the bed sediment and reactor for this reach
        allocate(BedSediment :: me%bedSediment)
        allocate(Reactor :: me%reactor)
        call rslt%addErrors([ &
            .errors. me%bedSediment%create(me%x, me%y, me%w), &
            .errors. me%reactor%create(me%x, me%y, me%alpha_hetero) &
        ])

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

        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOGR%toFile("Creating " // trim(me%ref) // ": success")
    end function


    !> Run the estuary reach simulation for this timestep
    subroutine updateEstuaryReach(me, t, q_runoff, q_overland, j_spm_runoff, j_np_runoff, &
                                 j_transformed_runoff, contributingArea, isWarmUp)
        class(EstuaryReach) :: me
        integer :: t
        real(dp) :: q_runoff                          !! Runoff (slow + quick flow) from the hydrological model [m/timestep]
        real(dp) :: q_overland                        !! Overland flow [m3/m2/timestep]
        real(dp) :: j_spm_runoff(:)                   !! Eroded sediment runoff to this reach [kg/timestep]
        real(dp) :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this reach [kg/timestep]
        real(dp) :: j_transformed_runoff(:,:,:)       !! Eroded NP runoff to this reach [kg/timestep]
        real(dp) :: contributingArea                  !! Area contributing to this reach (e.g. the soil profile) [m2]
        logical :: isWarmUp
        type(Result) :: rslt
        real(dp) :: Q_outflow
        real(dp) :: changeInVolume, previousVolume
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)
        real(dp) :: j_np_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: j_spm_in_total(C%nSizeClassesSpm)           ! Total inflow of SPM [kg/timestep]
        real(dp) :: j_np_in_total(C%npDim(1), C%npDim(2), C%npDim(3))   ! Total inflow of NP [kg/timestep]
        real(dp) :: fractionSpmDeposited(C%nSizeClassesSpm)     ! Fraction of SPM deposited on each time step [-]
        integer :: i, j, k                                      ! Iterator
        integer :: nDisp                                        ! Number of displacements to split this time step into
        real(dp) :: dt                                          ! Length of each displacement [s]
        real(dp) :: dQ_in                                       ! Water inflow for each displacement [m3/displacement]
        real(dp) :: dQ_out
        real(dp) :: dj_spm_erosion(C%nSizeClassesSpm)           ! SPM inflow due to erosion for each displacement [kg/displacement]
        real(dp) :: dj_spm_inflow(C%nSizeClassesSpm)            ! SPM inflow from inflow reaches for each displacement [kg/displacement]
        real(dp) :: dj_nm_erosion_sources(C%npDim(1), C%npDim(2), C%npDim(3))   ! NM inflow due to erosion and sources for each displacement [kg/displacement]
        real(dp) :: dj_nm_inflow(C%npDim(1), C%npDim(2), C%npDim(3))            ! NM inflow from inflow reaches for each displacement [kg/displacement]
        real(dp) :: dj_nm_transformed_erosion_sources(C%npDim(1), C%npDim(2), C%npDim(3))   ! Transformed NM inflow due to erosion and sources for each displacement [kg/displacement]
        real(dp) :: dj_nm_transformed_inflow(C%npDim(1), C%npDim(2), C%npDim(3))            ! Transformed NM inflow from inflow reaches for each displacement [kg/displacement]
        real(dp) :: dj_dissolved_sources                        ! Dissolved species inflow from sources on each displacement [kg/displacement]
        real(dp) :: dj_dissolved_inflow                         ! Dissolved species inflow from inflow reaches on each displacement [kg/displacement]
        real(dp) :: dj_spm_out(C%nSizeClassesSpm)               ! SPM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_nm_out(C%npDim(1), C%npDim(2), C%npDim(3))   ! NM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_nm_transformed_out(C%npDim(1), C%npDim(2), C%npDim(3))   ! Transformed NM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_dissolved_out                            ! Dissolved species outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_spm_outflow(C%nSizeClassesSpm)               ! SPM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_nm_outflow(C%npDim(1), C%npDim(2), C%npDim(3))   ! NM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_nm_transformed_outflow(C%npDim(1), C%npDim(2), C%npDim(3))   ! Transformed NM outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_dissolved_outflow                            ! Dissolved species outflow from reach on current displacement [kg/displacement]
        real(dp) :: dj_spm_resus(C%nSizeClassesSpm)             ! Mass of each sediment size class resuspended on each displacement [kg]
        real(dp) :: dj_spm_in(C%nSizeClassesSpm)
        real(dp) :: dj_nm_in(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: dj_nm_transformed_in(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: dj_dissolved_in
        real(dp) :: tpm_m_spm(C%nSizeClassesSpm)
        integer :: f
        real(dp) :: dj_spm_deposit(C%nSizeClassesSpm)
        real(dp) :: dj_nm_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: dj_nm_transformed_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: dj_nm_resus(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: dj_spm_resus_perArea(C%nSizeClassesSpm)     ! Mass of each sediment size class resuspended on each displacement, per unit area [kg/m2/disp]
        real(dp) :: dj_spm_deposit_perArea(C%nSizeClassesSpm)   ! Mass of each sediment size class deposited on each displacement, per unit area [kg/m2/disp]
        real(dp) :: tmp_dj_spm_resus_perArea(C%nSizeClassesSpm) ! Temp dj_spm_resus_perArea, to get around bed sediment procedures modifying input params - TODO sort this out
        real(dp) :: dj_nm_deposit_perArea(C%npDim(1), C%npDim(2), C%npDim(3))
        type(datetime) :: currentDate
        real :: T_water_t                                           ! Water temperature on this timestep [deg C]

        ! Reset all flows to zero, which is needed as flows are added to iteratively in the displacement loop
        call me%emptyFlows()
        
        ! Get the current date and use the day of year to get the water temp
        currentDate = C%startDate + timedelta(t-1)
        T_water_t = me%T_water(currentDate%yearday()) 

        ! Get the inflows from upstream water bodies
        ! TODO sort out routing and get rid of _final flow objects
        do i = 1, me%nInflows
            me%Q%inflow = me%Q%inflow - me%inflows(i)%item%Q_final%outflow
            me%j_spm%inflow = me%j_spm%inflow - me%inflows(i)%item%j_spm_final%outflow
            me%j_nm%inflow = me%j_nm%inflow - me%inflows(i)%item%j_nm_final%outflow
            me%j_nm_transformed%inflow = me%j_nm_transformed%inflow &
                                           - me%inflows(i)%item%j_nm_transformed_final%outflow
            me%j_dissolved%inflow = me%j_dissolved%inflow - me%inflows(i)%item%j_dissolved_final%outflow
        end do 
        
        ! Get the inflows from runoff and scale to this reach
        me%Q%runoff = q_runoff * contributingArea 

        ! TODO transfers and demands

        ! Inflows from point and diffuse sources, updates the NM flow object
        if (.not. C%ignoreNM .and. .not. isWarmUp) then
            call me%updateSources(t)
        end if

        ! Set the reach dimensions (using the timestep in hours for tidal harmonics) and calculate the change in volume 
        call me%setDimensions((t-1) * C%timeStep / C%minEstuaryTimestep)
        changeInVolume = me%changeInVolume((t-1)*24, t*24)
        ! Calculate the outflow based on the change in volume and inflows. +ve outflow indicates upstream tidal flow,
        ! -ve outflow indicates downstream tidal flow. This is used to determine what classes as "input" SPM/NM
        Q_outflow = changeInVolume - me%Q%inflow - me%Q%runoff - me%Q%transfers
        ! Input will always be from runoff, transfers and sources
        me%Q_in_total = me%Q%runoff + me%Q%transfers
        ! j_spm_input_total = me%j_spm_runoff() + me%j_spm_transfers()
        ! j_np_input_total = me%j_np_runoff() + me%j_np_transfers() + me%j_np_pointsource() + me%j_np_diffusesource()
        ! If outflow is positive (incoming tide) then some input will be provided by the inflowing outflow (which will be +ve)
        if (Q_outflow > 0) then
            me%Q_in_total = me%Q_in_total + me%Q%outflow
            ! j_spm_input_total = j_spm_input_total + me%j_spm_outflow()
            ! j_np_input_total = j_np_input_total + me%j_np_outflow()
        end if
        ! If inflow is positive, input will also be from inflows (tide might still be incoming for this particular reach)
        if (me%Q%inflow > 0.0_dp) then
            me%Q_in_total = me%Q_in_total + me%Q%inflow
            ! j_spm_input_total = j_spm_input_total + me%j_spm_inflows()
            ! j_np_input_total = j_np_input_total + me%j_np_inflows()
        end if
        ! Use the total inflow to calculate the velocity
        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)

        ! Set the erosion yields, which includes scaling the soil erosion by sediment transport
        ! capacity, calculating the bank ersoion, and storing these in the flow objects. This must
        ! be done after me%Q_in_total has been set
        call me%setErosionYields(j_spm_runoff, q_overland, contributingArea, j_np_runoff, j_transformed_runoff)

        ! Set the resuspension and settling rates [/s] (but don't settle until we're looping through displacements) 
        call me%setResuspensionRate(me%Q_in_total / C%timeStep, T_water_t)
        call me%setSettlingRate(T_water_t)

        ! If Q_in for this timestep is bigger than the reach volume, then we need to
        ! split into a number of displacements. If Q_in is zero, just have 1 displacement.
        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = C%timeStep / C%minEstuaryTimestep
        else
            ! Make sure the minimum displacement duration is that provided in config (defaults to 1 hour)
            nDisp = max(ceiling(me%Q_in_total / me%volume), C%timeStep / C%minEstuaryTimestep)
        end if
        dt = C%timeStep / nDisp                             ! Length of each displacement [s]
        dQ_in = me%Q_in_total / nDisp
        dj_SPM_erosion = (me%j_spm%soilErosion + me%j_spm%bankErosion) / nDisp
        dj_spm_inflow = me%j_spm%inflow / nDisp
        dj_nm_erosion_sources = (me%j_nm%soilErosion + me%j_nm%pointSources &
                              + me%j_nm%diffuseSources) / nDisp
        dj_nm_inflow = me%j_nm%inflow / nDisp
        dj_NM_transformed_erosion_sources = (me%j_nm_transformed%soilErosion &
                                          + me%j_nm_transformed%pointSources &
                                          + me%j_nm_transformed%diffuseSources) / nDisp
        dj_nm_transformed_inflow = me%j_nm_transformed%inflow / nDisp
        dj_dissolved_sources = (me%j_dissolved%pointSources + me%j_dissolved%diffuseSources) / nDisp
        dj_dissolved_inflow = me%j_dissolved%inflow / nDisp

        do i = 1, nDisp
            ! Calculate the timestep in hours from the displacement length, and pass to setDimensions
            ! to use to calculate tidal harmonics
            call me%setDimensions((t -1)*C%timeStep/3600 + i*(int(dt)/3600))
            ! Calculate the change in volume between this displacement and the next
            changeInVolume = me%changeInVolume((t-1)*24 + (i-1)*(int(dt)/3600), (t-1)*24 + i*(int(dt)/3600))
            ! Water mass balance (outflow = all the inflows + change in volume)
            dQ_out = -dQ_in + changeInVolume
            ! As flow changes so much over displacement, set resuspension rate on each
            call me%setResuspensionRate(abs(dQ_out) / dt, T_water_t)

            ! If this displacement's outflow is -ve, tidal flow must be downstream and
            ! outflowing SPM/NM is a function of this reach's SPM/NM conc, else if it is
            ! +ve, then it must be a function of the outflow reach's SPM/NM conc (if that
            ! outflow reach exists)
            if (dQ_out < 0 .and. .not. isZero(me%volume)) then
                ! SPM and NM outflows (which are downstream)
                dj_spm_out = max(me%m_spm * dQ_out / me%volume, -me%m_spm)
                dj_nm_out = max(me%m_np * dQ_out / me%volume, -me%m_np)
                dj_nm_transformed_out = max(me%m_transformed * dQ_out / me%volume, -me%m_transformed)
                dj_dissolved_out = max(me%m_dissolved * dQ_out / me%volume, -me%m_dissolved)
                ! Total SPM input to this displacement, from erosion and inflow reaches (but not depsition yet)
                dj_spm_in = dj_spm_erosion + dj_spm_inflow
                dj_nm_in = dj_nm_erosion_sources + dj_nm_inflow
                dj_nm_transformed_in = dj_nm_transformed_erosion_sources + dj_nm_transformed_inflow
                dj_dissolved_in = dj_dissolved_sources + dj_dissolved_inflow
            else if (dQ_out > 0 .and. associated(me%outflow%item)) then
                ! Add SPM inflowing from downstream reach
                ! TODO setting inflow by splitting outflow m_spm by nInflows is a hack, change this to 
                ! get the proper m_spm from each reach
                dj_spm_out = min(me%outflow%item%C_spm_final * dQ_out, me%outflow%item%m_spm / me%outflow%item%nInflows)
                dj_nm_out = min(me%outflow%item%C_np_final * dQ_out, me%outflow%item%m_np / me%outflow%item%nInflows)
                dj_nm_transformed_out = min(me%outflow%item%C_transformed_final * dQ_out, &
                                            me%outflow%item%m_transformed/me%outflow%item%nInflows)
                dj_dissolved_out = min(me%outflow%item%C_dissolved_final * dQ_out, &
                                       me%outflow%item%m_dissolved/me%outflow%item%nInflows)
                ! Set the "inflow" (upstream) based on this
                dj_spm_inflow = -min(me%m_spm * dQ_out / me%volume, me%m_spm)
                dj_nm_inflow = -min(me%m_np * dQ_out / me%volume, me%m_np)
                dj_nm_transformed_inflow = -min(me%m_transformed * dQ_out / me%volume, me%m_transformed)
                dj_dissolved_inflow = -min(me%m_dissolved * dQ_out / me%volume, me%m_dissolved)
                !!!!!! NEED TO REMOVE SPM, NM FROM THE OUTFLOW REACH !!!!!!!!!
                !!!!!! AND MAKE SURE IT'S NOT ALL ADVECTED !!!!!!!!!!!!!!!!!!!
                dj_spm_in = dj_spm_erosion + dj_spm_inflow                 ! Total SPM input, from inflowing outflow and erosion (not deposition)
                dj_nm_in = dj_nm_erosion_sources + dj_nm_inflow 
                dj_nm_transformed_in = dj_nm_transformed_erosion_sources + dj_nm_transformed_inflow
                dj_dissolved_in = dj_dissolved_sources + dj_dissolved_inflow
            else if (dQ_out > 0 .and. .not. associated(me%outflow%item)) then
                ! If there is no outflow but tidal flow is in, set inflow SPM/NM to zero
                dj_spm_out = 0.0_dp
                dj_nm_out = 0.0_dp
                dj_nm_transformed_out = 0.0_dp
                dj_dissolved_out = 0.0_dp
                dj_spm_in = dj_spm_erosion + dj_spm_inflow
                dj_nm_in = dj_nm_erosion_sources + dj_nm_inflow
                dj_nm_transformed_in = dj_nm_transformed_erosion_sources + dj_nm_transformed_inflow
                dj_dissolved_in = dj_dissolved_sources + dj_dissolved_inflow
            else
                dj_spm_out = 0.0_dp
                dj_nm_out = 0.0_dp
                dj_nm_transformed_out = 0.0_dp
                dj_dissolved_out = 0.0_dp
                dj_spm_in = 0.0_dp
                dj_nm_in = 0.0_dp
                dj_nm_transformed_in = 0.0_dp
                dj_dissolved_in = 0.0_dp
            end if

            tpm_m_spm = max(me%m_spm + dj_spm_in, 0.0_dp)           ! Check the SPM isn't making the new mass negative
            ! SPM deposition and resuspension. Use m_spm as previous m_spm + inflow - outflow, making sure to
            ! not pick up on the previous displacement's deposition (index 4+me%nInflows)
            dj_spm_deposit = min(me%k_settle * dt * tpm_m_spm, tpm_m_spm)
            dj_spm_resus = me%k_resus * me%bedSediment%Mf_bed_by_size() * dt

            ! Calculate the fraction of SPM from each size class that was deposited, for use in calculating mass of NM deposited
            do j = 1, C%nSizeClassesSpm
                if (isZero(dj_spm_deposit(j))) then
                    fractionSpmDeposited(j) = 0
                else
                    fractionSpmDeposited(j) = dj_spm_deposit(j) / (tpm_m_spm(j))  ! TODO include resus
                end if
            end do
            ! Update the deposition element of the SPM and NM flux array. Only heteroaggregated, 
            dj_nm_deposit = 0.0_dp
            dj_nm_transformed_deposit = 0.0_dp
            do j = 1, C%nSizeClassesSpm
                dj_nm_deposit(:,:,2+j) = min(me%m_np(:,:,2+j)*fractionSpmDeposited(j), me%m_np(:,:,2+j))     ! Only deposit heteroaggregated NM (index 3+)
                dj_nm_transformed_deposit(:,:,2+j) = min(me%m_transformed(:,:,2+j)*fractionSpmDeposited(j), &
                                                          me%m_transformed(:,:,2+j))
                ! dj_np(4+me%nInflows,:,:,2+j) = -min(me%m_np(:,:,2+j)*fractionSpmDeposited(j), me%m_np(:,:,2+j))     ! Only deposit heteroaggregated NM (index 3+)
                ! dj_transformed(4+me%nInflows,:,:,2+j) = &:w
                !     -min(me%m_transformed(:,:,2+j)*fractionSpmDeposited(j), me%m_transformed(:,:,2+j))
            end do

            !-- MASS BALANCES --!
            ! SPM and NM mass balance. As outflow was set before deposition etc fluxes, we need to check that masses aren't below zero again
            dj_nm_outflow = -min(me%m_np, dj_nm_out)               ! Maximum outflow is the current mass
            dj_spm_outflow = -min(me%m_spm, dj_spm_out)
            dj_nm_transformed_outflow = -min(me%m_transformed, dj_nm_transformed_out)
            dj_dissolved_outflow = -min(me%m_dissolved, dj_dissolved_out)
            me%m_spm = flushToZero(max(me%m_spm + dj_spm_in - dj_spm_outflow, 0.0_dp))
            me%m_np = flushToZero(max(me%m_np + dj_nm_in - dj_nm_outflow, 0.0_dp))
            me%m_transformed = flushToZero(max(me%m_transformed + dj_nm_transformed_in - dj_nm_transformed_outflow, 0.0_dp))
            me%m_dissolved = flushToZero(max(me%m_dissolved + dj_dissolved_in - dj_dissolved_outflow, 0.0_dp))

            ! Add the calculated fluxes (outflow and deposition) to the total. Don't update inflows
            ! (inflows, runoff, sources) as they've already been correctly before the disp loop
            me%Q%outflow = me%Q%outflow + dQ_out
            me%j_spm%outflow = me%j_spm%outflow + dj_spm_outflow                    ! dj_spm_outflow should already be -ve
            me%j_nm%outflow = me%j_nm%outflow + dj_nm_outflow
            me%j_nm_transformed%outflow = me%j_nm_transformed%outflow + dj_nm_transformed_outflow
            me%j_dissolved%outflow = me%j_dissolved%outflow + dj_dissolved_outflow
            me%j_spm%deposition = me%j_spm%deposition - dj_spm_deposit              ! Deposition is -ve
            me%j_spm%resuspension = me%j_spm%resuspension + dj_spm_resus
            me%j_nm%deposition = me%j_nm%deposition - dj_nm_deposit
            me%j_nm_transformed%deposition = me%j_nm_transformed%deposition - dj_nm_transformed_deposit

            ! Deposit SPM and NM to bed, and pull out resuspended NM mass
            dj_spm_resus_perArea = divideCheckZero(dj_spm_resus, me%bedArea) 
            dj_spm_deposit_perArea = divideCheckZero(dj_spm_deposit, me%bedArea)
            tmp_dj_spm_resus_perArea = dj_spm_resus_perArea
            dj_nm_deposit_perArea = divideCheckZero(dj_nm_deposit, me%bedArea)
            ! If we're including bed sediment, then deposit and resuspend to/from
            if (C%includeBedSediment) then
                ! Remove resuspended SPM from sediment
                call rslt%addErrors(.errors. me%bedSediment%resuspend(tmp_dj_spm_resus_perArea))
                ! bedSediment%resuspend modifies dj_spm_resus_perArea to be the amount of sediment passed in
                ! that isn't resuspended, so the amount actually resuspended is input - output:
                dj_spm_resus_perArea = dj_spm_resus_perArea - tmp_dj_spm_resus_perArea
                ! Update the deposition element of SPM array based on this
                ! dj_spm(4+me%nInflows,:) = dj_spm_resus_perArea * me%bedArea - dj_spm_deposit
                ! Add deposited SPM to sediment
                call rslt%addErrors(.errors. me%depositToBed(dj_spm_deposit))
                if (rslt%hasCriticalError()) return
                ! Fill bedSediment%delta_sed mass transfer matrix based on this passed deposition and resuspension

                if (.not. C%ignoreNM) then
                    call me%bedSediment%getmatrix(dj_spm_deposit_perArea, dj_spm_resus_perArea)
                    ! The above must be called before transferNM so that delta_sed is set. TODO change this to be internal to bed sediment
                    ! Now actually transfer the NM between the layers (only if there is NM to deposit)
                    if (.not. any(dj_nm_deposit_perArea == 0.0_dp)) then
                        call me%bedSediment%transferNM(dj_nm_deposit_perArea)
                    end if
                    ! Now we've computed transfers in bed sediment, we need to pull the resuspended NM out and add to mass balance matrices
                    dj_nm_resus = me%bedSediment%M_np(2,:,:,:) * me%bedArea
                    me%j_nm%resuspension = me%j_nm%resuspension + dj_nm_resus
                end if
            end if

            ! Concentrations
            me%C_spm = divideCheckZero(me%m_spm, me%volume)
            me%C_np = divideCheckZero(me%m_np, me%volume)
            me%C_transformed = divideCheckZero(me%m_transformed, me%volume)
            me%C_dissolved = divideCheckZero(me%m_dissolved, me%volume)
        end do

        ! Transform the NPs. TODO: Should this be done before or after settling/resuspension?
        ! TODO for the moment, ignoring heteroaggregation if no volume, need to figure out
        ! what to really do if there are no flows
        if (.not. isZero(me%volume)) then
            call rslt%addErrors([ &
                .errors. me%reactor%update( &
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
            ! Get the resultant transformed mass from the Reactor
            me%m_np = me%reactor%m_np
            me%m_transformed = me%reactor%m_transformed
            me%m_dissolved = me%reactor%m_dissolved
        end if

        ! Set the final concentrations, checking that the river has a volume
        me%C_spm = divideCheckZero(me%m_spm, me%volume)
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
                ) &
            )
        end do

        ! Set the updated flag to true
        me%isUpdated = .true.

        ! Add what we're doing here to the error trace and trigger any errors there are
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
    end subroutine


    !> Set the dimensions (width, depth, area, volume) of the reach
    subroutine setDimensions(me, tHours)
        class(EstuaryReach) :: me
        integer :: tHours

        ! Calculate actual depth based on these and number of hours through model run
        me%depth = me%calculateDepth(tHours)
        me%xsArea = me%depth*me%width                       ! Calculate the cross-sectional area of the reach [m2]
        me%bedArea = me%width*me%length*me%f_m              ! Calculate the BedSediment area [m2]
        me%surfaceArea = me%bedArea                         ! TODO maybe alter this for estuaries to make non-rectangular
        me%volume = me%depth*me%width*me%length*me%f_m      ! Reach volume
    end subroutine


    function changeInVolume(me, tStart, tFinal)
        class(EstuaryReach) :: me
        integer :: tStart                   !! Initial time [hours]
        integer :: tFinal                   !! Final time [hours]
        real(dp) :: changeInVolume          !! Change in reach volume between `tStart` and `tFinal`
        changeInVolume = (me%calculateDepth(tFinal) - me%calculateDepth(tStart))*me%width*me%length*me%f_m
    end function


    !> Parse input data for this EstuaryReach and store in state variables.
    function parseInputDataEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
        integer :: i                                ! Loop iterator
        integer, allocatable :: inflowArray(:,:)    ! Temporary array for storing inflows from data file in
        ! Calculate the distance to the estuary mouth from data
        me%distanceToMouth = me%calculateDistanceToMouth( &
            DATASET%x(me%x), &
            DATASET%y(me%y), &
            DATASET%estuaryMeanderingFactor, &
            DATASET%estuaryMouthCoords(1), &
            DATASET%estuaryMouthCoords(2) &
        )
        ! Width, as exponential function of distance from mouth, unless specified in data
        me%width = DATASET%estuaryWidthExpA * exp(-DATASET%estuaryWidthExpB * me%distanceToMouth)
        ! Mean depth as exponential function of distance from mouth
        me%meanDepth = DATASET%estuaryMeanDepthExpA * exp(-DATASET%estuaryMeanDepthExpB * me%distanceToMouth)
        ! if (allocated(me%domainOutflow)) me%isDomainOutflow = .true.    ! If we managed to set domainOutflow, then this reach is one
        me%f_m = DATASET%estuaryMeanderingFactor
        me%alpha_hetero = DATASET%estuaryAttachmentEfficiency
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

    !> Estimate the distance of the point (x,y) to the estuary mouth assuming the straight
    !! line distance mutiplied by a meandering factor.
    function calculateDistanceToMouth(me, x, y, f, x_mouth, y_mouth) result(distanceToMouth)
        class(EstuaryReach) :: me               !! This EstuaryReach instance
        real                :: x                !! x coordinate of cell to calculate distance to mouth of
        real                :: y                !! y coordinate of cell to calculate distance to mouth of
        real                :: f                !! Meandering factor
        real                :: x_mouth          !! x coordinate of estuary mouth
        real                :: y_mouth          !! y coordinate of estuary mouth
        real                :: distanceToMouth  !! The calculated distance to the estuary mouth
        distanceToMouth = f * sqrt((x_mouth - x)**2 + (y_mouth - y)**2)
    end function

    !> Calculate water depth from tidal harmonics.
    !! $$
    !!      D(x,t) = A_{S2} \cos \left( 2\pi \frac{t}{12} \right) + A_{M2} \cos \left( 2\pi \frac{t}{12.42} \right) + /
    !!          \frac{3}{4} \frac{xA_{M2}^2}{D_x (6.21 \times 3600) \sqrt{gD_x}} \cos(2\pi \frac{t}{6.21}) + z_0
    !! $$
    !! Ref: [Hardisty, 2007](https://doi.org/10.1002/9780470750889)
    function calculateDepth(me, tHours) result(depth)
        class(EstuaryReach), intent(in) :: me     !! The `EstuaryReach` instance.
        integer, intent(in) :: tHours             !! The current timestep (in hours)
        real(dp) :: depth

        depth = DATASET%estuaryTidalS2 * cos(2.0_dp*C%pi*tHours/12.0_dp) + DATASET%estuaryTidalM2 &
            * cos(2.0_dp*C%pi*tHours/12.42_dp) + (0.75_dp) * ((me%distanceToMouth * DATASET%estuaryTidalM2 ** 2) &
            / (me%meanDepth * 22356.0_dp * sqrt(9.81_dp * me%meanDepth))) &
            * cos(2*C%pi*tHours/6.21_dp) + me%meanDepth
        ! If the depth is negative (which it really shouldn't be...), set it to zero
        if (depth < 0) depth = 0.0_dp
    end function

    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    function calculateVelocity(me, D, Q, W) result(v)
        class(EstuaryReach), intent(in) :: me    !! This `EstuaryReach` instance
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
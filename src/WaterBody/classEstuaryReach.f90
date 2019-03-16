module classEstuaryReach
    use Globals
    use spcReach
    use UtilModule
    use ResultModule
    use classBedSediment1
    use classLogger, only: LOG
    use classDataInterfacer, only: DATA
    use classReactor1
    use classBiota1
    implicit none

    type, public, extends(Reach) :: EstuaryReach
        real(dp) :: meanDepth               !! Mean estuary depth for use in tidal depth calculations [m]
        real(dp) :: distanceToMouth         !! Distance to the mouth of the estuary [m]
        real(dp) :: tidalM2                 !! Tidal harmonic coefficient M2 [-]
        real(dp) :: tidalS2                 !! Tidal harmonic coefficient S2 [-]

      contains
        ! Create/destroy
        procedure :: create => createEstuaryReach
        procedure :: destroy => destroyEstuaryReach
        ! Simulators
        procedure :: update => updateEstuaryReach
        procedure :: setDimensions
        ! Data handlers
        procedure :: parseInputData => parseInputDataEstuaryReach
        ! Calculators
        procedure :: calculateWidth => calculateWidth
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
        procedure :: changeInVolume => changeInVolume
    end type

  contains

    function createEstuaryReach(me, x, y, w, gridCellArea) result(rslt)
        class(EstuaryReach) :: me                 !! This `EstuaryReach` instance
        integer :: x                            !! Grid cell x-position index
        integer :: y                            !! Grid cell y-position index
        integer :: w                            !! Water body index within the cell
        real(dp) :: gridCellArea                !! Containing grid cell area [m2]
        type(Result) :: rslt                    !! Result object to return errors in
        integer :: i, j                         ! Iterator

        ! Set reach references (indices set in WaterBody%create) and grid cell area
        call rslt%addErrors(.errors. me%WaterBody%create(x, y, w, gridCellArea))
        me%ref = trim(ref("EstuaryReach", x, y, w))

        ! Parse input data and allocate/initialise variables. The order here is important:
        ! allocation depends on the input data.
        call rslt%addErrors(.errors. me%parseInputData())
        call me%allocateAndInitialise()

        ! Create the BedSediment for this RiverReach
        ! TODO: Get the type of BedSediment from the data file, and check for allst
        allocate(BedSediment1 :: me%bedSediment)
        allocate(Reactor1 :: me%reactor)
        allocate(Biota1 :: me%biota)
        call rslt%addErrors([ &
            .errors. me%bedSediment%create(me%ncGroup), &
            .errors. me%reactor%create(me%x, me%y, me%alpha_hetero), &
            .errors. me%biota%create() &
        ])
        
        ! Create the PointSource object(s), if this reach has any
        if (me%hasPointSource) then
            do i = 1, size(me%pointSources)
                call rslt%addErrors(.errors. me%pointSources(i)%create(me%x, me%y, i, [trim(me%ref)]))
            end do
        end if
        ! Create the DiffuseSource object(s), if this reach has any
        if (me%hasDiffuseSource) then
            do i = 1, size(me%diffuseSources)
                call rslt%addErrors(.errors. me%diffuseSources(i)%create(me%x, me%y, i, [trim(me%ref)]))
            end do
        end if

        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOG%toFile("Creating " // trim(me%ref) // ": success")
    end function


    !> Destroy this `EstuaryReach`
    function destroyEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me                             !! This `EstuaryReach` instance
        type(Result) :: rslt                                !! The `Result` object
        ! TODO: Write some destroy logic
    end function

    !> Run the estuary reach simulation for this timestep
    function updateEstuaryReach(me, t, q_runoff, j_spm_runoff, j_np_runoff) result(rslt)
        class(EstuaryReach) :: me
        integer :: t
        real(dp), optional :: q_runoff                          !! Runoff (slow + quick flow) from the hydrological model [m/timestep]
        real(dp), optional :: j_spm_runoff(:)                   !! Eroded sediment runoff to this reach [kg/timestep]
        real(dp), optional :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this reach [kg/timestep]
        type(Result) :: rslt
        !--- Locals ---!
        real(dp) :: Q_outflow
        real(dp) :: changeInVolume, previousVolume
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)
        real(dp) :: j_np_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: j_spm_in_total(C%nSizeClassesSpm)           ! Total inflow of SPM [kg/timestep]
        real(dp) :: j_np_in_total(C%npDim(1), C%npDim(2), C%npDim(3))   ! Total inflow of NP [kg/timestep]
        real(dp) :: fractionSpmDeposited(C%nSizeClassesSpm)     ! Fraction of SPM deposited on each time step [-]
        integer :: i, j                                         ! Iterator
        integer :: nDisp                                        ! Number of displacements to split this time step into
        real(dp) :: dt                                          ! Length of each displacement [s]
        real(dp) :: dQ(size(me%Q))                              ! Water flow array (Q) for each displacement
        real(dp) :: dj_spm(size(me%j_spm, 1), C%nSizeClassesSpm)   ! SPM flow array (j_spm) for each displacement
        real(dp) :: dj_np(size(me%j_np, 1), C%npDim(1), C%npDim(2), C%npDim(3))   ! NM flow array (j_np) for each displacement
        real(dp) :: dj_spm_deposit(C%nSizeClassesSpm)           ! Deposited SPM for each displacement
        real(dp) :: j_spm_deposit(C%nSizeClassesSpm)            ! To keep track of SPM deposited
        real(dp) :: dj_spm_resus(C%nSizeClassesSpm)             ! Mass of each sediment size class resuspended on each displacement [kg]
        real(dp) :: dj_spm_in(C%nSizeClassesSpm)
        real(dp) :: dj_np_in(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: tpm_m_spm(C%nSizeClassesSpm)

        ! Initialise flows to zero
        fractionSpmDeposited = 0
        j_spm_deposit = 0
        me%Q = 0                    ! Final Q, j_spm etc still stored in Q_final, j_spm_final etc for other reaches to use
        me%j_spm = 0
        me%j_np = 0
        me%j_ionic = 0
        if (t == 1) then
            previousVolume = 0.0_dp
        else
            previousVolume = me%volume      ! me%volume will be changed by setDimensions() below
        end if
        
        ! Inflows from water bodies, making sure to use their *final* flow arrays to ensure we're not
        ! getting their outflow on this timestep, rather than the last timestep
        do i = 1, me%nInflows
            ! TODO for some reason the getters j_spm_outflow_final() and j_np... don't work here with gfortran (internal compiler error)
            call me%set_Q_inflow(-me%inflows(i)%item%Q_final(1), i)
            call me%set_j_spm_inflow(-me%inflows(i)%item%j_spm_final(1,:), i)
            call me%set_j_np_inflow(-me%inflows(i)%item%j_np_final(1,:,:,:), i)
        end do

        ! Inflows from runoff
        if (present(q_runoff)) call me%set_Q_runoff(q_runoff*me%gridCellArea)   ! Convert [m/timestep to m3/timestep] TODO what does HMF output?
        if (present(j_spm_runoff)) call me%set_j_spm_runoff(j_spm_runoff)
        if (present(j_np_runoff)) call me%set_j_np_runoff(j_np_runoff)

        ! TODO Inflows from transfers

        ! Inflows from sources (if there are any):
        ! Run the update method, which sets PointSource's j_np_pointsource variable
        ! for this time step. j_np_pointsource = 0 if there isn't a point source
        ! Same for diffuse sources, convert j_np_diffuseSource from kg/m2/timestep to kg/reach/timestep
        do i = 1, me%nDiffuseSources
            call rslt%addErrors(.errors. me%diffuseSources(i)%update(t))
            call me%set_j_np_diffusesource(me%diffuseSources(i)%j_np_diffuseSource*me%bedArea, i)
        end do
        do i = 1, me%nPointSources
            call rslt%addErrors(.errors. me%pointSources(i)%update(t))
            call me%set_j_np_pointsource(me%pointSources(i)%j_np_pointSource, i)
        end do

        ! Set the reach dimensions (using the timestep in hours for tidal harmonics) and calculate the velocity
        call me%setDimensions((t-1)*C%timeStep/3600)
        changeInVolume = me%changeInVolume((t-1)*24, t*24)

        ! Calculate the outflow based on the change in volume and inflows. +ve outflow indicates upstream tidal flow,
        ! -ve outflow indicates downstream tidal flow. This is used to determine what classes as "input" SPM/NM
        Q_outflow = changeInVolume - me%Q_inflows() - me%Q_runoff() - me%Q_transfers()
        ! Input will always be from runoff, transfers and sources
        ! TODO that ^ is not true for transfers
        me%Q_in_total = me%Q_runoff() + me%Q_transfers()
        ! j_spm_input_total = me%j_spm_runoff() + me%j_spm_transfers()
        ! j_np_input_total = me%j_np_runoff() + me%j_np_transfers() + me%j_np_pointsource() + me%j_np_diffusesource()
        ! If outflow is positive, then some input will be provided by the inflowing outflow
        if (Q_outflow > 0) then
            me%Q_in_total = me%Q_in_total + me%Q_outflow()
            ! j_spm_input_total = j_spm_input_total + me%j_spm_outflow()
            ! j_np_input_total = j_np_input_total + me%j_np_outflow()
        end if
        ! If inflow is positive, input will also be from inflows
        if (me%Q_inflows() > 0) then
            me%Q_in_total = me%Q_in_total + me%Q_inflows()
            ! j_spm_input_total = j_spm_input_total + me%j_spm_inflows()
            ! j_np_input_total = j_np_input_total + me%j_np_inflows()
        end if

        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)

        ! HACK
        if (me%volume < 10.0_dp) then
            me%volume = 0.0_dp
        end if

        ! Set the resuspension rate me%k_resus and settling rate me%k_settle
        ! (but don't acutally settle until we're looping through
        ! displacements). This can be done now as settling/resuspension rates
        ! don't depend on anything that changes on each displacement
        call me%setResuspensionRate()                   ! Computes resuspension rate [s-1] over complete timestep
        call me%setSettlingRate()                       ! Computes settling rate [s-1] over complete timestep

        ! If Q_in for this timestep is bigger than the reach volume, then we need to
        ! split into a number of displacements. If Q_in is zero, just have 1 displacement.
        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = 1
        else
            nDisp = ceiling(me%Q_in_total/me%volume)        ! Number of displacements
        end if
        nDisp = max(nDisp, C%timeStep/3600)                 ! Make sure there is at least 1 displacement per hour
        dt = C%timeStep/nDisp                               ! Length of each displacement [s]
        dQ = me%Q/nDisp                                     ! Water flow array for each displacement
        dj_spm = me%j_spm/nDisp                             ! SPM flow array for each displacement
        dj_np = me%j_np/nDisp                               ! NM flow array for each displacement
        if (allocated(me%m_np_disp)) deallocate(me%m_np_disp)               ! Deallocate if already allocated from previous time step
        if (allocated(me%C_np_disp)) deallocate(me%C_np_disp)               ! Deallocate if already allocated from previous time step
        allocate(me%m_np_disp(nDisp, C%npDim(1), C%npDim(2), C%npDim(3)))   ! Allocate the NM mass array for each displacement
        allocate(me%C_np_disp(nDisp, C%npDim(1), C%npDim(2), C%npDim(3)))   ! Allocate the NM conc array for each displacement

        do i = 1, nDisp
            ! Calculate the change in volume between this displacement and the next
            changeInVolume = me%changeInVolume((t-1)*24 + (i-1)*(int(dt)/3600), (t-1)*24 + i*(int(dt)/3600))
            ! Calculate the timestep in hours from the displacement length, and pass to setDimensions
            ! to use to calculate tidal harmonics
            call me%setDimensions((t-1)*C%timeStep/3600 + i*(int(dt)/3600))
            ! Water mass balance (outflow = all the inflows + change in volume)
            dQ(1) = -sum(dQ(2:)) + changeInVolume

            ! If this displacement's outflow is -ve, tidal flow must be downstream and
            ! outflowing SPM/NM is a function of this reach's SPM/NM conc, else if it is
            ! +ve, then it must be a function of the outflow reach's SPM/NM conc (if that
            ! outflow reach exists)
            if (dQ(1) < 0 .and. .not. isZero(me%volume)) then
                dj_spm(1,:) = max(me%m_spm * dQ(1) / me%volume, -me%m_spm)  ! SPM outflow
                dj_np(1,:,:,:) = max(me%m_np * dQ(1) / me%volume, -me%m_np) ! NM outflow
                dj_spm_in = sum(dj_spm(2:,:), dim=1) - dj_spm(4+me%nInflows,:)    ! Total SPM input to this displacement (not deposition)
                dj_np_in = sum(dj_np(2:,:,:,:), dim=1) - dj_np(4+me%nInflows,:,:,:)     ! Total NM input to this displacement (not deposition)
            else if (dQ(1) > 0 .and. associated(me%outflow%item)) then
                dj_spm(1,:) = me%outflow%item%C_spm * dQ(1)                 ! SPM outflow
                dj_np(1,:,:,:) = me%outflow%item%C_np * dQ(1)               ! NM outflow
                dj_spm_in = dj_spm(1,:) + sum(dj_spm(2+me%nInflows:,:), dim=1) - dj_spm(4+me%nInflows,:)              ! Total SPM input
                dj_np_in = dj_np(1,:,:,:) + sum(dj_np(2+me%nInflows:,:,:,:), dim=1) - dj_np(4+me%nInflows,:,:,:)    ! Total NM input
            else if (dQ(1) > 0 .and. .not. associated(me%outflow%item)) then
                ! If there is no outflow but tidal flow is in, set inflow SPM/NM to zero
                dj_spm(1,:) = 0.0_dp
                dj_np(1,:,:,:) = 0.0_dp
                dj_spm_in = sum(dj_spm(2+me%nInflows:,:), dim=1) - dj_spm(4+me%nInflows,:)
                dj_np_in = sum(dj_np(2+me%nInflows:,:,:,:), dim=1) - dj_np(4+me%nInflows,:,:,:)
            else
                dj_spm(1,:) = 0.0_dp
                dj_np(1,:,:,:) = 0.0_dp
                dj_spm_in = 0.0_dp
                dj_np_in = 0.0_dp
            end if

            tpm_m_spm = max(me%m_spm + dj_spm_in, 0.0_dp)           ! Check the SPM isn't making the new mass negative
            ! SPM deposition and resuspension. Use m_spm as previous m_spm + inflow - outflow, making sure to
            ! not pick up on the previous displacement's deposition (index 4+me%nInflows)
            dj_spm_deposit = min(me%k_settle*dt*(tpm_m_spm), &
                tpm_m_spm)
            dj_spm_resus = me%k_resus * me%bedSediment%Mf_bed_by_size() * dt


            ! Calculate the fraction of SPM from each size class that was deposited, for use in calculating mass of NM deposited
            do j = 1, C%nSizeClassesSpm 
                if (isZero(dj_spm_deposit(j))) then
                    fractionSpmDeposited(j) = 0
                else
                    fractionSpmDeposited(j) = dj_spm_deposit(j) / (tpm_m_spm(j))  ! TODO include resus
                end if
            end do
            ! Update the deposition element of the SPM and NM flux array
            dj_spm(4+me%nInflows,:) = dj_spm_resus - dj_spm_deposit
            do j = 1, C%nSizeClassesSpm
                dj_np(4+me%nInflows,:,:,2+j) = -min(me%m_np(:,:,2+j)*fractionSpmDeposited(j), me%m_np(:,:,2+j))   ! Only deposit heteroaggregated NM (index 3+)
            end do
            
            ! SPM and NM mass balance. As outflow was set before deposition etc fluxes, we need to check that masses aren't below zero again
            me%m_spm = max(me%m_spm + sum(dj_spm, dim=1), 0.0_dp)
            me%m_np = max(me%m_np + sum(dj_np, dim=1), 0.0_dp)
            me%m_np_disp(i,:,:,:) = me%m_np
            if (.not. isZero(me%volume)) then
                me%C_np = me%m_np / me%volume
                me%C_np_disp(i,:,:,:) = me%m_np / me%volume
            else
                me%C_np = 0
                me%C_np_disp(i,:,:,:) = 0.0_dp
            end if

            ! Add the calculated fluxes (outflow and deposition) to the total. Don't update inflows
            ! (inflows, runoff, sources) as they've already been correctly before the disp loop
            call me%set_Q_outflow(me%Q_outflow() + dQ(1))
            call me%set_j_spm_outflow(me%j_spm_outflow() + dj_spm(1,:))
            call me%set_j_spm_deposit(me%j_spm_deposit() + dj_spm(4+me%nInflows,:))
            call me%set_j_np_outflow(me%j_np_outflow() + dj_np(1,:,:,:))
            call me%set_j_np_deposit(me%j_np_deposit() + dj_np(4+me%nInflows,:,:,:))

            ! If we're including bed sediment, then deposit and resuspend to/from
            if (C%includeBedSediment) then
                call rslt%addErrors(.errors. &
                    me%bedSediment%resuspend(dj_spm_resus / me%bedArea))    ! remove resuspended SPM from BedSediment
                if (rslt%hasCriticalError()) return                         ! exit if a critical error has been thrown

                call rslt%addErrors(.errors. me%depositToBed(dj_spm_deposit)) ! add deposited SPM to BedSediment 
                if (rslt%hasCriticalError()) return                         ! exit if a critical error has been thrown
            end if

            ! Write stuff to output file
            write(7,*) t, ",", (t-1)*C%timeStep + i*C%timeStep/nDisp, ",", me%x, ",", me%y, ",", me%w, ",", &
                sum(me%m_np), ",", sum(me%C_np), ",", me%volume, ",", me%Q_outflow()

            ! TODO Deposit and resuspend NM to/from bed sediment
        end do

        ! Update the SPM concentration. isZero check used to avoid numerical errors
        ! from very small m_spm numbers.
        do j = 1, C%nSizeClassesSpm
            if (isZero(me%m_spm(j)) .or. isZero(me%volume)) then
                me%C_spm(j) = 0.0_dp
                me%m_spm(j) = 0.0_dp            ! Needed because sometimes m_spm ends up as 1e-300 and causes FPEs
            else
                me%C_spm(j) = me%m_spm(j) / me%volume
            end if
        end do   

        ! Transform the NPs. TODO: Should this be done before or after settling/resuspension?
        ! TODO for the moment, ignoring heteroaggregation if no volume, need to figure out
        ! what to really do if there are no flows
        if (.not. isZero(me%volume)) then
            call rslt%addErrors([ &
                .errors. me%reactor%update( &
                    t, &
                    me%m_np, &  
                    me%C_spm, &
                    me%T_water, &
                    me%W_settle_np, &
                    me%W_settle_spm, &
                    10.0_dp, &                      ! HACK: Where is the shear rate from?
                    me%volume &
                ) &
            ])
            ! Get the resultant transformed mass from the Reactor
            me%m_np = me%reactor%m_np
        end if

        ! Set the final concentrations, checking that the river has a volume
        if (.not. isZero(me%volume)) then
            me%C_spm = me%m_spm/me%volume
            me%C_np = me%m_np/me%volume
        else
            me%C_spm = 0.0_dp
            me%C_np = 0.0_dp
        end if

        ! Update the biota
        ! TODO which forms/states of NM should go to biota?
        call rslt%addErrors(.errors. me%biota%update(t, [sum(me%C_np), 0.0_dp]))

        ! If there's no SPM left, add the "all SPM advected" warning
        ! TODO Maybe the same for NPs
        do i = 1, C%nSizeClassesSpm
            if (isZero(me%m_spm(i)) .and. j_spm_in_total(i) /= 0) then
                call rslt%addError(ErrorInstance( &
                    code = 500, &
                    message = "All SPM in size class " // trim(str(i)) // " (" // trim(str(C%d_spm(i)*1e6)) // &
                            " um) advected from EstuaryReach.", &
                    isCritical = .false.) &
                )
            end if 
        end do

        ! Add what we're doing here to the error trace
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function


    !> Set the dimensions (width, depth, area, volume) of the reach
    subroutine setDimensions(me, tHours)
        class(EstuaryReach) :: me
        integer :: tHours
        me%depth = me%calculateDepth(tHours)
        me%xsArea = me%depth*me%width                       ! Calculate the cross-sectional area of the reach [m2]
        me%bedArea = me%width*me%length*me%f_m              ! Calculate the BedSediment area [m2]
        me%surfaceArea = me%bedArea                         ! For river reaches, set surface area equal to bed area
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

        ! Set the data interfacer's group to the group for this reach
        call rslt%addErrors(.errors. DATA%setGroup([character(len=100) :: &
            'Environment', &
            ref('GridCell', me%x, me%y), &
            me%ref &
        ]))
        me%ncGroup = DATA%grp

        ! Check if this reach has/   any diffuse sources. me%hasDiffuseSource defauls to .false.
        ! Allocate me%diffuseSources accordingly. The DiffuseSource class actually gets the data.
        if (DATA%grp%hasGroup("PointSource") .or. DATA%grp%hasGroup("PointSource_1")) then
            me%hasPointSource = .true.
            allocate(me%pointSources(1))
            i = 2               ! Any extra point sources?
            do while (DATA%grp%hasGroup("PointSource_" // trim(str(i))))
                deallocate(me%pointSources)
                allocate(me%pointSources(i))
                i = i+1
            end do
            me%nPointSources = size(me%pointSources)
        else
            me%nPointSources = 0
        end if

        ! Check if this reach has any diffuse sources. me%hasDiffuseSource defauls to .false.
        ! Allocate me%diffuseSources accordingly. The DiffuseSource class actually gets the data.
        if (DATA%grp%hasGroup("DiffuseSource") .or. DATA%grp%hasGroup("DiffuseSource_1")) then
            me%hasDiffuseSource = .true.
            allocate(me%diffuseSources(1))
            i = 2               ! Any extra diffuse sources?
            do while (DATA%grp%hasGroup("DiffuseSource_" // trim(str(i))))
                deallocate(me%diffuseSources)
                allocate(me%diffuseSources(i))
                i = i+1
            end do
            me%nDiffuseSources = size(me%diffuseSources)
        else
            me%nDiffuseSources = 0
        end if

        ! Get the length of the reach, if present. Otherwise, set to 0 and GridCell will deal with calculating
        ! length. Note that errors might be thrown from GridCell if the reaches lengths within the GridCell are
        ! not physically possible within the reach (e.g., too short).
        call rslt%addErrors([ &
            .errors. DATA%get('length', me%length, 0.0_dp), &   ! Length is calculated by GridCell if it defaults here
                ! Note that errors might be thrown from GridCell if the reaches' lengths within GridCell are
                ! not physicaly possible within the reach (e.g. too short)       
            .errors. DATA%get('slope', me%slope), &             ! TODO: Slope should default to GridCell slope
            .errors. DATA%get('f_m', me%f_m, C%defaultMeanderingFactor), &         ! Meandering factor
            .errors. DATA%get('alpha_res', me%alpha_resus), &   ! Resuspension alpha parameter
            .errors. DATA%get('beta_res', me%beta_resus), &     ! Resuspension beta parameter
            .errors. DATA%get('alpha_hetero', me%alpha_hetero, C%default_alpha_hetero_estuary), &
                ! alpha_hetero defaults to that specified in config.nml
            .errors. DATA%get('domain_outflow', me%domainOutflow, silentlyFail=.true.), &
            .errors. DATA%get('mean_depth', me%meanDepth, 0.0_dp), &
            .errors. DATA%get('width', me%width, 0.0_dp), &
            .errors. DATA%get('tidal_M2', me%tidalM2, C%tidalM2), &
            .errors. DATA%get('tidal_S2', me%tidalS2, C%tidalS2), &
            .errors. DATA%get('distance_to_mouth', me%distanceToMouth), &
            .errors. DATA%get('stream_order', me%streamOrder) &
        ])
        if (allocated(me%domainOutflow)) me%isDomainOutflow = .true.    ! If we managed to set domainOutflow, then this reach is one

        ! HACK for the width of the Thames
        if (me%width == 0.0_dp) then
            ! Hardisty, pg 50
            me%width = 5000 * exp(-4.25*me%distanceToMouth/80000)
        end if

        ! HACK for depth of Thames
        if (me%meanDepth == 0.0_dp) then
            ! Hardisty, pg 53. 6 m in mean depth at Southend Pier
            me%meanDepth = 6 * exp(-1.4*me%distanceToMouth/80000)
        end if
        
        ! ROUTING: Get the references to the inflow(s) EstuaryReaches and
        ! store in inflowRefs(). Do some auditing as well.
        if (DATA%grp%hasVariable("inflows")) then
            call rslt%addErrors(.errors. DATA%get('inflows', inflowArray))
            ! There mustn't be more than 7 inflows to a reach (one from
            ! each side/corner of the inflow GridCell)
            if (size(inflowArray, 2) > 7) then
                call rslt%addError(ErrorInstance(code=403))
            ! If there is an inflow group but nothing in it, this reach
            ! must be a headwater
            else if (size(inflowArray, 2) == 0) then
                allocate(me%inflowRefs(0))                          
                me%nInflows = 0
                me%isHeadwater = .true.
            else
                ! Set the number of inflows from the input inflowArray
                allocate(me%inflowRefs(size(inflowArray, 2)))
                me%nInflows = size(me%inflowRefs)
                ! Loop through the inflow from data and store them at the object level
                do i = 1, me%nInflows                               ! Loop through the inflows
                    me%inflowRefs(i)%x = inflowArray(1,i)           ! Inflow x reference
                    me%inflowRefs(i)%y = inflowArray(2,i)           ! Inflow y reference
                    me%inflowRefs(i)%w = inflowArray(3,i)           ! Inflow EstuaryReach reference
                    ! Check the inflow is from a neighbouring EstuaryReach
                    if (abs(me%inflowRefs(i)%x - me%x) > 1 .or. abs(me%inflowRefs(i)%y - me%y) > 1) then
                        call rslt%addError(ErrorInstance(code=401))
                    end if
                    ! Is this reach an inflow to the GridCell (i.e., are the inflows to this reach
                    ! from another GridCell)? We only need to check for the first inflow (i=1),
                    ! as the next bit checks that all inflows are from the same GridCell
                    if (i == 1 .and. (me%inflowRefs(i)%x /= me%x .or. me%inflowRefs(i)%y /= me%y)) then
                        me%isGridCellInflow = .true.
                    end if
                    ! If there is more than one inflow to the reach, it must be
                    ! an inflow to the cell. Therefore, we need to check all
                    ! inflows are from the same cell
                    if (i > 1 .and. me%inflowRefs(i)%x /= me%inflowRefs(1)%x .and. me%inflowRefs(i)%y /= me%inflowRefs(1)%y) then
                        call rslt%addError(ErrorInstance(code=402))
                    end if
                end do
            end if
        else
        ! Else there mustn't be any inflows (i.e. it's a headwater)
            allocate(me%inflowRefs(0))                          
            me%nInflows = 0
            me%isHeadwater = .true.
        end if
        ! Allocate inflows() array (the array of pointers) to the correct size
        allocate(me%inflows(me%nInflows))
        
        ! If the data has an outflow to the model domain specified, set that
        if (me%ncGroup%hasVariable("domain_outflow")) then
            me%isDomainOutflow = .true.
        end if

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
        class(EstuaryReach), intent(in) :: me     !! The `EstuaryReach` instance
        real(dp), intent(in) :: Q               !! `GridCell` discharge \( Q \) [m3/s]
        real(dp) :: width                       !! The calculated width \( W \) [m]
        width = 1.22*Q**0.557
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

        depth = me%tidalS2 * cos(2.0_dp*C%pi*tHours/12.0_dp) + me%tidalM2 * cos(2.0_dp*C%pi*tHours/12.42_dp) &
            + (0.75_dp) * ((me%distanceToMouth * me%tidalM2 ** 2)/(me%meanDepth * 22356.0_dp * sqrt(9.81_dp * me%meanDepth))) &
            * cos(2*C%pi*tHours/6.21_dp) + C%tidalDatum
        ! If the depth is negative, set it to zero
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
!> Container module for class `Environment1`
module classEnvironment1
    use mo_netcdf
    use Globals
    use UtilModule
    use spcEnvironment
    use ResultModule
    use classGridCell2
    use classDataInterfacer, only: DATA
    use classDatabase, only: DATASET
    use classSampleSite, only: SampleSite
    implicit none
    private
    
    !> The `Environment1` class acts as a container for all other
    !! environmental compartments, triggering their creation, simulation
    !! and passing data between them
    type, public, extends(Environment) :: Environment1

      contains
        procedure :: create => createEnvironment1
        procedure :: destroy => destroyEnvironment1
        procedure :: update => updateEnvironment1
        procedure :: updateReach => updateReachEnvironment1
        procedure :: parseInputData => parseInputDataEnvironment1
        ! Getters
        procedure :: get_m_np => get_m_npEnvironment1
    end type

  contains

    !> Create the `Environment`, which sets up the grid and river structure.
    !! The `Environment` instance must be a target so that `SubRiver` inflows
    !! can point to another `SubRiver` object:
    !! ([see here](https://stackoverflow.com/questions/45761050/pointing-to-a-objects-type-variable-fortran/))
    function createEnvironment1(me) result(r)
        class(Environment1), target :: me
            !! This `Environment` instace. Must be target so `SubRiver`s can be pointed at.
        type(Result) :: r                                       !! `Result` object to return any error(s) in
        integer :: x, y, w, i, j, b, ix, iy, iw, rr             ! Iterators
        character(len=100) :: gridCellRef                       ! To store GridCell name in, e.g. "GridCell_x_y"
        integer :: gridCellType                                 ! Integer representing the GridCell type
        type(ReachPointer), allocatable :: tmpHeadwaters(:)     ! Temporary headwaters array
        type(ErrorInstance), allocatable :: errors(:)           ! Errors to return

        ! Allocate grid cells array to be the shape of the grid
        allocate(me%colGridCells(DATASET%gridShape(1), DATASET%gridShape(2)))
        ! Loop over grid and create cells
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                ! If this grid cell isn't masked, create it
                if (.not. DATASET%gridMask(x,y)) then
                    allocate(GridCell2::me%colGridCells(x,y)%item)
                    call r%addErrors(.errors. &
                        me%colGridCells(x,y)%item%create(x,y) &
                    )
                ! If it is masked, still create it but tell it that it's empty
                else
                    allocate(GridCell2::me%colGridCells(x,y)%item)
                    call r%addErrors(.errors. &
                        me%colGridCells(x,y)%item%create(x,y,isEmpty=.true.) &
                    )
                end if
            end do
        end do
        
        if (.not. r%hasCriticalError()) then            
            ! Now we need to create links between waterbodies, which wasn't possible before all cells
            ! and their waterbodies were created. We do this by pointing reach%inflows and reach%outflow
            ! to correct waterbody object.
            do y = 1, DATASET%gridShape(2)
                do x = 1, DATASET%gridShape(1)
                    if (.not. me%colGridCells(x,y)%item%isEmpty) then
                        do w = 1, me%colGridCells(x,y)%item%nReaches      ! Loop through the reaches
                            associate (reach => me%colGridCells(x,y)%item%colRiverReaches(w)%item)
                                ! Loop through the inflows for this reach
                                do i = 1, reach%nInflows
                                    iw = reach%inflowsArr(i,1)
                                    ix = reach%inflowsArr(i,2)
                                    iy = reach%inflowsArr(i,3)
                                    ! We've already checked the inflows are in the model domain, so set this
                                    ! reach's inflow to the correct river
                                    reach%inflows(i)%item => me%colGridCells(ix,iy)%item%colRiverReaches(iw)%item
                                    ! Set the outflow of this reach's inflow to this reach
                                    reach%inflows(i)%item%outflow%item => reach
                                    ! If the inflow is a river and this reach is an estuary, set the estuary to
                                    ! be the tidal limit
                                    if (reach%ref(1:3) == 'Est' .and. reach%inflows(i)%item%ref(1:3) == 'Riv') then
                                        reach%isTidalLimit = .true.
                                    end if
                                end do
                                ! If this is a headwater, add to headwaters array to start routing from
                                if (reach%isHeadwater) then
                                    me%nHeadwaters = me%nHeadwaters + 1                 ! Extend nHeadwater by one
                                    allocate(tmpHeadwaters(me%nHeadwaters))         ! Move around the allocation to add extra element
                                    if (me%nHeadwaters > 1) then
                                        tmpHeadwaters(1:me%nHeadwaters-1) = me%headwaters
                                    end if
                                    call move_alloc(tmpHeadwaters, me%headwaters)
                                    me%headwaters(me%nHeadwaters)%item => reach         ! Point to this reach
                                end if
                            end associate
                        end do
                    end if
                end do
            end do

            ! Now parse the calibration data, if we're meant to be calibrating
            if (C%calibrationRun) then
                allocate(me%sites(18))         ! TODO get number of sites at runtime
                ! TODO move all this data parsing stuff elsewhere and tidy up
                open(unit=10, file=C%siteData)
                read(10, *)
                do i = 1, 18
                    read(10, *) me%sites(i)%ref, me%sites(i)%x, me%sites(i)%y, me%sites(i)%r, me%sites(i)%easts, &
                                me%sites(i)%norths, me%sites(i)%Q, me%sites(i)%C_spm, me%sites(i)%description
                    me%sites(i)%Q = me%sites(i)%Q * C%timeStep
                    me%sites(i)%C_spm = me%sites(i)%C_spm * 0.001
                    me%sites(i)%reach%item => me%colGridCells(me%sites(i)%x,me%sites(i)%y)%item%colRiverReaches(me%sites(i)%r)%item
                    if (trim(C%startSite) == trim(me%sites(i)%ref)) then
                        me%startSite => me%sites(i)
                        me%startSite%isStartSite = .true.
                    end if
                    if (trim(C%endSite) == trim(me%sites(i)%ref)) then
                        me%endSite => me%sites(i)
                        me%endSite%isEndSite = .true.
                    end if
                end do
                ! Set all the sites (except the end site) as boundary sites and set their boundary conditions (C_spm)
                do i = 1, size(me%sites)
                    if (.not. me%sites(i)%isEndSite) then
                        me%sites(i)%reach%item%isBoundary = .true.
                        me%sites(i)%reach%item%boundary_C_spm = me%sites(i)%C_spm           ! [kg/m3]
                        me%sites(i)%reach%item%boundary_Q = me%sites(i)%Q                   ! [m3/timestep]
                    end if
                end do
            end if
        end if
        
        call r%addToTrace('Creating the Environment')           ! Add this procedure to the trace
        call LOG%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors= .errors. r)          ! Trigger any errors present
        call r%clear()                                          ! Remove any errors so we don't trigger them twice
        call LOG%toConsole('Creating the Environment: \x1B[32msuccess\x1B[0m')
    end function

    !> Destroy the `Environment` instance
    function destroyEnvironment1(me) result(r)
        class(Environment1) :: me                               !! This `Environment` instance
        type(Result) :: r                                       !! Return error(s) in `Result` object
        ! Destroy logic here
    end function

    !> Perform simulations for the `Environment`
    function updateEnvironment1(me, t) result(r)
        use omp_lib
        class(Environment1), target :: me                       !! This `Environment` instance
        integer :: t                                            !! Current time step
        type(Result) :: r                                       !! Return error(s) in `Result` object
        type(ReachPointer) :: reach                             ! Pointer to the reach we're updating
        integer :: i, j, x, y
        real(dp) :: lengthRatio                                 ! Reach length as a proportion of total river length in cell
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3)) ! NP runoff for this time step
        logical :: goDownstream                                 ! Have all the inflow reaches been updated yet?
        logical :: endSiteReached                               ! When doing the calibration loop, did we reach the end site?

        call LOG%add("Performing simulation for time step #" // trim(str(t)) // "...")

        ! Update all grid cells first
        !!$omp parallel do private(y)
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                call r%addErrors(.errors. me%colGridCells(x,y)%item%update(t))
            end do
        end do
        !!$omp end parallel do

        ! If this isn't a calibration run, then all of the reaches should be updated in routing order
        if (.not. C%calibrationRun) then
            ! Loop through the headwaters and route from these downstream
            do i = 1, me%nHeadwaters
                reach%item => me%headwaters(i)%item
                ! Update the headwater
                call r%addErrors(.errors. me%updateReach(t, reach))
                ! Check this reach has an outflow, before moving on to the outflow and updating that,
                ! and so on downstream until we hit a reach that has inflows that haven't been updated.
                ! If this is the case, we exit the loop and another headwater's downstream routing
                ! will pick up where the current headwater's routing has stopped. We also check that
                ! there is a downstream reach. The goDownstream flag is in charge of telling the loop
                ! whether to proceed or not.
                if (associated(reach%item%outflow%item)) then
                    reach%item => reach%item%outflow%item
                    goDownstream = .true.
                    do while (goDownstream)
                        call r%addErrors(.errors. me%updateReach(t, reach))
                        if (.not. associated(reach%item%outflow%item)) then
                            goDownstream = .false.
                        else
                            ! Point reach to the next downstream reach
                            reach%item => reach%item%outflow%item
                            ! Check all of the next reach's inflows have been updated,
                            ! otherwise the do loop will stop and another headwater's
                            ! downstream routing will pick up where we've left off
                            do j = 1, reach%item%nInflows
                                if (.not. reach%item%inflows(j)%item%isUpdated) then
                                    goDownstream = .false.
                                end if
                            end do
                        end if
                    end do
                end if
            end do
        ! If this is a calibration run, we need to start at the specified sites in the config file and go
        ! downstream.
        else

            goDownstream = .true.
            reach%item => me%startSite%reach%item
            do while (goDownstream)
                call r%addErrors(.errors. me%updateReach(t, reach))
                if (.not. associated(reach%item%outflow%item)) then
                    goDownstream = .false.          ! Can't go any further!
                    endSiteReached = .false.        ! We never reached the given end site
                else if (trim(reach%item%ref) == trim(me%endSite%reach%item%ref)) then
                    goDownstream = .false.
                    endSiteReached = .true.
                else
                    ! Point reach to the next downstream reach
                    reach%item => reach%item%outflow%item
                    ! print *, reach%item%ref
                    ! Check all of the next reach's inflows have been updated,
                    ! otherwise the do loop will stop and another headwater's
                    ! downstream routing will pick up where we've left off
                    ! do j = 1, reach%item%nInflows
                    !     if (.not. reach%item%inflows(j)%item%isUpdated) then
                    !         goDownstream = .false.
                    !     end if
                    ! end do
                end if
            end do

        end if
        ! Finalise the routing by setting outflows to temporary outflows that were stored
        ! to avoid routing using the wrong timestep's outflow as an inflow.
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                call me%colGridCells(x,y)%item%finaliseUpdate()
            end do
        end do
    end function
    
    !> Update an individual reach, also updating the containng grid cell, if it hasn't
    !! already been updated.
    function updateReachEnvironment1(me, t, reach) result(rslt)
        class(Environment1), target :: me   !! This `Environment1` instance
        integer :: t                        !! Time step
        type(ReachPointer) :: reach         !! Pointer to the reach to update
        type(Result) :: rslt                !! The Result object to return any errors in
        type(GridCellPointer) :: cell       ! Pointer to this reach's grid cell
        real(dp) :: lengthRatio             ! Length ratio of this reach to the total reach length in cell
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))     ! Proportion of cell's NM runoff going to this reach

        cell%item => me%colGridCells(reach%item%x, reach%item%y)%item
        ! Determine the proportion of this reach's length to the the total
        ! river length in this GridCell and use it to proportion NM runoff
        lengthRatio = reach%item%length/cell%item%getTotalReachLength()
        j_np_runoff = lengthRatio*cell%item%colSoilProfiles(1)%item%m_np_eroded    ! [kg/timestep]
        ! Update the reach for this timestep
        call rslt%addErrors(.errors. &
            reach%item%update( &
                t = t, &
                q_runoff = cell%item%q_runoff_timeSeries(t), &
                j_spm_runoff = cell%item%erodedSediment*lengthRatio, &
                j_np_runoff = j_np_runoff &
            ) &
        )
    end function
    
    !> Obtain and parse input data for this `Environment` object
    function parseInputDataEnvironment1(me) result(r)
        class(Environment1) :: me                           !! This `Environment1` instance
        type(NcVariable) :: var
        real(dp) :: fillValue
        type(Result) :: r                                   !! The `Result` object to return
        ! Get the grid dimensions for the Environment
        call r%addErrors(.errors. DATA%setGroup(['Environment']))
        call r%addErrors([ &
            .errors. DATA%get('grid_dimensions', me%gridDimensions), &
            .errors. DATA%get('routed_reaches', me%routedReachIndices) &
        ])
        
    end function
    
    function get_m_npEnvironment1(me) result(m_np)
        class(Environment1) :: me
        real(dp) :: m_np(C%nSizeClassesNP, 4, 2 + C%nSizeClassesSpm)
        integer :: x, y, rr
        m_np = 0
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                do rr = 1, size(me%colGridCells(x,y)%item%colRiverReaches)
                    m_np = m_np + me%colGridCells(x,y)%item%colRiverReaches(rr)%item%reactor%m_np
                end do
            end do
        end do
        
    end function

end module

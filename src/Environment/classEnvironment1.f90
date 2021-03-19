!> Container module for class `Environment1`
module classEnvironment1
    use mo_netcdf
    use Globals
    use UtilModule
    use spcEnvironment
    use ResultModule
    use classGridCell2
    use classDatabase, only: DATASET
    use classSampleSite, only: SampleSite
    use DefaultsModule, only: iouCalibrationSites
    use datetime_module
    use mod_datetime
    implicit none
    private
    
    !> The `Environment1` class acts as a container for all other
    !! environmental compartments, triggering their creation, simulation
    !! and passing data between them
    type, public, extends(Environment) :: Environment1

      contains
        procedure :: create => createEnvironment1
        procedure :: update => updateEnvironment1
        procedure :: updateReach => updateReachEnvironment1
        procedure :: determineStreamOrder => determineStreamOrderEnvironment1
        procedure :: parseNewBatchData => parseNewBatchDataEnvironment1
        procedure :: parseCalibrationData
        ! Getters
        procedure :: get_m_np => get_m_npEnvironment1
        procedure :: get_C_np_soil => get_C_np_soilEnvironment1
        procedure :: get_C_np_water => get_C_np_waterEnvironment1
        procedure :: get_C_np_sediment => get_C_np_sedimentEnvironment1
        procedure :: getBedSedimentArea => getBedSedimentAreaEnvironment1
        procedure :: get_m_sediment_byLayer => get_m_sediment_byLayerEnvironment1
    end type

  contains

    !> Create the `Environment`, which sets up the grid and river structure.
    !! The `Environment` instance must be a target so that `SubRiver` inflows
    !! can point to another `SubRiver` object:
    !! ([see here](https://stackoverflow.com/questions/45761050/pointing-to-a-objects-type-variable-fortran/))
    function createEnvironment1(me) result(r)
        class(Environment1), target :: me
            !! This `Environment` instace. Must be target so children can be pointed at.
        type(Result) :: r                                       !! `Result` object to return any error(s) in
        integer :: x, y, w, i, ix, iy, iw                       ! Iterators
        type(ReachPointer), allocatable :: tmpHeadwaters(:)     ! Temporary headwaters array

        me%nGridCells = 0
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
                    me%nGridCells = me%nGridCells + 1
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
                                    ! Check if this reach is a grid cell inflow (and thus the inflow reach is a grid cell outflow)
                                    if (ix /= x .or. iy /= y) then
                                        reach%inflows(i)%item%isGridCellOutflow = .true.
                                        reach%isGridCellInflow = .true.
                                    end if
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

            ! Finally, perform any creation operations that required proper cell linking (e.g. snapping point sources
            ! to the correct cells)
            do y = 1, DATASET%gridShape(2)
                do x = 1, DATASET%gridShape(1)
                    call me%colGridCells(x,y)%item%finaliseCreate()
                    me%nWaterbodies = me%nWaterbodies + me%colGridCells(x,y)%item%nReaches
                end do
            end do
        
            ! Allocate the routedReaches to the number of waterbodies, and set the stream order for all the reaches
            allocate(me%routedReaches(me%nWaterbodies))
            call me%determineStreamOrder()

            ! Parse calibration data, if there are any
            call me%parseCalibrationData()
        end if

        ! Allocate the per timestep spatial mean water conc array. Being with 0 timesteps, as this array
        ! is reallocated on each timestep (to account for batch runs)
        allocate(me%C_np_water_t(0, C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%C_np_sediment_t(0, C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%m_sediment_t_byLayer(0, C%nSedimentLayers, C%nSizeClassesSpm))
        me%C_np_water_t = 0.0_dp
        me%C_np_sediment_t = 0.0_dp
        me%m_sediment_t_byLayer = 0.0_dp
        
        call r%addToTrace('Creating the Environment')           ! Add this procedure to the trace
        call LOGR%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors= .errors. r)          ! Trigger any errors present
        call r%clear()                                          ! Remove any errors so we don't trigger them twice
        call LOGR%toConsole('Creating the Environment: \x1B[32msuccess\x1B[0m')
    end function

    subroutine parseCalibrationData(me)
        class(Environment1), target :: me
        integer             :: nSites
        integer             :: nObservations
        integer             :: i, j             ! Iterators
        character(len=19)   :: datetimeStr      ! Temp string to store datetime in
        integer             :: year, month, day

            ! Now parse the calibration data, if we're meant to be calibrating
            if (C%calibrationRun) then
                if (trim(C%calibrationMode) == 'mean') then
                    allocate(me%sites(18))         ! TODO get number of sites at runtime
                    allocate(me%otherSites(size(C%otherSites)))
                    ! TODO move all this data parsing stuff elsewhere and tidy up
                    open(unit=iouCalibrationSites, file=C%siteData)
                    read(iouCalibrationSites, *)
                    do i = 1, 18
                        read(iouCalibrationSites, *) me%sites(i)%ref, me%sites(i)%x, me%sites(i)%y, &
                            me%sites(i)%r, me%sites(i)%easts, me%sites(i)%norths, me%sites(i)%Q, &
                            me%sites(i)%C_spm, me%sites(i)%description
                        me%sites(i)%Q = me%sites(i)%Q * C%timeStep
                        me%sites(i)%C_spm = me%sites(i)%C_spm * 0.001
                        me%sites(i)%reach%item => &
                            me%colGridCells(me%sites(i)%x,me%sites(i)%y)%item%colRiverReaches(me%sites(i)%r)%item
                        if (trim(C%startSite) == trim(me%sites(i)%ref)) then
                            me%startSite => me%sites(i)
                            me%startSite%isStartSite = .true.
                        end if
                        if (trim(C%endSite) == trim(me%sites(i)%ref)) then
                            me%endSite => me%sites(i)
                            me%endSite%isEndSite = .true.
                        end if
                        do j = 1, size(C%otherSites)
                            if (trim(C%otherSites(j)) == trim(me%sites(i)%ref)) then
                                me%otherSites(j) = me%sites(i)
                            end if
                        end do
                    end do
                    close(iouCalibrationSites)
                    ! Set all the sites (except the end site) as boundary sites and set their boundary conditions (C_spm)
                    do i = 1, size(me%sites)
                        if (.not. me%sites(i)%isEndSite) then
                            me%sites(i)%reach%item%isBoundary = .true.
                            me%sites(i)%reach%item%boundary_C_spm = me%sites(i)%C_spm           ! [kg/m3]
                        end if
                        me%sites(i)%reach%item%calibrationSiteRef = me%sites(i)%ref
                    end do
                else if (trim(C%calibrationMode) == 'dynamic') then
                    open(iouCalibrationSites, file=C%siteData)
                    read(iouCalibrationSites, *)             ! Skip the column names
                    read(iouCalibrationSites, *) nSites      ! Number of sites
                    allocate(me%sites(nSites))
                    do i = 1, nSites
                        read(iouCalibrationSites, *) nObservations
                        allocate(me%sites(i)%dates(nObservations))
                        allocate(me%sites(i)%C_spm_timeseries(nObservations))
                        allocate(me%sites(i)%Q_timeseries(nObservations))
                        do j = 1, nObservations
                            read(iouCalibrationSites, *) datetimeStr, me%sites(i)%ref, &
                                me%sites(i)%x, me%sites(i)%y, me%sites(i)%r, me%sites(i)%easts, me%sites(i)%norths, &
                                me%sites(i)%Q_timeseries(j), me%sites(i)%C_spm_timeseries(j)
                            me%sites(i)%Q_timeseries(j) = me%sites(i)%Q_timeseries(j) * C%timeStep
                            me%sites(i)%C_spm_timeseries(j) = me%sites(i)%C_spm_timeseries(j) * 0.001
                            read(datetimeStr(1:4), *) year
                            read(datetimeStr(6:7), *) month
                            read(datetimeStr(9:10), *) day
                            me%sites(i)%dates(j) = datetime(year, month, day)
                        end do
                        ! Only tell the reach about this site if it's not the end site
                        if (trim(C%endSite) /= trim(me%sites(i)%ref)) then
                            me%sites(i)%reach%item => &
                                me%colGridCells(me%sites(i)%x, me%sites(i)%y)%item%colRiverReaches(me%sites(i)%r)%item
                            me%sites(i)%reach%item%isBoundary = .true.
                            me%sites(i)%reach%item%calibrationSiteRef = me%sites(i)%ref
                            allocate(me%sites(i)%reach%item%boundary_C_spm_timeseries, source=me%sites(i)%C_spm_timeseries)
                            allocate(me%sites(i)%reach%item%boundary_Q_timeseries, source=me%sites(i)%Q_timeseries)
                            allocate(me%sites(i)%reach%item%boundary_dates, source=me%sites(i)%dates)
                        end if
                    end do
                    close(iouCalibrationSites)
                end if
            end if
    end subroutine

    !> Perform simulations for the `Environment`
    subroutine updateEnvironment1(me, t, tInBatch)
        use omp_lib
        class(Environment1), target :: me                           !! This `Environment` instance
        integer                     :: t                            !! Current time step
        integer                     :: tInBatch                     !! Current time step in full batch run
        integer                     :: i, x, y                      ! Iterators
        type(datetime)              :: currentDate                  ! Current simulation date
        real(dp), allocatable       :: tmp_C_np(:,:,:,:)            ! Temporary array
        real(dp), allocatable       :: tmp_m_sediment(:,:,:)        ! Temporary array
        
        ! Get the current date and log it
        currentDate = C%startDate + timedelta(t-1)
        call LOGR%add("Performing simulation for " // trim(currentDate%strftime('%Y-%m-%d')) // &
                      " (time step #" // trim(str(tInBatch)) // ")...")
        
        !!$omp parallel do private(y,x)
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                ! Only update if this cell isn't masked
                if (DATASET%simulationMask(x,y)) then
                    call me%colGridCells(x,y)%item%update(t)
                end if
            end do
        end do
        !!$omp end parallel do
        
        ! Loop through the routed reaches array (which is in the correct order) and update each reach
        do i = 1, me%nWaterbodies
           call me%updateReach(t, me%routedReaches(i))
        end do
        
        ! Finalise the routing by setting outflows to temporary outflows that were stored
        ! to avoid routing using the wrong timestep's outflow as an inflow.
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                ! Only finalise update if cell isn't masked
                if (DATASET%simulationMask(x,y)) then
                    call me%colGridCells(x,y)%item%finaliseUpdate()
                end if
            end do
        end do

        ! Add to the per timestep spatial weighted mean water and sediment conc array
        ! Here we simply append to the array because we don't want to loose data from
        ! a previous chunk, if we're in batch run mode
        call move_alloc(me%C_np_water_t, tmp_C_np)
        allocate(me%C_np_water_t(size(tmp_C_np, dim=1) + 1, size(tmp_C_np, dim=2), size(tmp_C_np, dim=3), size(tmp_C_np, dim=4)))
        me%C_np_water_t(:size(tmp_C_np, dim=1), :, :, :) = tmp_C_np
        me%C_np_water_t(size(tmp_C_np, dim=1) + 1, :, :, :) = me%get_C_np_water()
        ! Same for sediment
        call move_alloc(me%C_np_sediment_t, tmp_C_np)
        allocate(me%C_np_sediment_t(size(tmp_C_np, dim=1) + 1, size(tmp_C_np, dim=2), size(tmp_C_np, dim=3), size(tmp_C_np, dim=4)))
        me%C_np_sediment_t(:size(tmp_C_np, dim=1), :, :, :) = tmp_C_np
        me%C_np_sediment_t(size(tmp_C_np, dim=1) + 1, :, :, :) = me%get_C_np_sediment()
        ! Sediment mass
        call move_alloc(me%m_sediment_t_byLayer, tmp_m_sediment)
        allocate(me%m_sediment_t_byLayer(size(tmp_m_sediment, dim=1) + 1, size(tmp_m_sediment, dim=2), size(tmp_m_sediment, dim=3)))
        me%m_sediment_t_byLayer(:size(tmp_m_sediment, dim=1), :, :) = tmp_m_sediment
        me%m_sediment_t_byLayer(size(tmp_m_sediment, dim=1) + 1, :, :) = me%get_m_sediment_byLayer()
    end subroutine
    
    !> Update an individual reach, also updating the containng grid cell, if it hasn't
    !! already been updated.
    subroutine updateReachEnvironment1(me, t, reach)
        class(Environment1), target :: me                               !! This `Environment1` instance
        integer                     :: t                                !! Time step
        type(ReachPointer)          :: reach                            !! Pointer to the reach to update
        type(GridCellPointer)       :: cell                             ! Pointer to this reach's grid cell
        real(dp)                    :: lengthRatio                      ! Length ratio of this reach to the total reach length in cell
        real(dp)                    :: j_spm_runoff(C%nSizeClassesSpm)  ! Sediment runoff [kg/timestep]
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))     ! Proportion of cell's NM runoff going to this reach
        real(dp) :: j_transformed_runoff(C%npDim(1), C%npDim(2), C%npDim(3)) ! Proportion of cell's transformed NM runoff going to this reach
        ! Get this reach's cell
        cell%item => me%colGridCells(reach%item%x, reach%item%y)%item
        ! Only update if this cell isn't masked
        if (DATASET%simulationMask(cell%item%x, cell%item%y)) then
            ! Determine the proportion of this reach's length to the the total
            ! river length in this GridCell and use it to proportion NM runoff
            lengthRatio = reach%item%length/cell%item%getTotalReachLength()
            ! Convert eroded sediment from kg/m2/day to kg/reach/day.
            j_spm_runoff = cell%item%erodedSediment * cell%item%area * lengthRatio                      ! [kg/timestep]
            j_np_runoff = lengthRatio*cell%item%colSoilProfiles(1)%item%m_np_eroded                     ! [kg/timestep]
            j_transformed_runoff = lengthRatio*cell%item%colSoilProfiles(1)%item%m_transformed_eroded   ! [kg/timestep]

            ! Update the reach for this timestep
            call reach%item%update( &
                t = t, &
                q_runoff = cell%item%q_runoff_timeSeries(t), &
                q_overland = real(DATASET%quickflow(cell%item%x, cell%item%y, t), 8), &
                j_spm_runoff = j_spm_runoff, &
                j_np_runoff = j_np_runoff, &
                j_transformed_runoff = j_transformed_runoff, &
                contributingArea = cell%item%area * lengthRatio &
            )
        end if
    end subroutine

    subroutine determineStreamOrderEnvironment1(me)
        class(Environment1) :: me               !! This Environment instance
        integer             :: streamOrder      !! Index to keep track of stream order
        type(ReachPointer)  :: reach            ! Pointer to the reach we're updating
        logical             :: goDownstream     ! Flag to determine whether to go to next downstream reach
        integer             :: i, j, rr, x, y   ! Iterators
        
        streamOrder = 1
        ! Loop through the headwaters and route from these downstream. We don't need to do this separately
        ! for calibration runs, because each reach with a sample site has already been told its boundary
        ! conditions and thus will ignore inflows from upstream. Just bare in mind that only the calibration
        ! output results will be valid, not the full model outputs.
        do i = 1, me%nHeadwaters
            reach%item => me%headwaters(i)%item
            ! Add this headwater to the routed reaches array and fill its stream order
            me%routedReaches(streamOrder)%item => reach%item
            reach%item%streamOrder = streamOrder
            reach%item%isUpdated = .true.
            streamOrder = streamOrder + 1
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
                    me%routedReaches(streamOrder)%item => reach%item
                    reach%item%streamOrder = streamOrder
                    reach%item%isUpdated = .true.
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
                    streamOrder = streamOrder + 1
                end do
            end if
        end do
        ! Reset the isUpdated flag
        ! TODO tidy up
        do y = 1, size(me%colGridCells, 2)                             ! Loop through the rows
            do x = 1, size(me%colGridCells, 1)                         ! Loop through the columns
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do rr = 1, me%colGridCells(x,y)%item%nReaches
                        me%colGridCells(x,y)%item%colRiverReaches(rr)%item%isUpdated = .false.
                    end do
                end if
            end do
        end do

    end subroutine

    subroutine parseNewBatchDataEnvironment1(me)
        class(Environment1) :: me
        integer :: x, y
        ! Loop through grid cells and parse their new batch data
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                call me%colGridCells(x,y)%item%parseNewBatchData()
            end do
        end do
    end subroutine
   
    !> Get the mass of NM in all waterbodies in the environment
    !! TODO is this used? If so, check it works
    function get_m_npEnvironment1(me) result(m_np)
        class(Environment1) :: me
        real(dp) :: m_np(C%nSizeClassesNM, 4, 2 + C%nSizeClassesSpm)
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

    !> Calculate the mean soil PEC in the environment
    function get_C_np_soilEnvironment1(me) result(C_np_soil)
        class(Environment1)     :: me                                                               !! This Environment instance
        real(dp), allocatable   :: C_np_soil(:,:,:)                                                 !! Mass concentration of NM in environment [kg/kg soil]
        real(dp)                :: C_np_soil_i(me%nGridCells, C%npDim(1), C%npDim(2), C%npDim(3))   ! Per grid NM conc [kg/kg soil]
        integer                 :: x, y, i                                                          ! Iterators
        allocate(C_np_soil(C%npDim(1), C%npDim(2), C%npDim(3)))
        i = 1
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    associate (cell => me%colGridCells(x,y)%item)
                        C_np_soil_i(i, :, :, :) = cell%get_C_np_soil()
                    end associate
                    i = i + 1
                end if
            end do
        end do
        C_np_soil = divideCheckZero(sum(C_np_soil_i, dim=1), me%nGridCells)
    end function

    !> Get the mean water NM PEC at this moment in time, by looping over all grid cells
    !! and their water bodies and averaging.
    function get_C_np_waterEnvironment1(me) result(C_np_water)
        class(Environment1)     :: me                                                                   !! This Environment instance
        real(dp), allocatable   :: C_np_water(:,:,:)                                                    !! Mean water PEC [kg/m3]
        real(dp)                :: C_np_water_i(me%nGridCells, C%npDim(1), C%npDim(2), C%npDim(3))      ! Per cell mean water PEC [kg/m3]
        real(dp)                :: volumes(me%nGridCells)                                               ! Per cell sediment volumes, for weighted average [m3]
        integer                 :: x, y, i                                                              ! Iterators
        allocate(C_np_water(C%npDim(1), C%npDim(2), C%npDim(3)))
        i = 1
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    associate (cell => me%colGridCells(x,y)%item)
                        C_np_water_i(i, :, :, :) = cell%get_C_np_water()
                        volumes(i) = cell%getWaterVolume()
                    end associate
                    i = i + 1
                end if
            end do
        end do
        C_np_water = weightedAverage(C_np_water_i, volumes)
    end function

    !> Get the mean sediment NM PEC [kg/kg] at this moment in time, by looping over all grid cells
    !! and their water bodies and getting the weighted average.
    function get_C_np_sedimentEnvironment1(me) result(C_np_sediment)
        class(Environment1)     :: me                                                                   !! This Environment instance
        real(dp), allocatable   :: C_np_sediment(:,:,:)                                                 !! Mean sediment PEC [kg/kg]
        real(dp)                :: C_np_sediment_i(me%nGridCells, C%npDim(1), C%npDim(2), C%npDim(3))   ! Per cell mean sediment PEC [kg/kg]
        real(dp)                :: sedimentMasses(me%nGridCells)                                        ! Per cell sediment masses, for weighted average [kg]
        integer                 :: x, y, i                                                              ! Iterators
        allocate(C_np_sediment(C%npDim(1), C%npDim(2), C%npDim(3)))
        i = 1
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    associate (cell => me%colGridCells(x,y)%item)
                        C_np_sediment_i(i, :, :, :) = cell%get_C_np_sediment()
                        sedimentMasses(i) = cell%getBedSedimentMass()
                    end associate
                    i = i + 1
                end if
            end do
        end do
        C_np_sediment = weightedAverage(C_np_sediment_i, sedimentMasses)
    end function

    function getBedSedimentAreaEnvironment1(me) result(bedArea)
        class(Environment1) :: me
        real(dp)            :: bedArea
        integer             :: x, y
        bedArea = 0.0_dp
        do y = 1, size(me%colGridCells, dim=2)
            do x = 1, size(me%colGridCells, dim=1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    bedArea = bedArea + me%colGridCells(x,y)%item%getBedSedimentArea()
                end if
            end do
        end do
    end function

    !> Get the mass of sediment [kg] in the environment, broken down by layer and sediment
    !! size class.
    function get_m_sediment_byLayerEnvironment1(me) result(m_sediment_byLayer)
        class(Environment1)     :: me                           !! This Environment instance
        real(dp), allocatable   :: m_sediment_byLayer(:,:)      !! Mass of sediment in the environment [kg]
        integer                 :: x, y, i, j, k                ! Iterators
        allocate(m_sediment_byLayer(C%nSedimentLayers, C%nSizeClassesSpm))
        m_sediment_byLayer = 0.0_dp
        ! Loop through cells and reaches and sum up the sediment mass. GFortran compiler
        ! bugs meant I was struggling to encase the reach/sediment actions within their
        ! own class, so as a work around they will be pulled directly from here
        do y = 1, size(me%colGridCells, dim=2)
            do x = 1, size(me%colGridCells, dim=1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do i = 1, me%colGridCells(x,y)%item%nReaches
                        associate (sediment => me%colGridCells(x,y)%item%colRiverReaches(i)%item%bedSediment)
                            do j = 1, C%nSedimentLayers
                                do k = 1, C%nSizeClassesSpm
                                    m_sediment_byLayer(j,k) = m_sediment_byLayer(j,k) &
                                        + sediment%colBedSedimentLayers(j)%item%colFineSediment(k)%M_f() &
                                        * me%colGridCells(x,y)%item%colRiverReaches(i)%item%bedArea
                                end do
                            end do
                        end associate
                    end do
                end if
            end do
        end do
    end function

end module

!> Container module for class `Environment`
module EnvironmentModule
    use mo_netcdf
    use GlobalsModule
    use UtilModule
    use AbstractEnvironmentModule
    use ResultModule
    use GridCellModule
    use ContaminantModule
    use DataInputModule, only: DATASET
    use datetime_module, only: datetime, timedelta
    implicit none
    private
    
    !> The `Environment` class acts as a container for all other
    !! environmental compartments, triggering their creation, simulation
    !! and passing data between them
    type, public, extends(AbstractEnvironment) :: Environment
      contains
        procedure :: create => createEnvironment
        procedure :: update => updateEnvironment
        procedure :: updateReach => updateReachEnvironment
        procedure :: determineStreamOrder => determineStreamOrderEnvironment
        procedure :: parseNewBatchData => parseNewBatchDataEnvironment
        ! Getters
        procedure :: parseInputData => parseInputDataEnvironment
        procedure :: get_m_contaminant => get_m_contaminantEnvironment
        procedure :: get_C_contaminant_soil => get_C_contaminant_soilEnvironment
        procedure :: get_C_contaminant_water => get_C_contaminant_waterEnvironment
        procedure :: get_C_contaminant_sediment => get_C_contaminant_sedimentEnvironment
        procedure :: getBedSedimentArea => getBedSedimentAreaEnvironment
        procedure :: get_m_sediment_byLayer => get_m_sediment_byLayerEnvironment
    end type

  contains

    !> Create the `Environment`, which sets up the grid and river structure.
    !! The `Environment` instance must be a target so that `SubRiver` inflows
    !! can point to another `SubRiver` object:
    !! ([see here](https://stackoverflow.com/questions/45761050/pointing-to-a-objects-type-variable-fortran/))
    function createEnvironment(me) result(r)
        class(Environment), target :: me
            !! This `Environment` instance. Must be target so children can be pointed at.
        type(Result) :: r
        integer :: x, y, w, i, ix, iy, iw
        type(ReachPointer), allocatable :: tmpHeadwaters(:)
        integer :: allst
        character(len=256) :: tr

        tr = "Environment%createEnvironment"
        me%nGridCells = 0  ! Inherited from AbstractEnvironment
        me%nHeadwaters = 0
        me%nWaterbodies = 0
        ! Allocate grid cells array to be the shape of the grid
        allocate(me%colGridCells(DATASET%gridShape(1), DATASET%gridShape(2)), stat=allst)
        if (allst /= 0) then
            call r%addError(ErrorInstance(code=1, message="Allocation error for colGridCells", trace=[tr]))
            return
        end if
        ! Loop over grid and create cells
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                allocate(GridCell :: me%colGridCells(x,y)%item)
                ! If this grid cell isn't masked, create it
                if (.not. DATASET%gridMask(x,y)) then
                    call r%addErrors(.errors. me%colGridCells(x,y)%item%create(x,y))
                    me%nGridCells = me%nGridCells + 1
                ! If it is masked, still create it but tell it that it's empty
                else
                    call r%addErrors(.errors. me%colGridCells(x,y)%item%create(x,y,isEmpty=.true.))
                end if
            end do
        end do
        
        if (.not. r%hasCriticalError()) then            
            ! Create links between waterbodies
            do y = 1, DATASET%gridShape(2)
                do x = 1, DATASET%gridShape(1)
                    if (.not. me%colGridCells(x,y)%item%isEmpty) then
                        do w = 1, me%colGridCells(x,y)%item%nReaches
                            associate (reach => me%colGridCells(x,y)%item%colRiverReaches(w)%item)
                                do i = 1, reach%nInflows
                                    iw = reach%inflowsArr(i,1)
                                    ix = reach%inflowsArr(i,2)
                                    iy = reach%inflowsArr(i,3)
                                    reach%inflows(i)%item => me%colGridCells(ix,iy)%item%colRiverReaches(iw)%item
                                    reach%inflows(i)%item%outflow%item => reach
                                    if (ix /= x .or. iy /= y) then
                                        reach%inflows(i)%item%isGridCellOutflow = .true.
                                        reach%isGridCellInflow = .true.
                                    end if
                                    if (reach%ref(1:3) == 'Est' .and. reach%inflows(i)%item%ref(1:3) == 'Riv') then
                                        reach%isTidalLimit = .true.
                                    end if
                                end do
                                if (reach%isHeadwater) then
                                    me%nHeadwaters = me%nHeadwaters + 1
                                    allocate(tmpHeadwaters(me%nHeadwaters), stat=allst)
                                    if (allst /= 0) then
                                        call r%addError(ErrorInstance(code=1, &
                                            message="Allocation error for tmpHeadwaters", trace=[tr]))
                                        return
                                    end if
                                    if (me%nHeadwaters > 1) then
                                        tmpHeadwaters(1:me%nHeadwaters-1) = me%headwaters
                                    end if
                                    call move_alloc(tmpHeadwaters, me%headwaters)
                                    me%headwaters(me%nHeadwaters)%item => reach
                                end if
                            end associate
                        end do
                    end if
                end do
            end do

            ! Finalise creation operations
            do y = 1, DATASET%gridShape(2)
                do x = 1, DATASET%gridShape(1)
                    call me%colGridCells(x,y)%item%finaliseCreate()
                    me%nWaterbodies = me%nWaterbodies + me%colGridCells(x,y)%item%nReaches
                end do
            end do
        
            ! Allocate routedReaches
            allocate(me%routedReaches(me%nWaterbodies), stat=allst)
            if (allst /= 0) then
                call r%addError(ErrorInstance(code=1, message="Allocation error for routedReaches", trace=[tr]))
                return
            end if
            call me%determineStreamOrder()
        end if

        ! Allocate temporal arrays
        allocate(me%contaminant_water_t(0), me%contaminant_sediment_t(0), &
                 me%m_sediment_t_byLayer(0, C%nSedimentLayers, C%nSizeClassesSpm), stat=allst)
        if (allst /= 0) then
            call r%addError(ErrorInstance(code=1, message="Allocation error for temporal arrays", trace=[tr]))
            return
        end if
        
        call r%addToTrace('Creating the Environment')
        call LOGR%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors=.errors.r)
        call r%clear()
        call LOGR%toConsole('Creating the Environment: '//COLOR_GREEN//'success'//COLOR_RESET)
    end function

    !> Perform simulations for the `Environment`
    subroutine updateEnvironment(me, t, tInBatch, isWarmUp)
        use omp_lib
        class(Environment), target :: me                           !! This `Environment` instance
        integer, intent(in) :: t                                               !! Current time step
        integer, intent(in) :: tInBatch                                        !! Current time step in full batch run
        logical, intent(in) :: isWarmUp                                        !! Are we in a warm up period?
        integer :: i, x, y                                         ! Iterators
        type(datetime) :: currentDate                              ! Current simulation date
        type(Contaminant), allocatable :: tmp_contaminant(:)       ! Temporary array for contaminants
        real(dp), allocatable :: tmp_m_sediment(:,:,:)             ! Temporary array for sediment
        integer :: allst                                           ! Allocation status
        type(Result) :: r_ct, rslt1, rslt2
        character(len=256) :: tr
        
        tr = "Environment%updateEnvironment"
        ! Get the current date and log it
        currentDate = C%startDate + timedelta(t-1)
        if (isWarmUp) then
            call LOGR%add("Warm up period (time step #" // trim(str(tInBatch)) // ")...")
        else
            call LOGR%add("Performing simulation for " // trim(currentDate%strftime('%Y-%m-%d')) // &
                          " (time step #" // trim(str(tInBatch)) // ")...")
        end if
        
        !!$omp parallel do private(y,x)
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                ! Only update if this cell isn't masked
                if (DATASET%simulationMask(x,y)) then
                    call me%colGridCells(x,y)%item%update(t, isWarmUp)
                end if
            end do
        end do
        !!$omp end parallel do
        
        ! Loop through the routed reaches array (which is in the correct order) and update each reach
        do i = 1, me%nWaterbodies
           call me%updateReach(t, me%routedReaches(i), isWarmUp)
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
        call move_alloc(me%contaminant_water_t, tmp_contaminant)
        allocate(me%contaminant_water_t(size(tmp_contaminant)+1), stat=allst)
        if (allst /= 0) then
            call LOGR%add("Error allocating contaminant_water_t")
            return
        end if
        if (size(tmp_contaminant) > 0) then
            me%contaminant_water_t(1:size(tmp_contaminant)) = tmp_contaminant
        end if

        ! --- FUNCTION create() must be captured, not CALLed ---
        r_ct = me%contaminant_water_t(size(tmp_contaminant)+1)%create()
        if (r_ct%hasError()) then
            call LOGR%toFile(errors=r_ct%errors)
            call ERROR_HANDLER%trigger(errors=r_ct%errors)
            return
        end if
        me%contaminant_water_t(size(tmp_contaminant)+1) = me%get_C_contaminant_water()

        ! Append new sediment‐phase contaminant
        call move_alloc(me%contaminant_sediment_t, tmp_contaminant)
        allocate(me%contaminant_sediment_t(size(tmp_contaminant)+1), stat=allst)
        if (allst /= 0) then
            call LOGR%add("Error allocating contaminant_sediment_t")
            return
        end if
        if (size(tmp_contaminant) > 0) then
            me%contaminant_sediment_t(1:size(tmp_contaminant)) = tmp_contaminant
        end if

        rslt1 = me%contaminant_sediment_t(size(tmp_contaminant)+1)%create()
        if (rslt1%hasError()) then
            call LOGR%toFile(errors=rslt1%errors)
            call ERROR_HANDLER%trigger(errors=rslt1%errors)
            return
        end if
        me%contaminant_sediment_t(size(tmp_contaminant)+1) = me%get_C_contaminant_sediment()

        ! Append new sediment‐by‐layer mass
        call move_alloc(me%m_sediment_t_byLayer, tmp_m_sediment)
        allocate(me%m_sediment_t_byLayer(size(tmp_m_sediment,1)+1, C%nSedimentLayers, C%nSizeClassesSpm), stat=allst)
        if (allst /= 0) then
            call LOGR%add("Error allocating m_sediment_t_byLayer")
            return
        end if
        me%m_sediment_t_byLayer(1:size(tmp_m_sediment,1),:,:) = tmp_m_sediment
        me%m_sediment_t_byLayer(size(tmp_m_sediment,1)+1,:,:) = me%get_m_sediment_byLayer()
    end subroutine
    
    !> Update an individual reach, also updating the containing grid cell, if it hasn't
    !! already been updated.
    subroutine updateReachEnvironment(me, t, reach, isWarmUp)
        class(Environment), target :: me
        integer, intent(in) :: t
        type(ReachPointer), intent(inout) :: reach
        logical, intent(in) :: isWarmUp
        type(GridCellPointer) :: cell
        real(dp)              :: lengthRatio
        real(dp)              :: j_spm_runoff(C%nSizeClassesSpm)
        type(Contaminant)     :: j_contaminant_runoff
        type(Result)          :: r_cr      ! for create()
        character(len=256)    :: tr

        tr = "Environment%updateReachEnvironment"
        cell%item => me%colGridCells(reach%item%x, reach%item%y)%item

        if (DATASET%simulationMask(cell%item%x, cell%item%y)) then
            ! compute partitioning
            lengthRatio = reach%item%length / cell%item%getTotalReachLength()
            j_spm_runoff = cell%item%erodedSediment * cell%item%area * lengthRatio

            ! initialize contaminant-runoff object
            r_cr = j_contaminant_runoff%create()
            if (r_cr%hasError()) then
                call LOGR%toFile(errors=r_cr%errors)
                call ERROR_HANDLER%trigger(errors=r_cr%errors)
                return
            end if

            if (allocated(cell%item%colSoilProfiles)) then
                call j_contaminant_runoff%multiply_scalar( &
                    cell%item%colSoilProfiles(1)%item%m_contaminant_eroded, &
                    lengthRatio )
            end if

            ! now update the reach
            call reach%item%update( &
                t                     = t, &
                q_runoff              = cell%item%q_runoff_timeSeries(t), &
                q_overland            = real(DATASET%quickflow(cell%item%x, cell%item%y, t), dp), &
                j_spm_runoff          = j_spm_runoff, &
                j_contaminant_runoff  = j_contaminant_runoff, &
                contributingArea      = cell%item%area * lengthRatio, &
                isWarmUp              = isWarmUp )

            call j_contaminant_runoff%finalise()
        end if
    end subroutine



    subroutine determineStreamOrderEnvironment(me)
        class(Environment), intent(inout) :: me               !! This Environment instance
        integer :: streamOrder                 !! Index to keep track of stream order
        type(ReachPointer) :: reach            ! Pointer to the reach we're updating
        logical :: goDownstream                ! Flag to determine whether to go to next downstream reach
        integer :: i, j, rr, x, y              ! Iterators
        
        streamOrder = 1
        ! Loop through the headwaters and route from these downstream
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

    function parseInputDataEnvironment(me) result(r)
        class(Environment), intent(inout) :: me
        type(Result)      :: r
        integer           :: x, y
        character(len=256):: tr

        tr = "Environment%parseInputDataEnvironment"

        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
            ! Just call the subroutine on each concrete GridCell
            call me%colGridCells(x,y)%item%parseInputData()
            end do
        end do

        call r%addToTrace(tr // ": parsed all grid‑cell input data")
    end function parseInputDataEnvironment



    subroutine parseNewBatchDataEnvironment(me)
        class(Environment), intent(inout) :: me
        integer :: x, y
        ! Loop through grid cells and parse their new batch data
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                call me%colGridCells(x,y)%item%parseNewBatchData()
            end do
        end do
    end subroutine
   
    !> Get the total mass of Contaminant in all waterbodies in the environment
    function get_m_contaminantEnvironment(me) result(m_contaminant)
        class(Environment), intent(in) :: me
        type(Contaminant) :: m_contaminant
        type(Result) :: rslt
        integer :: x, y, rr

        rslt = m_contaminant%create()
        if (rslt%hasError()) then
            call rslt%addToTrace('Environment%get_m_contaminantEnvironment')
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do rr = 1, me%colGridCells(x,y)%item%nReaches
                        call m_contaminant%add(me%colGridCells(x,y)%item%colRiverReaches(rr)%item%reactor%contaminant)
                    end do
                end if
            end do
        end do
    end function

    function get_C_contaminant_soilEnvironment(me) result(C_contaminant_soil)
        class(Environment), intent(in) :: me
        type(Contaminant) :: C_contaminant_soil
        type(Contaminant) :: m_total
        type(Contaminant) :: m_profile
        type(Result) :: r
        real(dp) :: m_total_soil
        integer :: x, y, p

        r = m_total%create()
        if (r%hasError()) then
            call LOGR%toFile(errors=r%errors)
            call ERROR_HANDLER%trigger(errors=r%errors)
            r = C_contaminant_soil%create()
            return
        end if

        m_total_soil = 0.0_dp
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do p = 1, me%colGridCells(x,y)%item%nSoilProfiles
                        associate(profile => me%colGridCells(x,y)%item%colSoilProfiles(p)%item)
                            m_profile = profile%get_m_contaminant()
                            call m_total%add(m_profile)
                            m_total_soil = m_total_soil + profile%bulkDensity * profile%area * sum(C%soilLayerDepth)
                            call m_profile%finalise()
                        end associate
                    end do
                end if
            end do
        end do

        C_contaminant_soil = m_total%divideCheckZero(m_total_soil)
        call m_total%finalise()
    end function

    !> Get the mean water‐phase Contaminant PEC at this moment in time,
    !! by looping over all grid cells and averaging.
    function get_C_contaminant_waterEnvironment(me) result(C_contaminant_water)
        class(Environment), intent(in) :: me
        type(Contaminant) :: C_contaminant_water
        type(Contaminant) :: m_total
        type(Contaminant) :: m_cell
        type(Result) :: r
        real(dp) :: total_volume
        integer :: x, y

        r = m_total%create()
        if (r%hasError()) then
            call LOGR%toFile(errors=r%errors)
            call ERROR_HANDLER%trigger(errors=r%errors)
            r = C_contaminant_water%create()
            return
        end if

        total_volume = 0.0_dp
        do y = 1, size(me%colGridCells,2)
            do x = 1, size(me%colGridCells,1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    m_cell = me%colGridCells(x,y)%item%get_m_contaminant_water()
                    call m_total%add(m_cell)
                    total_volume = total_volume + me%colGridCells(x,y)%item%getWaterVolume()
                    call m_cell%finalise()
                end if
            end do
        end do

        ! Result is concentration object [kg m-3] by species/form/phase.
        C_contaminant_water = m_total%divideCheckZero(total_volume)
        call m_total%finalise()
    end function

    !> Get the mean sediment Contaminant PEC [kg/kg] at this moment in time,
    ! by looping over all grid cells and their water bodies and getting the
    !! weighted average.
    function get_C_contaminant_sedimentEnvironment(me) result(C_contaminant_sediment)
        class(Environment), intent(in) :: me
        type(Contaminant) :: C_contaminant_sediment
        type(Contaminant) :: m_total
        type(Contaminant) :: m_cell
        type(Result) :: r
        real(dp) :: total_sediment_mass
        integer :: x, y

        r = m_total%create()
        if (r%hasError()) then
            call LOGR%toFile(errors=r%errors)
            call ERROR_HANDLER%trigger(errors=r%errors)
            r = C_contaminant_sediment%create()
            return
        end if

        total_sediment_mass = 0.0_dp
        do y = 1, size(me%colGridCells,2)
            do x = 1, size(me%colGridCells,1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    m_cell = me%colGridCells(x,y)%item%get_m_contaminant_sediment()
                    call m_total%add(m_cell)
                    total_sediment_mass = total_sediment_mass + me%colGridCells(x,y)%item%getBedSedimentMass()
                    call m_cell%finalise()
                end if
            end do
        end do

        ! Result is bulk sediment concentration object [kg kg-1 dry sediment] by species/form/phase.
        C_contaminant_sediment = m_total%divideCheckZero(total_sediment_mass)
        call m_total%finalise()
    end function 

    function getBedSedimentAreaEnvironment(me) result(bedArea)
        class(Environment), intent(in) :: me
        real(dp) :: bedArea
        integer :: x, y
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
    function get_m_sediment_byLayerEnvironment(me) result(m_sediment_byLayer)
        class(Environment), intent(in) :: me
        real(dp), allocatable :: m_sediment_byLayer(:,:)
        integer :: x, y, i, j, k
        character(len=256) :: tr
        tr = "Environment%get_m_sediment_byLayerEnvironment"
        allocate(m_sediment_byLayer(C%nSedimentLayers, C%nSizeClassesSpm))
        m_sediment_byLayer = 0.0_dp
        do y = 1, size(me%colGridCells, dim=2)
            do x = 1, size(me%colGridCells, dim=1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do i = 1, me%colGridCells(x,y)%item%nReaches
                        associate (sediment => me%colGridCells(x,y)%item%colRiverReaches(i)%item%bedSediment)
                            do j = 1, C%nSedimentLayers
                                do k = 1, C%nSizeClassesSpm
                                    m_sediment_byLayer(j,k) = m_sediment_byLayer(j,k) + &
                                        sediment%colBedSedimentLayers(j)%item%colFineSediment(k)%M_f() * &
                                        me%colGridCells(x,y)%item%colRiverReaches(i)%item%bedArea
                                end do
                            end do
                        end associate
                    end do
                end if
            end do
        end do
    end function
    
end module

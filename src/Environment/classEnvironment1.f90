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

        ! call r%addErrors(.errors. me%parseInputData())
        ! allocate(me%routedReaches(size(me%routedReachIndices,3), size(me%routedReachIndices,2)))
        ! Make sure everything is null to begin with
        ! do j = 1, size(me%routedReaches,2)
            ! do i = 1, size(me%routedReaches,1)
                ! me%routedReaches(i,j)%item => null()
            ! end do
        ! end do

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

        !     ! Set the size of Environment variable that holds the grid cells
        !     allocate(me%colGridCells(me%gridDimensions(1),me%gridDimensions(2)))
            
        !     do y = 1, me%gridDimensions(2)                                ! Loop through the y dimensions of the grid
        !         do x = 1, me%gridDimensions(1)                            ! Loop through the x dimensions of the grid
        !             gridCellRef = ref("GridCell", x, y)
        !             ! We need to reset the group to the Environment each time as the below loop
        !             ! sets the group to the grid cell group
        !             call r%addErrors(.errors. DATA%setGroup(['Environment']))
        !             ! Check if the GridCell is defined in the data file before creating
        !             ! it and adding to colGridCells. If it doesn't exist, specify it is
        !             ! an empty GridCell.

        !             if (DATA%grp%hasGroup(trim(gridCellRef))) then
        !                 ! Get the type of the GridCell (e.g., GridCell1, GridCell2) to
        !                 ! create, from the data file.
        !                 call r%addErrors(.errors. DATA%setGroup([character(len=100) :: 'Environment', trim(gridCellRef)]))
        !                 call r%addErrors(.errors. DATA%get('type', gridCellType, 1))
        !                 select case (gridCellType)
        !                     case (1)
        !                         allocate(GridCell1::me%colGridCells(x,y)%item)  ! Allocate to type 1, GridCell1
        !                         call r%addErrors(.errors. &                     ! Call the create method to create the GridCell
        !                             me%colGridCells(x,y)%item%create(x,y) &
        !                         )
        !                         ! if (me%colGridCells(x,y)%item%isHeadwater) then
        !                         !     me%nHeadwaters = me%nHeadwaters + 1
        !                         !     allocate(tmpHeadwaters(me%nHeadwaters))                             ! Extend the me%headwaters array by 1
        !                         !     tmpHeadwaters(1:me%nHeadwaters-1) = me%headwaters
        !                         !     call move_alloc(tmpHeadwaters, me%headwaters)
        !                         !     me%headwaters(me%nHeadwaters)%item => me%colGridCells(x,y)%item     ! Point to this grid cell
        !                         ! end if
        !                     case default
        !                         call r%addError( &                              ! Invalid type error
        !                             ErrorInstance(110,"Invalid GridCell type index for " // &
        !                                 trim(gridCellRef) // ": " // trim(str(gridCellType)) // ".") &
        !                         )
        !                 end select
        !             else
        !                 allocate(GridCell1::me%colGridCells(x,y)%item)          ! Otherwise, create an empty GridCell1
        !                 call r%addErrors(.errors. &
        !                     me%colGridCells(x,y)%item%create(x,y,isEmpty=.true.) &
        !                 )
        !             end if
        !         end do
        !     end do
            
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

                                ! do i = 1, riverReach%nInflows                       ! Loop through the inflows for this reach
                                !     ix = riverReach%inflowRefs(i)%x
                                !     iy = riverReach%inflowRefs(i)%y

                                !     ! Check the inflow specified exists in the model domain
                                !     if (ix > 0 .and. ix .le. me%gridDimensions(1) &
                                !         .and. iy > 0 .and. iy .le. me%gridDimensions(2)) then
                                !         ! Point this reach's inflow to the actual RiverReach
                                !         riverReach%inflows(i)%item => &
                                !             me%colGridCells(ix,iy)%item%colRiverReaches(riverReach%inflowRefs(i)%w)%item
                                !         ! Set this inflow's outflow to this reach
                                !         riverReach%inflows(i)%item%outflow%item => riverReach
                                !         ! If this is a GridCell inflow, then set its inflows to be GridCell outflows
                                !         if (riverReach%isGridCellInflow) then
                                !             riverReach%inflows(i)%item%isGridCellOutflow = .true.
                                !             ! Also set the inflow grid cell's outflow pointer
                                !             me%colGridCells(ix,iy)%item%outflow%item => me%colGridCells(x,y)%item
                                !         end if
                                !         ! If the inflow is a river and this reach is an estuary, set the estuary to
                                !         ! be the tidal limit
                                !         if (riverReach%ref(1:3) == 'Est' &
                                !             .and. riverReach%inflows(i)%item%ref(1:3) == 'Riv') then
                                !             riverReach%isTidalLimit = .true.
                                !         end if
                                !     else
                                !         ! TODO For the moment, if the inflow doesn't exist in the
                                !         ! model domain, it is just ignored. In the future, it should
                                !         ! be implemented as a domain inflow of some sort
                                !         riverReach%nInflows = 0
                                !         deallocate(riverReach%inflows)
                                !         allocate(riverReach%inflows(0))
                                !     end if
                                ! end do
                            end associate
                        end do
                    end if
                end do
            end do

            ! Finally, we can populate the GridCell%routedRiverReaches array and do
            ! things like determine reach lengths
            ! do y = 1, size(me%colGridCells, 2)                  ! Loop through the columns
            !     do x = 1, size(me%colGridCells, 1)              ! Loop through the rows
            !         call r%addErrors(.errors. me%colGridCells(x,y)%item%finaliseCreate())
            !     end do
            ! end do

            ! Now we can use the routed reach indices from the data file to point the routed reaches array
            ! to the correct reaches. This must be done after the finaliseCreate method, as that may
            ! deallocate reaches that are both headwaters and domain outflows - we want to make sure we're
            ! not pointing to anything that is deallocated
            ! do j = 1, size(me%routedReachIndices, 2)            ! Seeds
            !     do i = 1, size(me%routedReachIndices, 3)        ! Branches
            !         x = me%routedReachIndices(1,j,i)
            !         y = me%routedReachIndices(2,j,i)
            !         rr = me%routedReachIndices(3,j,i)

            !         ! Check this element is actually a reach reference (Fortran doesn't have ragged arrays
            !         ! so empty elements are set as zero in input data).
            !         if (x > 0 .and. y > 0 .and. rr > 0) then
            !             if (allocated(me%colGridCells(x,y)%item%colRiverReaches) .and. &
            !                 size(me%colGridCells(x,y)%item%colRiverReaches) > 0) then
            !                 me%routedReaches(i,j)%item => me%colGridCells(x,y)%item%colRiverReaches(rr)%item
            !             end if
            !         else
            !             me%routedReaches(i,j)%item => null()
            !         end if
            !     end do
            ! end do
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
        class(Environment1) :: me                               !! This `Environment` instance
        integer :: t                                            !! Current time step
        type(Result) :: r                                       !! Return error(s) in `Result` object
        type(ReachPointer) :: reach                             ! Pointer to the reach we're updating
        integer :: i, j, x, y
        real(dp) :: lengthRatio                                 ! Reach length as a proportion of total river length in cell
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3)) ! NP runoff for this time step
        logical :: goDownstream                                 ! Have all the inflow reaches been updated yet?

        call LOG%add("Performing simulation for time step #" // trim(str(t)) // "...")

        ! Update all grid cells first
        !!$omp parallel do private(y)
        do y = 1, DATASET%gridShape(2)
            do x = 1, DATASET%gridShape(1)
                call r%addErrors(.errors. me%colGridCells(x,y)%item%update(t))
            end do
        end do
        !!$omp end parallel do

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

        ! ! Loop through the routed reaches
        ! do j = 1, size(me%routedReaches, 2)                 ! Iterate over successively higher stream orders
        !     !!$omp parallel do private(reach, i) firstprivate(r)
        !     do i = 1, size(me%routedReaches, 1)             ! Iterate over seeds in this stream order
        !         if (associated(me%routedReaches(i,j)%item)) then
        !             ! Update this reach, also updating the containing grid cell (if it hasn't been already)
        !             reach%item => me%routedReaches(i,j)%item
        !             call r%addErrors(.errors. me%updateReach(t, reach))
        !             ! Check this reach has an outflow, before moving on to it and looping until we hit
        !             ! a stream junction
        !             if (associated(reach%item%outflow%item)) then
        !                 reach%item => reach%item%outflow%item
        !                 do while (reach%item%nInflows == 1)
        !                     ! Update this reach (and its grid cell, if need be)
        !                     call r%addErrors(.errors. me%updateReach(t, reach))
        !                     if (.not. associated(reach%item%outflow%item)) then
        !                         exit
        !                     else
        !                         reach%item => reach%item%outflow%item
        !                     end if
        !                 end do
        !             end if
        !         end if
        !     end do
        !     !!$omp end parallel do
        ! end do

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

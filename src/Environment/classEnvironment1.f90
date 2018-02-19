!> Container module for class `Environment1`
module classEnvironment1
    use mo_netcdf
    use Globals
    use UtilModule
    use spcEnvironment
    use ResultModule
    use classGridCell1
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
        type(NcDataset) :: nc                                   ! NetCDF dataset
        type(NcVariable) :: var                                 ! NetCDF variable
        type(NcGroup) :: grp, gcGrp                             ! NetCDF group
        integer :: x, y, rr, i, b                               ! Iterators for GridCells, RiverReaches, inflows and branches
        integer :: iX, iY, iS                                   ! Indices for inflow grid and SubRiver coordinates
        character(len=100) :: gridCellRef                       ! To store GridCell name in, e.g. "GridCell_x_y"
        integer :: gridCellType                                 ! Integer representing the GridCell type
        logical :: isValidInflow = .true.                       ! Is inflow SubRiver is a neighbouring river
        integer :: nBranches                                    ! Number of branches in a given GridCell
        type(RiverReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)    ! Temporary array to store routedRiverReaches in
        type(ErrorInstance), allocatable :: errors(:)           ! Errors to return

        ! No errors to begin with
        allocate(errors(0))

        ! Set up the grid structure
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")                        ! Get the Environment group
        var = grp%getVariable("gridSize")                       ! Get the grid size from the Environment
        call var%getData(me%gridSize)
        
        ! Set the size of Environment variable that holds the grid cells
        allocate(me%colGridCells(me%gridSize(1),me%gridSize(2)))
        ! Loop through the y dimensions of the grid
        do y = 1, me%gridSize(2)
            ! Loop through the x dimensions of the grid
            do x = 1, me%gridSize(1)
                gridCellRef = ref("GridCell", x, y)
                ! Check if the GridCell is defined in the data file before creating
                ! it and adding to colGridCells. If it doesn't exist, specify it is
                ! an empty GridCell.
                if (grp%hasGroup(trim(gridCellRef))) then
                    ! Get the type of the GridCell (e.g., GridCell1, GridCell2) to
                    ! create, from the data file.
                    gcGrp = grp%getGroup(trim(gridCellRef))
                    var = gcGrp%getVariable("type")
                    call var%getData(gridCellType)
                    select case (gridCellType)
                        case (1)
                            allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y))    
                        case default
                            call r%addError( &                      ! Invalid type error
                                ErrorInstance(1,"Invalid GridCell type index for " // &
                                    trim(gridCellRef) // ": " // trim(str(gridCellType))) &
                            )
                    end select
                else
                    allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y,isEmpty=.true.))
                end if
            end do
        end do
        
        ! Now we need to create links between RiverReaches, which wasn't possible before all GridCells
        ! and their RiverReaches were created:
        ! - Firstly, point RiverReaches' inflows array element to GridCells' colRiverReaches array elements.
        ! - Also, point those inflows' outflow property to the correct reach.
        ! - After this, all RiverReaches will point to their inflow(s) and outflow correctly.
        ! - Finally, begin to set up the routedRiverReach array be filling each branch with its
        !   most upstream reach (i.e., the inflow to the GridCell).
        ! Most auditing is already done by RiverReach%parseInputData().
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    nBranches = 0                                               ! Reset the number of branches for each GridCell
                    do rr = 1, size(me%colGridCells(x,y)%item%colRiverReaches)
                        associate(riverReach => me%colGridCells(x,y)%item%colRiverReaches(rr)%item)
                            do i = 1, riverReach%nInflows                       ! Loop through the inflows for this reach
                                ! Point this reach's inflow to the actual RiverReach
                                riverReach%inflows(i)%item => &
                                    me%colGridCells(riverReach%inflowRefs(i)%x,riverReach%inflowRefs(i)%y) &
                                        %item%colRiverReaches(riverReach%inflowRefs(i)%rr)%item
                                ! Set this inflow's outflow to this reach
                                riverReach%inflows(i)%item%outflow%item => riverReach
                                ! If this is a GridCell inflow, then set its inflows to be GridCell outflows
                                if (riverReach%isGridCellInflow) then
                                    riverReach%inflows(i)%item%isGridCellOutflow = .true.  
                                end if
                            end do
                            ! Whether it is a GridCell inflow or a headwater will have been set by
                            ! RiverReach%parseInputData(). Now use to start filling routedRiverReach.
                            if (riverReach%isGridCellInflow .or. riverReach%isHeadwater) then
                                nBranches = nBranches + 1
                                allocate(tmpRoutedRiverReaches(nBranches,1))            ! Use temp array to reallocate to hold extra branch
                                ! Fill the temp array with the current routedRiverReaches array
                                tmpRoutedRiverReaches(1:nBranches-1,:) = me%colGridCells(x,y)%item%routedRiverReaches
                                ! Then add the new branch by pointing first element in this branch to the river reach
                                tmpRoutedRiverReaches(nBranches,1)%item => riverReach
                                call move_alloc(from=tmpRoutedRiverReaches, &           ! Move the temp array to the actual array
                                    to=me%colGridCells(x,y)%item%routedRiverReaches)
                                riverReach%branch = nBranches                       ! Set the branch index    
                            end if
                        end associate
                    end do
                    ! How many branches have we created?
                    me%colGridCells(x,y)%item%nBranches = nBranches
                    allocate(me%colGridCells(x,y)%item%nReachesInBranch(nBranches))
                    ! Currently only 1 reach per branch (will be changed by next loop through reaches, if it needs to be)
                    me%colGridCells(x,y)%item%nReachesInBranch = 1
                end if
            end do
        end do
        
        ! Finally, we can populate the routedRiverReaches array by looping through the GridCell
        ! inflows (already in routedRiverReaches array) and following their outflows
        do y = 1, size(me%colGridCells, 2)                                      ! Loop through the columns
            do x = 1, size(me%colGridCells, 1)                                  ! Loop through the rows
                if (.not. me%colGridCells(x,y)%item%isEmpty) then               ! Check the GridCell isn't empty
                    do b = 1, me%colGridCells(x,y)%item%nBranches               ! Loop through the branches
                        associate(riverReach => me%colGridCells(x,y)%item%routedRiverReaches(b,1)%item)
                            ! TODO: Maybe put headwater/grid cell inflows into separate pointer array
                            ! so we can loop through them more efficiently
                            if (riverReach%isGridCellInflow .or. riverReach%isHeadwater) then
                                call r%addErrors(.errors. me%colGridCells(x,y)%item%setBranchRouting(b,1))
                                !if (.not. riverReach%isGridCellOutflow) then
                                !    
                                !    allocate(tmpRoutedRiverReaches(&
                                !        me%colGridCells(x,y)%item%nBranches, &
                                !        size(me%colGridCells(x,y)%item%routedRiverReaches, 2)+1))
                                !    ! Fill the temp array with the current routedRiverReaches array
                                !    tmpRoutedRiverReaches(:,1:size(me%colGridCells(x,y)%item%routedRiverReaches, 2)) &
                                !        = me%colGridCells(x,y)%item%routedRiverReaches
                                !    ! Add the new reach to this branch
                                !    tmpRoutedRiverReaches(b,size(tmpRoutedRiverReaches, 2))%item => riverReach%outflow%item
                                !    call move_alloc(from=tmpRoutedRiverReaches, &       ! Move the temp array to the actual array
                                !        to=me%colGridCells(x,y)%item%routedRiverReaches)
                                !end if
                            end if
                        end associate
                    end do
                end if
            end do
        end do
        
        do y = 1, size(me%colGridCells, 2)
            do x = 1, size(me%colGridCells, 1)
                do rr = 1, size(me%colGridCells(x,y)%item%routedRiverReaches, 2)
                    do b = 1, size(me%colGridCells(x,y)%item%routedRiverReaches, 1)
                        if (associated(me%colGridCells(x,y)%item%routedRiverReaches(b,rr)%item)) then
                            print *, x, y, b, rr, trim(me%colGridCells(x,y)%item%routedRiverReaches(b,rr)%item%ref)
                        end if
                        
                    end do
                end do
                
                do b = 1, size(me%colGridCells(x,y)%item%nReachesInBranch)
                    print *, x, y, b, "nrib: ", me%colGridCells(x,y)%item%nReachesInBranch(b)
                end do
            end do
        end do
       

        call r%addErrors(errors)        ! Add any errors that have occurred
        call ERROR_HANDLER%trigger(errors=errors)
    end function

    !> Destroy the `Environment` instance
    function destroyEnvironment1(me) result(r)
        class(Environment1) :: me                               !! This `Environment` instance
        type(Result) :: r                                       !! Return error(s) in `Result` object
        ! Destroy logic here
    end function

    !> Perform simulations for the `Environment`
    function updateEnvironment1(me, t) result(r)
        class(Environment1) :: me                               !! This `Environment` instance
        integer :: t                                            !! Current time step
        type(Result) :: r                                       !! Return error(s) in `Result` object
        integer :: x, y, s
        ! Perform the main routing procedure
        do y = 1, size(me%colGridCells, 2)                      ! Loop through the rows
            do x = 1, size(me%colGridCells, 1)                  ! Loop through the columns
                r = me%colGridCells(x,y)%item%update(t)         ! Run routing simulation for each GridCell
            end do
        end do
        ! Finalise the routing by setting outflows to temporary outflows that were stored
        ! to avoid routing using the wrong timestep's outflow as an inflow.
        do y = 1, size(me%colGridCells, 2)                      ! Loop through the rows
            do x = 1, size(me%colGridCells, 1)                  ! Loop through the columns
                r = me%colGridCells(x,y)%item%finaliseUpdate()  ! finaliseUpdate() loops through SubRivers
            end do
        end do
    end function

end module

!> Container module for class `Environment1`
module classEnvironment1
    use mo_netcdf
    use Globals
    use UtilModule
    use spcEnvironment
    use ResultModule
    use classGridCell1
    use classDataInterfacer, only: DATA
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
        integer :: x, y, rr, i, b, ix, iy                       ! Iterators for GridCells, RiverReaches, inflows and branches
        character(len=100) :: gridCellRef                       ! To store GridCell name in, e.g. "GridCell_x_y"
        integer :: gridCellType                                 ! Integer representing the GridCell type
        logical :: isValidInflow = .true.                       ! Is inflow SubRiver is a neighbouring river
        type(RiverReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)    ! Temporary array to store routedRiverReaches in
        type(ErrorInstance), allocatable :: errors(:)           ! Errors to return

        call r%addErrors(.errors. me%parseInputData())
        
        if (.not. r%hasCriticalError()) then    

            ! Set the size of Environment variable that holds the grid cells
            allocate(me%colGridCells(me%gridDimensions(1),me%gridDimensions(2)))
            
            do y = 1, me%gridDimensions(2)                                ! Loop through the y dimensions of the grid
                do x = 1, me%gridDimensions(1)                            ! Loop through the x dimensions of the grid
                    gridCellRef = ref("GridCell", x, y)
                    ! We need to reset the group to the Environment each time as the below loop
                    ! sets the group to the grid cell group
                    call r%addErrors(.errors. DATA%setGroup(['Environment']))
                    ! Check if the GridCell is defined in the data file before creating
                    ! it and adding to colGridCells. If it doesn't exist, specify it is
                    ! an empty GridCell.

                    if (DATA%grp%hasGroup(trim(gridCellRef))) then
                        ! Get the type of the GridCell (e.g., GridCell1, GridCell2) to
                        ! create, from the data file.
                        call r%addErrors(.errors. DATA%setGroup([character(len=100) :: 'Environment', trim(gridCellRef)]))
                        call r%addErrors(.errors. DATA%get('type', gridCellType, 1))
                        select case (gridCellType)
                            case (1)
                                allocate(GridCell1::me%colGridCells(x,y)%item)  ! Allocate to type 1, GridCell1
                                call r%addErrors(.errors. &                     ! Call the create method to create the GridCell
                                    me%colGridCells(x,y)%item%create(x,y) &
                                )  
                            case default
                                call r%addError( &                              ! Invalid type error
                                    ErrorInstance(110,"Invalid GridCell type index for " // &
                                        trim(gridCellRef) // ": " // trim(str(gridCellType)) // ".") &
                                )
                        end select
                    else
                        allocate(GridCell1::me%colGridCells(x,y)%item)          ! Otherwise, create an empty GridCell1
                        call r%addErrors(.errors. &
                            me%colGridCells(x,y)%item%create(x,y,isEmpty=.true.) &
                        )
                    end if
                end do
            end do

            
            ! Now we need to create links between RiverReaches, which wasn't possible before all GridCells
            ! and their RiverReaches were created:
            ! - Firstly, point RiverReaches' inflows array element to GridCells' colRiverReaches array elements.
            ! - Also, point those inflows' outflow property to the correct reach.
            ! After this, all RiverReaches will point to their inflow(s) and outflow correctly.
            ! Most auditing is already done by RiverReach%parseInputData().
            do y = 1, me%gridDimensions(2)
                do x = 1, me%gridDimensions(1)
                    if (.not. me%colGridCells(x,y)%item%isEmpty) then
                        do rr = 1, size(me%colGridCells(x,y)%item%colRiverReaches)  ! Loop through the reaches
                            associate (riverReach => me%colGridCells(x,y)%item%colRiverReaches(rr)%item)
                                do i = 1, riverReach%nInflows                       ! Loop through the inflows for this reach
                                    ix = riverReach%inflowRefs(i)%x
                                    iy = riverReach%inflowRefs(i)%y
                                    ! Check the inflow specified exists in the model domain
                                    if (ix > 0 .and. ix .le. me%gridDimensions(1) &
                                        .and. iy > 0 .and. iy .le. me%gridDimensions(2)) then
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
                                    else
                                        ! TODO For the moment, if the inflow doesn't exist in the
                                        ! model domain, it is just ignored. In the future, it should
                                        ! be implemented as a domain inflow of some sort
                                        riverReach%nInflows = 0
                                        deallocate(riverReach%inflows)
                                        allocate(riverReach%inflows(0))
                                    end if
                                end do
                            end associate
                        end do
                    end if
                end do
            end do
            
            ! Finally, we can populate the GridCell%routedRiverReaches array and do
            ! things like determine reach lengths
            do y = 1, size(me%colGridCells, 2)                  ! Loop through the columns
                do x = 1, size(me%colGridCells, 1)              ! Loop through the rows
                    call r%addErrors(.errors. me%colGridCells(x,y)%item%finaliseCreate())
                end do
            end do

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
        class(Environment1) :: me                               !! This `Environment` instance
        integer :: t                                            !! Current time step
        type(Result) :: r                                       !! Return error(s) in `Result` object
        integer :: x, y, s

        call LOG%add("Performing simulation for time step #" // trim(str(t)) // "...")
        
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
    
    !> Obtain and parse input data for this `Environment` object
    function parseInputDataEnvironment1(me) result(r)
        class(Environment1) :: me                           !! This `Environment1` instance
        type(Result) :: r                                   !! The `Result` object to return
        ! Get the grid dimensions for the Environment
        call r%addErrors(.errors. DATA%setGroup(['Environment']))
        call r%addErrors(.errors. DATA%get('grid_dimensions', me%gridDimensions))
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

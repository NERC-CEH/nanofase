module classEnvironment1
    use mo_netcdf
    use Globals
    use UtilModule
    use spcEnvironment
    use ResultModule
    use classGridCell1
    implicit none
    private

    type, public, extends(Environment) :: Environment1
        private

      contains
        procedure :: create => createEnvironment1
        procedure :: destroy => destroyEnvironment1
        procedure :: update => updateEnvironment1
    end type

  contains

    !> Create the environment, which sets up the grid and river structure.
    !! The Environment instance must be a target so that SubRiver inflows
    !! can point to another SubRiver object ([see here](https://stackoverflow.com/questions/45761050/pointing-to-a-objects-type-variable-fortran/))
    function createEnvironment1(me) result(r)
        class(Environment1), target :: me                       !! This Environment instace. Must be target so SubRivers can be pointed at.
        type(Result) :: r                                       !! Result object to return
        type(NcDataset) :: nc                                   !! NetCDF dataset
        type(NcVariable) :: var                                 !! NetCDF variable
        type(NcGroup) :: grp, gcGrp                             !! NetCDF group
        integer :: x, y, s, i                                   !! Iterators for GridCells, SubRivers and inflows
        integer :: iX, iY, iS                                   !! Indices for inflow grid and SubRiver coordinates
        character(len=100) :: gridCellRef                       !! To store GridCell name in, e.g. "GridCell_x_y"
        integer :: gridCellType                                 !! Integer representing the GridCell type
        logical :: isValidInflow                                !! Is inflow SubRiver is a neighbouring river
        type(ErrorInstance), allocatable :: errors(:)           !! Errors to return

        ! No errors to begin with
        allocate(errors(0))

        ! Set up the grid structure
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")                        ! Get the Environment group
        var = grp%getVariable("gridSize")                       ! Get the grid size from the Environment
        call var%getData(me%gridSize)
        
        ! Set the size of Environment variable that holds the grid cells
        allocate(me%colGridCells(me%gridSize(1),me%gridSize(2)))
        ! Loop through the x dimensions of the grid
        do x = 1, me%gridSize(1)
            ! Loop through the y dimensions of the grid
            do y = 1, me%gridSize(2)
                gridCellRef = "GridCell_" // trim(str(x)) // &
                    "_" // trim(str(y))
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
                            ! TODO: Not valid type, throw an error
                    end select
                else
                    allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y,isEmpty=.true.))
                end if
            end do
        end do
! *** SL:
! don't we need to actually create the GridCell objects
! (and by extension the SubRivers etc.) here? or is it that
! that last block of code does that, in association with the
! 'interface GridCell1' in classGridCell1.f08? At the moment it Also
! looks as though the line
!
! allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y))
!
! will only work specifically with GridCell objects of type GridCell1?
! There's no appearent way of specifying the actual type of the GridCell...
! ...which we will need to do as we want to have (and use in anger)
! a number of different actual GridCell types
! ***
        ! Now we need to create links between SubRivers (wasn't possible before creating GridCells
        ! and their SubRivers). We will point SubRivers' inflows array elements to GridCells' colSubRivers
        ! array elements.
        ! TODO: Do something with result object! And audit that inflows are coming from rational cells
        do x = 1, size(me%colGridCells, 1)                                              ! Loop through the rows
            do y = 1, size(me%colGridCells, 2)                                          ! Loop through the columns
                if (.not. me%colGridCells(x,y)%item%isEmpty) then
                    do s = 1, size(me%colGridCells(x,y)%item%colSubRivers)                  ! Loop through the SubRivers
                        associate(subRiver => me%colGridCells(x,y)%item%colSubRivers(s)%item)
                            do i = 1, subRiver%nInflows                                      ! Loop through the inflows
                                ! Get the inflow coordinates
                                iX = subRiver%inflowRefs(i)%gridX
                                iY = subRiver%inflowRefs(i)%gridY
                                iS = subRiver%inflowRefs(i)%subRiver
! *** SL:
! is this the best place to do the auditing?
! I'm thinking that there could be errors that need careful thinking about
! in order to structure the code to pick them up,
! an example would be where a SubRiver is incorrectly linked to more than one
! downstream SubRiver. This could happen because of the way we are linking
! i.e. that the links are held in the downstream SubRiver and point at the "upstream"
! SubRiver(s). (Actually, all this means is that no two pointers from a SubRiver to an upstream
! SubRiver can be the same).
! Worth having a sit-down to go through how to structure this auditing.
! ***
                                isValidInflow = .true.
                                ! Check that (iX, iY) is a neighbouring or the same GridCell
                                if (abs(iX-x) > 1 .or. abs(iY-y) > 1) isValidInflow = .false.
                                ! If the inflow is coming from different cell, is it from the outflow to that cell?
                                if ((iX /= x .or. iY /= y) &
                                    .and. (me%colGridCells(iX,iY)%item%nSubRivers /= iS)) isValidInflow = .false.
                                ! If invalid inflow, generate an error
                                if (.not. isValidInflow) then
                                    errors = [errors, ErrorInstance( &
                                        code = 401, &
                                        message = "Invalid SubRiver inflow from " &
                                            // trim(adjustl(me%colGridCells(iX,iY)%item%colSubRivers(iS)%item%ref)) // &
                                            " to " // trim(adjustl(subRiver%ref)) // &
                                            ". Inflow must be from a neighbouring SubRiver." &
                                    )]
                                end if

                                ! Point this SubRiver's inflows pointer to the corresponding SubRiver
                                subRiver%inflows(i)%item => me%colGridCells(iX,iY)%item%colSubRivers(iS)%item
                            end do
                        end associate
                    end do
                end if
            end do
        end do
        call r%addErrors(errors)        ! Add any errors that have occurred
        call ERROR_HANDLER%trigger(errors=errors)
    end function

    !> Destroy the Environment instance
    function destroyEnvironment1(me) result(r)
        class(Environment1) :: me
        type(Result) :: r
        ! Destroy logic here
    end function

    !> Perform simulations for the Environment
    function updateEnvironment1(me, t) result(r)
        class(Environment1) :: me                               !! This Environment instance
        integer :: t                                            !! Current time step
        type(Result) :: r
        integer :: x, y, s
        ! Perform the main routing procedure
        do x = 1, size(me%colGridCells, 1)                      ! Loop through the rows
            do y = 1, size(me%colGridCells, 2)                  ! Loop through the columns
                r = me%colGridCells(x,y)%item%update(t)         ! Run routing simulation for each GridCell
            end do
        end do
        ! Finalise the routing by setting outflows to temporary outflows that were stored
        ! to avoid routing using the wrong timestep's outflow as an inflow.
        do x = 1, size(me%colGridCells, 1)                      ! Loop through the rows
            do y = 1, size(me%colGridCells, 2)                  ! Loop through the columns
                r = me%colGridCells(x,y)%item%finaliseUpdate()  ! finaliseUpdate() loops through SubRivers
            end do
        end do
        ! *** SL comments re SubRiver routing and the SubRiver%routing() function:
        ! the SubRiver%routing() command needs to go into a separate function (in GridCell not Environment???) that executes whatever
        ! transport commands are needed to route water and sediment etc. through and in a specific GridCell.
        ! At the moment we can focus solely on water and suspended sediment routing through a river system,
        ! but ultimately we need to loop through all GridCells of all types and handle routing for all possible
        ! situations, e.g. soil-> river/lake, in-lake water movements, estuarine transport, and marine transport.
        ! This is going to need some careful thinking because at the moment, considering only riverine transport,
        ! we are only dealing with a situation where water and sediment move in a single direction. For some groups
        ! of GridCell types we need to consider two-way exchange of material - particularly for the marine GridCells.
        ! We may well need, for example, to treat the marine cells as a distinct collection within Environment in
        ! order to achieve this.
        ! ***
    end function
end module

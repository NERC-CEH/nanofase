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
        type(NcGroup) :: grp                                    !! NetCDF group
        integer :: x, y, s, i                                   !! Iterators for GridCells, SubRivers and inflows
        integer :: iX, iY, iS                                   !! Indices for inflow grid and SubRiver coordinates
        character(len=100) :: gridCellRef                       !! To store GridCell name in, e.g. "GridCell_x_y"
        logical :: isValidInflow = .true.                       !! Is inflow SubRiver is a neighbouring river
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
                    "_" // trim(str(y))   ! str() function is from UtilModule
                ! Check if the GridCell is defined in the data file before creating
                ! it and adding to colGridCells. If it doesn't exist, specify it is
                ! an empty GridCell.
                if (grp%hasGroup(trim(gridCellRef))) then
                    allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y))                  
                else
                    allocate(me%colGridCells(x,y)%item, source=GridCell1(x,y,isEmpty=.true.))
                end if
            end do
        end do

        ! Now we need to create links between SubRivers (wasn't possible before creating GridCells
        ! and their SubRivers). We will point SubRivers' inflows array elements to GridCells' colSubRivers
        ! array elements.
        ! TODO: Do something with result object! And audit that inflows are coming from rational cells
        do x = 1, size(me%colGridCells, 1)                                              ! Loop through the rows
            do y = 1, size(me%colGridCells, 2)                                          ! Loop through the columns
                do s = 1, size(me%colGridCells(x,y)%item%colSubRivers)                  ! Loop through the SubRivers
                    associate(subRiver => me%colGridCells(x,y)%item%colSubRivers(s)%item)
                        do i =1, subRiver%nInflows                                      ! Loop through the inflows
                            ! Get the inflow coordinates
                            iX = subRiver%inflowRefs(i)%gridX
                            iY = subRiver%inflowRefs(i)%gridY
                            iS = subRiver%inflowRefs(i)%subRiver

                            ! Check that (iX, iY) is a neighbouring GridCell
                            if (abs(iX-x) > 1 .or. abs(iY-y) > 1) isValidInflow = .false.
                            ! If from same GridCell, check it's a neighbouring SubRiver
                            if (iX == x .and. iY == y .and. abs(iS-s) > 1) isValidInflow = .false.
                            ! If the inflow is coming from different cell, is it from the outflow to that cell?
                            if (iX == x .and. iY == y &
                                .and. (me%colGridCells(iX,iY)%item%nSubRivers /= iS)) isValidInflow = .false.
                            ! If invalid inflow, generate an error
                            if (.not. isValidInflow) then
                                errors = [errors, ErrorInstance( &
                                    code = 401, &
                                    message = "Invalid SubRiver inflow from " // subRiver%ref // &
                                        " to " // subRiver%ref // ". Inflow must be from a neighbouring SubRiver." &
                                )]
                            end if
                            
                            ! Point this SubRiver's inflows pointer to the corresponding SubRiver
                            subRiver%inflows(i)%item => me%colGridCells(iX,iY)%item%colSubRivers(iS)%item
                        end do
                        ! Now route the water and SPM through the SubRivers
                        r = subRiver%routing()
                    end associate
                end do
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
end module
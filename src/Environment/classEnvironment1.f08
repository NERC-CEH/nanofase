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
        procedure, public :: getQ
        procedure, public :: getSpm
    end type

  contains

    !> Create the environment, which sets up the grid and river structure:
    !!  - Get grid structure from data file and set up GridCell collection
    function createEnvironment1(me) result(r)
        class(Environment1), target :: me                       !! This Environment instace. Must be target so SubRivers can be pointed at
        type(Result) :: r                                       !! Result object to return
        type(NcDataset) :: nc                                   !! NetCDF dataset
        type(NcVariable) :: var                                 !! NetCDF variable
        type(NcGroup) :: grp                                    !! NetCDF group
        integer :: x, y, s, i                                   !! Iterators for GridCells, SubRivers and inflows
        integer :: iX, iY, iS                                   !! Indices for inflow grid and SubRiver coordinates
        character(len=100) :: gridCellRef                       !! To store GridCell name in, e.g. "GridCell_x_y"

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
        ! TODO: Do something with result object!
        do x = 1, size(me%colGridCells, 1)                                              ! Loop through the rows
            do y = 1, size(me%colGridCells, 2)                                          ! Loop through the columns
                do s = 1, size(me%colGridCells(x,y)%item%colSubRivers)                  ! Loop through the SubRivers
                    do i =1, me%colGridCells(x,y)%item%colSubRivers(s)%item%nInflows    ! Loop through the inflows
                        associate(subRiver => me%colGridCells(x,y)%item%colSubRivers(s)%item)
                            ! Get the inflow coordinates
                            iX = subRiver%inflowRefs(i)%gridX
                            iY = subRiver%inflowRefs(i)%gridY
                            iS = subRiver%inflowRefs(i)%subRiver
                            ! Point this SubRiver's inflows pointer to the corresponding SubRiver
                            subRiver%inflows(i)%item => me%colGridCells(iX,iY)%item%colSubRivers(iS)%item
                        end associate  
                    end do
                end do
            end do
        end do
    end function

    !> Destroy the Environment instance
    function destroyEnvironment1(me) result(r)
        class(Environment1) :: me
        type(Result) :: r
        ! Destroy logic here
    end function

    !> DUMMY FUNCTION for the time being.
    function getQ(me, gridX, gridY, subRiverN) result(r)
        class(Environment1) :: me
        integer :: gridX
        integer :: gridY
        integer :: subRiverN
        type(Result0D) :: r
        r = Result(data=1.0_dp)
    end function

    !> DUMMY FUNCTION for the time being.
    function getSpm(me, gridX, gridY, subRiverN, iSizeClass) result(r)
        class(Environment1) :: me
        integer :: gridX
        integer :: gridY
        integer :: subRiverN
        integer :: iSizeClass
        type(Result0D) :: r
        r = Result(data=1.0_dp)
    end function
end module
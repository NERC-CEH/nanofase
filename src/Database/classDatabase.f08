!> Module for interacting with the input data.
!! TODO: Not currently working as var%getData(variable) needs a specific
!! variable type passed to it, so using a polymorphic class(*) won't work
!! and we would need to take an approach similar to mo_netcdf in creating
!! procedures for every data type. Need to think about this a bit more.
module classDatabase
    use ResultModule
    use mo_netcdf
    implicit none
    private

    type, public :: Database
        character(len=256) :: filePath
        type(NcDataset) :: file

      contains
        procedure, public :: init => initDatabase
        procedure, public :: destroy => destroyDatabase
        procedure, public :: read => readDatabase
        procedure, public :: get => getVariable
    end type

  contains
    !> Initialise the NetCDF database
    function initDatabase(me, filePath) result(r)
        class(Database) :: me         !! This Database object
        character(len=*) :: filePath        !! Path to the data file
        type(Result) :: r                   !! The Result object
        me%filePath = filePath                                  ! Store the file path
        me%file = NcDataset(me%filePath, "r")   ! Open dataset as read-only
    end function

    !> Destroy the NetCDF database
    function destroyDatabase(me) result(r)
        class(Database) :: me
        type(Result) :: r
        ! me%file%close()
    end function

    function getVariable(me, location, variable) result(r)
        class(Database) :: me
        character(len=256), allocatable :: location(:)
        class(*) :: variable
        integer :: i
        type(NcVariable) :: var             !! NetCDF variable
        type(NcGroup) :: grp                !! NetCDF group
        type(Result) :: r
        do i = 1, size(location) - 1
            if (i == 1) then
                if (me%file%hasGroup(trim(location(1)))) then
                    grp = me%file%getGroup(trim(location(1)))
                else
                    ! error
                end if
            else
                if (grp%hasGroup(trim(location(1)))) then
                    grp = grp%getGroup(trim(location(1)))
                else
                    ! error
                end if
            end if
        end do
        ! Final array element should be the variable
        var = grp%getVariable(location(i+1))
        call var%getData(variable)
        r = Result(data=variable)
    end function

    !> Read data from the NetCDF database
    function readDatabase(me, ref) result(r)
        class(Database) :: me
        character(len=*), allocatable :: ref(:)
        type(Result) :: r
    end function
end module
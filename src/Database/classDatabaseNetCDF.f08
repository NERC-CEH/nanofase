module classDatabaseNetCDF
    use ResultModule
    private
    implicit none

    type, public :: DatabaseNetCDF
      contains
        procedure, public :: init => initDatabaseNetCDF
        procedure, public :: destroy => destroyDatabaseNetCDF
        procedure, public :: read => readDatabaseNetCDF
    end type

  contains
    !> Initialise the NetCDF database
    function initDatabaseNetCDF(me) result(r)
        class(DatabaseNetCDF) :: me
        type(Result) :: r
    end function

    !> Destroy the NetCDF database
    function destroyDatabaseNetCDF(me) result(r)
        class(DatabaseNetCDF) :: me
        type(Result) :: r
    end function

    !> Read data from the NetCDF database
    function readDatabaseNetCDF(me, ref) result(r)
        class(DatabaseNetCDF) :: me
        character(len=*), allocatable :: ref(:)
        type(Result) :: r
    end function
end module
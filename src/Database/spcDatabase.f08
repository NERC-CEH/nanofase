module spcDatabase
    use ResultModule
    private
    implicit none

    type, abstract :: Database
        character(len=256), allocatable :: files(:)         ! List of input files

      contains
        procedure(initDatabase), deferred :: init
        procedure(destroyDatabase), deferred :: destroy
        procedure(readDatabase), deferred :: read
    end type

    abstract interface
        !> Initialise the database
        function initDatabase(me) result(r)
            import spcDatabase, Result
            class(Database) :: me
            type(Result) :: r
        end function

        !> Destroy the database
        function destroyDatabase(me) result(r)
            import spcDatabase, Result
            class(Database) :: me
            type(Result) :: r
        end function

        !> Read data from the database
        function readDatabase(me, ref) result(r)
            import spcDatabase, Result
            class(Database) :: me
            character(len=*), allocatable :: ref(:)
            type(Result) :: r
        end function
    end interface
end module
module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    implicit none
    private

    type, public, abstract :: Environment
        integer, allocatable                :: gridSize(:)          ! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)    ! Array of GridCellElements objects to hold polymorphic GridCells
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
        procedure(updateEnvironment), deferred :: update
    end type

    abstract interface
        !> Interface to create an Environment object
        function createEnvironment(me) result(r)
            import Environment, Result
            class(Environment), target :: me
            type(Result) :: r
        end function
        !> Interface to destroy an Environment object
        function destroyEnvironment(me) result(r)
            import Environment, Result
            class(Environment) :: me
            type(Result) :: r
        end function
        !> Interface to perform simulations in Environment
        function updateEnvironment(me, t) result(r)
            import Environment, Result
            class(Environment) :: me
            integer :: t
            type(Result) :: r
        end function
    end interface
end module
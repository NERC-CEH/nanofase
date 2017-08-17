module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    implicit none
    private

    type, public, abstract :: Environment
        private
        type(GridCellElement), allocatable :: colGridCells(:)       ! Array of GridCellElements objects to hold polymorphic GridCells
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
    end type

    abstract interface
        !> Interface to create an Environment object
        function createEnvironment(me) result(r)
            import Environment, Result
            class(Environment) :: me
            type(Result) :: r
        end function
        !> Interface to destroy an Environment object
        function destroyEnvironment(me) result(r)
            import Environment, Result
            class(Environment) :: me
            type(Result) :: r
        end function
    end interface
end module
!> Module container for the `Environment` abstract base class
module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    implicit none
    private

    !> Abstract base class definition for `Environment`.
    type, public, abstract :: Environment
        integer, allocatable                :: gridSize(:)          !! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)    !! Array of `GridCellElement` objects to hold polymorphic `GridCell`s
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
        procedure(updateEnvironment), deferred :: update
    end type

    abstract interface
        !> Interface to create an Environment object
        function createEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment), target :: me            !! This `Environment` instance
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        !> Interface to destroy an Environment object
        function destroyEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me                    !! This `Environment` instance
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        !> Interface to perform simulations in Environment
        function updateEnvironment(me, t) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me                    !! This `Environment` instance
            integer :: t                                !! The current time step
            type(Result) :: r                           !! `Result` object containing any errors
        end function
    end interface
end module
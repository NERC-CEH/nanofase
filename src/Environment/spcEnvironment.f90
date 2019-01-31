!> Module container for the `Environment` abstract base class
module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    use mo_netcdf
    implicit none
    private

    !> Abstract base class definition for `Environment`.
    type, public, abstract :: Environment
        integer, allocatable                :: gridDimensions(:)    !! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)    !! Array of `GridCellElement` objects to hold polymorphic `GridCell`s
        type(GridCellPointer), allocatable  :: headwaters(:)        !! Array of `GridCell`s that contain headwaters
        type(ReachPointer), allocatable     :: routedReaches(:,:)   !! Array of pointers to routed reaches
        integer, allocatable                :: routedReachIndices(:,:,:)    !! Indices of the routed reaches, use to construct the routedReaches pointer
        integer                             :: nHeadwaters          !! The number of headwaters in the Environment
        type(NcGroup)                       :: ncGroup              !! NetCDF group for this `Environment` object
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
        procedure(updateEnvironment), deferred :: update
        procedure(parseInputDataEnvironment), deferred :: parseInputData
        ! Getters
        procedure(get_m_npEnvironment), deferred :: get_m_np
    end type

    abstract interface
    
        !> Interface to create an `Environment` object
        function createEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment), target :: me            !! This `Environment` instance
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        
        !> Interface to destroy an `Environment` object
        function destroyEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me                    !! This `Environment` instance
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        
        !> Interface to perform simulations in `Environment`
        function updateEnvironment(me, t) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me                    !! This `Environment` instance
            integer :: t                                !! The current time step
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        
        !> Interface to import and parse input data for the `Environment` object
        function parseInputDataEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me
            type(Result) :: r
        end function
        
        function get_m_npEnvironment(me) result(m_np)
            use Globals
            import Environment
            class(Environment) :: me
            real(dp) :: m_np(C%nSizeClassesNP, 4, 2 + C%nSizeClassesSpm)
        end function
    end interface
end module
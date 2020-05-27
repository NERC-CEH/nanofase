!> Module container for the `Environment` abstract base class
module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    use mo_netcdf
    use classSampleSite, only: SampleSite
    implicit none
    private

    !> Abstract base class definition for `Environment`.
    type, public, abstract :: Environment
        integer, allocatable                :: gridDimensions(:)    !! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)    !! Array of `GridCellElement` objects to hold polymorphic `GridCell`s
        type(ReachPointer), allocatable     :: headwaters(:)        !! Array of `GridCell`s that contain headwaters
        type(ReachPointer), allocatable     :: routedReaches(:)     !! Array of pointers to routed reaches
        integer                             :: nHeadwaters = 0      !! The number of headwaters in the Environment
        integer                             :: nWaterbodies = 0     !! The number of waterbodies in the Environment
        type(NcGroup)                       :: ncGroup              !! NetCDF group for this `Environment` object
        type(SampleSite), allocatable :: sites(:)                   !! Sample sites for calibrating with
        type(SampleSite), pointer :: startSite, endSite             !! Start and end calibration sites
        type(SampleSite), allocatable :: otherSites(:)
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
        procedure(updateEnvironment), deferred :: update
        procedure(updateReachEnvironment), deferred :: updateReach
        procedure(determineStreamOrderEnvironment), deferred :: determineStreamOrder
        procedure(parseNewBatchDataEnvironment), deferred :: parseNewBatchData
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
            class(Environment), target :: me            !! This `Environment` instance
            integer :: t                                !! The current time step
            type(Result) :: r                           !! `Result` object containing any errors
        end function

        function updateReachEnvironment(me, t, reach) result(rslt)
            use ResultModule, only: Result
            use spcReach, only: ReachPointer
            import Environment
            class(Environment), target :: me        !! This Environment instance
            integer :: t                            !! The current timestep
            type(ReachPointer) :: reach             !! The reach to update
            type(Result) :: rslt                    !! Result object to return errors in
        end function
        
        !> Interface to import and parse input data for the `Environment` object
        function parseInputDataEnvironment(me) result(r)
            use ResultModule, only: Result
            import Environment
            class(Environment) :: me
            type(Result) :: r
        end function

        !> Determine the stream order of water bodies in the Environment
        subroutine determineStreamOrderEnvironment(me)
            import Environment
            class(Environment) :: me
        end subroutine

        subroutine parseNewBatchDataEnvironment(me)
            import Environment
            class(Environment) :: me
        end subroutine
        
        function get_m_npEnvironment(me) result(m_np)
            use Globals
            import Environment
            class(Environment) :: me
            real(dp) :: m_np(C%nSizeClassesNM, 4, 2 + C%nSizeClassesSpm)
        end function
    end interface
end module
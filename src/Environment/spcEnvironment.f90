!> Module container for the `Environment` abstract base class
module spcEnvironment
    use Globals
    use ResultModule
    use spcGridCell
    use mo_netcdf
    use classSampleSite, only: SampleSite
    implicit none
    private

    type, public :: EnvironmentPointer
        class(Environment), pointer :: item => null()                   !! Pointer to polymorphic Environment object
    end type

    !> Abstract base class definition for `Environment`.
    type, public, abstract :: Environment
        integer, allocatable                :: gridDimensions(:)        !! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)        !! Array of `GridCellElement` objects to hold polymorphic `GridCell`s
        integer                             :: nGridCells = 0           !! Number of grid cells in the Environment
        type(ReachPointer), allocatable     :: headwaters(:)            !! Array of `GridCell`s that contain headwaters
        type(ReachPointer), allocatable     :: routedReaches(:)         !! Array of pointers to routed reaches
        integer                             :: nHeadwaters = 0          !! The number of headwaters in the Environment
        integer                             :: nWaterbodies = 0         !! The number of waterbodies in the Environment
        type(NcGroup)                       :: ncGroup                  !! NetCDF group for this `Environment` object
        type(SampleSite), allocatable :: sites(:)                       !! Sample sites for calibrating with
        type(SampleSite), pointer :: startSite, endSite                 !! Start and end calibration sites
        type(SampleSite), allocatable :: otherSites(:)
        ! Summary statistics
        real(dp), allocatable               :: C_np_water_t(:,:,:,:)    !! Water NM conc spatial mean on each timestep [kg/m3]
        real(dp), allocatable               :: C_np_sediment_t(:,:,:,:) !! Sediment NM conc spatial mean on each timestep [kg/kg]
        real(dp), allocatable               :: m_sediment_t_byLayer(:,:,:)  !! Sediment mass in each layer on each timestep [kg]
      contains
        procedure(createEnvironment), deferred :: create
        procedure(destroyEnvironment), deferred :: destroy
        procedure(updateEnvironment), deferred :: update
        procedure(updateReachEnvironment), deferred :: updateReach
        procedure(determineStreamOrderEnvironment), deferred :: determineStreamOrder
        procedure(parseNewBatchDataEnvironment), deferred :: parseNewBatchData
        ! Getters
        procedure(get_m_npEnvironment), deferred :: get_m_np
        procedure(get_C_np_soilEnvironment), deferred :: get_C_np_soil
        procedure(get_C_np_waterEnvironment), deferred :: get_C_np_water
        procedure(get_C_np_sedimentEnvironment), deferred :: get_C_np_sediment
        procedure(getBedSedimentAreaEnvironment), deferred :: getBedSedimentArea
        procedure(get_m_sediment_byLayerEnvironment), deferred :: get_m_sediment_byLayer
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
        subroutine updateEnvironment(me, t)
            import Environment
            class(Environment), target :: me            !! This `Environment` instance
            integer :: t                                !! The current time step
        end subroutine

        subroutine updateReachEnvironment(me, t, reach)
            use spcReach, only: ReachPointer
            import Environment
            class(Environment), target :: me        !! This Environment instance
            integer :: t                            !! The current timestep
            type(ReachPointer) :: reach             !! The reach to update
        end subroutine
        
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

        function get_C_np_soilEnvironment(me) result(C_np_soil)
            use Globals, only: C, dp
            import Environment
            class(Environment) :: me
            real(dp), allocatable :: C_np_soil(:,:,:)
        end function

        function get_C_np_waterEnvironment(me) result(C_np_water)
            use Globals, only: C, dp
            import Environment
            class(Environment) :: me
            real(dp), allocatable :: C_np_water(:,:,:)
        end function

        function get_C_np_sedimentEnvironment(me) result(C_np_sediment)
            use Globals, only: C, dp
            import Environment
            class(Environment) :: me
            real(dp), allocatable :: C_np_sediment(:,:,:)
        end function

        function getBedSedimentAreaEnvironment(me) result(bedArea)
            use Globals, only: dp
            import Environment
            class(Environment) :: me
            real(dp) :: bedArea
        end function

        function get_m_sediment_byLayerEnvironment(me) result(m_sediment_byLayer)
            use Globals, only: dp, C
            import Environment
            class(Environment)      :: me
            real(dp), allocatable   :: m_sediment_byLayer(:,:)
        end function

    end interface

end module
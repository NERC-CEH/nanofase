!> Module container for `SubRiver` class
module spcSubRiver
    use Globals                                                     ! global declarations
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    use spcRiverReach                                               ! use containing object type
    implicit none                                                   ! force declaration of all variables

    !> Container type for `class(RiverReach)`, the actual type of the `RiverReach` class.
    !! a variable of type `RiverReachElement` can be of any object type inheriting from the
    !! `RiverReach` abstract base class.
    type RiverReachElement                                          
        class(RiverReach), allocatable :: item                      !! Polymorphic `RiverReach` object
    end type

    !> An internal user-defined type, defining a reference to a `SubRiver` sending water to this
    !! `SubRiver`. Comprises row (x) and column (y) references to the `GridCell` containing the
    !! sending `SubRiver` and the in-cell `SubRiver` reference number
    type RoutingRef                                                 
        type(integer) :: gridX                                      !! `GridCell` x reference
        type(integer) :: gridY                                      !! `GridCell` y reference
        type(integer) :: subRiver                                   !! `SubRiver` reference
    end type

    !> `SubRiverPointer` used for `SubRiver` inflows array, so the elements within can
    !! point to other `GridCell`'s colSubRiver elements
    type SubRiverPointer                                            ! As item is a pointer, this definition can come before type(SubRiver)
        class(SubRiver), pointer :: item => null()                  !! Pointer to polymorphic `SubRiver` object
    end type
    
    !> A `SubRiver` class acts as a container for a collection
    !! of `RiverReach` objects which collectively define the layout of the
    !! flowing waters within each `GridCell`. The `RiverReach` class routes,
    !! water, suspended sediments (and ultimately nanoparticles) through
    !! the flowing waters within the `GridCell`
    type, abstract, public :: SubRiver
        character(len=100) :: ref
            !! `SubRiver` reference of the format *SubRiver_x_y_n*, where *x* is `GridCell` row,
            !! *y* is `GridCell` column and *n* is `SubRiver` number in `GridCell`
        real(dp) :: length                                          !! The length of the `SubRiver` (without any meandering factor)
        type(RoutingRef), allocatable :: inflowRefs(:)
            !! Array of references to source `SubRiver`s for this `SubRiver` (sources can be in a different grid cell).
            !! This is used temporarilly to enable `me%inflows` array to be filled with pointers, to save us getting
            !! them from the data file again
        integer :: nInflows                                         !! The number of inflows to the `SubRiver`
        integer :: nReaches                                         !! The number of reaches in the `SubRiver`
        integer, allocatable :: reachTypes(:)                       !! Integer array of reach type identifiers
        real(dp) :: Qin                                             !! Inflow per timestep [m3]
        real(dp) :: Qout                                            !! Discharge from the Subriver [m3]
        real(dp), allocatable :: Q_runoff_timeSeries(:)             !! Complete time series runoff data [m3/timestep]
        real(dp) :: Qrunoff                                         !! Initial runoff from the hydrological model [m3]
        real(dp) :: tmpQout
            !! Temporary variable to store `Qout` whilst other `SubRiver`s are using previous time step's `Qout`.
            !! Otherwise, `Qin` to a `SubRiver` might be set to the this time step's `Qout` instead of the previous
        real(dp), allocatable :: spmIn(:)                           !! Inflow SPM masses [kg] for each size class
        real(dp), allocatable :: spmOut(:)                          !! Outflow SPM masses [kg], one per size class
        real(dp), allocatable :: tmpSpmOut(:)                       !! Temporary outflow SPM masses (see tmpQout description)
        real(dp), allocatable :: m_spm(:)                           !! Mass of SPM currently in `SubRiver` [kg], per size class
        real(dp), allocatable :: tmpm_spm(:)                        !! Temporary SPM mass [kg]
        type(RiverReachElement), allocatable :: colReaches(:)       !! Array of `RiverReachElement` objects
        type(SubRiverPointer), allocatable :: inflows(:)            !! Array of pointers to inflows
        integer :: allst                                            !! Allocation status
      contains
        procedure(createSubRiver), deferred :: create               ! create the SubRiver object. Exposed name: create
        procedure(destroySubRiver), deferred :: destroy             ! remove the SubRiver object and all contained objects. Exposed name: destroy
        procedure(updateSubRiver), deferred :: update               ! route water and suspended solids through a SubRiver. Exposed name: routing
        procedure(finaliseUpdateSubRiver), deferred :: finaliseUpdate ! Finalise the routing by setting temp outflows to actual outflows
        procedure :: getQOut => getQOutSubRiver                     ! Return the outflow Q [m3]
        procedure :: getSpmOut => getSpmOutSubRiver                 ! Return the outflow SPM for all size classes [kg]
        procedure :: getSpmOutBySizeClass => getSpmOutBySizeClassSubRiver ! Return the outflow SPM for an individual size class [kg]
    end type

    !> Container type for polymorphic `SubRiver` objects, such that different
    !! actual types inheriting from `SubRiver` can be stored in same array
    type SubRiverElement
        class(SubRiver), allocatable :: item                        !! Polymorphic `SubRiver` object
    end type

    abstract interface
        !> Create the `SubRiver` object by reading data in from file
        function createSubRiver(me, x, y, s, length, Q_runoff_timeSeries) result(r)
            use Globals
            use ResultModule, only: Result
            import SubRiver
            class(SubRiver) :: me                                   !! The `SubRiver` instance
            type(integer), intent(in) :: x                          !! The row number of the enclosing `GridCell`
            type(integer), intent(in) :: y                          !! The column number of the enclosing `GridCell`
            type(integer), intent(in) :: s                          !! Reference `SubRiver` number
            real(dp) :: length                                      !! The `SubRiver` length
            real(dp), allocatable :: Q_runoff_timeSeries(:)         !! Any initial runoff
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function
        !> Destory this `SubRiver`
        function destroySubRiver(me) result(r)
            use ResultModule, only: Result
            import SubRiver
            class(SubRiver) :: me                                   !! The `SubRiver` instance
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function
        !> Run the simulation for the current time step. Routes inflow(s)
        !! through the `SubRiver`
        function updateSubRiver(me, t, j_spm_runoff) result(r)
            use Globals
            use ResultModule, only: Result
            import SubRiver
            class(SubRiver) :: me                                   !! The `SubRiver` instance
            integer :: t                                            !! The current time step
            real(dp) :: j_spm_runoff(:)				                !! Eroded sediment runoff [kg/timestep]
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function
        !> Finalise the simulation by setting an state variable from
        !! temporary variables that were used to avoid time step conflicts
        function finaliseUpdateSubRiver(me) result(r)
            use ResultModule, only: Result
            import SubRiver
            class(SubRiver) :: me                                   !! The `SubRiver` instance  
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function
    end interface

  contains
    !> Get the discharge from the `SubRiver`
    function getQOutSubRiver(me) result(QOut)
        class(SubRiver) :: me                                       !! This `SubRiver` instance
        real(dp) :: QOut                                            !! Discharge out of the `SubRiver` [m3]
        QOut = me%QOut
    end function

    !> Get the SPM discharge from the `SubRiver`
    function getSpmOutSubRiver(me) result(spmOut)
        class(SubRiver) :: me                                       !! This `SubRiver` instance
        real(dp) :: spmOut(size(me%spmOut))                         !! SPM discharge for all size classes [kg]
        spmOut = me%spmOut
    end function

    !> Get the SPM discharge from the `SubRiver`, for a given size class.
    function getSpmOutBySizeClassSubRiver(me, n) result(spmOut)
        class(SubRiver) :: me                                       !! This `SubRiver` instance
        integer :: n                                                !! Size class
        real(dp) :: spmOut                                          !! SPM discharge [kg]
        if (allocated(me%spmOut)) then
            spmOut = me%spmOut(n)
        end if
    end function
end module

module spcSubRiver
                                                                    ! superclass for SubRiver subclasses
                                                                    ! defines properties and methods required in any implmentation
                                                                    ! of a River class
                                                                    ! a River class acts as a container for a collection of RiverReach objects which collectively define the
                                                                    ! layout of the flowing waters within each grid cell
                                                                    ! the RiverReach class routes, water, suspended sediments (and ultimately nanoparticles) through the flowing waters within
                                                                    ! the grid cell
                                                                    ! IMPORTED MODULES
                                                                    ! Description
                                                                    ! -----------
    use Globals                                                     ! global declarations
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    use spcRiverReach                                               ! use containing object type
    implicit none                                                   ! force declaration of all variables

    type RiverReachElement                                          ! container type for class(RiverReach), the actual type of the RiverReach class
        class(RiverReach), allocatable :: item                      ! a variable of type RiverReachElement can be of any object type inheriting from the
    end type                                                        ! RiverReach superclass

    type RoutingRef                                                 ! an internal user-defined type, defining a reference to a SubRiver sending water to this
        type(integer) :: gridX                                      ! SubRiver, or receiving water from it. Comprises row (X) and column (Y) references to the GridCell
        type(integer) :: gridY                                      ! containing the sending/receiving subriver
        type(integer) :: subRiver                                   ! (as this SubRiver) and the in-cell SubRiver reference number
    end type

    ! SubRiverPointer used for SubRiver inflows array, so the elements within can point to other GridCell's colSubRiver elements
    type SubRiverPointer
        class(SubRiver), pointer :: item => null()                  ! as item is a pointer, this definition can come before type(SubRiver)
    end type
    
    type, abstract, public :: SubRiver                              ! type declaration for superclass
        character(len=100) :: ref                                   ! SubRiver reference of the format SubRiver_x_y_n, where x is GridCell row,
                                                                    ! y is GridCell column and n is SubRiver number in GridCell
                                                                    ! PROPERTIES
                                                                    ! Description
                                                                    ! -----------
        real(dp) :: length                                          ! The length of the SubRiver (without any meandering factor)
        type(RoutingRef), allocatable :: inflowRefs(:)              ! array of references to source subrivers for this subriver (sources can be in a different grid cell)
                                                                    ! this is used temporarilly to enable me%inflows array to be filled with pointers, to save us getting
                                                                    ! them from the data file again
        integer :: nInflows                                         ! the number of inflows to the SubRiver
        integer :: nReaches                                         ! the number of reaches in the SubRiver
        integer, allocatable :: reachTypes(:)                       ! integer array of Reach type identifiers
        real(dp) :: Qin                                             ! Inflow per timestep [m3]
        real(dp) :: Qout                                            ! discharge from the Subriver [m3]
        real(dp), allocatable :: QrunoffTimeSeries(:)               ! Complete time series runoff data [m3/timestep]
        real(dp) :: Qrunoff                                         ! Initial runoff from the hydrological model [m3]
        real(dp) :: tmpQout                                         ! Temporary variable to store Qout whilst other SubRivers are using previous timestep's Qout.
                                                                    ! Otherwise, Qin to a SubRiver might be set to the this timestep's Qout instead of the previous
        real(dp), allocatable :: spmIn(:)                           ! Inflow SPM masses [kg] for each size class
        real(dp), allocatable :: spmOut(:)                          ! Outflow SPM masses [kg], one per size class
        real(dp), allocatable :: tmpSpmOut(:)                       ! Temporary outflow SPM masses (see tmpQout description)
        real(dp), allocatable :: m_spm(:)                           ! Mass of SPM currently in SubRiver [kg], per size class
        real(dp), allocatable :: tmpm_spm(:)                        ! Temporary SPM mass [kg]
        ! need a function somewhere (probably in RiverReach) to convert SPM mass in a size class to particle number
        ! this is needed *** for settling rates *** and for heteroaggregation with nanoparticles

        integer :: allst                                            ! array allocation status, must be public to be accessible by subclasses
                                                                    ! CONTAINED OBJECTS
                                                                    ! Description
                                                                    ! -----------
        type(RiverReachElement), allocatable :: colReaches(:)       ! array of RiverReachElement objects
        type(SubRiverPointer), allocatable :: inflows(:)            ! array of pointers to inflows
      contains
                                                                    ! METHODS
                                                                    ! Description
                                                                    ! -----------
        procedure(createSubRiver), deferred :: create               ! create the SubRiver object. Exposed name: create
        procedure(destroySubRiver), deferred :: destroy             ! remove the SubRiver object and all contained objects. Exposed name: destroy
        procedure(updateSubRiver), deferred :: update               ! route water and suspended solids through a SubRiver. Exposed name: routing
        procedure(finaliseUpdateSubRiver), deferred :: finaliseUpdate ! Finalise the routing by setting temp outflows to actual outflows
        procedure :: getQOut => getQOutSubRiver                     ! Return the outflow Q [m3]
        procedure :: getSpmOut => getSpmOutSubRiver                 ! Return the outflow SPM for all size classes [kg]
        procedure :: getSpmOutBySizeClass => getSpmOutBySizeClassSubRiver ! Return the outflow SPM for an individual size class [kg]
    end type

    type SubRiverElement                                            ! container type for class(SubRiver), the actual type of the SubRiver class
        class(SubRiver), allocatable :: item                        ! a variable of type SubRiver can be of any object type inheriting from the
    end type

    abstract interface
        function createSubRiver(me, x, y, s, length, QrunoffTimeSeries) result(r) ! create the SubRiver object by reading data in from file
            use Globals
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            type(integer), intent(in) :: x                          ! the row number of the enclosing GridCell
            type(integer), intent(in) :: y                          ! the column number of the enclosing GridCell
            type(integer), intent(in) :: s                          ! reference SubRiver number
            real(dp) :: length                                      ! The SubRiver length
            real(dp), allocatable :: QrunoffTimeSeries(:)           ! Any initial runoff
            type(Result) :: r                                       ! the result object
        end function
        function destroySubRiver(me) result(r)
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            type(Result) :: r                                       ! the result object
        end function
        function updateSubRiver(me, t) result(r)                      ! routes inflow(s) through the SubRiver
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            integer :: t                                            ! What time step are we on?
            type(Result) :: r                                       ! the result object
        end function
        function finaliseUpdateSubRiver(me) result(r)
            import SubRiver, Result
            class(SubRiver) :: me
            type(Result) :: r
        end function
    end interface

  contains
    !> Get the discharge from the SubRiver.
    function getQOutSubRiver(me) result(QOut)
        class(SubRiver) :: me                                       !! This SubRiver instance
        real(dp) :: QOut                                            !! Discharge out of the SubRiver [m3]
        QOut = me%QOut
    end function

    !> Get the SPM discharge from the SubRiver.
    function getSpmOutSubRiver(me) result(spmOut)
        class(SubRiver) :: me                                       !! This SubRiver instance
        real(dp) :: spmOut(size(me%spmOut))                         !! SPM discharge for all size classes [kg]
        spmOut = me%spmOut
    end function

    !> Get the SPM discharge from the SubRiver, for a given size class.
    function getSpmOutBySizeClassSubRiver(me, n) result(spmOut)
        class(SubRiver) :: me                                       !! This SubRiver instance
        integer :: n                                                !! Size class
        real(dp) :: spmOut                                          !! SPM discharge [kg]
        if (allocated(me%spmOut)) then
            spmOut = me%spmOut(n)
        end if
    end function
end module

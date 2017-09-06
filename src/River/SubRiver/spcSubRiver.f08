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
    ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
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
        character(len=256) :: name                                  ! a name for the object - is this used/needed?
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
        type(integer) :: nReaches                                   ! the number of reaches in the SubRiver
        type(integer), allocatable :: reachTypes(:)                 ! integer array of Reach type identifiers
        type(real(dp)) :: QOut                                      ! discharge from the Subriver // m3
        type(real(dp)), allocatable :: spmIn(:,:)                   ! inflow SPM masses //kg, 1st rank for each RiverReach, 2nd for each size class (?)
        type(real(dp)), allocatable :: spmOut(:)                    ! array of SPM masses //kg, one per size class, in discharge
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
        procedure(routingSubRiver), deferred :: routing             ! route water and suspended solids through a SubRiver. Exposed name: routing
        procedure :: getQOut
        procedure :: getSpmOut
    end type

    type SubRiverElement                                            ! container type for class(SubRiver), the actual type of the SubRiver class
        class(SubRiver), allocatable :: item                        ! a variable of type SubRiver can be of any object type inheriting from the
    end type

    abstract interface
        function createSubRiver(me, x, y, s, length) result(r)      ! create the SubRiver object by reading data in from file
            use Globals
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            type(integer), intent(in) :: x                          ! the row number of the enclosing GridCell
            type(integer), intent(in) :: y                          ! the column number of the enclosing GridCell
            type(integer), intent(in) :: s                          ! reference SubRiver number
            real(dp) :: length                                      ! The SubRiver length
            type(Result) :: r                                       ! the result object
        end function
        function destroySubRiver(me) result(r)
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            type(Result) :: r                                       ! the result object
        end function
        function routingSubRiver(me) result(r)                      ! routes inflow(s) through the SubRiver
            import SubRiver, Result
            class(SubRiver) :: me                                   ! the SubRiver instance
            type(Result) :: r                                       ! the result object
        end function
    end interface

  contains
    !> Get the discharge from the SubRiver.
    function getQOut(me) result(QOut)
        class(SubRiver) :: me                                       !! This SubRiver instance
        real(dp) :: QOut                                            !! Discharge out of the SubRiver [m3]
        QOut = me%QOut
    end function

    !> Get the SPM discharge from the SubRiver.
    function getSpmOut(me, n) result(spmOut)
        class(SubRiver) :: me                                       !! This SubRiver instance
        integer :: n                                                !! Size class
        real(dp) :: spmOut                                          !! SPM discharge [m3]
        if (allocated(me%spmOut)) then
            spmOut = me%spmOut(n)
        end if
    end function
end module

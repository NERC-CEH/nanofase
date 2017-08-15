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
  use Globals                                                        ! global declarations
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  implicit none                                                      ! force declaration of all variables
  type RiverReachElement                                             ! container type for class(RiverReach), the actual type of the RiverReach class
    class(RiverReach), allocatable :: item                           ! a variable of type RiverReachElement can be of any object type inheriting from the
  end type                                                           ! RiverReach superclass
  type RoutingRef                                                    ! an internal user-defined type, defining a reference to a SubRiver sending water to this
    type(integer) :: GridX                                           ! SubRiver, or receiving water from it. Comprises row (X) and column (Y) references to the GridCell
    type(integer) :: GridY                                           ! containing the sending/receiving subriver (set to null (-999) if it is the same GridCell
    type(integer) :: SubRiver                                        ! as this SubRiver) and the in-cell SubRiver reference number
  end type
  type SubRiver                                                      ! user-defined type definition for SubRiver
  end type
  type, abstract, public :: SubRiver                                 ! type declaration for superclass
    character(len=256) :: name                                       ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(RiverReachElement), allocatable :: colReaches(:)            ! array of RiverReachElement objects
    type(RoutingRef), allocatable :: inflow_ref(:)                   ! array of references to source subrivers for this subriver (sources can be in a different grid cell)
    type(RoutingRef) :: outflow_ref                                  ! reference to outflow subriver (outflow can be to a different grid cell)
    type(integer) :: nReaches                                        ! the number of reaches in the SubRiver
    type(real(dp)) :: discharge                                      ! discharge from the Subriver // m3
    type(real(dp)), allocatable :: spm(:)                            ! array of SPM masses //kg, one per size class, in discharge
    ! need a function somewhere (probably in RiverReach) to convert SPM mass in a size class to particle number
    ! this is needed *** for settling rates *** and for heteroaggregation with nanoparticles

    !integer :: nSubRivers                                            ! the number of SubRiver objects (a maximum of three)
    !integer :: GridX                                                 ! the row number of the GridCell object containing this object
    !integer :: GridY                                                 ! the column number of the GridCell object containing this object
    integer :: nSPMSC                                                ! number of SPM size classes
    integer, private :: allst                                        ! array allocation status
    integer, private :: err                                          ! ?
    type(Result), private :: r                                       ! Result object for returning from functions, for error checking
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public, deferred :: create => createSubRiver          ! create the SubRiver object. Exposed name: create
    procedure, public, deferred :: destroy => destroySubRiver        ! remove the SubRiver object and all contained objects. Exposed name: destroy
    procedure, public, deferred :: routing => routingSubRiver        ! route water and suspended solids through a SubRiver. Exposed name: routing
                                                                     ! PRIVATE ROUTINES
                                                                     ! Description
    ! IS IT NECESSARY TO DECLARE ALL PRIVATE CLASS-LEVEL METHODS HERE?
    ! IT WOULD BE MORE FLEXIBLE NOT TO HAVE TO DO THIS
                                                                     ! -----------
  end type
  abstract interface
    function createSubRiver(Me, Gx, Gy, SC) result(r)
      class(SubRiver) :: Me                                          ! the SubRiver instance
      type(integer), intent(in) :: Gx                                ! the row number of the enclosing GridCell
      type(integer), intent(in) :: Gy                                ! the column number of the enclosing GridCell
      type(integer), intent(in) :: SC                                ! the number of SPM size classes
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
      type(Result) :: r                                              ! the result object
      type(integer) :: x                                             ! loop counter
      type(integer) :: y                                             ! loop counter
      type(character(len=*)) :: srs                                  ! string to dynamically hold group names for SubRiver data in input file
      type(integer) :: nInflows                                      ! number of inflows for each SubRiver

      ! IS IT NECESSARY TO DECLARE ALL PRIVATE FUNCTION-LEVEL VARIABLES HERE?
      ! IT WOULD BE MORE FLEXIBLE NOT TO HAVE TO DO THIS

    end function
    function destroySubRiver(Me) result(r)
      class(SubRiver) :: Me                                          ! the SubRiver instance
      type(Result) :: r                                              ! the result object
    end function
    function routingSubRiver(Me) result(r)                           ! routes inflow(s) through the SubRiver
      class(River) :: Me                                             ! the River instance
      type(Result) :: r                                              ! the result object
    end function
  end abstract interface
end module

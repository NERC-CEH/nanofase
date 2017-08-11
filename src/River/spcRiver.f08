module spcRiver
                                                                 ! superclass for River subclasses
                                                                 ! defines properties and methods required in any implmentation
                                                                 ! of a River class
                                                                 ! a River class acts as a container for a collection of RiverReach objects which collectively define the
                                                                 ! layout of the flowing waters within each grid cell
                                                                 ! the RiverReach class routes, water, suspended sediments (and ultimately nanoparticles) through the flowing waters within
                                                                 ! the grid cell
                                                                 ! IMPORTED MODULES
                                                                 ! Description
                                                                 ! -----------
  use Globals                                                    ! global declarations
  use netcdf                                                     ! input/output handling
  use mo_netcdf                                                  ! input/output handling
  use ResultModule                                               ! error handling classes, required for
  use ErrorInstanceModule                                        ! generation of trace error messages
  implicit none                                                  ! force declaration of all variables
  type RiverReachElement                                         ! container type for class(RiverReach), the actual type of the RiverReach class
    class(RiverReach), allocatable :: item
  end type
  type RoutingRef                                                ! type definition for a SubRiver routing (inflow or outflow) reference
    type(integer) :: GridRow                                     ! comprises row and column numbers and SubRiver number
    type(integer) :: GridCol                                     ! row, column references, if reference is not to this River object (otherwise=0)
    type(integer) :: SubRiver                                    ! integer reference to SubRiver, if the in/outflow is in this River object (otherwise=0)
  end type
  type OutflowRef
  type SubRiver                                                  ! type definition for SubRiver, which is a contiguous set of reaches
    type(RiverReachElement), allocatable :: Reach(:)             ! array of RiverReachElement objects
    type(RoutingRef), allocatable :: inflow_ref(:)               ! array of references to source subrivers for this subriver (sources can be in a different grid cell)
    type(RoutingRef) :: outflow_ref                              ! reference to outflow subriver (outflow can be to a different grid cell)
    type(integer) :: nReaches                                    ! the number of reaches in the SubRiver
  end type
  type, abstract, public :: River                                ! type declaration for superclass
    character(len=256) :: name                                   ! a name for the object
                                                                 ! PROPERTIES
                                                                 ! Description
                                                                 ! -----------
    integer :: nSubRivers                                        ! the number of SubRiver objects (a maximum of three)
    integer :: GridRow                                           ! the row number of the GridCell object containing this object
    integer :: GridCol                                           ! the column number of the GridCell object containing this object
    integer, private :: allst                                    ! array allocation status
    integer, private :: err                                      ! ?
    type(Result), private :: r                                   ! Result object for returning from functions, for error checking
                                                                 ! CONTAINED OBJECTS
                                                                 ! Description
                                                                 ! -----------
    type(SubRiver), allocatable :: objSubRiver(:)                ! contained SubRiver objects
  contains
                                                                 ! METHODS
                                                                 ! Description
                                                                 ! -----------
    procedure, public, deferred :: create => createRiver         ! create the River object. Exposed name: create
    procedure, public, deferred :: destroy => destroyRiver       ! remove the River object and all contained objects. Exposed name: destroy
                                                                 ! PRIVATE ROUTINES
                                                                 ! Description
    ! IS IT NECESSARY TO DECLARE ALL PRIVATE CLASS-LEVEL METHODS HERE?
    ! IT WOULD BE MORE FLEXIBLE NOT TO HAVE TO DO THIS
                                                                 ! -----------
  end type
  abstract interface
    function createRiver(Me, R, C) result(r)
      class(River) :: Me                                         ! the River instance
      type(integer), intent(in) :: R                             ! the row number of the enclosing GridCell
      type(integer), intent(in) :: C                             ! the column number of the enclosing GridCell
      type(NcDataset) :: NC                                      ! NetCDF dataset
      type(NcVariable) :: var                                    ! NetCDF variable
      type(NcGroup) :: grp                                       ! NetCDF group
      type(Result) :: r                                          ! the result object
      type(integer) :: x                                         ! loop counter
      type(integer) :: y                                         ! loop counter
      type(character(len=*)) :: srs                              ! string to dynamically hold group names for SubRiver data in input file
      type(integer) :: nInflows                                  ! number of inflows for each SubRiver

      ! IS IT NECESSARY TO DECLARE ALL PRIVATE FUNCTION-LEVEL VARIABLES HERE?
      ! IT WOULD BE MORE FLEXIBLE NOT TO HAVE TO DO THIS

    end function
    function destroyRiver(Me) result(r)
      class(River) :: Me                                         ! the River instance
      type(Result) :: r                                          ! the result object
    end function
    function Routing(Me) result(r)                               ! routes inflow(s) through the river system
      class(River) :: Me                                         ! the River instance
      type(Result) :: r                                          ! the result object
      ! description to go here
    end function
  end abstract interface
end module

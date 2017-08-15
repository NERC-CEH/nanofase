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
  use spcRiverReach                                                  ! use containing object type
  ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
  implicit none                                                      ! force declaration of all variables
  type RiverReachElement                                             ! container type for class(RiverReach), the actual type of the RiverReach class
    class(RiverReach), allocatable :: item                           ! a variable of type RiverReachElement can be of any object type inheriting from the
  end type                                                           ! RiverReach superclass
  type RoutingRef                                                    ! an internal user-defined type, defining a reference to a SubRiver sending water to this
    type(integer) :: GridX                                           ! SubRiver, or receiving water from it. Comprises row (X) and column (Y) references to the GridCell
    type(integer) :: GridY                                           ! containing the sending/receiving subriver
    type(integer) :: SubRiver                                        ! as this SubRiver) and the in-cell SubRiver reference number
  end type
  type, abstract, public :: SubRiver                                 ! type declaration for superclass
    character(len=256) :: name                                       ! a name for the object - is this used/needed?
    character(len=100) :: ref                                        ! SubRiver reference of the format SubRiver_x_y_n, where x is GridCell row,
                                                                     ! y is GridCell column and n is SubRiver number in GridCell
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(RoutingRef), allocatable :: inflow_ref(:)                   ! array of references to source subrivers for this subriver (sources can be in a different grid cell)
    type(RoutingRef) :: outflow_ref                                  ! reference to outflow subriver (outflow can be to a different grid cell)
    type(integer) :: nReaches                                        ! the number of reaches in the SubRiver
    type(real(dp)) :: Qout                                           ! discharge from the Subriver // m3
    type(real(dp)), allocatable :: SPMout(:)                         ! array of SPM masses //kg, one per size class, in discharge
    ! need a function somewhere (probably in RiverReach) to convert SPM mass in a size class to particle number
    ! this is needed *** for settling rates *** and for heteroaggregation with nanoparticles

    ! nSPMSC is available globally (C%nSizeClassesSPM) so not needed here
    ! integer :: nSPMSC                                                ! number of SPM size classes
    integer, private :: allst                                        ! array allocation status
    integer, private :: err                                          ! ?
    type(Result), private :: r                                       ! Result object for returning from functions, for error checking
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
    type(RiverReachElement), allocatable :: colReaches(:)            ! array of RiverReachElement objects
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
    function createSubRiver(Me, Gx, Gy, SC, SRr) result(r)          ! create the SubRiver object by reading data in from file
      class(SubRiver) :: Me                                          ! the SubRiver instance
      type(integer), intent(in) :: Gx                                ! the row number of the enclosing GridCell
      type(integer), intent(in) :: Gy                                ! the column number of the enclosing GridCell
      type(integer), intent(in) :: SC                                ! the number of SPM size classes
      type(integer), intent(in) :: SRr                               ! reference SubRiver number
      type(Result) :: r                                              ! the result object
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
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

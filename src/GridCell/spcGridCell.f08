module spcGridCell
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
  use spcSubRiver                                                    ! use containing object type
  ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
  implicit none                                                      ! force declaration of all variables
  type SubRiverElement                                               ! container type for class(SubRiver), the actual type of the SubRiver class
    class(SubRiver), allocatable :: item                             ! a variable of type SubRiver can be of any object type inheriting from the
  end type                                                           ! RiverReach superclass
  type SoilProfileElement                                            ! container type for class(SoilProfile), the actual type of the SoilProfile class
    class(SoilProfile), allocatable :: item                          ! a variable of type SoilProfile can be of any object type inheriting from the
  end type                                                           ! SoilProfile superclass
  type PointSourceElement                                            ! container type for class(PointSource), the actual type of the PointSource class
    class(PointSource), allocatable :: item                          ! a variable of type PointSource can be of any object type inheriting from the
  end type                                                           ! PointSource superclass
  type DiffuseSourceElement                                          ! container type for class(DiffuseSource), the actual type of the DiffuseSource class
    class(DiffuseSource), allocatable :: item                        ! a variable of type DiffuseSource can be of any object type inheriting from the
  end type                                                           ! DiffuseSource superclass
  type, abstract, public :: GridCell                                 ! type declaration for superclass
    character(len=256) :: name                                       ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(integer) :: GridX                                           ! grid cell x reference
    type(integer) :: GridY                                           ! grid cell y reference
    type(SubRiverElement), allocatable :: colSubRivers(:)            ! array of SubRiverElement objects to hold the subrivers
    type(SoilProfileElement), allocatable :: colSoilProfiles(:)      ! array of SoilProfileElement objects to hold the soil profiles
    ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
    type(PointSourceElement), allocatable :: colPointSources(:)      ! array of PointSourceElement objects to hold the point sources
    type(DiffuseSourceElement) :: objDiffuseSource                   ! DiffuseSourceElement object to hold the diffuse source
    type(integer) :: nSubRivers                                      ! Number of contained sub rivers
    type(integer) :: nSoilProfiles                                   ! Number of contained soil profiles
    type(integer) :: nPointSources                                   ! Number of contained point sources
    type(logical) :: DiffS                                           ! Yes=diffuse source present; NO=no diffuse source
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
    procedure, public, deferred :: create => createGridCell          ! create the GridCell object. Exposed name: create
    procedure, public, deferred :: destroy => destroyGridCell        ! remove the GridCell object and all contained objects. Exposed name: destroy
    procedure, public, deferred :: routing => routingGridCell        ! route water and suspended solids through all SubRiver objects. Exposed name: routing
  end type
  abstract interface
    function createGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
    end function
    function destroyGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
    end function
    function routingGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
    end function
  end abstract interface
end module

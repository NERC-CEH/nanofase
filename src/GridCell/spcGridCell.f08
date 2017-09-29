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
  use spcSoilProfile
  use classDiffuseSource
  use classPointSource
  ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
  implicit none                                                      ! force declaration of all variables
  ! MOVING UDTs TO THEIR RESPECTIVE MODULES AS, E.G., SubRiverElement NOT JUST USED HERE
  ! BUT ALSO IN spcGridCell.
  ! type SubRiverElement                                               ! container type for class(SubRiver), the actual type of the SubRiver class
  !   class(SubRiver), allocatable :: item                             ! a variable of type SubRiver can be of any object type inheriting from the
  ! end type                                                           ! RiverReach superclass
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
    character(len=256) :: ref                                        ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(integer) :: gridX                                           ! grid cell x reference
    type(integer) :: gridY                                           ! grid cell y reference
    type(SubRiverElement), allocatable :: colSubRivers(:)            ! array of SubRiverElement objects to hold the subrivers
    type(SoilProfileElement), allocatable :: colSoilProfiles(:)      ! array of SoilProfileElement objects to hold the soil profiles
    ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
    type(PointSourceElement), allocatable :: colPointSources(:)      ! array of PointSourceElement objects to hold the point sources
    type(DiffuseSourceElement) :: objDiffuseSource                   ! DiffuseSourceElement object to hold the diffuse source
    type(integer) :: nSubRivers = 0                                  ! Number of contained sub rivers
    type(integer) :: nSoilProfiles = 0                               ! Number of contained soil profiles
    type(integer) :: nPointSources = 0                               ! Number of contained point sources
    type(logical) :: DiffS                                           ! Yes=diffuse source present; NO=no diffuse source
    real(dp), allocatable :: QrunoffTimeSeries(:)                    ! Runoff from the hydrological model
    real(dp) :: Qrunoff                                              ! Runoff from the hydrological model
    logical :: isEmpty = .false.                                     ! Is there anything going on in the GridCell or should we skip over when simulating?
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure(createGridCell), deferred :: create                    ! create the GridCell object. Exposed name: create
    procedure(destroyGridCell), deferred :: destroy                  ! remove the GridCell object and all contained objects. Exposed name: destroy
    procedure(routingGridCell), deferred :: routing                  ! route water and suspended solids through all SubRiver objects. Exposed name: routing
    procedure(finaliseRoutingGridCell), deferred :: finaliseRouting
  end type

  type GridCellElement                                               ! Container type for polymorphic GridCells
    class(GridCell), allocatable :: item
  end type

  abstract interface
    function createGridCell(Me, x, y, isEmpty) result(r)
      import GridCell, Result
      class(GridCell) :: Me                                          !! The GridCell instance.
      integer :: x, y                                                !! The (x,y) position of the GridCell.
      logical, optional :: isEmpty                                   !! Is anything to be simulated for this GridCell?
      type(Result) :: r
    end function
    function destroyGridCell(Me) result(r)
      import GridCell, Result
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
    end function
    function routingGridCell(Me, t) result(r)
      import GridCell, Result
      class(GridCell) :: Me                                          ! The GridCell instance.
      integer :: t                                                   ! What time step are we on?
      type(Result) :: r
    end function
    function finaliseRoutingGridCell(me) result(r)
      import GridCell, Result
      class(GridCell) :: me
      type(Result) :: r
    end function
  end interface
end module

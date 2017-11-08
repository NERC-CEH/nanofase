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
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  use spcSubRiver                                                    ! use containing object type
  use spcSoilProfile
  use classDiffuseSource
  use classPointSource
  implicit none                                                      ! force declaration of all variables

                                                                    ! SoilProfile superclass
  type PointSourceElement                                            ! container type for class(PointSource), the actual type of the PointSource class
    class(PointSource), allocatable :: item                          ! a variable of type PointSource can be of any object type inheriting from the
  end type                                                           ! PointSource superclass
  type DiffuseSourceElement                                          ! container type for class(DiffuseSource), the actual type of the DiffuseSource class
    class(DiffuseSource), allocatable :: item                        ! a variable of type DiffuseSource can be of any object type inheriting from the
  end type                                                           ! DiffuseSource superclass

  type, abstract, public :: GridCell                                 ! type declaration for superclass
    character(len=256) :: ref                                        ! a name for the object
    type(NcGroup) :: ncGroup                                         ! The NetCDF group for this dataset
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(integer) :: gridX                                           ! grid cell x reference
    type(integer) :: gridY                                           ! grid cell y reference
    real(dp) :: area                                                 ! Area of the GridCell
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
    real(dp) :: slope                                                ! The slope of the GridCell
    real(dp) :: n_river                                              ! Manning's roughness coefficient for the river
    real(dp), allocatable :: erodedSediment(:)                       ! Sediment yield eroded on this timestep [kg/timestep], simulated by SoilProfile(s)
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
    procedure(updateGridCell), deferred :: update                    ! route water and suspended solids through all SubRiver objects. Exposed name: routing
    procedure(finaliseUpdateGridCell), deferred :: finaliseUpdate  
  end type

  type GridCellElement                                               ! Container type for polymorphic GridCells
    class(GridCell), allocatable :: item
  end type

  abstract interface
    function createGridCell(me, x, y, isEmpty) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          !! The GridCell instance.
      integer :: x, y                                                !! The (x,y) position of the GridCell.
      logical, optional :: isEmpty                                   !! Is anything to be simulated for this GridCell?
      type(Result) :: r
    end function
    function destroyGridCell(me) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          ! The GridCell instance.
      type(Result) :: r
    end function
    function updateGridCell(me, t) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          ! The GridCell instance.
      integer :: t                                                   ! What time step are we on?
      type(Result) :: r
    end function
    function finaliseUpdateGridCell(me) result(r)
      import GridCell, Result
      class(GridCell) :: me
      type(Result) :: r
    end function
  end interface
end module

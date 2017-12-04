!> Module container for abstract base class `GridCell`
module spcGridCell
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

  !> Abstract base class `GridCell`. Extended classes are responsible
  !! for running creation and simulation procedures for `SoilProfile`
  !! and `RiverReach`es.
  type, abstract, public :: GridCell
    character(len=256) :: ref                                        !! A name for the object
    type(NcGroup) :: ncGroup                                         !! The NetCDF group for this dataset
    type(integer) :: gridX                                           !! `GridCell` x reference
    type(integer) :: gridY                                           !! `GridCell` y reference
    real(dp) :: area                                                 !! Area of the `GridCell`
    type(SubRiverElement), allocatable :: colSubRivers(:)            !! Array of `SubRiverElement` objects to hold the subrivers
    type(SoilProfileElement), allocatable :: colSoilProfiles(:)      !! Array of `SoilProfileElement` objects to hold the soil profiles
    ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
    type(PointSourceElement), allocatable :: colPointSources(:)      !! Array of `PointSourceElement` objects to hold the point sources
    type(DiffuseSourceElement) :: objDiffuseSource                   !! `DiffuseSourceElement` object to hold the diffuse source
    type(integer) :: nSubRivers = 0                                  !! Number of contained `SubRiver`s
    type(integer) :: nSoilProfiles = 0                               !! Number of contained `SoilProfile`s
    type(integer) :: nPointSources = 0                               !! Number of contained `PointSource`s
    type(logical) :: DiffS                                           !! Yes=diffuse source present; NO=no diffuse source
    real(dp), allocatable :: QrunoffTimeSeries(:)                    !! Runoff from the hydrological model
    real(dp), allocatable :: Q_evap_timeSeries(:)                    !! Evaporation time series [m/s]
    real(dp), allocatable :: Q_precip_timeSeries(:)                  !! Precipitation time series [m/s]
    real(dp) :: Qrunoff                                              !! Runoff from the hydrological model for this time step
    real(dp) :: slope                                                !! The slope of the `GridCell`
    real(dp) :: n_river                                              !! Manning's roughness coefficient for the river
    real(dp), allocatable :: erodedSediment(:)                       !! Sediment yield eroded on this timestep [kg/timestep], simulated by `SoilProfile`(s)
    logical :: isEmpty = .false.                                     ! Is there anything going on in the `GridCell` or should we skip over when simulating?
  contains
    procedure(createGridCell), deferred :: create                    ! create the GridCell object. Exposed name: create
    procedure(destroyGridCell), deferred :: destroy                  ! remove the GridCell object and all contained objects. Exposed name: destroy
    procedure(updateGridCell), deferred :: update                    ! route water and suspended solids through all SubRiver objects. Exposed name: routing
    procedure(finaliseUpdateGridCell), deferred :: finaliseUpdate  
  end type
  !> Container type for polymorphic `GridCell`s
  type GridCellElement                                               
    class(GridCell), allocatable :: item                            !! Polymorphic `GridCell` object
  end type

  abstract interface
    !> Create this `GridCell`
    function createGridCell(me, x, y, isEmpty) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          !! The `GridCell` instance
      integer :: x, y                                                !! The (x,y) position of the `GridCell`
      logical, optional :: isEmpty                                   !! Is anything to be simulated for this `GridCell`?
      type(Result) :: r                                              !! The `Result` object to return any errors in
    end function
    !> Destroy this `GridCell`
    function destroyGridCell(me) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          !! The `GridCell` instance
      type(Result) :: r                                              !! The `Result` object to return any errors in
    end function
    !> Run the `GridCell`'s simulation for this time step
    function updateGridCell(me, t) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          !! The `GridCell` instance
      integer :: t                                                   !! The current time step
      type(Result) :: r                                              !! The `Result` object to return any errors in
    end function
    !> Finalise the `GridCell`'s state variables for this time step
    function finaliseUpdateGridCell(me) result(r)
      import GridCell, Result
      class(GridCell) :: me                                          !! The `GridCell` instance
      type(Result) :: r                                              !! The `Result` object to return any errors in
    end function
  end interface
end module

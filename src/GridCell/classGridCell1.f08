module classGridCell1
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
  type, public, extends(GridCell) :: GridCell1                       ! type declaration for subclass
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public, deferred :: create => createGridCell1         ! create the GridCell object. Exposed name: create
    procedure, public, deferred :: destroy => destroyGridCell1       ! remove the GridCell object and all contained objects. Exposed name: destroy
    procedure, public, deferred :: routing => routingGridCell1       ! route water and suspended solids through all SubRiver objects. Exposed name: routing
  end type
  abstract interface
    function createGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
      ! DATA REQUIREMENTS
      ! number of grid cells
      ! number of soil profiles
      ! number of subrivers
      ! x and y references
      ! calls to create functions for:
      ! colSoilProfiles
      ! colSubRivers
      ! colPointSources
      ! objDiffuseSource, if required (Me%DiffS=.True.)
    end function
    function destroyGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r
      type(integer) :: x                                             ! loop counter
      do x = 1, Me%nSubRivers
        Me%colSubRivers(x)%destroy                                   ! remove all SubRiver objects and any contained objects
      end do
      do x = 1, Me%nSoilProfiles
        Me%colSoilProfiles(x)%destroy                                ! remove all SoilProfile objects and any contained objects
      end do
      do x = 1, Me%nPointSources
        Me%colPointSources(x)%destroy                                ! remove all PointSource objects and any contained objects
      end do
      Me%objDiffuseSource%destroy                                    ! remove the DiffuseSource object and any contained objects
    end function
    function routingGridCell(Me) result(r)
      class(GridCell) :: Me                                          ! The GridCell instance.
      type(Result) :: r                                              ! Result object
      type(integer) :: x                                             ! Loop counter
      do x = 1, Me%nSubRivers
        call Me%colSubRivers(x)%Routing                              ! call the routing method for each SubRiver in turn
                                                                     ! outflow discharge and SPM fluxes for each SubRiver are
                                                                     ! stored within that SubRiver, ready to be picked up by
                                                                     ! the downstream SubRiver
      end do
    end function
  end abstract interface
end module

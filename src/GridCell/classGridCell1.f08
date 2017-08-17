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
  use spcGridCell
  implicit none                                                      ! force declaration of all variables
  type, public, extends(GridCell) :: GridCell1                       ! type declaration for subclass
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure :: create => createGridCell1         ! create the GridCell object. Exposed name: create
    procedure :: destroy => destroyGridCell1       ! remove the GridCell object and all contained objects. Exposed name: destroy
    procedure :: routing => routingGridCell1       ! route water and suspended solids through all SubRiver objects. Exposed name: routing
  end type

  contains
    function createGridCell1(Me) result(r)
      class(GridCell1) :: Me                                          ! The GridCell instance.
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
    function destroyGridCell1(Me) result(r)
      class(GridCell1) :: Me                                          ! The GridCell instance.
      type(Result) :: r
      type(integer) :: x                                             ! loop counter
      do x = 1, Me%nSubRivers
        r = Me%colSubRivers(x)%item%destroy()                       ! remove all SubRiver objects and any contained objects
      end do
      do x = 1, Me%nSoilProfiles
        r = Me%colSoilProfiles(x)%item%destroy()                    ! remove all SoilProfile objects and any contained objects
      end do
      do x = 1, Me%nPointSources
        r = Me%colPointSources(x)%item%destroy()                    ! remove all PointSource objects and any contained objects
      end do
      r = Me%objDiffuseSource%item%destroy()                        ! remove the DiffuseSource object and any contained objects
    end function
    function routingGridCell1(Me) result(r)
      class(GridCell1) :: Me                                          ! The GridCell instance.
      type(Result) :: r                                              ! Result object
      type(integer) :: x                                             ! Loop counter
      do x = 1, Me%nSubRivers
        r = Me%colSubRivers(x)%item%routing()                        ! call the routing method for each SubRiver in turn
                                                                     ! outflow discharge and SPM fluxes for each SubRiver are
                                                                     ! stored within that SubRiver, ready to be picked up by
                                                                     ! the downstream SubRiver
      end do
    end function
end module

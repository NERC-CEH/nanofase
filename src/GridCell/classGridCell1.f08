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
  use UtilModule                                                     ! useful functions, e.g. str()
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  use spcGridCell
  use classSubRiver1
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
    procedure :: finaliseRouting => finaliseRoutingGridCell1
  end type

  !> Interface so that we can create new GridCells by `gc = GridCell1()`
  interface GridCell1
      module procedure newGridCell1
  end interface
! *** SL:
! neat - didn't realise that this was possible. Will it work with
! polymorphism, i.e. can we use it to create GridCell objects of any
! type that implements spcGridCell? At the moment, to me it looks as though that
! isn't possible...
  contains
    !> Return a newly-created GridCell1 object. This is bound to GridCell1 interface
    !! and is not type-bound.
  ! *** SL:
  ! I don't quite understand this - if the returned object is of type GridCell1
  ! as opposed to GridCell, isn't it type-bound?
  ! ***
    !! TODO: Do something with result object
    function newGridCell1(x, y, isEmpty) result(me)
      type(GridCell1) :: me                         !! The new GridCell to return
      integer :: x, y                               !! Location of the GridCell
      logical, optional :: isEmpty                  !! Is anything to be simulated in this GridCell?
      type(Result) :: r                             !! Result object
      ! Create the new GridCell, specifying isEmpty if it's present (it default to false if not)
      if (present(isEmpty)) r = me%create(x, y, isEmpty)
      if (.not. present(isEmpty)) r = me%create(x, y)
    end function

    function createGridCell1(Me, x, y, isEmpty) result(r)
      class(GridCell1)      :: Me                   !! The GridCell instance.
      type(Result)          :: r                    !! The Result object to return.
      integer               :: x, y                 !! Location of the GridCell
      logical, optional     :: isEmpty              !! Is anything to be simulated in this GridCell?
      type(NcDataset)       :: nc                   !! NetCDF dataset
      type(NcVariable)      :: var                  !! NetCDF variable
      type(NcGroup)         :: grp                  !! NetCDF group
      integer               :: s                    !! Iterator for SubRivers
      integer               :: t                    !! Iterator for time series
      character(len=100)    :: subRiverPrefix       !! Prefix for SubRivers ref, e.g. SubRiver_1_1
      real(dp)              :: subRiverLength       !! Length of the SubRivers
      real(dp), allocatable :: subRiverRunoffTimeSeries(:) !! Runoff to each SubRiver

      ! DATA REQUIREMENTS
      ! number of grid cells
      ! number of soil profiles - update 2017-08-17: only one soil profile now
      ! number of subrivers
      ! x and y references
      ! calls to create functions for:
      ! colSoilProfiles
      ! colSubRivers
      ! colPointSources
      ! objDiffuseSource, if required (Me%DiffS=.True.)

      ! Set the GridCell's position, whether it's empty and its name
      me%gridX = x
      me%gridY = y
      if (present(isEmpty)) me%isEmpty = isEmpty    ! isEmpty defaults to false if not present
      me%ref = "GridCell_" // trim(str(me%gridX)) // &
                    "_" // trim(str(me%gridY))      ! str() function is from UtilModule

      ! Only carry on if there's stuff to be simulated for this GridCell
      if (me%isEmpty .eqv. .false.) then
        me%Qrunoff = 0                                            ! Default to no runoff
        ! Add SubRivers to the GridCell (if any are present in the data file)
        ! TODO: We only really want to be opening the data file once (it might
        ! already be open in parent classes, i.e. Environment), so
        ! consider opening in Globals
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(me%ref)                              ! Get this GridCell's group
        var = grp%getVariable("nSubRivers")                     ! Get the number of SubRivers for looping over
        call var%getData(me%nSubRivers)
        allocate(me%colSubRivers(me%nSubRivers))                ! Allocate the colSubRivers array to the number of SubRivers in the GridCell

        ! Get the time-dependent runoff data from the file and put in array ready for us
        allocate(me%QrunoffTimeSeries(C%nTimeSteps))
        if (grp%hasVariable("runoff")) then
          var = grp%getVariable("runoff")
          call var%getData(me%QrunoffTimeSeries)                 
          me%QrunoffTimeSeries = me%QrunoffTimeSeries*C%timeStep ! runoff from data file should be in m3/s, convert to m3/timestep
        else
          me%QrunoffTimeSeries = 0
        end if
        allocate(subRiverRunoffTimeSeries(size(me%QrunoffTimeSeries)))
        
        subRiverPrefix = "SubRiver_" // trim(str(me%gridX)) // &
                      "_" // trim(str(me%gridY)) // "_"
        ! Set SubRiver size to half of the grid cell size if there's more than one SubRiver,
        ! otherwise the full size of the grid cell. TODO: Constrain number of SubRivers somewhere
        ! so this makes sense.
        if (me%nSubRivers > 1) then
          subRiverLength = C%gridCellSize / 2.0_dp
        else
          subRiverLength = C%gridCellSize
        end if
        ! Loop through SubRivers, incrementing s (from SubRiver_x_y_s), until none found
        do s = 1, me%nSubRivers
          ! Split the runoff between SubRivers
          do t = 1, size(me%QrunoffTimeSeries)
            if (me%QrunoffTimeSeries(t) > 0) then
              subRiverRunoffTimeSeries(t) = me%QrunoffTimeSeries(t)/me%nSubRivers
            else
              subRiverRunoffTimeSeries(t) = 0
            end if
          end do
          ! if (me%Qrunoff > 0) then
          !   subRiverRunoff = me%Qrunoff/me%nSubRivers
          ! else
          !   subRiverRunoff = 0
          ! end if
          ! Check that group actually exists
          ! TODO: Maybe perform this check somewhere else - or at least perform some error checking here
          if (grp%hasGroup(trim(subRiverPrefix) // trim(str(s)))) then
            ! Allocate a new SubRiver to the colSubRivers array
            allocate(me%colSubRivers(s)%item, source=SubRiver1( &
              me%gridX, &
              me%gridY, &
              s, &
              subRiverLength, &
              subRiverRunoffTimeSeries &
              ) &
            )
          end if
        end do
      end if
    end function

    function destroyGridCell1(Me) result(r)
      class(GridCell1) :: Me                                          ! The GridCell instance.
      type(Result) :: r
      type(integer) :: x                                             ! loop counter
      do x = 1, me%nSubRivers
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
    function routingGridCell1(Me, t) result(r)
      class(GridCell1) :: Me                                         ! The GridCell instance.
      integer :: t                                                   ! What time step are we on?
      type(Result) :: r                                              ! Result object
      type(integer) :: s                                             ! Loop counter
      ! Check that the GridCell is not empty before simulating anything
      ! TODO: Think about where these isEmpty tests are done - here or in the Environment?
      ! There's also redundency here: me%nSubRivers is initialised to 0 and is only
      ! set to something else if the GridCell isn't empty.
      if (.not. me%isEmpty) then
        do s = 1, me%nSubRivers
          r = Me%colSubRivers(s)%item%routing(t)                        ! call the routing method for each SubRiver in turn
                                                                       ! outflow discharge and SPM fluxes for each SubRiver are
                                                                       ! stored within that SubRiver, ready to be picked up by
                                                                       ! the downstream SubRiver
        end do
      end if
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! routing procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another SubRiver whilst the SubRivers
    !! are looped through.
    function finaliseRoutingGridCell1(me) result(r)
      class(GridCell1) :: me                                      !! This SubRiver1 instace
      integer :: s                                                !! Iterator of SubRivers
      type(Result) :: r                                           !! The Result object
      if (.not. me%isEmpty) then
        do s = 1, me%nSubRivers
          r = me%colSubRivers(s)%item%finaliseRouting()
        end do
      end if
    end function
end module

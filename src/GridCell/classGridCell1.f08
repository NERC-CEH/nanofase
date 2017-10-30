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
        procedure :: create => createGridCell1                      ! create the GridCell object. Exposed name: create
        procedure :: destroy => destroyGridCell1                    ! remove the GridCell object and all contained objects. Exposed name: destroy
        procedure :: update => updateGridCell1                      ! route water and suspended solids through all SubRiver objects. Exposed name: update
        procedure :: finaliseUpdate => finaliseUpdateGridCell1
        procedure :: erodeSoil => erodeSoilGridCell1
    end type

    !> Interface so that we can create new GridCells by `gc = GridCell1()`
    interface GridCell1
        module procedure newGridCell1
    end interface

  contains
    !> Return a newly-created GridCell1 object.
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

    !> Create a GridCell with coordinates x and y.
    function createGridCell1(me, x, y, isEmpty) result(r)
        class(GridCell1)      :: me                   !! The GridCell instance.
        type(Result)          :: r                    !! The Result object to return.
        integer               :: x, y                 !! Location of the GridCell
        logical, optional     :: isEmpty              !! Is anything to be simulated in this GridCell?
        type(NcDataset)       :: nc                   !! NetCDF dataset
        type(NcVariable)      :: var                  !! NetCDF variable
        type(NcGroup)         :: grp                  !! NetCDF group
        integer               :: s                    !! Iterator for SubRivers
        integer               :: t                    !! Iterator for time series
        integer               :: rock_usle            !! % rock in top of soil profile, to calculate CFRG_usle param
        character(len=100)    :: subRiverPrefix       !! Prefix for SubRivers ref, e.g. SubRiver_1_1
        real(dp)              :: subRiverLength       !! Length of the SubRivers
        real(dp), allocatable :: subRiverRunoffTimeSeries(:) !! Runoff to each SubRiver
        type(Result)          :: srR                  !! Result object for individual SubRivers
        type(SubRiver1), allocatable :: sr1           !! SubRiver1 object for storing created SubRiver1s in

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

        ! Set the GridCell's position, area, whether it's empty and its name
        me%gridX = x
        me%gridY = y
        me%area = C%gridCellSize**2                   ! TODO: This will be changed to take into account lat-lon
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

            ! Get the time-dependent runoff data from the file and put in array ready for use
            ! TODO: Runoff data currently m3/s, but maybe this should be m/s instead?
            allocate(me%QrunoffTimeSeries(C%nTimeSteps))
            if (grp%hasVariable("runoff")) then
                var = grp%getVariable("runoff")
                call var%getData(me%QrunoffTimeSeries)                 
                me%QrunoffTimeSeries = me%QrunoffTimeSeries*C%timeStep ! Convert to m3/timestep
            else
                me%QrunoffTimeSeries = 0
            end if
            allocate(subRiverRunoffTimeSeries(size(me%QrunoffTimeSeries)))

            ! Get USLE params
            var = grp%getVariable('C_usle')          ! Cover and land management factor [-]
            call var%getData(me%C_usle)
            var = grp%getVariable('K_usle')          ! Soil erodibility factor [t ha h ha-1 MJ-1 mm-1]
            call var%getData(me%K_usle)
            var = grp%getVariable('P_usle')          ! Support practice factor [-]
            call var%getData(me%P_usle)
            var = grp%getVariable('LS_usle')         ! Topographic factor [-]
            call var%getData(me%K_usle)
            var = grp%getVariable('rock_usle')       ! % rock in top of soil profile [-]
            call var%getData(rock_usle)
            me%CFRG_usle = exp(-0.052*rock_usle)     ! Coarse fragment factor [-]
            var = grp%getVariable('erodedSedimentRUSLE')    ! RUSLE2015's calculated eroded sediment
            call var%getData(me%erodedSedimentRUSLE)
            
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
                ! Check that group actually exists
                ! TODO: Maybe perform this check somewhere else - or at least perform some error checking here
                if (grp%hasGroup(trim(subRiverPrefix) // trim(str(s)))) then
                    allocate(sr1)                                   ! Create the new SubRiver
                    srR = sr1%create(me%gridX, me%gridY, s, subRiverLength, subRiverRunoffTimeSeries)
                    call r%addErrors(errors = .errors. srR)         ! Add any errors to final Result object
                    call move_alloc(sr1, me%colSubRivers(s)%item)   ! Allocate a new SubRiver to the colSubRivers array
                end if
            end do
        end if

        call r%addToTrace("Creating " // trim(me%ref))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function

    function destroyGridCell1(Me) result(r)
        class(GridCell1) :: Me                              !! The GridCell instance.
        type(Result) :: r                                   !! The Result object
        type(integer) :: x                                  !! loop counter
        do x = 1, me%nSubRivers
            r = Me%colSubRivers(x)%item%destroy()           ! remove all SubRiver objects and any contained objects
        end do
        do x = 1, Me%nSoilProfiles
            r = Me%colSoilProfiles(x)%item%destroy()        ! remove all SoilProfile objects and any contained objects
        end do
        do x = 1, Me%nPointSources
            r = Me%colPointSources(x)%item%destroy()        ! remove all PointSource objects and any contained objects
        end do
        r = Me%objDiffuseSource%item%destroy()              ! remove the DiffuseSource object and any contained objects
    end function

    !> Perform the simulations required for an individual timestep t.
    function updateGridCell1(Me, t) result(r)
        class(GridCell1) :: Me                                         ! The GridCell instance
        integer :: t                                                   ! The timestep we're on
        type(Result) :: srR                                            ! Result object for SubRivers
        type(Result) :: r                                              ! Result object
        type(integer) :: s                                             ! Loop counter
        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            ! First, calculate the soil erosion for this GridCell
            r = me%erodeSoil(t)
            ! Loop through each SubRiver and run its update procedure
            do s = 1, me%nSubRivers
                srR = me%colSubRivers(s)%item%update(t)
                call r%addErrors(errors = .errors. srR)
            end do
        end if
        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another SubRiver whilst the SubRivers
    !! are looped through.
    function finaliseUpdateGridCell1(me) result(r)
        class(GridCell1) :: me                                      !! This SubRiver1 instace
        integer :: s                                                !! Iterator of SubRivers
        type(Result) :: r                                           !! The Result object
        if (.not. me%isEmpty) then
            do s = 1, me%nSubRivers
                r = me%colSubRivers(s)%item%finaliseUpdate()
            end do
        end if
    end function

    !> Obtains from data or calculates soil erosion for this timstep t.
    !! Updates this GridCell's state variable erodedSediment accordingly.
    function erodeSoilGridCell1(me, t) result(r)
        class(GridCell1)    :: me               !! This GridCell instance
        integer             :: t                !! The timestep we're on
        type(NcDataset)     :: nc               !! NetCDF dataset
        type(NcVariable)    :: var              !! NetCDF variable
        type(NcGroup)       :: grp              !! NetCDF group
        real(dp)            :: Q_surf           !! Surface runoff \( Q_{\text{surf}} \)
        real(dp)            :: alpha_half       !! Fraction of rainfall in half-hour of highest intensity \( \alpha_{0.5} \)
        real(dp)            :: LS               !! MUSLE's topographic factor [-]
        real(dp)            :: q_peak           !! Peak rainfall \( q_{\text{peak}} \)
        type(Result)        :: r                !! The Result object
        ! Check what the config file says.
        ! TODO: Change these to be obtained from config.json file and have
        ! more logical names, e.g. CONFIG%soilErosion.
        ! TODO: Specify defaults when/where the config file is loaded, we don't
        ! want to be doing this on every time step!
        if (C%soilErosion /= 'rusle2015' .and. C%soilErosion /= 'musle') then
            ! Throw a warning if config isn't "data" or "musle"
            call r%addError(ErrorInstance( &
                code = 203, &
                message = "Unknown soil erosion config option (" // &
                            trim(C%soilErosion) // ") specified in config file. " // &
                            "Defaulting to MUSLE.", &
                isCritical = .false. &
            ))
            C%soilErosion = 'musle'             ! Default to MUSLE
        end if
        ! TODO: Move this to Database object
        nc = NcDataset(C%inputFile, "r")        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(me%ref)              ! Get this GridCell's group
        ! Are we using MUSLE or data from input?
        if (C%soilErosion == 'musle') then
            ! Drive erosion with 10% of quickflow and convert to mm/day
            Q_surf = ((me%QrunoffTimeSeries(t)*86400e2)/me%area)
            ! var = grp%getVariable('alpha_half')     ! Fraction of rainfall falling in half-hour of highest intensity
            ! call var%getData(alpha_half)
            ! var = grp%getVariable('LS')             ! Topographic factor
            ! call var%getData(LS)
        else
            ! Else, set this timestep's eroded sediment as that from RUSLE2015
            me%erodedSediment = me%erodedSedimentRUSLE
        end if
    end function

end module
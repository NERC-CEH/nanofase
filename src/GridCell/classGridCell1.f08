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
        real(dp)              :: usle_C_min(C%nTimeSteps)   !! Minimum cover factor for USLE
        real(dp)              :: usle_C_av(C%nTimeSteps)    !! Average cover factor for USLE
        integer               :: usle_rock            !! % rock in top of soil profile, to calculate usle_CFRG param
        character(len=100)    :: subRiverPrefix       !! Prefix for SubRivers ref, e.g. SubRiver_1_1
        real(dp)              :: subRiverLength       !! Length of the SubRivers
        real(dp), allocatable :: subRiverRunoffTimeSeries(:) !! Runoff to each SubRiver
        type(Result)          :: srR                  !! Result object for individual SubRivers
        type(SubRiver1), allocatable :: sr1           !! SubRiver1 object for storing created SubRiver1s in

        ! Allocate the object properties that need to be
        allocate(me%usle_C(C%nTimeSteps))
        allocate(me%usle_alpha_half(C%nTimeSteps))
        allocate(me%rusle2015_erodedSediment(C%nTimeSteps))

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

            !-- SOIL EROSION -------------------------------------------------------------!
            ! C     Cover and land management factor, defined as the ratio of soil loss
            !       from land cropped under specified conditions to the corresponding loss
            !       from clean-tilled, continuous flow. Should be time-dependent (as crop
            !       cover changes).
            if (grp%hasVariable('usle_C')) then             ! First, check if time series of C-factors is available
                var = grp%getVariable('usle_C')
                call var%getData(me%usle_C)
            else                                            ! Else, we can estimate C
                if (grp%hasVariable('usle_C_min')) then     ! Use minimum C to estimate C
                    var = grp%getVariable('usle_C_min')
                    call var%getData(usle_C_min)
                else if (grp%hasVariable('usle_C_av')) then ! Average annual C can be used to estimate minimum C
                    var = grp%getVariable('usle_C_av')
                    call var%getData(usle_C_av)
                    usle_C_min = 1.463*ln(usle_C_av) + 0.1034
                else                                        ! If neither exist, default C_min to 1 (fallow soil)
                    call r%addError(ErrorInstance( &
                        code = 201, &
                        message = "Values for usle_C_min or usle_C_av not found in input file. " // &
                                    "usle_C_min defaulting to 1 (fallow soil).", &
                        isCritical = .false. &
                    ))
                    usle_C_min = 1
                end if
                if (grp%hasVariable('usle_rsd')) then       ! Residue on surface also needed to esimate C
                    var = grp%getVariable('usle_rsd')
                    call var%getData(usle_rsd)
                else                                        ! Default to zero (no residue = no crops)
                    call r%addError(ErrorInstance( &
                        code = 201, &
                        message = "Value for usle_rsd not found in input file. " // &
                                    "Defaulting to 0 (no crops).", &
                        isCritical = .false. &
                    ))
                    usle_rsd = 0
                end if 
                ! Defaults to 0.8, based on C_min = 1 and rsd = 0.
                me%usle_C = exp((ln(0.8) - ln(usle_C_min))*exp(-0.00115*usle_rsd) + ln(usle_C_min))
            end if
            
            ! K     Soil erodibility factor, which depends on soil structure and is treated as
            !       time-independent.
            if (grp%hasVariable('usle_K')) then
                var = grp%getVariable('usle_K')
                call var%getData(me%usle_K)
            else
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_K not found in input file." &
                ))
                return              ! We can't carry on without usle_K, so return early
            end if

            ! P     Support practice factor, the ratio of soil loss with a specific support
            !       practice to the corresponding loss with up-and-down slope culture. Support
            !       practices: Contour tillage, strip-cropping, terrace systems.
            if (grp%hasVariable('usle_P')) then
                var = grp%getVariable('usle_P')
                call var%getData(me%usle_P)
            else
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_P not found in input file." // &
                                "Defaulting to 1 (no support practice).", &
                    isCritical = .false. &
                ))
                me%usle_P = 1
            end if

            ! LS    Topographic factor, a function of the terrain's topography.
            if (grp%hasVariable('usle_LS')) then
                var = grp%getVariable('usle_LS')
                call var%getData(me%usle_LS)
            else
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_LS not found in input file." &
                ))
                return              ! We can't carry on without usle_LS, so return early
            end if

            ! CFRG  Coase fragment factor, CFRG = exp(0.035 * % rock in first soil layer).
            if (grp%hasVariable('usle_rock')) then
                var = grp%getVariable('usle_rock')          ! % rock in top of soil profile [-]
                call var%getData(usle_rock)
                me%usle_CFRG = exp(-0.052*rock_usle)        ! Coarse fragment factor [-]
            else
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_rock not found in input file." // &
                                "Defaulting to 0 (no rock in top soil layer).", &
                    isCritical = .false. &
                ))
                me%usle_CFRG = 1                            ! Default to 1 (rock_usle = 0)
            end if

            ! Params affecting q_peak
            ! alpha_half        Fraction of rainfall happening in maximum half hour
            if (grp%hasVariable('usle_alpha_half')) then
                var = grp%getVariable('usle_alpha_half')
                call var%getData(me%usle_alpha_half)
            else
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_alpha_half not found in input file." // &
                                "Defaulting to 0.33.", &
                    isCritical = .false. &
                ))
                me%usle_alpha_half = 0.33                   ! Default to 0.33
            end if

            ! Other stuff:
            ! area_hru
            ! L_sb, n_sb, slp_sb, area_sb
            ! slp_ch, L_ch


            ! Check if RUSLE2015's eroded sediment data has been provided, for comparison's sake
            if (grp%hasVariable('rusle2015_erodedSediment')) then
                var = grp%getVariable('rusle2015_erodedSediment')
                call var%getData(me%rusle2015_erodedSediment)
            end if
            
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
        
        Q_surf = ((me%QrunoffTimeSeries(t)*86400e2)/me%area)

    end function

end module
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
    use mo_netcdf                                                      ! input/output handling
    use ResultModule                                                   ! error handling classes, required for
    use ErrorInstanceModule                                            ! generation of trace error messages
    use spcGridCell
    use classSoilProfile1
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
        procedure :: finaliseUpdate => finaliseUpdateGridCell1      ! Set variables for this timestep that couldn't be updated whilst simulation running
        procedure :: parseInputData => parseInputDataGridCell1      ! Parse the input data and store in object properties
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
        type(SoilProfile1)    :: soilProfile          !! The soil profile contained in this GridCell
        integer               :: s                    !! Iterator for SubRivers
        integer               :: t                    !! Iterator for time series
        character(len=100)    :: subRiverPrefix       !! Prefix for SubRivers ref, e.g. SubRiver_1_1
        real(dp)              :: subRiverLength       !! Length of the SubRivers
        real(dp), allocatable :: subRiverRunoffTimeSeries(:) !! Runoff to each SubRiver
        type(Result)          :: srR                  !! Result object for individual SubRivers
        type(SubRiver1), allocatable :: sr1           !! SubRiver1 object for storing created SubRiver1s in

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%QrunoffTimeSeries(C%nTimeSteps))
        allocate(subRiverRunoffTimeSeries(C%nTimeSteps))
        allocate(me%colSoilProfiles(1))
        me%Qrunoff = 0                                  ! Default to no runoff

        ! Set the GridCell's position, area, whether it's empty and its name
        me%gridX = x
        me%gridY = y
        me%area = C%gridCellSize**2                   ! TODO: This will be changed to take into account lat-lon
        if (present(isEmpty)) me%isEmpty = isEmpty    ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module

        ! Only carry on if there's stuff to be simulated for this GridCell
        if (me%isEmpty .eqv. .false.) then
            r = me%parseInputData()                         ! Parse and store input data in this object

            ! Create a soil profile and add to this GridCell
            r = soilProfile%create(me%gridX, me%gridY, 1, me%slope, me%n_river, me%area)
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)

            ! Add SubRivers to the GridCell (if any are present in the data file)
            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. r%hasCriticalError()) then
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
                    ! TODO: Maybe perform this check somewhere else
                    if (me%ncGroup%hasGroup(trim(subRiverPrefix) // trim(str(s)))) then
                        allocate(sr1)                                   ! Create the new SubRiver
                        srR = sr1%create(me%gridX, me%gridY, s, subRiverLength, subRiverRunoffTimeSeries)
                        call r%addErrors(errors = .errors. srR)         ! Add any errors to final Result object
                        call move_alloc(sr1, me%colSubRivers(s)%item)   ! Allocate a new SubRiver to the colSubRivers array
                    else
                        call r%addError(ErrorInstance( &
                            code = 501, &
                            message = "No input data provided for " // trim(subRiverPrefix) // trim(str(s)) // &
                                        " - check nSubRivers is set correctly." &
                        ))
                    end if
                end do
            end if
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
            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            r = me%colSoilProfiles(1)%item%update(t, me%QrunoffTimeSeries(t))
            me%erodedSediment = me%colSoilProfiles(1)%item%erodedSediment
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

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell1(me) result(r)
        class(GridCell1)        :: me                   !! This GridCell1 object
        type(NcDataset)         :: nc                   !! NetCDF dataset
        type(NcVariable)        :: var                  !! NetCDF variable
        type(NcGroup)           :: grp                  !! NetCDF group
        type(Result)            :: r                    !! The result object

        ! Open the dataset
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        me%ncGroup = grp%getGroup(me%ref)                              ! Get this GridCell's group
        ! Get the number of SubRivers for looping over
        if (me%ncGroup%hasVariable("nSubRivers")) then
            var = me%ncGroup%getVariable("nSubRivers")                     
            call var%getData(me%nSubRivers)
        else
            me%nSubRivers = 0   ! If nSubRivers isn't present, default to having no SubRivers
        end if
        allocate(me%colSubRivers(me%nSubRivers))        ! Allocate the colSubRivers array to the number of SubRivers in the GridCell
        ! Get the time-dependent runoff data from the file and put in array ready for use
        ! TODO: Runoff data currently m3/s, but maybe this should be m/s instead?
        if (me%ncGroup%hasVariable("runoff")) then
            var = me%ncGroup%getVariable("runoff")
            call var%getData(me%QrunoffTimeSeries)                 
            me%QrunoffTimeSeries = me%QrunoffTimeSeries*C%timeStep ! Convert to m3/timestep
        else
            me%QrunoffTimeSeries = 0
        end if
        ! Slope of the GridCell [m/m]
        if (me%ncGroup%hasVariable('slope')) then
            var = me%ncGroup%getVariable('slope')
            call var%getData(me%slope)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for slope not found in input file." &
            ))
        end if
        ! Manning's roughness coefficient [-]
        if (me%ncGroup%hasVariable('n_river')) then
            var = me%ncGroup%getVariable('n_river')
            call var%getData(me%n_river)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for n_river not found in input file. " // &
                            "Defaulting to 0.035 (natural streams and major rivers).", &
                isCritical = .false. &
            ))
            me%n_river = 0.035
        end if
    end function
    
end module
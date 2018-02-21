!> Module containing `GridCell1` class.
module classGridCell1
    use Globals
    use UtilModule 
    use mo_netcdf
    use ResultModule
    use ErrorInstanceModule
    use spcGridCell
    use classSoilProfile1
    use classRiverReach1
    implicit none
    !> `GridCell1` is responsible for running creation and simulation
    !! procedures for `SoilProfile` and `RiverReach`es.
    type, public, extends(GridCell) :: GridCell1
      contains
        procedure :: create => createGridCell1
        procedure :: finaliseCreate => finaliseCreateGridCell1
        procedure :: destroy => destroyGridCell1
        procedure, private :: setBranchRouting
        procedure, private :: setRiverReachLengths
        procedure, private :: createRiverReaches
        procedure :: update => updateGridCell1
        procedure :: finaliseUpdate => finaliseUpdateGridCell1
        procedure :: parseInputData => parseInputDataGridCell1
    end type

  contains

    !> Create a GridCell with coordinates x and y.
    function createGridCell1(me, x, y, isEmpty) result(r)
        class(GridCell1), target :: me                   !! The `GridCell1` instance.
        type(Result)          :: r                    !! The `Result` object to return.
        integer               :: x, y                 !! Location of the `GridCell`
        logical, optional     :: isEmpty              !! Is anything to be simulated in this `GridCell`?
        type(SoilProfile1)    :: soilProfile          ! The soil profile contained in this GridCell

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%Q_runoff_timeSeries(C%nTimeSteps))
        allocate(me%Q_evap_timeSeries(C%nTimeSteps))
        allocate(me%Q_precip_timeSeries(C%nTimeSteps))
        !allocate(subRiverRunoff_timeSeries(C%nTimeSteps))
        allocate(me%colSoilProfiles(1))
        allocate(me%routedRiverReaches(0,0))            ! No routed RiverReach pointers to begin with
        me%Q_runoff = 0                                 ! Default to no runoff

        ! Set the GridCell's position, area, whether it's empty and its name
        me%x = x
        me%y = y
        me%area = C%defaultGridSize**2                  ! TODO: This will be changed to take into account lat-lon
        if (present(isEmpty)) me%isEmpty = isEmpty      ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module

        ! Only carry on if there's stuff to be simulated for this GridCell
        if (me%isEmpty .eqv. .false.) then
            r = me%parseInputData()                         ! Parse and store input data in this object

            ! Create a soil profile and add to this GridCell
            call r%addErrors(.errors. &
                soilProfile%create( &
                    me%x, &
                    me%y, &
                    1, &
                    me%slope, &
                    me%n_river, &
                    me%area, &
                    me%Q_precip_timeSeries, &
                    me%Q_evap_timeSeries &
                ) &
            )
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)
            
            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. r%hasCriticalError()) then
                ! Add RiverReaches to the GridCell (if any are present in the data file)
                call r%addErrors(.errors. me%createRiverReaches())
                
                !subRiverPrefix = "SubRiver_" // trim(str(me%gridX)) // &
                !                    "_" // trim(str(me%gridY)) // "_"
                !! Set SubRiver size to half of the grid cell size if there's more than one SubRiver,
                !! otherwise the full size of the grid cell. TODO: Constrain number of SubRivers somewhere
                !! so this makes sense.
                !if (me%nSubRivers > 1) then
                !    subRiverLength = C%gridCellSize / 2.0_dp
                !else
                !    subRiverLength = C%gridCellSize
                !end if
                ! Loop through SubRivers, incrementing s (from SubRiver_x_y_s), until none found
                !do s = 1, me%nSubRivers
                !    ! Split the runoff between SubRivers
                !    do t = 1, size(me%Q_runoff_timeSeries)
                !        if (me%Q_runoff_timeSeries(t) > 0) then
                !            subRiverRunoff_timeSeries(t) = me%Q_runoff_timeSeries(t)/me%nSubRivers
                !        else
                !            subRiverRunoff_timeSeries(t) = 0
                !        end if
                !    end do
                !    ! Check that group actually exists
                !    ! TODO: Maybe perform this check somewhere else
                !    if (me%ncGroup%hasGroup(trim(subRiverPrefix) // trim(str(s)))) then
                !        allocate(sr1)                                   ! Create the new SubRiver
                !        srR = sr1%create(me%gridX, me%gridY, s, subRiverLength, subRiverRunoff_timeSeries)
                !        call r%addErrors(errors = .errors. srR)         ! Add any errors to final Result object
                !        call move_alloc(sr1, me%colSubRivers(s)%item)   ! Allocate a new SubRiver to the colSubRivers array
                !    else
                !        call r%addError(ErrorInstance( &
                !            code = 501, &
                !            message = "No input data provided for " // trim(subRiverPrefix) // trim(str(s)) // &
                !                        " - check nSubRivers is set correctly." &
                !        ))
                !    end if
                !end do
            end if
        end if

        call r%addToTrace("Creating " // trim(me%ref))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function
    
    !> Create the RiverReaches contained in this GridCell, and begin to populate
    !! the routedRiverReaches array by specifying the head of each river branch
    !! in the GridCell
    function createRiverReaches(me) result(r)
        class(GridCell1), target :: me          !! This `GridCell1` instance
        type(Result) :: r                       !! The `Result` object to return any errors in
        integer :: rr, b                        ! Iterator for reaches and branches
        type(RiverReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)
            ! Temporary array for appending to routedRiverReaches array
        
        b = 0                                   ! No river branches to begin with
        ! Loop through all the reaches in this GridCell.
        ! Don't type check for the moment
        do rr = 1, me%nRiverReaches
            allocate(RiverReach1::me%colRiverReaches(rr)%item)
            call r%addErrors(.errors. &
                me%colRiverReaches(rr)%item%create( &
                    me%x, &
                    me%y, &
                    rr &
                ) &
            )
            ! If the RiverReach we just created is the head of a branch in this
            ! GridCell, then use it to start filling the routedRiverReaches array.
            ! Whether it is a GridCell inflow or a headwater will have been set
            ! by RiverReach%parseInputData().
            if (me%colRiverReaches(rr)%item%isGridCellInflow &
                    .or. me%colRiverReaches(rr)%item%isHeadwater) then
                b = b + 1               ! Add another branch to the index
                allocate(tmpRoutedRiverReaches(b,1))   ! Use temp array to append to me%routedRiverReaches
                ! Fill the temp array with the current routedRiverReaches array
                tmpRoutedRiverReaches(1:b-1,:) = me%routedRiverReaches
                ! Then add the new branch by pointing first element in this branch to this reach
                tmpRoutedRiverReaches(b,1)%item => me%colRiverReaches(rr)%item
                call move_alloc(from=tmpRoutedRiverReaches, to=me%routedRiverReaches)
                me%colRiverReaches(rr)%item%branch = b
            end if
        end do
                
        me%nBranches = b                            ! Number of branches in this GridCell
        allocate(me%branchLengths(b))               ! Set the size of the array that holds the lengths of each branch
        allocate(me%nReachesInBranch(b))            ! Set the size of the array that holds the number of reaches in a branch
        me%nReachesInBranch = 1                     ! Currently only 1 reach per branch
                    
        ! We can't carry on setting the routingRiverReach array yet, as inflows/outflow array
        ! haven't been filled yet - this is done by Environment after all of the GridCells
        ! are set up (because inflows/outflows might point to different GridCells).
    end function

    
    !> Set the routedRiverReaches array, presuming that the first reach in each
    !! branch has already been set by the GridCell%create method. Then set
    !! river reach lengths.
    function finaliseCreateGridCell1(me) result(r)
        class(GridCell1) :: me              !! This `GridCell1` instance
        type(Result) :: r                   !! The `Result` object to return
        integer :: b                        ! River branch iterator
        ! Loop through the branch and call the setBranchRouting
        ! function, which follows a particular branch downstream and fills
        ! routedRiverReaches accordingly
        if (.not. me%isEmpty) then
            do b = 1, me%nBranches
                call r%addErrors(.errors. me%setBranchRouting(b,1))
                call r%addErrors(.errors. me%setRiverReachLengths(b))
            end do
        end if
    end function
    
    !> Recursively called function that sets the next elemet in the routedRiverReaches array,
    !! based on the `riverReach%outflow` property. `rr` must be the current final reach in the array.
    recursive function setBranchRouting(me, b, rr) result(r)
        class(GridCell1) :: me              !! This `GridCell1` instance
        integer :: b                        !! Which branch to route
        integer :: rr                       !! The `RiverReach` index to start from
        type(Result) :: r                   !! The `Result` object to return
        type(RiverReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)
        class(RiverReach), allocatable :: finalRiverReach
        
        ! Get the current final river reach in the branch (given that rr was passed correctly)
        allocate(finalRiverReach, source=me%routedRiverReaches(b,rr)%item)
        ! If it isn't an outflow to the GridCell, there must be further downstream
        ! branches, so recursively add them
        if (.not. finalRiverReach%isGridCellOutflow .and. associated(finalRiverReach%outflow%item)) then
            ! The number of reaches in routedRiverReaches array might be larger than
            ! number of reaches in this branch, so make sure we're only appending if
            ! those elements don't exist
            if (rr+1 > size(me%routedRiverReaches,2)) then
                ! Allocate to the current number of reaches in the branch, plus 1
                allocate(tmpRoutedRiverReaches(me%nBranches, size(me%routedRiverReaches,2)+1))
                ! Fill the temp array with the current routedRiverReaches array
                tmpRoutedRiverReaches(:,1:size(me%routedRiverReaches,2)) = me%routedRiverReaches
                ! Add the new reach to this branch
                tmpRoutedRiverReaches(b,size(tmpRoutedRiverReaches,2))%item => &
                    finalRiverReach%outflow%item
                call move_alloc(from=tmpRoutedRiverReaches, to=me%routedRiverReaches)
            else
                ! If rr+1 is equal to or smaller than array size, then we assume
                ! the rr+1'th element is un-associated and fill it straight away
                me%routedRiverReaches(b,rr+1)%item => finalRiverReach%outflow%item
            end if
            ! Call recursively until me%isGridCellOutflow isn't satisfied
            r = me%setBranchRouting(b,rr+1)
        else
            ! Once we've reached the final reach in the branch (i.e. the outflow
            ! from the GridCell), then set the number of branch to the current
            ! RiverReach index
            me%nReachesInBranch(b) = rr
        end if
    end function
    
    function setRiverReachLengths(me, b) result(r)
        class(GridCell1) :: me                      !! This `GridCell` instance
        integer :: b                                !! The branch to calculate the reach lengths for
        type(Result) :: r                           !! The Result object to return any errors in
        real(dp) :: dx, dy                          ! Distance from inflow to outflow
        real(dp) :: branchLength                    ! Length of this branch
        integer :: rr
        real(dp) :: specifiedLengths(me%nReachesInBranch(b))
        integer :: nReachesUnspecifiedLength        ! Number of reaches with unspecified length
        real(dp) :: unspecifiedLengths              ! Calculated length for reaches with unspecified length
        
        call r%addToTrace("Determining RiverReach lengths for branch " // trim(str(b)))  
        nReachesUnspecifiedLength = 0               ! Initialise to zero
        
        ! Use the x,y positions of inflow to the first reach and outflow from the
        ! last reach to calculate the straight-line length of the branch through
        ! the GridCell
        associate (firstReach => me%routedRiverReaches(b,1)%item, &
            finalReach => me%routedRiverReaches(b,me%nReachesInBranch(b))%item)
            ! If inflows and outflows are defined (isn't a headwater or unspecified domain outflow)
            if (.not. firstReach%isHeadwater .and. associated(finalReach%outflow%item)) then
                ! A touch of trigonometry to calculate the branch length
                dx = abs(firstReach%inflows(1)%item%x - finalReach%outflow%item%x)*0.5*me%dx   ! Distance from inflow to outflow in x direction
                dy = abs(firstReach%inflows(1)%item%y - finalReach%outflow%item%y)*0.5*me%dy   ! Distance from inflow to outflow in y direction
            else if (firstReach%isHeadwater .and. associated(finalReach%outflow%item)) then
                ! Assume source is the centre of the GridCell
                dx = abs(me%x - finalReach%outflow%item%x)*0.5*me%dx
                dy = abs(me%y - finalReach%outflow%item%y)*0.5*me%dy
            else if (.not. firstReach%isHeadwater .and. .not. associated(finalReach%outflow%item)) then
                ! Check if domainOutflow specified in data file, if not, trigger error
                if (size(finalReach%domainOutflow) == 2) then
                    dx = abs(firstReach%inflows(1)%item%x - finalReach%domainOutflow(1))*0.5*me%dx
                    dy = abs(firstReach%inflows(1)%item%y - finalReach%domainOutflow(2))*0.5*me%dy
                else
                    call r%addError(ErrorInstance( &
                        code=404, &
                        message=trim(finalReach%ref) // " outflow could not be determined. " // &
                        "Reaches must either be specified as inflow to downstream reach, or have a model domain outflow specified.") &
                    )
                    return
                end if
            end if
        end associate
        
            ! Set the length of this branch based on the above
        branchLength = sqrt(dx**2 + dy**2)
            
        ! Loop through reaches and get their lengths from data
        do rr = 1, me%nReachesInBranch(b)
            specifiedLengths(rr) = me%routedRiverReaches(b,rr)%item%l
            ! Add to tally of how many reaches don't have length in data file
            if (abs(specifiedLengths(rr)) < C%epsilon) nReachesUnspecifiedLength = nReachesUnspecifiedLength + 1
        end do
        ! If the sum of their lenghts is greater than branch length, warn about it
        ! but keep all reach lengths the same - effectively letting specified lengths
        ! act as a meandering factor. Else, split up the "empty" space into the
        ! number of reaches that have unspecified length and give them that length
        if (sum(specifiedLengths) > branchLength) then
            call r%addError(ErrorInstance(code=405, isCritical=.false.))
            branchLength = sum(specifiedLengths)        ! Set the branch length to the specified length
        else
            ! How much length is left over after lengths specified in data file?
            unspecifiedLengths = (branchLength - sum(specifiedLengths))/nReachesUnspecifiedLength
            ! Give reaches with unspecified length an equal proportion of that length
            do rr = 1, me%nReachesInBranch(b)
                if (abs(specifiedLengths(rr)) < C%epsilon) then
                    me%routedRiverReaches(b,rr)%item%l = unspecifiedLengths
                end if
            end do
        end if
        ! Set this element of the branchLengths array at GridCell object level
        me%branchLengths(b) = branchLength              
    end function
    
    !> Destroy this `GridCell`
    function destroyGridCell1(Me) result(r)
        class(GridCell1) :: Me                              !! The `GridCell` instance.
        type(Result) :: r                                   !! The `Result` object
        integer :: x                                  !! loop counter
        
        do x = 1, Me%nSoilProfiles
            r = Me%colSoilProfiles(x)%item%destroy()        ! remove all SoilProfile objects and any contained objects
        end do
        do x = 1, Me%nPointSources
            r = Me%colPointSources(x)%item%destroy()        ! remove all PointSource objects and any contained objects
        end do
        r = Me%objDiffuseSource%item%destroy()              ! remove the DiffuseSource object and any contained objects
    end function

    !> Perform the simulations required for an individual time step
    function updateGridCell1(Me, t) result(r)
        class(GridCell1) :: Me                                         !! The `GridCell` instance
        integer :: t                                                   !! The timestep we're on
        type(Result) :: r                                              !! `Result` object to return errors in
        type(Result) :: srR                                            ! Result object for SubRivers
        integer :: rr                                             ! Loop counter
        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            r = me%colSoilProfiles(1)%item%update(t, me%Q_runoff_timeSeries(t))
            me%erodedSediment = me%colSoilProfiles(1)%item%erodedSediment

            ! Loop through each RiverReach and run its update procedure
            ! TODO: Aportion j_spm_runoff properly!
            !do rr = 1, me%nRiverReaches
            !    call r%addErrors(.errors. &
            !        me%colRiverReaches(rr)%item%update( &
            !            t = t, &
            !            j_spm_runoff = me%erodedSediment/me%nRiverReaches &
            !        ) &
            !    )
            !end do
            !
            ! Loop through each SubRiver and run its update procedure
    !        do s = 1, me%nSubRivers
    !            srR = me%colSubRivers(s)%item%update( &
    !                t = t, &
    !                j_spm_runoff = me%erodedSediment/me%nSubRivers &
				!)
    !            call r%addErrors(errors = .errors. srR)
    !        end do
        end if
        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another `SubRiver` whilst the `SubRiver`s
    !! are looped through.
    function finaliseUpdateGridCell1(me) result(r)
        class(GridCell1) :: me                                      !! This `SubRiver1` instace
        type(Result) :: r                                           !! The `Result` object
        integer :: s                                                ! Iterator of SubRivers
        !if (.not. me%isEmpty) then
        !    do s = 1, me%nSubRivers
        !        r = me%colSubRivers(s)%item%finaliseUpdate()
        !    end do
        !end if
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell1(me) result(r)
        class(GridCell1)        :: me                   !! This `GridCell1` object
        type(Result)            :: r                    !! The `Result` object
        type(NcDataset)         :: nc                   ! NetCDF dataset
        type(NcVariable)        :: var                  ! NetCDF variable
        type(NcGroup)           :: grp                  ! NetCDF group
        integer, allocatable    :: xySize(:)            ! The size of the GridCell

        ! Open the dataset (as read only)
        nc = NcDataset(C%inputFile, "r")
        grp = nc%getGroup("Environment")
        me%ncGroup = grp%getGroup(me%ref)               ! Get this GridCell's group
        ! Get the number of RiverReaches in this GridCell. If not present, nRiverReaches
        ! defaults to 0
        if (me%ncGroup%hasVariable("nRiverReaches")) then
            var = me%ncGroup%getVariable("nRiverReaches")                     
            call var%getData(me%nRiverReaches)
        end if
        
        ! Allocate the colRiverReaches array to the number of RiverReaches in the GridCell
        allocate(me%colRiverReaches(me%nRiverReaches))
        
        ! Get the size of the GridCell [m]
        if (me%ncGroup%hasVariable("size")) then
            var = me%ncGroup%getVariable("size")
            call var%getData(xySize)
            me%dx = xySize(1)                   ! Size in x direction
            me%dy = xySize(2)                   ! Size in y direction
        else
            ! If not present, default to defaultGridSize from config
            me%dx = C%defaultGridSize
            me%dy = C%defaultGridSize
        end if
        
        ! Get the time-dependent runoff data from the file and put in array ready for use
        ! If using HMF data, this is slow flow + quick flow. [m/s]
        if (me%ncGroup%hasVariable("runoff")) then
            var = me%ncGroup%getVariable("runoff")
            call var%getData(me%Q_runoff_timeSeries)                 
            me%Q_runoff_timeSeries = me%Q_runoff_timeSeries*me%area*C%timeStep ! Convert to m3/timestep
        else
            me%Q_runoff_timeSeries = 0
        end if
        ! Precipitation [m/s]
        if (me%ncGroup%hasVariable("precip")) then
            var = me%ncGroup%getVariable("precip")
            call var%getData(me%q_precip_timeSeries)                 
            me%Q_precip_timeSeries = me%q_precip_timeSeries*C%timeStep    ! Convert to m/timestep
        else
            me%Q_precip_timeSeries = 0
        end if
        ! Evaporation [m/s]
        if (me%ncGroup%hasVariable("evap")) then
            var = me%ncGroup%getVariable("evap")
            call var%getData(me%q_evap_timeSeries)                 
            me%Q_evap_timeSeries = me%q_evap_timeSeries*C%timeStep       ! Convert to m/timestep
        else
            me%Q_evap_timeSeries = 0
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
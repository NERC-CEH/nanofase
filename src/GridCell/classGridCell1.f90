!> Module containing `GridCell1` class.
module classGridCell1
    use Globals
    use UtilModule
    use mo_netcdf
    use classDataInterfacer
    use classLogger
    use ResultModule
    use ErrorInstanceModule
    use spcGridCell
    use classSoilProfile1
    use classRiverReach
    use classEstuaryReach
    use classCrop
    implicit none
    !> `GridCell1` is responsible for running creation and simulation
    !! procedures for `SoilProfile` and `RiverReach`es.
    type, public, extends(GridCell) :: GridCell1
    contains
        ! Create/destroy
        procedure :: create => createGridCell1
        procedure :: finaliseCreate => finaliseCreateGridCell1
        procedure :: destroy => destroyGridCell1
        procedure, private :: setBranchRouting
        procedure, private :: setRiverReachLengths
        procedure, private :: createReaches
        ! Simulators
        procedure :: update => updateGridCell1
        procedure :: finaliseUpdate => finaliseUpdateGridCell1
        procedure :: demands => demandsGridCell1
        procedure :: transfers => transfersGridCell1
        ! Data handlers
        procedure :: parseInputData => parseInputDataGridCell1
        ! Getters
        procedure :: get_Q_out => get_Q_outGridCell1
        procedure :: get_j_spm_out => get_j_spm_outGridCell1
        procedure :: get_j_np_out => get_j_np_outGridCell1
        procedure :: get_m_spm => get_m_spmGridCell1
        procedure :: get_m_np => get_m_npGridCell1
    end type

  contains

    !> Create a GridCell with coordinates x and y.
    function createGridCell1(me, x, y, isEmpty) result(r)
        class(GridCell1), target :: me              !! The `GridCell1` instance.
        type(Result)          :: r                  !! The `Result` object to return.
        integer               :: x, y               !! Location of the `GridCell`
        logical, optional     :: isEmpty            !! Is anything to be simulated in this `GridCell`?
        integer               :: s                  ! Iterator for `DiffuseSource`s
        type(SoilProfile1)    :: soilProfile        ! The soil profile contained in this GridCell

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%colSoilProfiles(1))
        allocate(me%routedRiverReaches(0,0))            ! No routed RiverReach pointers to begin with
        allocate(me%j_np_diffuseSource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2))
        me%q_runoff = 0                                 ! Default to no runoff


        ! Set the GridCell's position, whether it's empty and its name
        me%x = x
        me%y = y
        if (present(isEmpty)) me%isEmpty = isEmpty      ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module

        ! Only carry on if there's stuff to be simulated for this GridCell
        if (me%isEmpty .eqv. .false.) then

            r = me%parseInputData()                         ! Parse and store input data in this object

            ! Create the DiffuseSource object(s), if this cell has any
            if (me%hasDiffuseSource) then
                do s = 1, size(me%diffuseSources, 1)
                    call r%addErrors(.errors. me%diffuseSources(s)%create(me%x, me%y, s))
                end do
            end if

            ! Create a soil profile and add to this GridCell
            call r%addErrors(.errors. &
                soilProfile%create( &
                    me%x, &
                    me%y, &
                    1, &
                    me%slope, &
                    me%n_river, &
                    me%area, &
                    me%q_quickflow_timeSeries, &
                    me%q_precip_timeSeries, &
                    me%q_evap_timeSeries &
                ) &
            )
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)

            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. r%hasCriticalError()) then
                ! Add RiverReaches to the GridCell (if any are present in the data file)
                call r%addErrors(.errors. me%createReaches())
            end if
        end if

        call r%addToTrace("Creating " // trim(me%ref))
        call LOG%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear errors from the Result object so they're not reported twice
        call LOG%toConsole("\tCreating " // trim(me%ref) // ": \x1B[32msuccess\x1B[0m")
        call LOG%toFile("Creating " // trim(me%ref) // ": success")
    end function
    
    !> Create the RiverReaches contained in this GridCell, and begin to populate
    !! the routedRiverReaches array by specifying the head of each river branch
    !! in the GridCell
    function createReaches(me) result(r)
        class(GridCell1), target :: me          !! This `GridCell1` instance
        type(Result) :: r                       !! The `Result` object to return any errors in
        integer :: rr, b                        ! Iterator for reaches and branches
        type(ReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)
            ! Temporary array for appending to routedRiverReaches array

        b = 0                                   ! No river branches to begin with

        ! Loop through all the reaches in this GridCell.
        ! Don't type check for the moment
        do rr = 1, me%nReaches
            ! Is this a river or estuary reach?
            if (me%reachTypes(rr)=='riv') then
                allocate(RiverReach::me%colRiverReaches(rr)%item)
            else
                allocate(EstuaryReach::me%colRiverReaches(rr)%item)
            end if
            call r%addErrors(.errors. &
                me%colRiverReaches(rr)%item%create( &
                    me%x, &
                    me%y, &
                    rr, &
                    me%area &
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
                if (size(me%routedRiverReaches,2) > 0) tmpRoutedRiverReaches(1:b-1,:) = me%routedRiverReaches
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
        integer :: rr                       ! RiverReach iterator
        ! Loop through the branch and call the setBranchRouting
        ! function, which follows a particular branch downstream and fills
        ! routedRiverReaches accordingly
        if (.not. me%isEmpty) then
            do b = 1, me%nBranches
                call r%addErrors(.errors. me%setBranchRouting(b,1))
                call r%addErrors(.errors. me%setRiverReachLengths(b))
            end do
        end if
        ! Trigger any errors
        call r%addToTrace("Finalising creation of " // trim(me%ref))
        call LOG%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear errors from the Result object so they're not reported twice
    end function
    
    !> Recursively called function that sets the next elemet in the routedRiverReaches array,
    !! based on the `riverReach%outflow` property. `rr` must be the current final reach in the array.
    recursive function setBranchRouting(me, b, rr) result(r)
        class(GridCell1) :: me              !! This `GridCell1` instance
        integer :: b                        !! Which branch to route
        integer :: rr                       !! The `RiverReach` index to start from
        type(Result) :: r                   !! The `Result` object to return
        type(ReachPointer), allocatable :: tmpRoutedRiverReaches(:,:)
        class(Reach), allocatable :: finalRiverReach
        
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
            call r%addErrors(.errors. me%setBranchRouting(b,rr+1))
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
            ! If this is a domain outflow
            else if (.not. associated(finalReach%outflow%item)) then
                ! Check the reach has inflows
                if (size(firstReach%inflows) > 0) then
                    ! Check if domainOutflow specified in data file, if not, trigger error
                    if (size(finalReach%domainOutflow) == 2) then
                        dx = abs(firstReach%inflows(1)%item%x - finalReach%domainOutflow(1))*0.5*me%dx
                        dy = abs(firstReach%inflows(1)%item%y - finalReach%domainOutflow(2))*0.5*me%dy
                    else
                        call r%addError(ErrorInstance( &
                            code=404, &
                            message=trim(finalReach%ref) // " outflow could not be determined. " // &
                            "Reaches must either be specified as inflow to downstream reach, " // &
                            "or have a model domain outflow specified.") &
                        )
                        call r%addToTrace("Determining RiverReach lengths for branch " // trim(str(b)))
                        return              ! Get out of here early otherwise we'll get FPEs below!
                    end if
                ! If the reach has no inflows but is a domain outflow, there's not a lot we
                ! can do for the moment (until we implement proper boundary conditions for inflows).
                ! So, just remove all reaches from the grid cell.
                ! TODO Set up proper inflow boundary conditions
                else
                    deallocate(me%colRiverReaches)
                    deallocate(me%routedRiverReaches)
                    deallocate(me%nReachesInBranch)
                    allocate(me%colRiverReaches(0))
                    allocate(me%routedRiverReaches(0,0))
                    allocate(me%nReachesInBranch(0))
                    me%nReaches = 0
                    me%nRiverReaches = 0
                    me%nEstuaryReaches = 0
                    return
                end if
            end if
        end associate
        
        ! Set the length of this branch based on the above
        branchLength = sqrt(dx**2 + dy**2)
            
        ! Loop through reaches and get their lengths from data
        do rr = 1, me%nReachesInBranch(b)
            specifiedLengths(rr) = me%routedRiverReaches(b,rr)%item%length
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
                    me%routedRiverReaches(b,rr)%item%length = unspecifiedLengths
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
        integer :: x                                        ! Loop iterator
        ! TODO: Is this method needed?
    end function

    !> Perform the simulations required for an individual time step
    function updateGridCell1(me, t) result(r)
        class(GridCell1) :: me                                  !! The `GridCell` instance
        integer :: t                                            !! The timestep we're on
        type(Result) :: r                                       !! `Result` object to return errors in
        integer :: i                                            ! Iterator
        integer :: rr                                           ! Loop counter
        real(dp) :: lengthRatio                                 ! Reach length as a proportion of total river length
        real(dp) :: j_np_runoff(C%nSizeClassesNP, 4, 2 + C%nSizeClassesSpm) ! NP runoff for this time step
        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            ! Reset variables
            me%j_np_diffuseSource = 0.0_dp
            
            ! Get any inputs from diffuse source
            if (me%hasDiffuseSource) then
                do i = 1, size(me%diffuseSources)
                    call r%addErrors(.errors. me%diffuseSources(i)%update(t))
                    me%j_np_diffuseSource = me%j_np_diffuseSource + me%diffuseSources(i)%j_np_diffuseSource     ! [kg/m2/timestep]
                end do
            end if

            ! Demands and transfers
            call r%addErrors([ &
                .errors. me%demands(), &
                .errors. me%transfers() &
            ])

            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            ! TODO Add DiffuseSource to soil profile
            call r%addErrors( &
                .errors. me%colSoilProfiles(1)%item%update(t, me%j_np_diffuseSource) &
            )
            if (r%hasCriticalError()) return
            me%erodedSediment = me%colSoilProfiles(1)%item%erodedSediment

            ! Loop through the reaches and call their update methods for this
            ! timestep, providing them the eroded sediment split according
            ! to their length
            ! TODO Add diffuse source to reaches
            do rr = 1, me%nReaches
                ! Determine the proportion of this reach's length to the the total
                ! river length in this GridCell
                lengthRatio = me%colRiverReaches(rr)%item%length/sum(me%branchLengths)
                ! HACK to set NP runoff to 1e-9 SPM runoff
                ! j_np_runoff = lengthRatio*sum(me%erodedSediment)*0
                j_np_runoff = lengthRatio*me%colSoilProfiles(1)%item%m_np_eroded    ! [kg/timestep]
                ! Update the reach for this timestep
                call r%addErrors(.errors. &
                    me%colRiverReaches(rr)%item%update( &
                        t = t, &
                        q_runoff = me%q_runoff_timeSeries(t), &
                        j_spm_runoff = me%erodedSediment*lengthRatio, &
                        j_np_runoff = j_np_runoff &
                    ) &
                )
            end do
        end if

        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOG%toFile(errors = .errors. r)            ! Log any errors to the output file
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear the errors so we're not reporting twice
        call LOG%toFile("Performing simulation for " // trim(me%ref) // " on time step #" // trim(str(t)) // ": success")
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another `RiverReach` whilst the reaches
    !! are looped through.
    subroutine finaliseUpdateGridCell1(me)
        class(GridCell1) :: me                                      !! This `GridCell1` instace
        integer :: rr                                               ! Iterator for reaches
        if (.not. me%isEmpty) then
            do rr = 1, me%nReaches
                call me%colRiverReaches(rr)%item%finaliseUpdate()
            end do
        end if
    end subroutine
    
    !> Process the water demands for this `GridCell`
    function demandsGridCell1(me) result(r)
        class(GridCell1) :: me
        type(Result) :: r
        integer :: pcLossUrban = 0                      ! TODO where should this come from?
        integer :: pcLossRural = 0                      ! TODO where should this come from?
        integer :: pcLossLivestockConsumption = 10      ! TODO where should this come from?
        real(dp) :: cattleDemandPerCapita = 140         ! TODO where should this come from?
        real(dp) :: sheepGoatDemandPerCapita = 70       ! TODO where should this come from?
        real(dp) :: totalUrbanDemand
        real(dp) :: totalLivestockDemand
        real(dp) :: totalRuralDemand
        
        ! TODO Population increase factor is excluded here - check this is okay?
        ! I'm thinking that population increase can be factored into population
        ! numbers in dataset instead
        totalUrbanDemand = (me%urbanPopulation * me%urbanDemandPerCapita * 1.0e-9)/(1.0_dp - 0.01_dp * pcLossUrban)   ! [Mm3/day]
        totalLivestockDemand = ((me%cattlePopulation * cattleDemandPerCapita + me%sheepGoatPopulation * sheepGoatDemandPerCapita) &
                                * 0.01_dp * pcLossLivestockConsumption * 1.0e-9) / (1.0_dp - 0.01_dp * pcLossRural)
        totalRuralDemand = ((me%totalPopulation - me%urbanPopulation) * me%ruralDemandPerCapita * 1.0e-9) &
                            / (1.0_dp - 0.01_dp * pcLossRural)
        ! TODO See Virginie's email 29/08/2018
        
    end function
    
    !> Process the water abstractions and transferss for this `GridCell`
    function transfersGridCell1(me) result(r)
        class(GridCell1) :: me
        type(Result) :: r
        ! Transfer some water!
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell1(me) result(r)
        class(GridCell1)        :: me                   !! This `GridCell1` object
        type(Result)            :: r                    !! The `Result` object
        integer, allocatable    :: xySize(:)            ! The size of the GridCell
        integer                 :: i                    ! Iterator
        type(Result)            :: rslt                 ! Variable to store Result in
        integer                 :: hasLargeCityInt      ! Integer representation of hasLargeCity logical
        integer                 :: cropType             ! Temporary var to store crop type int in
        real(dp)                :: cropArea             ! Temporary var to store crop area in
        integer                 :: cropPlantingMonth    ! Temporary var to store crop planting month in

        ! Allocate arrays to store flows in
        allocate(me%q_runoff_timeSeries(C%nTimeSteps))
        allocate(me%q_quickflow_timeSeries(C%nTimeSteps))
        allocate(me%q_evap_timeSeries(C%nTimeSteps))
        allocate(me%q_precip_timeSeries(C%nTimeSteps))
        allocate(me%T_water_timeSeries(C%nTimeSteps))
        
        ! Set the data interfacer's group to the group for this GridCell
        call r%addErrors(.errors. DATA%setGroup([character(len=100)::'Environment', me%ref]))

        ! Check if this reach has any diffuse sources. me%hasDiffuseSource defauls to .false.
        ! Allocate me%diffuseSources accordingly. The DiffuseSource class actually gets the data.
        if (DATA%grp%hasGroup("DiffuseSource") .or. DATA%grp%hasGroup("DiffuseSource_1")) then
            me%hasDiffuseSource = .true.
            allocate(me%diffuseSources(1))
            i = 2               ! Any extra diffuse sources?
            do while (DATA%grp%hasGroup("DiffuseSource_" // trim(str(i))))
                deallocate(me%diffuseSources)
                allocate(me%diffuseSources(i))
                i = i+1
            end do
        end if
        
        ! Get the number of river reaches. Defaults to 0
        call r%addErrors(.errors. DATA%get('n_river_reaches', me%nRiverReaches, silentlyFail=.true.))
        ! Get the number of estuary reaches and allocate colReaches. Defaults to 0
        call r%addErrors(.errors. DATA%get('n_estuary_reaches', me%nEstuaryReaches, silentlyFail=.true.))
        me%nReaches = me%nRiverReaches + me%nEstuaryReaches
        allocate(me%colRiverReaches(me%nReaches))
        allocate(me%reachTypes(me%nReaches))

        ! Are reaches rivers or estuaries?
        do i=1, me%nReaches
            if (DATA%grp%hasGroup(ref('RiverReach', me%x, me%y, i))) then
                me%reachTypes(i) = 'riv'
            else if (DATA%grp%hasGroup(ref('EstuaryReach', me%x, me%y, i))) then
                me%reachTypes(i) = 'est'
            else
                call r%addError(ErrorInstance( &
                    message="Mismatch between specified number of reaches and number of data groups for those reaches. " &
                        // "Check nReaches matches the number of reach groups in the input data."))
            end if
        end do

        
        ! Get the size of the GridCell [m]. Defaults to defaultGridSize from config
        call r%addErrors(.errors. DATA%get('size', xySize, [C%defaultGridSize, C%defaultGridSize]))
        me%dx = xySize(1)
        me%dy = xySize(2)
        me%area = me%dx*me%dy       ! Set the area based on this
        
        ! Get hydrology, topography and water data
        call r%addErrors([ &
            .errors. DATA%get('runoff', me%q_runoff_timeSeries, 0.0_dp), &
            .errors. DATA%get('quickflow', me%q_quickflow_timeSeries, 0.0_dp), &
            .errors. DATA%get('precip', me%q_precip_timeSeries, 0.0_dp), &          ! [m/s]
            .errors. DATA%get('evap', me%q_evap_timeSeries, 0.0_dp), &
            .errors. DATA%get('slope', me%slope), &
            .errors. DATA%get('n_river', me%n_river, 0.035_dp), &
            .errors. DATA%get('T_water', me%T_water_timeSeries, C%defaultWaterTemperature, warnIfDefaulting=.false.) &
        ])
        ! Convert to m/timestep
        me%q_runoff_timeSeries = me%q_runoff_timeSeries*C%timeStep      
        me%q_quickflow_timeSeries = me%q_quickflow_timeSeries*C%timeStep
        me%q_precip_timeSeries = me%q_precip_timeSeries*C%timeStep
        me%q_evap_timeSeries = me%q_evap_timeSeries*C%timeStep
        
        ! Try and set the group to the demands group. It will produce an error if group
        ! doesn't exist - use this to set me%hasDemands to .false.
        rslt = DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands'])
        if (.not. rslt%hasError()) then
            me%hasDemands = .true.
            ! Now get the data from the group. These should all default to zero.
            ! TODO What should the default surface water to total water ratio be?
            call r%addErrors([ &
                .errors. DATA%get('total_population', me%totalPopulation, 0.0_dp), &
                .errors. DATA%get('urban_population', me%urbanPopulation, 0.0_dp), &
                .errors. DATA%get('cattle_population', me%cattlePopulation, 0.0_dp), &
                .errors. DATA%get('sheep_goat_population', me%sheepGoatPopulation, 0.0_dp), &
                .errors. DATA%get('urban_demand', me%urbanDemandPerCapita, 0.0_dp), &
                .errors. DATA%get('rural_demand', me%ruralDemandPerCapita, 0.0_dp), &
                .errors. DATA%get('industrial_demand', me%industrialDemand, 0.0_dp), &
                .errors. DATA%get('sw_to_tw_ratio', me%surfaceWaterToTotalWaterRatio, 0.42_dp, warnIfDefaulting=.true.), &
                .errors. DATA%get('has_large_city', hasLargeCityInt, 0) &
            ])
            me%hasLargeCity = lgcl(hasLargeCityInt)     ! Convert int to bool
            
            ! Check if there are any crops to get. These will be retrieved iteratively
            ! (i.e. crop_1, crop_2, crop_3). Then get the data for those crops and create
            ! array of Crop objects in me%crops
            i = 1
            do while (DATA%grp%hasGroup("crop_" // trim(str(i))))
                allocate(me%crops(i))
                call r%addErrors(.errors. &
                    DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands', 'crop_' // trim(str(i))]))
                call r%addErrors([ &
                    .errors. DATA%get('crop_area', cropArea), &
                    .errors. DATA%get('crop_type', cropType), &
                    .errors. DATA%get('planting_month', cropPlantingMonth) &
                ])
                me%crops(i) = Crop(cropType, cropArea, cropPlantingMonth)
                i = i+1
            end do
        end if        

    end function

!---------------!
!--- GETTERS ---!
!---------------!

    function get_Q_outGridCell1(me, b) result(Q_out)
        class(GridCell1) :: me                      !! This `GridCell1` instance
        integer, optional :: b                      !! Branch
        real(dp) :: Q_out
        integer :: r
        Q_out = 0
        ! If branch is present, just get the outflow for that branch,
        ! else, sum the outflows from each branch
        if (present(b)) then
            Q_out = me%routedRiverReaches(b,me%nReachesInBranch(b))%item%Q(1)
        else
            do b = 1, me%nBranches
                Q_out = Q_out + me%routedRiverReaches(b,me%nReachesInBranch(b))%item%Q(1)
            end do
        end if
    end function

    function get_j_spm_outGridCell1(me, b) result(j_spm_out)
        class(GridCell1) :: me                      !! This `GridCell1` instance
        integer, optional :: b                      !! Branch
        real(dp) :: j_spm_out(C%nSizeClassesSpm)
        j_spm_out = 0
        ! If branch is present, just get the outflow for that branch,
        ! else, sum the outflows from each branch
        if (present(b)) then
            j_spm_out = me%routedRiverReaches(b,me%nReachesInBranch(b))%item%j_spm(1,:)
        else
            do b = 1, me%nBranches
                j_spm_out = j_spm_out + me%routedRiverReaches(b,me%nReachesInBranch(b))%item%j_spm(1,:)
            end do
        end if
    end function

    function get_j_np_outGridCell1(me, b) result(j_np_out)
        class(GridCell1) :: me                      !! This `GridCell1` instance
        integer, optional :: b                      !! Branch
        real(dp) :: j_np_out(C%nSizeClassesNP)
        j_np_out = 0
        ! TODO: Sum NP outflows here
    end function

    !> Get the total mass of SPM currently in the GridCell
    function get_m_spmGridCell1(me, b) result(m_spm)
        class(GridCell1) :: me                      !! This `GridCell1` instance
        integer, optional :: b                      !! Branch
        real(dp) :: m_spm(C%nSizeClassesSpm)
        integer :: rr
        m_spm = 0
        if (present(b)) then
            ! Loop through the reaches in the branch and sum the SPM masses
            do rr = 1, me%nReachesInBranch(b)
                m_spm = m_spm + me%routedRiverReaches(b,rr)%item%m_spm
            end do
        else
            ! Loop through the reaches and sum the SPM masses
            do rr = 1, me%nReaches
                m_spm = m_spm + me%colRiverReaches(rr)%item%m_spm
            end do
        end if
    end function

    !> Get the total mass of NPs currently in the GridCell
    function get_m_npGridCell1(me, b) result(m_np)
        class(GridCell1) :: me                      !! This `GridCell1` instance
        integer, optional :: b                      !! Branch
        real(dp) :: m_np(C%nSizeClassesNP)
        m_np = 0
        ! TODO: Sum NP masses here
    end function
    
end module
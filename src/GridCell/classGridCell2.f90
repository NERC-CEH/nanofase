module classGridCell2
    use Globals
    use UtilModule
    use classDatabase, only: DATASET
    use classLogger
    use ResultModule
    use spcGridCell
    use classSoilProfile1
    use classRiverReach
    use classEstuaryReach
    use classCrop
    implicit none

    !> Responsible for the creation of simulation of grid cells
    !! and contained compartments (e.g. rivers, soils).
    type, public, extends(GridCell) :: GridCell2
      contains
        ! Create/destroy
        procedure :: create => createGridCell2
        procedure :: finaliseCreate => finaliseCreateGridCell2
        procedure :: destroy => destroyGridCell2
        procedure, private :: createReaches
        procedure :: snapPointSourcesToReach => snapPointSourcesToReachGridCell2
        ! Simulators
        procedure :: update => updateGridCell2
        procedure :: finaliseUpdate => finaliseUpdateGridCell2
        procedure :: demands => demandsGridCell2
        procedure :: transfers => transfersGridCell2
        ! Data handlers
        procedure :: parseInputData => parseInputDataGridCell2
        procedure :: parseNewBatchData => parseNewBatchDataGridCell2
        ! Getters
        procedure :: get_Q_out => get_Q_outGridCell2
        procedure :: get_j_spm_out => get_j_spm_outGridCell2
        procedure :: get_j_np_out => get_j_np_outGridCell2
        procedure :: get_m_spm => get_m_spmGridCell2
        procedure :: get_m_np => get_m_npGridCell2
        procedure :: getTotalReachLength => getTotalReachLengthGridCell2
        ! Calculators
        procedure :: reachLineParamsFromInflowsOutflow => reachLineParamsFromInflowsOutflowGridCell2
    end type

  contains

    !> Create a GridCell with coordinates x and y.
    function createGridCell2(me, x, y, isEmpty) result(rslt)
        class(GridCell2), target :: me              !! The `GridCell2` instance.
        type(Result)          :: rslt               !! The `Result` object to return.
        integer               :: x, y               !! Spatial index of the grid cell
        logical, optional     :: isEmpty            !! Is anything to be simulated in this `GridCell`?
        integer               :: s                  ! Iterator for `DiffuseSource`s
        type(SoilProfile1)    :: soilProfile        ! The soil profile contained in this GridCell

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%colSoilProfiles(1))
        allocate(me%routedRiverReaches(0,0))            ! No routed RiverReach pointers to begin with
        allocate(me%j_np_diffuseSource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2))
        me%q_runoff = 0   

        ! Set the GridCell's position, whether it's empty and its name
        me%x = x
        me%y = y
        if (present(isEmpty)) me%isEmpty = isEmpty      ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module
        
        ! Only carry on if there's stuff to be simulated for this GridCell
        if (.not. me%isEmpty) then
            
            ! Parse the input data for this cell
            rslt = me%parseInputData()

            ! Create two diffuse sources, atmospheric and soil. Water will be
            ! dealt with separately by waterbody classes
            allocate(me%diffuseSources(2))
            call me%diffuseSources(1)%create(me%x, me%y, 1, 'soil')
            call me%diffuseSources(2)%create(me%x, me%y, 2, 'atmospheric')

            ! Create a soil profile and add to this GridCell
            call rslt%addErrors(.errors. &
                soilProfile%create( &
                    me%x, &
                    me%y, &
                    1, &
                    me%slope, &
                    me%n_river, &
                    me%area, &
                    me%q_precip_timeSeries, &
                    me%q_evap_timeSeries &
                ) &
            )
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)

            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. rslt%hasCriticalError()) then
                ! Add RiverReaches to the GridCell (if any are present in the data file)
                call rslt%addErrors(.errors. me%createReaches())
            end if
        end if

        call rslt%addToTrace("Creating " // trim(me%ref))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
        call rslt%clear()                  ! Clear errors from the Result object so they're not reported twice
        if (.not. me%isEmpty) then
            call LOGR%toConsole("\tCreating " // trim(me%ref) // ": \x1B[32msuccess\x1B[0m")
            call LOGR%toFile("Creating " // trim(me%ref) // ": success")
        else
            call LOGR%toConsole("\tCreating " // trim(me%ref) // ": \x1B[32mempty\x1B[0m")
            call LOGR%toFile("Creating " // trim(me%ref) // ": empty")
        end if
    end function

    !> Finalise creation should be done after routing is complete, and is meant for
    !! procedures that rely on waterbodies being linked to their inflows/outflow
    function finaliseCreateGridCell2(me) result(rslt)
        class(GridCell2) :: me              !! This `GridCell2` instance
        type(Result) :: rslt                !! The `Result` object to return
        integer :: i
        ! Snap point sources to the closest reach
        call me%snapPointSourcesToReach()
        ! Run each waterbody's finalise creation method, which at the moment
        ! just allocates variables (which need to be done after point sources
        ! were set up)
        do i = 1, me%nReaches
            call me%colRiverReaches(i)%item%finaliseCreate()
        end do
        ! Trigger any errors
        call rslt%addToTrace("Finalising creation of " // trim(me%ref))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
        call rslt%clear()           ! Clear errors from the Result object so they're not reported twice
    end function

    subroutine snapPointSourcesToReachGridCell2(me)
        class(GridCell2) :: me
        integer :: i, j
        real, allocatable :: lineParams(:,:)
        real, allocatable :: distanceToReach(:)
        real :: x0, y0
        real :: fracIndicies(2)
        integer :: reachIndexToSnapTo

        ! Make sure there are no point sources already allocated
        do i = 1, me%nReaches
            if (allocated(me%colRiverReaches(i)%item%pointSources)) then
                deallocate(me%colRiverReaches(i)%item%pointSources)
            end if
            allocate(me%colRiverReaches(i)%item%pointSources(0))
        end do

        ! If there's just one reach, just snap all point sources to that
        if (me%nReaches == 1) then
            do j = 1, DATASET%nPointSources(me%x, me%y)
                call me%colRiverReaches(1)%item%addPointSource(j)
            end do
        ! Else if there is more than one reach, we need to figure out which reach to snap each
        ! point to
        else if (me%nReaches > 1) then
            ! Generate reach coord, with axis placed at bottom left of cell and representing each
            ! cell as being 2x2, so we can calculate distance between point sources and each reach
            allocate(lineParams(me%nReaches,3), &
                distanceToReach(me%nReaches))
            do i = 1, me%nReaches
                lineParams(i,:) = me%reachLineParamsFromInflowsOutflow(i)
            end do
            ! Loop through the point sources and find the closest line by using
            ! d = |ax0 + by0 + c|/sqrt(a^2 + b^2), where ax + by + c = 0 is the line of
            ! the reach, and (x0, y0) is the point source coords in index notation
            do j = 1, DATASET%nPointSources(me%x, me%y)
                x0 = DATASET%emissionsPointWaterCoords(me%x, me%y, j, 1)
                y0 = DATASET%emissionsPointWaterCoords(me%x, me%y, j, 2)
                fracIndicies = DATASET%coordsToFractionalCellIndex(x0, y0)
                do i = 1, me%nReaches
                    ! Calculate distance from point given by fracIndicies and the line
                    ! with params lineParams(i,:)
                    distanceToReach(i) = abs(lineParams(i,1) * fracIndicies(1) + lineParams(i,2) * fracIndicies(2) &
                        + lineParams(i,3)) / sqrt(lineParams(i,1) ** 2 + lineParams(i,2) ** 2)
                end do
                ! Use minloc to get the index of the minimum value in the distanceToReach array,
                ! and use that to add this point source to the correct reach
                reachIndexToSnapTo = minloc(distanceToReach, dim=1)
                call me%colRiverReaches(reachIndexToSnapTo)%item%addPointSource(j)
            end do
        end if
    end subroutine

    function createReaches(me) result(rslt)
        class(GridCell2), target :: me          !! This `GridCell2` instance
        type(Result) :: rslt                    !! The `Result` object to return any errors in
        integer :: i, w
        integer, allocatable :: neighbours(:,:,:,:)

        ! Loop through waterbodies and create them
        do w = 1, me%nReaches
            ! What type of waterbody is this?
            if (me%reachTypes(w) == 'riv') then
                allocate(RiverReach::me%colRiverReaches(w)%item)
            else if (me%reachTypes(w) == 'est') then
                allocate(EstuaryReach::me%colRiverReaches(w)%item)
            else
                call rslt%addError(ErrorInstance( &
                    message="Trying to create waterbody of unknown type " // trim(me%reachTypes(w)) // "." &
                ))
            end if
            ! Call creation method
            call rslt%addErrors(.errors. &
                me%colRiverReaches(w)%item%create(me%x, me%y, w, me%area) &
            )
        end do
    end function

    !> Destroy this `GridCell`
    function destroyGridCell2(Me) result(r)
        class(GridCell2) :: Me                              !! The `GridCell` instance.
        type(Result) :: r                                   !! The `Result` object
        integer :: x                                        ! Loop iterator
        ! TODO:  Get rid
    end function

    !> Perform the simulations required for an individual time step
    function updateGridCell2(me, t) result(r)
        class(GridCell2) :: me                                  !! The `GridCell` instance
        integer :: t                                            !! The timestep we're on
        type(Result) :: r                                       !! `Result` object to return errors in
        integer :: i, b                                            ! Iterator
        real(dp) :: lengthRatio                                 ! Reach length as a proportion of total river length
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3)) ! NP runoff for this time step
        real(dp) :: j_transformed_diffuseSource(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: j_dissolved_diffuseSource

        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then

            ! Reset variables
            me%j_np_diffuseSource = 0.0_dp
            j_transformed_diffuseSource = 0.0_dp
            j_dissolved_diffuseSource = 0.0_dp

            do i = 1, size(me%diffuseSources)
                call me%diffuseSources(i)%update(t)
                me%j_np_diffuseSource = me%j_np_diffuseSource + me%diffuseSources(i)%j_np_diffuseSource     ! [kg/m2/timestep]
                j_transformed_diffuseSource = j_transformed_diffuseSource + me%diffuseSources(i)%j_transformed_diffuseSource
                j_dissolved_diffuseSource = j_dissolved_diffuseSource + me%diffuseSources(i)%j_dissolved_diffuseSource
            end do

            ! Demands and transfers
            call r%addErrors([ &
                .errors. me%demands(), &
                .errors. me%transfers() &
            ])

            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            call r%addErrors( &
                .errors. me%colSoilProfiles(1)%item%update( &
                    t, &
                    me%j_np_diffuseSource, &
                    j_transformed_diffuseSource, &
                    j_dissolved_diffuseSource &
                ) &
            )
            me%erodedSediment = me%colSoilProfiles(1)%item%erodedSediment

            ! Reaches will be updated separately in reach routing order, by the `Environment` object
        end if

        ! Set flag to see we've run the update for this timestep
        me%isUpdated = .true.

        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOGR%toFile(errors = .errors. r)            ! Log any errors to the output file
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear the errors so we're not reporting twice
        call LOGR%toFile("Performing simulation for " // trim(me%ref) // " on time step #" // trim(str(t)) // ": success")
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another `RiverReach` whilst the reaches
    !! are looped through.
    subroutine finaliseUpdateGridCell2(me)
        class(GridCell2) :: me                                      !! This `GridCell2` instace
        integer :: rr                                               ! Iterator for reaches
        if (.not. me%isEmpty) then
            do rr = 1, me%nReaches
                call me%colRiverReaches(rr)%item%finaliseUpdate()
            end do
            me%isUpdated = .false.      ! Reset updated flag for the next timestep
        end if
    end subroutine

    !> Process the water demands for this `GridCell`
    function demandsGridCell2(me) result(r)
        class(GridCell2) :: me
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
    function transfersGridCell2(me) result(r)
        class(GridCell2) :: me
        type(Result) :: r
        ! Transfer some water!
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell2(me) result(r)
        class(GridCell2)        :: me                   !! This `GridCell2` object
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
        allocate(me%q_evap_timeSeries(C%nTimeSteps))
        allocate(me%q_precip_timeSeries(C%nTimeSteps))
        allocate(me%T_water_timeSeries(C%nTimeSteps))

        ! Get grid cell size from grid resolution
        me%dx = DATASET%gridRes(1)
        me%dy = DATASET%gridRes(2)
        me%area = me%dx * me%dy
        
        ! Set the data interfacer's group to the group for this GridCell
        ! call r%addErrors(.errors. DATA%setGroup([character(len=100)::'Environment', me%ref]))

        ! Get the number of waterbodies
        me%nReaches = DATASET%nWaterbodies(me%x, me%y)
        allocate(me%colRiverReaches(me%nReaches))
        allocate(me%reachTypes(me%nReaches))
        ! What are the types of those waterbodies?
        if (DATASET%isEstuary(me%x, me%y)) then
            me%reachTypes = 'est'
        else
            me%reachTypes = 'riv'
        end if

        ! Get hydrology, topography and water data
        ! call r%addErrors([ &
        !     ! .errors. DATA%get('runoff', me%q_runoff_timeSeries, 0.0_dp), &
        !     ! .errors. DATA%get('quickflow', me%q_quickflow_timeSeries, 0.0_dp), &
        !     ! .errors. DATA%get('precip', me%q_precip_timeSeries, 0.0_dp), &          ! [m/s]
        !     ! .errors. DATA%get('evap', me%q_evap_timeSeries, 0.0_dp), &
        !     .errors. DATA%get('slope', me%slope), &
        !     .errors. DATA%get('n_river', me%n_river, 0.035_dp), &
        !     .errors. DATA%get('T_water', me%T_water_timeSeries, C%defaultWaterTemperature, warnIfDefaulting=.false.) &
        ! ])
        ! TODO get the following from data
        me%slope = 0.0005
        me%n_river = 0.035_dp
        me%T_water_timeSeries = 10.0_dp
        ! FLAT DATA
        me%q_runoff_timeSeries = DATASET%runoff(me%x, me%y, :)
        me%q_precip_timeSeries = DATASET%precip(me%x, me%y, :)
        me%q_evap_timeSeries = DATASET%evap(me%x, me%y, :)

        ! TODO demands data (see commented out bit below)
        
        ! Try and set the group to the demands group. It will produce an error if group
        ! doesn't exist - use this to set me%hasDemands to .false.
        ! rslt = DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands'])
        ! if (.not. rslt%hasError()) then
        !     me%hasDemands = .true.
        !     ! Now get the data from the group. These should all default to zero.
        !     ! TODO What should the default surface water to total water ratio be?
        !     call r%addErrors([ &
        !         .errors. DATA%get('total_population', me%totalPopulation, 0.0_dp), &
        !         .errors. DATA%get('urban_population', me%urbanPopulation, 0.0_dp), &
        !         .errors. DATA%get('cattle_population', me%cattlePopulation, 0.0_dp), &
        !         .errors. DATA%get('sheep_goat_population', me%sheepGoatPopulation, 0.0_dp), &
        !         .errors. DATA%get('urban_demand', me%urbanDemandPerCapita, 0.0_dp), &
        !         .errors. DATA%get('rural_demand', me%ruralDemandPerCapita, 0.0_dp), &
        !         .errors. DATA%get('industrial_demand', me%industrialDemand, 0.0_dp), &
        !         .errors. DATA%get('sw_to_tw_ratio', me%surfaceWaterToTotalWaterRatio, 0.42_dp, warnIfDefaulting=.true.), &
        !         .errors. DATA%get('has_large_city', hasLargeCityInt, 0) &
        !     ])
        !     me%hasLargeCity = lgcl(hasLargeCityInt)     ! Convert int to bool
            
        !     ! Check if there are any crops to get. These will be retrieved iteratively
        !     ! (i.e. crop_1, crop_2, crop_3). Then get the data for those crops and create
        !     ! array of Crop objects in me%crops
        !     i = 1
        !     do while (DATA%grp%hasGroup("crop_" // trim(str(i))))
        !         allocate(me%crops(i))
        !         call r%addErrors(.errors. &
        !             DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands', 'crop_' // trim(str(i))]))
        !         call r%addErrors([ &
        !             .errors. DATA%get('crop_area', cropArea), &
        !             .errors. DATA%get('crop_type', cropType), &
        !             .errors. DATA%get('planting_month', cropPlantingMonth) &
        !         ])
        !         me%crops(i) = Crop(cropType, cropArea, cropPlantingMonth)
        !         i = i+1
        !     end do
        ! end if  

    end function

    subroutine parseNewBatchDataGridCell2(me)
        class(GridCell2) :: me          !! This grid cell instance
        integer :: i                    ! Iterators

        if (.not. me%isEmpty) then
            ! Allocate arrays to store flows in
            deallocate(me%q_runoff_timeSeries, &
                me%q_evap_timeSeries, &
                me%q_precip_timeSeries, &
                me%T_water_timeSeries)
            allocate(me%q_runoff_timeSeries(C%nTimeSteps))
            allocate(me%q_evap_timeSeries(C%nTimeSteps))
            allocate(me%q_precip_timeSeries(C%nTimeSteps))
            allocate(me%T_water_timeSeries(C%nTimeSteps))

            me%slope = 0.0005
            me%n_river = 0.035_dp
            me%T_water_timeSeries = 10.0_dp
            me%q_runoff_timeSeries = DATASET%runoff(me%x, me%y, :)
            me%q_precip_timeSeries = DATASET%precip(me%x, me%y, :)
            me%q_evap_timeSeries = DATASET%evap(me%x, me%y, :)

            ! Parse this batches soil data
            call me%colSoilProfiles(1)%item%parseNewBatchData()

            ! Number of point sources per grid cell might have changed, so we
            ! need to re-snap them to the closest reach
            call me%snapPointSourcesToReach()
            ! Now loop through reaches and alter size of j matrices to account
            ! for potentially different number of point sources
            do i = 1, me%nReaches
                call me%colRiverReaches(i)%item%parseNewBatchData()
            end do

            ! Reaches and sources don't need updating as they either
            ! get their data from grid cell, or directly from DATASET,
            ! which has already been updated.
        end if
    end subroutine

!---------------!
!--- GETTERS ---!
!---------------!

    function get_Q_outGridCell2(me, b) result(Q_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
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

    function get_j_spm_outGridCell2(me, b) result(j_spm_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
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

    function get_j_np_outGridCell2(me, b) result(j_np_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: j_np_out(C%nSizeClassesNP)
        j_np_out = 0
        ! TODO: Sum NP outflows here
    end function

    !> Get the total mass of SPM currently in the GridCell
    function get_m_spmGridCell2(me, b) result(m_spm)
        class(GridCell2) :: me                      !! This `GridCell2` instance
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
    function get_m_npGridCell2(me, b) result(m_np)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: m_np(C%nSizeClassesNP)
        m_np = 0
        ! TODO: Sum NP masses here
    end function

    !> Get the total length of all reaches in the cell
    function getTotalReachLengthGridCell2(me) result(totalReachLength)
        class(GridCell2)    :: me
        real(dp)            :: totalReachLength
        integer             :: r
        totalReachLength = 0
        ! Loop through reaches to get total length
        do r = 1, me%nReaches
            totalReachLength = totalReachLength + me%colRiverReaches(r)%item%length
        end do
    end function

    !> Get the a, b and c parameters of the straight line ax + bx + c = 0,
    !! for the reach with index i in this GridCell. From these line parameters,
    !! the distance to a point (source) can be calculated.
    function reachLineParamsFromInflowsOutflowGridCell2(me, i) result(lineParams)
        class(GridCell2)    :: me
        integer             :: i        !! The reach to calculate line equation for
        real                :: lineParams(3)
        integer             :: j
        integer             :: x_in, y_in, x_out, y_out
        real                :: x0, y0, x1, y1, a, b, c
        ! Calculate the point of the inflow and outflow of each reach
        if (me%colRiverReaches(i)%item%nInflows > 0) then
            x_in = me%colRiverReaches(i)%item%inflows(1)%item%x
            y_in = me%colRiverReaches(i)%item%inflows(1)%item%y
            x0 = (x_in + 0.5) + 0.5 * (me%x - x_in)
            y0 = (y_in + 0.5) + 0.5 * (me%y - y_in)
        else        ! Must be the centre of the cell (headwater)
            x0 = me%x + 0.5
            y0 = me%y + 0.5
        end if
        ! TODO calculate domainOutflow here as opposed to getting from data
        if (.not. me%colRiverReaches(i)%item%isDomainOutflow) then
            x_out = me%colRiverReaches(i)%item%outflow%item%x
            y_out = me%colRiverReaches(i)%item%outflow%item%y
        else
            x_out = DATASET%outflow(1, me%x, me%y)
            y_out = DATASET%outflow(2, me%x, me%y)
        end if
        x1 = (x_out + 0.5) + 0.5 * (me%x - x_out)
        y1 = (y_out + 0.5) + 0.5 * (me%y - y_out)
        ! Calculate the parameters to the general straight line
        ! ax + bx + c = 0 from this, which can be used to calculate
        ! distance to point
        if ((x1 - x0) /= 0) then
            a = -(y1 - y0)/(x1 - x0)
            b = 1
        else
            a = 1
            b = 0
        end if
        c = -(a * x0 + b * y0)
        lineParams = [a, b, c]
    end function
    
end module
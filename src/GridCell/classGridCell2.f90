module classGridCell2
    use Globals
    use UtilModule
    use classDatabase, only: DATASET
    use classLogger
    use ResultModule
    use spcGridCell
    use classSoilProfile1
    ! use classRiverReach
    use RiverReachModule
    ! use classEstuaryReach
    use EstuaryReachModule
    use classCrop
    implicit none

    !> Responsible for the creation of simulation of grid cells
    !! and contained compartments (e.g. rivers, soils).
    type, public, extends(GridCell) :: GridCell2
      contains
        ! Create/destroy
        procedure :: create => createGridCell2
        procedure :: finaliseCreate => finaliseCreateGridCell2
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
        procedure :: get_Q_outflow => get_Q_outflowGridCell2
        procedure :: get_j_spm_outflow => get_j_spm_outflowGridCell2
        procedure :: get_m_spm => get_m_spmGridCell2
        procedure :: get_j_spm_inflow => get_j_spm_inflowGridCell2
        procedure :: get_j_spm_soilErosion => get_j_spm_soilErosionGridCell2
        procedure :: get_j_spm_bankErosion => get_j_spm_bankErosionGridCell2
        procedure :: get_j_spm_deposition => get_j_spm_depositionGridCell2
        procedure :: get_j_spm_resuspension => get_j_spm_resuspensionGridCell2
        procedure :: get_m_np_water => get_m_np_waterGridCell2
        procedure :: get_m_transformed_water => get_m_transformed_waterGridCell2
        procedure :: get_m_dissolved_water => get_m_dissolved_waterGridCell2
        procedure :: get_C_spm => get_C_spmGridCell2
        procedure :: get_C_np_soil => get_C_np_soilGridCell2
        procedure :: get_C_np_water => get_C_np_waterGridCell2
        procedure :: get_C_np_sediment => get_C_np_sedimentGridCell2
        procedure :: get_C_np_sediment_byVolume => get_C_np_sediment_byVolumeGridCell2
        procedure :: get_C_np_sediment_l => get_C_np_sediment_lGridCell2
        procedure :: get_C_np_sediment_l_byVolume => get_C_np_sediment_l_byVolumeGridCell2
        procedure :: get_C_transformed_water => get_C_transformed_waterGridCell2
        procedure :: get_C_dissolved_water => get_C_dissolved_waterGridCell2
        procedure :: get_m_np_sediment => get_m_np_sedimentGridCell2
        procedure :: get_m_np_buried_sediment => get_m_np_buried_sedimentGridCell2
        procedure :: get_sediment_mass => get_sediment_massGridCell2
        procedure :: get_j_nm_deposition => get_j_nm_depositionGridCell2
        procedure :: get_j_transformed_deposition => get_j_transformed_depositionGridCell2
        procedure :: get_j_nm_resuspension => get_j_nm_resuspensionGridCell2
        procedure :: get_j_transformed_resuspension => get_j_transformed_resuspensionGridCell2
        procedure :: get_j_nm_outflow => get_j_nm_outflowGridCell2
        procedure :: get_j_transformed_outflow => get_j_transformed_outflowGridCell2
        procedure :: get_j_dissolved_outflow => get_j_dissolved_outflowGridCell2
        procedure :: getWaterVolume => getWaterVolumeGridCell2
        procedure :: getWaterDepth => getWaterDepthGridCell2
        procedure :: getBedSedimentArea => getBedSedimentAreaGridCell2
        procedure :: getBedSedimentMass => getBedSedimentMassGridCell2
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
        type(SoilProfile1)    :: soilProfile        ! The soil profile contained in this GridCell

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%colSoilProfiles(1))
        allocate(me%j_np_diffuseSource(C%npDim(1), C%npDim(2), C%npDim(3)))
        me%q_runoff = 0

        ! Set the GridCell's position, whether it's empty and its name
        me%x = x
        me%y = y
        if (present(isEmpty)) me%isEmpty = isEmpty      ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module
        me%nSoilProfiles = 0                            ! Default to no soil profiles
        
        ! Only carry on if there's stuff to be simulated for this GridCell
        if (.not. me%isEmpty) then

            ! If cell not empty, then create just one soil profile
            me%nSoilProfiles = 1
            
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
                    me%n_river, &
                    me%area, &
                    me%q_precip_timeSeries, &
                    me%q_evap_timeSeries &
                ) &
            )
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)
            me%distributionSediment = me%colSoilProfiles(1)%item%distributionSediment

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
    subroutine finaliseCreateGridCell2(me)
        class(GridCell2) :: me              !! This `GridCell2` instance
        integer :: i
        ! Snap point sources to the closest reach
        call me%snapPointSourcesToReach()
        ! Run each waterbody's finalise creation method, which at the moment
        ! just allocates variables (which need to be done after point sources
        ! were set up)
        do i = 1, me%nReaches
            call me%colRiverReaches(i)%item%finaliseCreate()
        end do
    end subroutine

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

    !> Create the reaches within this grid cell
    function createReaches(me) result(rslt)
        class(GridCell2), target :: me          !! This `GridCell2` instance
        type(Result) :: rslt                    !! The `Result` object to return any errors in
        integer :: i
        ! Loop through waterbodies and create them
        do i = 1, me%nReaches
            ! What type of waterbody is this?
            if (me%reachTypes(i) == 'riv') then
                allocate(RiverReach::me%colRiverReaches(i)%item)
            else if (me%reachTypes(i) == 'est') then
                allocate(EstuaryReach::me%colRiverReaches(i)%item)
            else
                call rslt%addError(ErrorInstance( &
                    message="Trying to create waterbody of unknown type " // trim(me%reachTypes(i)) // "." &
                ))
            end if
            ! Call creation method
            call rslt%addErrors(.errors. &
                me%colRiverReaches(i)%item%create(me%x, me%y, i, me%distributionSediment) &
            )
        end do
    end function

    !> Perform the simulations required for an individual time step
    subroutine updateGridCell2(me, t)
        class(GridCell2) :: me                                  !! The `GridCell` instance
        integer :: t                                            !! The timestep we're on
        type(Result) :: r                                       ! `Result` object
        integer :: i                                            ! Iterator
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
            ! TODO extend to multiple soil profiles
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
        call LOGR%toFile("Performing simulation for " // trim(me%ref) // " on time step #" // trim(str(t)) // ": success")
    end subroutine

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
        type(Result)            :: rslt                 ! Variable to store Result in
        ! integer                 :: hasLargeCityInt      ! Integer representation of hasLargeCity logical
        ! integer                 :: cropType             ! Temporary var to store crop type int in
        ! real(dp)                :: cropArea             ! Temporary var to store crop area in
        ! integer                 :: cropPlantingMonth    ! Temporary var to store crop planting month in


        ! Allocate arrays to store flows in
        allocate(me%q_runoff_timeSeries(C%nTimeSteps))
        allocate(me%q_evap_timeSeries(C%nTimeSteps))
        allocate(me%q_precip_timeSeries(C%nTimeSteps))
        allocate(me%T_water_timeSeries(C%nTimeSteps))

        ! Get grid cell size from grid resolution
        me%dx = DATASET%gridRes(1)
        me%dy = DATASET%gridRes(2)
        me%area = me%dx * me%dy
        
        ! Get the number of waterbodies
        me%nReaches = DATASET%nWaterbodies(me%x, me%y)
        allocate(me%colRiverReaches(me%nReaches))
        allocate(me%reachTypes(me%nReaches))
        ! What are the types of those waterbodies?
        ! Currently, all reach types in a cell must be the same, but the functionality to have
        ! different reach types exists (hence the aggregatedReachType variable)
        if (DATASET%isEstuary(me%x, me%y)) then
            me%reachTypes = 'est'
            me%aggregatedReachType = 'est'
        else
            me%reachTypes = 'riv'
            me%aggregatedReachType = 'riv'
        end if

        ! TODO get the following from data
            me%n_river = 0.035_dp
            me%T_water_timeSeries = 10.0_dp
            
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

            me%n_river = 0.035_dp
            me%T_water_timeSeries = 10.0_dp
            me%q_runoff_timeSeries = DATASET%runoff(me%x, me%y, :)
            me%q_precip_timeSeries = DATASET%precip(me%x, me%y, :)
            me%q_evap_timeSeries = DATASET%evap(me%x, me%y, :)

            ! Parse this batch's soil data
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

    !> Get the ouflow from this grid cell, which is the sum of the branch outflows
    function get_Q_outflowGridCell2(me) result(Q_outflow)
        class(GridCell2)    :: me               !! This `GridCell2` instance
        real(dp)            :: Q_outflow        !! Outflow from this grid cell [m3/timestep]
        integer             :: i                ! Iterator
        Q_outflow = 0
        ! Loop through the reaches and sum up the outflow from those that are a grid cell outflow
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellOutflow) then
                Q_outflow = Q_outflow + me%colRiverReaches(i)%item%Q%outflow
            end if
        end do
    end function

    !> Get the outflow of SPM from this grid cell
    function get_j_spm_outflowGridCell2(me) result(j_spm_outflow)
        class(GridCell2)    :: me                       !! This `GridCell2` instance
        real(dp)            :: j_spm_outflow(C%nSizeClassesSpm) !! Outflow from this grid cell [kg/timestep]
        integer             :: i                        ! Iterator
        j_spm_outflow = 0.0_dp
        ! Loop through reaches and sum the SPM outflow for the grid cell outflows
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellOutflow) then
                j_spm_outflow = j_spm_outflow + me%colRiverReaches(i)%item%Q%outflow
            end if
        end do
    end function

    !> Get the total mass of SPM currently in the GridCell
    function get_m_spmGridCell2(me) result(m_spm)
        class(GridCell2)    :: me                   !! This `GridCell2` instance
        real(dp)            :: m_spm(C%nSizeClassesSpm) !! SPM mass in this reach
        integer                 :: i                ! Iterator
        m_spm = 0.0_dp
        ! Loop through the reaches and sum the SPM masses
        do i = 1, me%nReaches
            m_spm = m_spm + me%colRiverReaches(i)%item%m_spm
        end do
    end function

    !> Get the mass of SPM inflowing to this grid cell
    function get_j_spm_inflowGridCell2(me) result(j_spm_inflow)
        class(GridCell2)    :: me               !! This grid cell instance
        real(dp)            :: j_spm_inflow(C%nSizeClassesSpm) ! Total mass of SPM inflowing [kg/timestep]
        integer             :: i                ! Iterator
        j_spm_inflow = 0.0_dp
        ! Loop through the inflows and sum the inflowing SPM
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellInflow) then
                j_spm_inflow = j_spm_inflow + me%colRiverReaches(i)%item%Q%inflow
            end if
        end do
    end function

    !> Get the total mass of eroded soil that reaches water bodies in this cell.
    !! Note this may be different to eroded yields from the soil profile due to the
    !! sediment transport capacity limited inputs to water bodies
    function get_j_spm_soilErosionGridCell2(me) result(j_spm_soilErosion)
        class(GridCell2)    :: me               !! This grid cell instance
        real(dp)            :: j_spm_soilErosion(C%nSizeClassesSpm) ! Total mass of soil erosion [kg/timestep]
        integer             :: i                ! Iterator
        j_spm_soilErosion = 0.0_dp
        ! Loop through water bodies and sum the eroded soil
        do i = 1, me%nReaches
            j_spm_soilErosion = j_spm_soilErosion + me%colRiverReaches(i)%item%j_spm%soilErosion
        end do
    end function

    !> Get the total mass of bank erosion into water bodies in this grid cell
    function get_j_spm_bankErosionGridCell2(me) result(j_spm_bankErosion)
        class(GridCell2)    :: me               !! This grid cell instance
        real(dp)            :: j_spm_bankErosion(C%nSizeClassesSpm) ! Total mass of bank erosion [kg/timestep]
        integer             :: i                ! Iterator
        j_spm_bankErosion = 0.0_dp
        ! Loop through water bodies and sum the bank erosion
        do i = 1, me%nReaches
            j_spm_bankErosion = j_spm_bankErosion + me%colRiverReaches(i)%item%j_spm%bankErosion
        end do
    end function

    !> Get the total mass of deposited SPM in this cell
    function get_j_spm_depositionGridCell2(me) result(j_spm_deposition)
        class(GridCell2)    :: me               !! This grid cell instance
        real(dp)            :: j_spm_deposition(C%nSizeClassesSpm) ! Total mass of deposited SPM [kg/timestep]
        integer             :: i                ! Iterator
        j_spm_deposition = 0.0_dp
        ! Loop through water bodies and sum the deposited SPM 
        do i = 1, me%nReaches
            j_spm_deposition = j_spm_deposition + me%colRiverReaches(i)%item%j_spm%deposition
        end do
    end function

    !> Get the total mass of resuspended SPM in this cell
    function get_j_spm_resuspensionGridCell2(me) result(j_spm_resuspension)
        class(GridCell2)    :: me               !! This grid cell instance
        real(dp)            :: j_spm_resuspension(C%nSizeClassesSpm) ! Total mass of resuspended SPM [kg/timestep]
        integer             :: i                ! Iterator
        j_spm_resuspension = 0.0_dp
        ! Loop through water bodies and sum the resuspended SPM 
        do i = 1, me%nReaches
            j_spm_resuspension = j_spm_resuspension + me%colRiverReaches(i)%item%j_spm%resuspension
        end do
    end function

    !> Get the total mass of NM currently in waterbodies in the GridCell
    function get_m_np_waterGridCell2(me) result(m_np)
        class(GridCell2)        :: me                      !! This `GridCell2` instance
        real(dp), allocatable   :: m_np(:,:,:)
        integer                 :: w
        allocate(m_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_np = 0.0_dp
        do w = 1, me%nReaches
            m_np = m_np + me%colRiverReaches(w)%item%m_np
        end do
    end function

    !> Get the total mass of transformed NM currently in waterbodies in the GridCell
    function get_m_transformed_waterGridCell2(me) result(m_transformed)
        class(GridCell2)        :: me                      !! This `GridCell2` instance
        real(dp), allocatable   :: m_transformed(:,:,:)
        integer                 :: w
        allocate(m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_transformed = 0.0_dp
        do w = 1, me%nReaches
            m_transformed = m_transformed + me%colRiverReaches(w)%item%m_transformed
        end do
    end function

    !> Get the total mass of dissolved species currently in the GridCell
    function get_m_dissolved_waterGridCell2(me) result(m_dissolved)
        class(GridCell2)    :: me                      !! This `GridCell2` instance
        real(dp)            :: m_dissolved
        integer             :: w
        m_dissolved = 0.0_dp
        do w = 1, me%nReaches
            m_dissolved = m_dissolved + me%colRiverReaches(w)%item%m_dissolved
        end do
    end function

    !> Get the total mass of NM currently in the sediment in the GridCell
    function get_m_np_sedimentGridCell2(me) result(m_np)
        class(GridCell2)    :: me                       !! This `GridCell2` instance
        real(dp), allocatable :: m_np(:,:,:)
        integer             :: w                        ! Waterbody iterator
        allocate(m_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_np = 0.0_dp
        do w = 1, me%nReaches
            associate (reach => me%colRiverReaches(w)%item) 
                m_np = m_np + reach%bedSediment%get_m_np() * reach%bedArea
            end associate
        end do
    end function
   
    !> Get the total mass of sediment in this grid cell
    function get_sediment_massGridCell2(me) result(sediment_mass) 
        class(GridCell2)    :: me                   !! This GridCell instance
        real(dp)            :: sediment_mass        !! Mass of sediment in grid cell [kg]
        integer             :: i                    ! Iterator
        sediment_mass = 0.0_dp
        do i = 1, me%nReaches
            sediment_mass = sediment_mass + me%colRiverReaches(i)%item%bedSediment%Mf_bed_all() &
                            * me%colRiverReaches(i)%item%bedArea
        end do
    end function

    !> Get the average SPM concentration in the grid cell, weighted by water volume in 
    !! each of the water bodies
    function get_C_spmGridCell2(me) result(C_spm)
        class(GridCell2)        :: me                       !! This grid cell
        real(dp), allocatable   :: C_spm(:)                 !! Average SPM concentration in grid cell
        real(dp)                :: C_spm_w(me%nReaches,C%nSizeClassesSpm)
        real(dp)                :: volumes(me%nReaches)
        integer                 :: i                        !! Iterator for water bodies
        allocate(C_spm(C%nSizeClassesSpm))
        ! Loop over the water bodies in this cell and get SPM and volume
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                C_spm_w(i, :) = reach%C_spm
                volumes(i) = reach%volume
            end associate
        end do
        ! Get the weighted average across the reaches, using the volumes as the weight
        C_spm = weightedAverage(C_spm_w, volumes)
    end function

    !> Get the a, b and c parameters of the straight line ax + bx + c = 0,
    !! for the reach with index i in this GridCell. From these line parameters,
    !! the distance to a point (source) can be calculated.
    function reachLineParamsFromInflowsOutflowGridCell2(me, i) result(lineParams)
        class(GridCell2)    :: me                           !! This GridCell
        integer             :: i                            !! The reach to calculate line equation for
        real                :: lineParams(3)                !! Line parameters to return
        integer             :: x_in, y_in, x_out, y_out     ! Inflow and outflow indices of this reach
        real                :: x0, y0, x1, y1, a, b, c      ! Inflow and outflow coords and line params
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
        ! Get the outflow i coords, whether it's in the model domain or not
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

    function get_C_np_soilGridCell2(me) result(C_np_soil)
        class(GridCell2)    :: me                                               !! This GridCell instance
        real(dp), allocatable :: C_np_soil(:,:,:)                               !! Mass concentration of NM in this GridCell [kg/kg soil]
        real(dp)            :: C_np_soil_p(me%nSoilProfiles, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per profile NM concentration [kg/kg soil]
        integer             :: i                                                ! Iterator 
        allocate(C_np_soil(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the soil profiles and get soil PEC
        ! TODO when multiple soil profiles implemented, make sure this gets the weighted average
        do i = 1, me%nSoilProfiles
            associate (profile => me%colSoilProfiles(i)%item)
                C_np_soil_p(i, :, :, :) = profile%get_C_np()
            end associate
        end do
        C_np_soil = divideCheckZero(sum(C_np_soil_p, dim=1), me%nSoilProfiles)
    end function

    !> Get the current weighted mean of NM conc in the water bodies in this grid cell,
    !! weighted by the current water volume in the cell
    function get_C_np_waterGridCell2(me) result(C_np_water)
        class(GridCell2)        :: me                                               !! This GridCell instance
        real(dp), allocatable   :: C_np_water(:,:,:)                                !! Mass concentration of NM in this GridCell [kg/m3]
        real(dp)                :: C_np_water_w(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per waterbody NM concentration [kg/m3]
        real(dp)                :: volumes(me%nReaches)                             ! Volumes [m3] of each reach, used for weighting
        integer                 :: i                                                ! Iterator 
        allocate(C_np_water(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get water PEC and volume
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                C_np_water_w(i, :, :, :) = reach%C_np
                volumes(i) = reach%volume
            end associate
        end do
        ! Get the weighted average across the reaches, using the volumes as the weight
        C_np_water = weightedAverage(C_np_water_w, volumes)
    end function
   
    !> Get the current weighted mean sediment PEC [kg/kg] in this grid cell,
    !! weighted by the current sediment masses in the cell
    function get_C_np_sedimentGridCell2(me) result(C_np_sediment)
        class(GridCell2)        :: me                                                   !! This GridCell instance
        real(dp), allocatable   :: C_np_sediment(:,:,:)                                 !! Mass concentration of NM in this GridCell's sediment [kg/kg]
        real(dp)                :: C_np_sediment_b(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per sediment NM concentration [kg/kg]
        real(dp)                :: sedimentMasses(me%nReaches)                          ! Mass of sediment in each reach, used to weight average [kg]
        integer                 :: i                                                    ! Iterator
        allocate(C_np_sediment(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get sediment PEC and bed area
        do i = 1, me%nReaches
            associate (bedSediment => me%colRiverReaches(i)%item%bedSediment)
                ! Get the NM PEC [kg/kg] for each sediment
                C_np_sediment_b(i, :, :, :) = bedSediment%get_C_np_byMass()
                ! Get the sediment mass from BedSediment [kg/m2] and multiply by bed area to give total mass
                sedimentMasses(i) = bedSediment%Mf_bed_all() * me%colRiverReaches(i)%item%bedArea
            end associate
        end do
        ! Get the weighted mean across the bed sediments, using sediment mass as the weight
        C_np_sediment = weightedAverage(C_np_sediment_b, sedimentMasses)
    end function

    !> Get the current weighted mean sediment PEC [kg/m3] in this grid cell,
    !! weighted by the current volume of sediment in the grid cell
    function get_C_np_sediment_byVolumeGridCell2(me) result(C_np_sediment)
        class(GridCell2)        :: me                                                   !! This GridCell instance
        real(dp), allocatable   :: C_np_sediment(:,:,:)                                 !! Volume concentration of NM in this GridCell's sediment [kg/m3]
        real(dp)                :: C_np_sediment_b(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per sediment NM concentration [kg/m3]
        real(dp)                :: sedimentVolumes(me%nReaches)                         ! Volume of sediment in each reach, used to weight average [m3]
        integer                 :: i                                                    ! Iterator
        allocate(C_np_sediment(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get sediment PEC and bed area
        do i = 1, me%nReaches
            associate (bedSediment => me%colRiverReaches(i)%item%bedSediment)
                ! Get the NM PEC [kg/m3] for each sediment
                C_np_sediment_b(i, :, :, :) = bedSediment%get_C_np()
                ! Calculate the sediment volume from the bed area and depth
                sedimentVolumes(i) = me%colRiverReaches(i)%item%bedArea * sum(C%sedimentLayerDepth)
            end associate
        end do
        ! Get the weighted mean across the bed sediments, using sediment mass as the weight
        C_np_sediment = weightedAverage(C_np_sediment_b, sedimentVolumes)
    end function

    !> Get the current weighted mean sediment PEC [kg/m3] for sediment layer l,
    !! weighted by the current volume of sediment layer l in the grid cell
    function get_C_np_sediment_l_byVolumeGridCell2(me, l) result(C_np_sediment)
        class(GridCell2)        :: me                                                   !! This GridCell instance
        integer                 :: l                                                    !! Sediment layer index
        real(dp), allocatable   :: C_np_sediment(:,:,:)                                 !! Volume concentration of NM in this GridCell's sediment [kg/m3]
        real(dp)                :: C_np_sediment_b(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per sediment NM concentration [kg/m3]
        real(dp)                :: sedimentVolumes(me%nReaches)                         ! Volume of sediment in each reach, used to weight average [m3]
        integer                 :: i                                                    ! Iterator
        allocate(C_np_sediment(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get sediment PEC and bed area for layer l
        do i = 1, me%nReaches
            associate (bedSediment => me%colRiverReaches(i)%item%bedSediment)
                ! Get the NM PEC [kg/m3] for each layer
                C_np_sediment_b(i, :, :, :) = bedSediment%get_C_np_l(l)
                ! Calculate the sediment volume from the bed area and layer depth
                sedimentVolumes(i) = me%colRiverReaches(i)%item%bedArea * C%sedimentLayerDepth(l)
            end associate
        end do
        ! Get the weighted mean across the sediment layers, using sediment mass as the weight
        C_np_sediment = weightedAverage(C_np_sediment_b, sedimentVolumes)
    end function

    !> Get the current weighted mean sediment PEC [kg/kg] for sediment layer l,
    !! weighted by the current mass of sediment in layers
    function get_C_np_sediment_lGridCell2(me, l) result(C_np_sediment)
        class(GridCell2)        :: me                                                   !! This GridCell instance
        integer                 :: l                                                    !! Sediment layer index
        real(dp), allocatable   :: C_np_sediment(:,:,:)                                 !! Mass concentration of NM in this GridCell's sediment [kg/kg]
        real(dp)                :: C_np_sediment_b(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per sediment NM concentration [kg/kg]
        real(dp)                :: sedimentMasses(me%nReaches)                          ! Mass of sediment layer l in each reach, used to weight average [kg]
        integer                 :: i                                                    ! Iterator
        allocate(C_np_sediment(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get sediment PEC and bed area for layer l
        do i = 1, me%nReaches
            associate (bedSediment => me%colRiverReaches(i)%item%bedSediment)
                ! Get the NM PEC [kg/m3] for each layer
                C_np_sediment_b(i, :, :, :) = bedSediment%get_C_np_l(l)
                ! Calculate the sediment volume from the bed area and layer depth
                sedimentMasses(i) = bedSediment%Mf_bed_by_layer(l) * me%colRiverReaches(i)%item%bedArea
            end associate
        end do
        ! Get the weighted mean across the sediment layers, using sediment mass as the weight
        C_np_sediment = weightedAverage(C_np_sediment_b, sedimentMasses)
    end function

    !> Get the current weighted mean of transformed NM conc in the water bodies in this grid cell,
    !! weighted by the current water volume in the cell
    function get_C_transformed_waterGridCell2(me) result(C_transformed_water)
        class(GridCell2)        :: me                                               !! This GridCell instance
        real(dp), allocatable   :: C_transformed_water(:,:,:)                       !! Mass concentration of NM in this GridCell [kg/m3]
        real(dp)                :: C_transformed_water_w(me%nReaches, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per waterbody NM concentration [kg/m3]
        real(dp)                :: volumes(me%nReaches)                             ! Volumes [m3] of each reach, used for weighting
        integer                 :: i                                                ! Iterator 
        allocate(C_transformed_water(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Loop over the water bodies in this cell and get water PEC and volume
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                C_transformed_water_w(i, :, :, :) = reach%C_transformed
                volumes(i) = reach%volume
            end associate
        end do
        ! Get the weighted average across the reaches, using the volumes as the weight
        C_transformed_water = weightedAverage(C_transformed_water_w, volumes)
    end function

    !> Get the current weighted mean of dissolved species conc in the water bodies in this grid cell,
    !! weighted by the current water volume in the cell
    function get_C_dissolved_waterGridCell2(me) result(C_dissolved_water)
        class(GridCell2)    :: me                                   !! This GridCell instance
        real(dp)            :: C_dissolved_water                    !! Mass concentration of NM in this GridCell [kg/m3]
        real(dp)            :: C_dissolved_water_w(me%nReaches)     ! Per waterbody NM concentration [kg/m3]
        real(dp)            :: volumes(me%nReaches)                 ! Volumes [m3] of each reach, used for weighting
        integer             :: i                                    ! Iterator 
        ! Loop over the waterbodies in this cell and get water PEC and volume
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                C_dissolved_water_w(i) = reach%C_dissolved
                volumes(i) = reach%volume
            end associate
        end do
        ! Get the weighted average across the reaches, using the volumes as the weight
        C_dissolved_water = weightedAverage(C_dissolved_water_w, volumes)
    end function

    !> Get the mass of NM buried for all the bed sediments in this grid cell
    function get_m_np_buried_sedimentGridCell2(me) result(m_np_buried)
        class(GridCell2)        :: me                   ! This GridCell instance
        real(dp), allocatable   :: m_np_buried(:,:,:)   ! Mass of NM buried [kg]
        integer                 :: i                    ! Iterator
        allocate(m_np_buried(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_np_buried = 0.0_dp
        ! Loop over the waterbodies in this cell and get mass of sediment buried
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                m_np_buried = m_np_buried &
                    + reach%bedSediment%get_m_np_buried() * reach%bedArea
            end associate
        end do
    end function

    !> Get the sum of MN deposition for this grid cell 
    function get_j_nm_depositionGridCell2(me) result(j_nm_deposition)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_nm_deposition(:,:,:)   !! The NM deposited
        integer                 :: i                        ! Iterator
        allocate(j_nm_deposition(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_nm_deposition = 0.0_dp
        ! Loop over the water bodies and sum up the deposited NM 
        do i = 1, me%nReaches
            j_nm_deposition = j_nm_deposition + me%colRiverReaches(i)%item%j_nm%deposition
        end do
    end function

    !> Get the sum of MN deposition for this grid cell 
    function get_j_transformed_depositionGridCell2(me) result(j_transformed_deposition)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_transformed_deposition(:,:,:)   !! The transformed NM deposited
        integer                 :: i                        ! Iterator
        allocate(j_transformed_deposition(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_transformed_deposition = 0.0_dp
        ! Loop over the water bodies and sum up the deposited transformed NM 
        do i = 1, me%nReaches
            j_transformed_deposition = j_transformed_deposition + me%colRiverReaches(i)%item%j_nm_transformed%deposition
        end do
    end function

    !> Get the sum of NM resuspended for this grid cell 
    function get_j_nm_resuspensionGridCell2(me) result(j_nm_resuspension)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_nm_resuspension(:,:,:) !! The NM resuspended
        integer                 :: i                        ! Iterator
        allocate(j_nm_resuspension(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_nm_resuspension = 0.0_dp
        ! Loop over the water bodies in this cell sum the resuspended NM 
        do i = 1, me%nReaches
            j_nm_resuspension = j_nm_resuspension + me%colRiverReaches(i)%item%j_nm%resuspension
        end do
    end function

    !> Get the sum of transformed NM resuspended for this grid cell 
    function get_j_transformed_resuspensionGridCell2(me) result(j_transformed_resuspension)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_transformed_resuspension(:,:,:) !! The NM resuspended
        integer                 :: i                        ! Iterator
        allocate(j_transformed_resuspension(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_transformed_resuspension = 0.0_dp
        ! Loop over the water bodies in this cell sum the resuspended transformed NM 
        do i = 1, me%nReaches
            j_transformed_resuspension = j_transformed_resuspension + me%colRiverReaches(i)%item%j_nm_transformed%resuspension
        end do
    end function

    !> Get the sum of NM outflowing from this grid cell 
    function get_j_nm_outflowGridCell2(me) result(j_nm_outflow)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_nm_outflow(:,:,:)      !! The NM outflowing
        integer                 :: i                        ! Iterator
        allocate(j_nm_outflow(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_nm_outflow = 0.0_dp
        ! Loop over the water bodies in this cell and sum outflows if they are grid cell outflows 
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                if (reach%isGridCellOutflow) then
                    j_nm_outflow = j_nm_outflow + reach%j_nm%outflow
                end if
            end associate
        end do
    end function

    !> Get the sum of transformed NM outflowing from this grid cell 
    function get_j_transformed_outflowGridCell2(me) result(j_transformed_outflow)
        class(GridCell2)        :: me                       !! This GridCell instance
        real(dp), allocatable   :: j_transformed_outflow(:,:,:) !! The transformed NM outflowing
        integer                 :: i                        ! Iterator
        allocate(j_transformed_outflow(C%npDim(1), C%npDim(2), C%npDim(3)))
        j_transformed_outflow = 0.0_dp
        ! Loop over the water bodies in this cell and sum outflows if they are grid cell outflows 
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                if (reach%isGridCellOutflow) then
                    j_transformed_outflow = j_transformed_outflow + reach%j_nm_transformed%outflow
                end if
            end associate
        end do
    end function

    !> Get the sum of dissolved species outflowing from this grid cell 
    function get_j_dissolved_outflowGridCell2(me) result(j_dissolved_outflow)
        class(GridCell2)    :: me                       !! This GridCell instance
        real(dp)            :: j_dissolved_outflow      !! The dissolved species outflowing
        integer             :: i                        ! Iterator
        j_dissolved_outflow = 0.0_dp
        ! Loop over the water bodies in this cell and sum outflows if they are grid cell outflows 
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                if (reach%isGridCellOutflow) then
                    j_dissolved_outflow = j_dissolved_outflow + reach%j_dissolved%outflow
                end if
            end associate
        end do
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

    !> Get the total volume of water [m3] in this grid cell
    function getWaterVolumeGridCell2(me) result(waterVolume)
        class(GridCell2)    :: me               !! This GridCell instance
        real(dp)            :: waterVolume      !! Water volume [m3] 
        integer             :: i                ! Iterator
        waterVolume = 0.0_dp
        do i = 1, me%nReaches
            waterVolume = waterVolume + me%colRiverReaches(i)%item%volume
        end do
    end function

    !> Get the average depth of water [m] in this grid cell, weighted by reach lengths
    function getWaterDepthGridCell2(me) result(waterDepth)
        class(GridCell2)    :: me
        real(dp)            :: waterDepth
        real(dp)            :: waterDepth_i(me%nReaches)
        real(dp)            :: lengths(me%nReaches)
        integer             :: i
        ! Loop over reaches and get their depths and lengths
        do i = 1, me%nReaches
            waterDepth_i(i) = me%colRiverReaches(i)%item%depth
            lengths(i) = me%colRiverReaches(i)%item%length
        end do
        waterDepth = weightedAverage(waterDepth_i, lengths)
    end function

    !> Get the total bed sediment area [m2] in this grid cell
    function getBedSedimentAreaGridCell2(me) result(bedArea)
        class(GridCell2)    :: me               !! This GridCell instance
        real(dp)            :: bedArea          !! Bed sediment area [m2]
        integer             :: i                ! Iterator
        bedArea = 0.0_dp
        do i = 1, me%nReaches
            bedArea = bedArea + me%colRiverReaches(i)%item%bedArea
        end do
    end function

    !> Get the total mass of sediment [kg] in this grid cell
    function getBedSedimentMassGridCell2(me) result(sedimentMass)
        class(GridCell2)    :: me               !! This GridCell instance
        real(dp)            :: sedimentMass     !! Bed sediment mass [kg]
        integer             :: i                ! Iterator
        sedimentMass = 0.0_dp
        ! Loop over the reaches and sum the total masses of sediment in each reach
        do i = 1, me%nReaches
            sedimentMass = sedimentMass &
                + me%colRiverReaches(i)%item%bedSediment%Mf_bed_all() &             ! Sediment mass in this reach, kg/m2
                * me%colRiverReaches(i)%item%bedArea                                ! Mutiply by bed area to get total mass in this reach 
        end do
    end function

end module
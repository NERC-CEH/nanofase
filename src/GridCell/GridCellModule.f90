module GridCellModule
    use GlobalsModule
    use UtilModule
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    use ResultModule
    use AbstractGridCellModule
    use SoilProfileModule
    use RiverReachModule
    use EstuaryReachModule
    use CropModule
    use ContaminantModule
    implicit none

    !> Responsible for the creation and simulation of grid cells
    !! and contained compartments (e.g., rivers, soils).
    type, public, extends(AbstractGridCell) :: GridCell
      contains
        ! Create/destroy
        procedure :: create => createGridCell
        procedure :: finaliseCreate => finaliseCreateGridCell
        procedure, private :: createReaches
        procedure :: snapPointSourcesToReach => snapPointSourcesToReachGridCell
        ! Simulators
        procedure :: update => updateGridCell
        procedure :: finaliseUpdate => finaliseUpdateGridCell
        procedure :: demands => demandsGridCell
        procedure :: transfers => transfersGridCell
        ! Data handlers
        procedure :: parseInputData => parseInputDataGridCell
        procedure :: parseNewBatchData => parseNewBatchDataGridCell
        ! Getters
        procedure :: get_Q_outflow => get_Q_outflowGridCell
        procedure :: get_j_spm_outflow => get_j_spm_outflowGridCell
        procedure :: get_m_spm => get_m_spmGridCell
        procedure :: get_j_spm_inflow => get_j_spm_inflowGridCell
        procedure :: get_j_spm_soilErosion => get_j_spm_soilErosionGridCell
        procedure :: get_j_spm_bankErosion => get_j_spm_bankErosionGridCell
        procedure :: get_j_spm_deposition => get_j_spm_depositionGridCell
        procedure :: get_j_spm_resuspension => get_j_spm_resuspensionGridCell
        procedure :: get_m_contaminant_water => get_m_contaminant_waterGridCell
        procedure :: get_C_spm => get_C_spmGridCell
        procedure :: get_C_contaminant_soil => get_C_contaminant_soilGridCell
        procedure :: get_C_contaminant_water => get_C_contaminant_waterGridCell
        procedure :: get_C_contaminant_sediment => get_C_contaminant_sedimentGridCell
        procedure :: get_C_contaminant_sediment_byVolume => get_C_contaminant_sediment_byVolumeGridCell
        procedure :: get_C_contaminant_sediment_l => get_C_contaminant_sediment_lGridCell
        procedure :: get_C_contaminant_sediment_l_byVolume => get_C_contaminant_sediment_l_byVolumeGridCell
        procedure :: get_m_contaminant_sediment => get_m_contaminant_sedimentGridCell
        procedure :: get_m_contaminant_buried_sediment => get_m_contaminant_buried_sedimentGridCell
        procedure :: get_sediment_mass => get_sediment_massGridCell
        procedure :: get_j_contaminant_deposition => get_j_contaminant_depositionGridCell
        procedure :: get_j_contaminant_resuspension => get_j_contaminant_resuspensionGridCell
        procedure :: get_j_contaminant_outflow => get_j_contaminant_outflowGridCell
        procedure :: getWaterVolume => getWaterVolumeGridCell
        procedure :: getWaterDepth => getWaterDepthGridCell
        procedure :: getBedSedimentArea => getBedSedimentAreaGridCell
        procedure :: getBedSedimentMass => getBedSedimentMassGridCell
        procedure :: get_C_dissolved_water => get_C_dissolved_waterGridCell
        procedure :: getTotalReachLength => getTotalReachLengthGridCell
        ! Calculators
        procedure :: reachLineParamsFromInflowsOutflow => reachLineParamsFromInflowsOutflowGridCell
    end type

  contains

    !> Create a GridCell with coordinates x and y.
    function createGridCell(me, x, y, isEmpty) result(rslt)
        class(GridCell), target :: me                   !! The `GridCell` instance.
        type(Result)            :: rslt                 !! The `Result` object to return.
        integer                 :: x, y                 !! Spatial index of the grid cell
        logical, optional       :: isEmpty              !! Is anything to be simulated in this `GridCell`?
        type(SoilProfile)       :: soilProfile          ! The soil profile contained in this GridCell
        type(Result)            :: rslt_temp            ! Temporary Result for error handling
        character(len=100)      :: compartment          ! Compartment for contaminant initialization
        character(len=7) :: comp_wat

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%colSoilProfiles(1))
        allocate(me%j_contaminant_diffuseSource(2)) ! Two diffuse sources (soil, atmospheric)
        if (me%aggregatedReachType == 'riv') then
            comp_wat = 'water'//repeat(' ',2)   ! make it length=7
        else
            comp_wat = 'estuary'
        end if
        rslt_temp = me%contaminant_water%create_from_data( &
            DATASET%nc, &
            trim(comp_wat), &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        call rslt%addErrors(.errors. rslt_temp)
        rslt_temp = me%contaminant_sediment%create_from_data( &
            DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        call rslt%addErrors(.errors. rslt_temp)
        rslt_temp = me%j_contaminant_diffuseSource(1)%create_from_data( &
            DATASET%nc, 'soil', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        call rslt%addErrors(.errors. rslt_temp)
        rslt_temp = me%j_contaminant_diffuseSource(2)%create_from_data( &
            DATASET%nc, 'atmospheric', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        call rslt%addErrors(.errors. rslt_temp)
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
            call me%parseInputData()

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
                    me%q_precip_timeseries, &
                    me%q_evap_timeseries &
                ))
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)
            allocate(me%distributionSediment, source=me%colSoilProfiles(1)%item%distributionSediment)

            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. rslt%hasCriticalError()) then
                ! Add river reaches to the grid cell (if any are present in the data file)
                call rslt%addErrors(.errors. me%createReaches())
            end if
        end if

        call rslt%addToTrace("Creating " // trim(me%ref))
        call LOGR%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
        call rslt%clear()                  ! Clear errors from the Result object so they're not reported twice
        if (.not. me%isEmpty) then
            call LOGR%toConsole(" > Creating " // trim(me%ref) // ": "//COLOR_GREEN//"success"//COLOR_RESET)
            call LOGR%toFile("Creating " // trim(me%ref) // ": success")
        else
            call LOGR%toConsole(" > Creating " // trim(me%ref) // ": "//COLOR_GREEN//"empty"//COLOR_RESET)
            call LOGR%toFile("Creating " // trim(me%ref) // ": empty")
        end if
    end function

    !> Finalise creation should be done after routing is complete, and is meant for
    !! procedures that rely on waterbodies being linked to their inflows/outflow
    subroutine finaliseCreateGridCell(me)
        class(GridCell) :: me               !! This GridCell instance
        integer         :: i                ! Iterator
        ! Snap point sources to the closest reach
        call me%snapPointSourcesToReach()
        ! Run each waterbody's finalise creation method, which at the moment
        ! just allocates variables (which need to be done after point sources
        ! were set up)
        do i = 1, me%nReaches
            call me%colRiverReaches(i)%item%finaliseCreate()
        end do
    end subroutine

    subroutine snapPointSourcesToReachGridCell(me)
        class(GridCell)     :: me                       !! The GridCell instance
        integer             :: i, j                     ! Iterators
        real, allocatable   :: lineParams(:,:)
        real, allocatable   :: distanceToReach(:)
        real                :: x0, y0
        real                :: fracIndices(2)
        integer             :: reachIndexToSnapTo

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
                fracIndices = DATASET%coordsToFractionalCellIndex(x0, y0)
                do i = 1, me%nReaches
                    ! Calculate distance from point given by fracIndices and the line
                    ! with params lineParams(i,:)
                    distanceToReach(i) = abs(lineParams(i,1) * fracIndices(1) + lineParams(i,2) * fracIndices(2) &
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
        class(GridCell), target :: me           !! This GridCell instance
        type(Result) :: rslt                    !! The Result object to return any errors in
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
    subroutine updateGridCell(me, t, isWarmUp)
        class(GridCell) :: me               !! The GridCell instance
        integer         :: t                !! The timestep we're on
        logical         :: isWarmUp         !! Are we in a warm up period?
        type(Result)    :: r                ! Result object
        integer         :: i                ! Iterator
        type(Contaminant) :: temp_contaminant
        character(len=100) :: compartment

        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            do i = 1, size(me%j_contaminant_diffuseSource)
                call me%j_contaminant_diffuseSource(i)%finalise()
                compartment = merge('soil       ', 'atmospheric', i == 1)  ! pad 'soil' to length 11
                call r%addErrors(.errors. me%j_contaminant_diffuseSource(i)%create_from_data( &
                    DATASET%nc, &
                    compartment, &
                    DATASET%contaminantDensity, &
                    DATASET%soilConstantAttachmentEfficiency, &
                    DATASET%riverAttachmentEfficiency, &
                    DATASET%estuaryAttachmentEfficiency, &
                    DATASET%contaminant_k_diss_pristine, &
                    DATASET%contaminant_k_diss_transformed, &
                    DATASET%contaminant_k_transform_pristine, &
                    real(DATASET%waterTemperature(1), dp)))
            end do

            ! Only input Contaminant if we're not in a warm up period
            if (.not. isWarmUp) then
                do i = 1, size(me%diffuseSources)
                    call me%diffuseSources(i)%update(t)
                    temp_contaminant = me%diffuseSources(i)%j_contaminant
                    call me%j_contaminant_diffuseSource(i)%add(temp_contaminant)
                end do
            end if

            ! Demands and transfers
            call r%addErrors([ &
                .errors. me%demands(), &
                .errors. me%transfers() &
            ])

            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            ! TODO extend to multiple soil profiles
            call r%addErrors(.errors. me%colSoilProfiles(1)%item%update(t, me%j_contaminant_diffuseSource(1)))
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

    !> Set the outflow from the temporary outflow variables that were set by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another `RiverReach` whilst the reaches
    !! are looped through.
    subroutine finaliseUpdateGridCell(me)
        class(GridCell) :: me               !! This GridCell instance
        integer         :: rr               ! Iterator for reaches
        if (.not. me%isEmpty) then
            do rr = 1, me%nReaches
                call me%colRiverReaches(rr)%item%finaliseUpdate()
            end do
            me%isUpdated = .false.      ! Reset updated flag for the next timestep
        end if
    end subroutine

    !> Process the water demands for this `GridCell`
    function demandsGridCell(me) result(r)
        class(GridCell) :: me
        type(Result)    :: r
        integer         :: pcLossUrban = 0                      ! TODO where should this come from?
        integer         :: pcLossRural = 0                      ! TODO where should this come from?
        integer         :: pcLossLivestockConsumption = 10      ! TODO where should this come from?
        real(dp)        :: cattleDemandPerCapita = 140         ! TODO where should this come from?
        real(dp)        :: sheepGoatDemandPerCapita = 70       ! TODO where should this come from?
        real(dp)        :: totalUrbanDemand
        real(dp)        :: totalLivestockDemand
        real(dp)        :: totalRuralDemand
        
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
    
    !> Process the water abstractions and transfers for this GridCell
    function transfersGridCell(me) result(r)
        class(GridCell) :: me
        type(Result)    :: r
        ! Transfer some water!
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    subroutine parseInputDataGridCell(me)
        class(GridCell) :: me
        allocate(me%q_runoff_timeSeries(C%nTimeSteps))
        allocate(me%q_evap_timeSeries(C%nTimeSteps))
        allocate(me%q_precip_timeSeries(C%nTimeSteps))
        allocate(me%T_water_timeSeries(C%nTimeSteps))
        me%dx = DATASET%gridRes(1)
        me%dy = DATASET%gridRes(2)
        me%area = me%dx * me%dy
        me%nReaches = DATASET%nWaterbodies(me%x, me%y)
        allocate(me%colRiverReaches(me%nReaches))
        allocate(me%reachTypes(me%nReaches))
        if (DATASET%isEstuary(me%x, me%y)) then
            me%reachTypes = 'est'
            me%aggregatedReachType = 'est'
        else
            me%reachTypes = 'riv'
            me%aggregatedReachType = 'riv'
        end if
        me%n_river = 0.035_dp
        me%T_water_timeSeries = 10.0_dp
        me%q_runoff_timeSeries = DATASET%runoff(me%x, me%y, :)
        me%q_precip_timeSeries = DATASET%precip(me%x, me%y, :)
        me%q_evap_timeSeries = DATASET%evap(me%x, me%y, :)
    end subroutine

    subroutine parseNewBatchDataGridCell(me)
        class(GridCell) :: me          !! This grid cell instance
        integer :: i                   ! Iterators

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
        end if
    end subroutine

!---------------!
!--- GETTERS ---!
!---------------!

    !> Get the outflow from this grid cell, which is the sum of the branch outflows
    function get_Q_outflowGridCell(me) result(Q_outflow)
        class(GridCell) :: me               !! This `GridCell` instance
        real(dp)        :: Q_outflow        !! Outflow from this grid cell [m3/timestep]
        integer         :: i                ! Iterator
        Q_outflow = 0
        ! Loop through the reaches and sum up the outflow from those that are a grid cell outflow
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellOutflow) then
                Q_outflow = Q_outflow + me%colRiverReaches(i)%item%Q%outflow
            end if
        end do
    end function

    !> Get the outflow of SPM from this grid cell
    function get_j_spm_outflowGridCell(me) result(j_spm_outflow)
        class(GridCell) :: me                       !! This `GridCell` instance
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)    !! Outflow from this grid cell [kg/timestep]
        integer         :: i                        ! Iterator
        j_spm_outflow = 0.0_dp
        ! Loop through reaches and sum the SPM outflow for the grid cell outflows
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellOutflow) then
                j_spm_outflow = j_spm_outflow + me%colRiverReaches(i)%item%j_spm%outflow
            end if
        end do
    end function

    !> Get the total mass of SPM currently in the GridCell
    function get_m_spmGridCell(me) result(m_spm)
        class(GridCell) :: me                   !! This `GridCell` instance
        real(dp)        :: m_spm(C%nSizeClassesSpm) !! SPM mass in this reach
        integer         :: i                ! Iterator
        m_spm = 0.0_dp
        ! Loop through the reaches and sum the SPM masses
        do i = 1, me%nReaches
            m_spm = m_spm + me%colRiverReaches(i)%item%m_spm
        end do
    end function

    !> Get the mass of SPM inflowing to this grid cell
    function get_j_spm_inflowGridCell(me) result(j_spm_inflow)
        class(GridCell) :: me               !! This grid cell instance
        real(dp)        :: j_spm_inflow(C%nSizeClassesSpm)! Total mass of SPM inflowing [kg/timestep]
        integer         :: i                ! Iterator
        j_spm_inflow = 0.0_dp
        ! Loop through the inflows and sum the inflowing SPM
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellInflow) then
                j_spm_inflow = j_spm_inflow + me%colRiverReaches(i)%item%j_spm%inflow
            end if
        end do
    end function

    !> Get the total mass of eroded soil that reaches water bodies in this cell.
    !! Note this may be different to eroded yields from the soil profile due to the
    !! sediment transport capacity limited inputs to water bodies
    function get_j_spm_soilErosionGridCell(me) result(j_spm_soilErosion)
        class(GridCell) :: me               !! This grid cell instance
        real(dp)        :: j_spm_soilErosion(C%nSizeClassesSpm) ! Total mass of soil erosion [kg/timestep]
        integer         :: i                ! Iterator
        j_spm_soilErosion = 0.0_dp
        ! Loop through water bodies and sum the eroded soil
        do i = 1, me%nReaches
            j_spm_soilErosion = j_spm_soilErosion + me%colRiverReaches(i)%item%j_spm%soilErosion
        end do
    end function

    !> Get the total mass of bank erosion into water bodies in this grid cell
    function get_j_spm_bankErosionGridCell(me) result(j_spm_bankErosion)
        class(GridCell) :: me               !! This grid cell instance
        real(dp)        :: j_spm_bankErosion(C%nSizeClassesSpm) ! Total mass of bank erosion [kg/timestep]
        integer         :: i                ! Iterator
        j_spm_bankErosion = 0.0_dp
        ! Loop through water bodies and sum the bank erosion
        do i = 1, me%nReaches
            j_spm_bankErosion = j_spm_bankErosion + me%colRiverReaches(i)%item%j_spm%bankErosion
        end do
    end function

    !> Get the total mass of deposited SPM in this cell
    function get_j_spm_depositionGridCell(me) result(j_spm_deposition)
        class(GridCell) :: me               !! This grid cell instance
        real(dp)        :: j_spm_deposition(C%nSizeClassesSpm) ! Total mass of deposited SPM [kg/timestep]
        integer         :: i                ! Iterator
        j_spm_deposition = 0.0_dp
        ! Loop through water bodies and sum the deposited SPM 
        do i = 1, me%nReaches
            j_spm_deposition = j_spm_deposition + me%colRiverReaches(i)%item%j_spm%deposition
        end do
    end function

    !> Get the total mass of resuspended SPM in this cell
    function get_j_spm_resuspensionGridCell(me) result(j_spm_resuspension)
        class(GridCell) :: me               !! This grid cell instance
        real(dp)        :: j_spm_resuspension(C%nSizeClassesSpm) ! Total mass of resuspended SPM [kg/timestep]
        integer         :: i                ! Iterator
        j_spm_resuspension = 0.0_dp
        ! Loop through water bodies and sum the resuspended SPM 
        do i = 1, me%nReaches
            j_spm_resuspension = j_spm_resuspension + me%colRiverReaches(i)%item%j_spm%resuspension
        end do
    end function

    !> Get the total mass of Contaminant currently in waterbodies in the GridCell
    function get_m_contaminant_waterGridCell(me) result(m_contaminant)
        class(GridCell) :: me
        type(Contaminant) :: m_contaminant
        integer :: w
        type(Result) :: rslt

        rslt = m_contaminant%create()
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create m_contaminant in get_m_contaminant_waterGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if
        do w = 1, me%nReaches
            m_contaminant = m_contaminant + me%colRiverReaches(w)%item%get_m_contaminant()
        end do
    end function

    !> Get the total mass of Contaminant currently in the sediment in the GridCell
    function get_m_contaminant_sedimentGridCell(me) result(m_contaminant)
    class(GridCell) :: me
    type(Contaminant) :: m_contaminant, tmp_cont
    integer :: w
    type(Result) :: rslt
    type(Result0D) :: res

    ! Initialize the result Contaminant object
    rslt = m_contaminant%create_from_data( &
        DATASET%nc, 'sediment', &
        DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
        DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
        DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
        DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
    if (rslt%hasCriticalError()) then
        call rslt%addToTrace("Failed to create m_contaminant in get_m_contaminant_sedimentGridCell")
        call LOGR%toFile(errors=rslt%errors)
        call ERROR_HANDLER%trigger(errors=rslt%errors)
        return
    end if

    ! Sum up each reach’s sediment contaminant, weighted by bedArea
    do w = 1, me%nReaches
        ! call the reach method, capture its Result0D
        res = me%colRiverReaches(w)%item%bedSediment%get_m_contaminant()
        if (res%hasError()) then
        call res%addToTrace("Failed to get contaminant in reach " // trim(str(w)))
        call LOGR%toFile(errors=res%errors)
        call ERROR_HANDLER%trigger(errors=res%errors)
        return
        end if

        ! extract the Contaminant out of the Result0D
        select type (data => res%getData())
        type is (Contaminant)
        tmp_cont = data
        class default
        call res%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant"))
        call res%addToTrace("Failed to extract Contaminant in reach " // trim(str(w)))
        call LOGR%toFile(errors=res%errors)
        call ERROR_HANDLER%trigger(errors=res%errors)
        return
        end select

        ! accumulate, scaling by the bed area
        call m_contaminant%add(tmp_cont * me%colRiverReaches(w)%item%bedArea)
    end do
    end function
   
    !> Get the total mass of sediment in this grid cell
    function get_sediment_massGridCell(me) result(sediment_mass) 
        class(GridCell) :: me                   !! This GridCell instance
        real(dp)        :: sediment_mass        !! Mass of sediment in grid cell [kg]
        integer         :: i                    ! Iterator
        sediment_mass = 0.0_dp
        do i = 1, me%nReaches
            sediment_mass = sediment_mass + me%colRiverReaches(i)%item%bedSediment%Mf_bed_all() &
                            * me%colRiverReaches(i)%item%bedArea
        end do
    end function

    !> Get the average SPM concentration in the grid cell, weighted by water volume in 
    !! each of the water bodies
    function get_C_spmGridCell(me) result(C_spm)
        class(GridCell)         :: me                       !! This grid cell
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
    function reachLineParamsFromInflowsOutflowGridCell(me, i) result(lineParams)
        class(GridCell) :: me                           !! This GridCell
        integer         :: i                            !! The reach to calculate line equation for
        real            :: lineParams(3)                !! Line parameters to return
        integer         :: x_in, y_in, x_out, y_out     ! Inflow and outflow indices of this reach
        real            :: x0, y0, x1, y1, a, b, c      ! Inflow and outflow coords and line params
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

    !> Weighted mean soil-phase contaminant concentration in this grid cell
    function get_C_contaminant_soilGridCell(me) result(cont)
        class(GridCell) :: me
        type(Contaminant) :: cont
        real(dp), allocatable :: arr(:,:,:)
        real(dp) :: partial(me%nSoilProfiles, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
        real(dp) :: weights(me%nSoilProfiles)
        integer :: i
        type(Contaminant) :: tmp_cont
        type(Result) :: rslt

        rslt = cont%create_from_data(DATASET%nc, 'soil', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create cont in get_C_contaminant_soilGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        do i = 1, me%nSoilProfiles
            associate(sp => me%colSoilProfiles(i)%item)
                tmp_cont = sp%get_m_contaminant()
                partial(i,:,:,:) = tmp_cont%c
                weights(i) = 1.0_dp
            end associate
        end do

        arr = weightedAverage(partial, weights)
        cont%c = arr
    end function

    !> Weighted mean water‑phase contaminant concentration in this grid cell
    function get_C_contaminant_waterGridCell(me) result(cont)
        class(GridCell)   :: me
        type(Contaminant) :: cont
        real(dp), allocatable :: arr(:,:,:)
        real(dp) :: partial(me%nReaches, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
        real(dp) :: weights(me%nReaches)
        integer  :: i
        type(Contaminant) :: tmp_cont
        real(dp) :: vol
        type(Result) :: rslt
        character(len=7) :: compstr

        !--- choose compartment name ---
        if (me%aggregatedReachType == 'riv') then
            compstr = 'water'//repeat(' ',2)
        else
            compstr = 'estuary'
        end if

        !--- initialize our Contaminant object from data ---
        rslt = cont%create_from_data( &
            DATASET%nc, trim(compstr), &
            DATASET%contaminantDensity, &
            DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, &
            DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, &
            DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, &
            real(DATASET%waterTemperature(1), dp) )
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create Contaminant in get_C_contaminant_waterGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        !--- build per‑reach concentration (c/volume) and weight (volume) ---
        do i = 1, me%nReaches
            tmp_cont = me%colRiverReaches(i)%item%get_m_contaminant()
            vol      = me%colRiverReaches(i)%item%volume
            if (vol > 0.0_dp) then
                partial(i,:,:,:) = tmp_cont%c / vol
                weights(i)       = vol
            else
                partial(i,:,:,:) = 0.0_dp
                weights(i)       = 0.0_dp
            end if
        end do

        !--- compute weighted average across reaches ---
        arr = weightedAverage(partial, weights)

        !--- store result and return ---
        cont%c = arr
    end function 

    !> Get the current weighted mean sediment PEC [kg/kg] in this grid cell,
    !! weighted by the current sediment masses in the cell
    function get_C_contaminant_sedimentGridCell(me) result(cont)
        class(GridCell) :: me
        type(Contaminant) :: cont
        real(dp), allocatable :: arr(:,:,:)
        real(dp) :: partial(me%nReaches, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
        real(dp) :: weights(me%nReaches)
        integer :: i
        type(Contaminant) :: tmp_cont
        type(Result0D) :: res
        type(Result) :: rslt

        ! Initialize the result Contaminant object
        rslt = cont%create_from_data(DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create cont in get_C_contaminant_sedimentGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        ! Build per-reach concentrations normalized to sediment mass (kg/kg)
        do i = 1, me%nReaches
            associate(bs => me%colRiverReaches(i)%item%bedSediment)
                res = bs%get_m_contaminant()
                if (res%hasError()) then
                    call res%addToTrace("Failed to get contaminant in reach " // trim(str(i)))
                    call LOGR%toFile(errors=res%errors)
                    call ERROR_HANDLER%trigger(errors=res%errors)
                    return
                end if
                select type (data => res%getData())
                    type is (Contaminant)
                        tmp_cont = data
                    class default
                        call res%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant"))
                        call res%addToTrace("Failed to extract Contaminant in reach " // trim(str(i)))
                        call LOGR%toFile(errors=res%errors)
                        call ERROR_HANDLER%trigger(errors=res%errors)
                        return
                end select
                if (bs%Mf_bed_all() > C%epsilon) then
                    partial(i,:,:,:) = tmp_cont%c / bs%Mf_bed_all()
                else
                    partial(i,:,:,:) = 0.0_dp
                end if
                weights(i) = bs%Mf_bed_all() * me%colRiverReaches(i)%item%bedArea
            end associate
        end do

        ! Compute weighted average
        arr = weightedAverage(partial, weights)

        ! Wrap into Contaminant
        cont%c = arr
end function

    !> Weighted mean sediment PEC [kg/m3] in this grid cell
    function get_C_contaminant_sediment_byVolumeGridCell(me) result(cont)
        class(GridCell) :: me
        type(Contaminant) :: cont
        real(dp), allocatable :: arr(:,:,:)
        real(dp) :: partial(me%nReaches, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
        real(dp) :: weights(me%nReaches)
        integer :: i
        type(Contaminant) :: tmp_cont
        type(Result0D) :: res
        type(Result) :: rslt
        real(dp) :: layerVol

        ! Initialize the result Contaminant object
        rslt = cont%create_from_data(DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create cont in get_C_contaminant_sediment_byVolumeGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        ! Build per-reach concentrations normalized to sediment volume (kg/m3)
        layerVol = sum(C%sedimentLayerDepth)
        do i = 1, me%nReaches
            associate(bs => me%colRiverReaches(i)%item%bedSediment)
                res = bs%get_m_contaminant()
                if (res%hasError()) then
                    call res%addToTrace("Failed to get contaminant in reach " // trim(str(i)))
                    call LOGR%toFile(errors=res%errors)
                    call ERROR_HANDLER%trigger(errors=res%errors)
                    return
                end if
                select type (data => res%getData())
                    type is (Contaminant)
                        tmp_cont = data
                    class default
                        call res%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant"))
                        call res%addToTrace("Failed to extract Contaminant in reach " // trim(str(i)))
                        call LOGR%toFile(errors=res%errors)
                        call ERROR_HANDLER%trigger(errors=res%errors)
                        return
                end select
                if (layerVol > C%epsilon) then
                    partial(i,:,:,:) = tmp_cont%c / (me%colRiverReaches(i)%item%bedArea * layerVol)
                else
                    partial(i,:,:,:) = 0.0_dp
                end if
                weights(i) = me%colRiverReaches(i)%item%bedArea * layerVol
            end associate
        end do

        ! Compute weighted average
        arr = weightedAverage(partial, weights)

        ! Wrap into Contaminant
        cont%c = arr
    end function

    !> Weighted mean sediment PEC [kg/kg] for layer l in this grid cell
    function get_C_contaminant_sediment_lGridCell(me, l) result(cont)
        class(GridCell) :: me
        integer :: l
        type(Contaminant) :: cont
        real(dp), allocatable :: arr(:,:,:)
        real(dp) :: partial(me%nReaches, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
        real(dp) :: weights(me%nReaches)
        integer :: i
        type(Contaminant) :: tmp_cont
        type(Result0D) :: res
        type(Result) :: rslt

        ! Initialize the result Contaminant object
        rslt = cont%create_from_data(DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create cont in get_C_contaminant_sediment_lGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if

        ! Build per-reach concentrations normalized to layer mass (kg/kg)
        do i = 1, me%nReaches
            associate(bs => me%colRiverReaches(i)%item%bedSediment)
                res = bs%get_m_contaminant_l(l)
                if (res%hasError()) then
                    call res%addToTrace("Failed to get contaminant for layer " // trim(str(l)) // " in reach " // trim(str(i)))
                    call LOGR%toFile(errors=res%errors)
                    call ERROR_HANDLER%trigger(errors=res%errors)
                    return
                end if
                select type (data => res%getData())
                    type is (Contaminant)
                        tmp_cont = data
                    class default
                        call res%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant_l"))
                        call res%addToTrace("Failed to extract Contaminant for layer " &
                        // trim(str(l)) // " in reach " // trim(str(i)))
                        call LOGR%toFile(errors=res%errors)
                        call ERROR_HANDLER%trigger(errors=res%errors)
                        return
                end select
                if (bs%Mf_bed_by_layer(l) > C%epsilon) then
                    partial(i,:,:,:) = tmp_cont%c / bs%Mf_bed_by_layer(l)
                else
                    partial(i,:,:,:) = 0.0_dp
                end if
                weights(i) = bs%Mf_bed_by_layer(l) * me%colRiverReaches(i)%item%bedArea
            end associate
        end do

        ! Compute weighted average
        arr = weightedAverage(partial, weights)

        ! Wrap into Contaminant
        cont%c = arr
    end function

    !> Weighted mean sediment PEC [kg/m3] for layer l in this grid cell
    function get_C_contaminant_sediment_l_byVolumeGridCell(me, l) result(cont)
    class(GridCell) :: me
    integer :: l
    type(Contaminant) :: cont
    real(dp), allocatable :: arr(:,:,:)
    real(dp) :: partial(me%nReaches, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3))
    real(dp) :: weights(me%nReaches)
    integer :: i
    type(Contaminant) :: tmp_cont
    type(Result0D) :: res
    type(Result) :: rslt

    ! Initialize the result Contaminant object
    rslt = cont%create_from_data(DATASET%nc, 'sediment', &
        DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
        DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
        DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
        DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
    if (rslt%hasCriticalError()) then
        call rslt%addToTrace("Failed to create cont in get_C_contaminant_sediment_l_byVolumeGridCell")
        call LOGR%toFile(errors=rslt%errors)
        call ERROR_HANDLER%trigger(errors=rslt%errors)
        return
    end if

    ! Build per-reach concentrations normalized to layer volume (kg/m3)
    do i = 1, me%nReaches
        associate(bs => me%colRiverReaches(i)%item%bedSediment)
            res = bs%get_m_contaminant_l(l)
            if (res%hasError()) then
                call res%addToTrace("Failed to get contaminant for layer " // trim(str(l)) // " in reach " // trim(str(i)))
                call LOGR%toFile(errors=res%errors)
                call ERROR_HANDLER%trigger(errors=res%errors)
                return
            end if
            select type (data => res%getData())
                type is (Contaminant)
                    tmp_cont = data
                class default
                    call res%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant_l"))
                    call res%addToTrace("Failed to extract Contaminant for layer " &
                    // trim(str(l)) // " in reach " // trim(str(i)))
                    call LOGR%toFile(errors=res%errors)
                    call ERROR_HANDLER%trigger(errors=res%errors)
                    return
            end select
            ! Normalize by sediment volume (bedArea * layer depth) to get PEC [kg/m3]
            if (C%sedimentLayerDepth(l) > C%epsilon) then
                partial(i,:,:,:) = tmp_cont%c / (me%colRiverReaches(i)%item%bedArea * C%sedimentLayerDepth(l))
            else
                partial(i,:,:,:) = 0.0_dp
            end if
            weights(i) = me%colRiverReaches(i)%item%bedArea * C%sedimentLayerDepth(l)
        end associate
    end do

    ! Compute weighted average
    arr = weightedAverage(partial, weights)

    ! Wrap into Contaminant
    cont%c = arr
end function

    !> Get the mass of Contaminant buried for all the bed sediments in this grid cell
    function get_m_contaminant_buried_sedimentGridCell(me) result(m_contaminant_buried)
        class(GridCell) :: me
        type(Contaminant) :: m_contaminant_buried
        integer :: i
        type(Result) :: rslt
        type(Contaminant) :: temp_contaminant

        rslt = m_contaminant_buried%create_from_data(DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create m_contaminant_buried in get_m_contaminant_buried_sedimentGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                ! Access the buried contaminant pool directly
                if (allocated(reach%bedSediment%m_contaminant)) then
                    temp_contaminant = reach%bedSediment%m_contaminant(C%nSedimentLayers + 3)
                    call m_contaminant_buried%add(temp_contaminant * reach%bedArea)
                else
                    call rslt%addError(ErrorInstance(code=105, message="Contaminant array not allocated in BedSediment"))
                    call rslt%addToTrace("Error in get_m_contaminant_buried_sedimentGridCell for reach " // trim(str(i)))
                    call LOGR%toFile(errors=rslt%errors)
                    call ERROR_HANDLER%trigger(errors=rslt%errors)
                    return
                end if
            end associate
        end do
    end function

    !> Get the sum of Contaminant deposition for this grid cell 
    function get_j_contaminant_depositionGridCell(me) result(j_contaminant_deposition)
        class(GridCell) :: me
        type(Contaminant) :: j_contaminant_deposition
        integer :: i
        type(Result) :: rslt

        rslt = j_contaminant_deposition%create_from_data( &
            DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create j_contaminant_deposition in get_j_contaminant_depositionGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if
        do i = 1, me%nReaches
            j_contaminant_deposition = j_contaminant_deposition + &
                me%colRiverReaches(i)%item%j_contaminant_deposition
        end do
    end function

    !> Get the sum of Contaminant resuspended for this grid cell 
    function get_j_contaminant_resuspensionGridCell(me) result(j_contaminant_resuspension)
        class(GridCell) :: me
        type(Contaminant) :: j_contaminant_resuspension
        integer :: i
        type(Result) :: rslt

        rslt = j_contaminant_resuspension%create_from_data( &
            DATASET%nc, 'sediment', &
            DATASET%contaminantDensity, DATASET%soilConstantAttachmentEfficiency, &
            DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
            DATASET%contaminant_k_diss_pristine, DATASET%contaminant_k_diss_transformed, &
            DATASET%contaminant_k_transform_pristine, real(DATASET%waterTemperature(1), dp))
        if (rslt%hasCriticalError()) then
            call rslt%addToTrace("Failed to create j_contaminant_resuspension in get_j_contaminant_resuspensionGridCell")
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if
        do i = 1, me%nReaches
            j_contaminant_resuspension = j_contaminant_resuspension + &
                me%colRiverReaches(i)%item%j_contaminant_resuspension
        end do
    end function

    !> Get the sum of Contaminant outflowing from this grid cell 
    function get_j_contaminant_outflowGridCell(me) result(j_contaminant_outflow)
        class(GridCell)   :: me
        type(Contaminant) :: j_contaminant_outflow
        integer           :: i
        type(Result)      :: rslt

        rslt = j_contaminant_outflow%create()
        if (rslt%hasCriticalError()) then
            call LOGR%toFile(errors=rslt%errors)
            call ERROR_HANDLER%trigger(errors=rslt%errors)
            return
        end if
        do i = 1, me%nReaches
            associate (reach => me%colRiverReaches(i)%item)
                if (reach%isGridCellOutflow) then
                    call j_contaminant_outflow%add(reach%j_contaminant_outflow)
                end if
            end associate
        end do
    end function

    !> Get the sum of dissolved species outflowing from this grid cell
    function get_j_dissolved_outflowGridCell(me) result(j_dissolved_outflow)
        class(GridCell) :: me
        real(dp)        :: j_dissolved_outflow
        integer         :: i
        type(Contaminant) :: tmp_cont
        j_dissolved_outflow = 0.0_dp
        do i = 1, me%nReaches
            if (me%colRiverReaches(i)%item%isGridCellOutflow) then
                tmp_cont = me%colRiverReaches(i)%item%get_m_contaminant()
                j_dissolved_outflow = j_dissolved_outflow + tmp_cont%m_dissolved
            end if
        end do
    end function

    !> Get the total length of all reaches in the cell
    function getTotalReachLengthGridCell(me) result(totalReachLength)
        class(GridCell) :: me
        real(dp)        :: totalReachLength
        integer         :: r
        totalReachLength = 0
        do r = 1, me%nReaches
            totalReachLength = totalReachLength + me%colRiverReaches(r)%item%length
        end do
    end function

    !> Get the total volume of water [m3] in this grid cell
    function getWaterVolumeGridCell(me) result(waterVolume)
        class(GridCell) :: me               !! This GridCell instance
        real(dp)        :: waterVolume      !! Water volume [m3] 
        integer         :: i                ! Iterator
        waterVolume = 0.0_dp
        do i = 1, me%nReaches
            waterVolume = waterVolume + me%colRiverReaches(i)%item%volume
        end do
    end function

    !> Get the average depth of water [m] in this grid cell, weighted by reach lengths
    function getWaterDepthGridCell(me) result(waterDepth)
        class(GridCell) :: me
        real(dp)        :: waterDepth
        real(dp)        :: waterDepth_i(me%nReaches)
        real(dp)        :: lengths(me%nReaches)
        integer         :: i
        do i = 1, me%nReaches
            waterDepth_i(i) = me%colRiverReaches(i)%item%depth
            lengths(i) = me%colRiverReaches(i)%item%length
        end do
        waterDepth = weightedAverage(waterDepth_i, lengths)
    end function

    !> Get the total bed sediment area [m2] in this grid cell
    function getBedSedimentAreaGridCell(me) result(bedArea)
        class(GridCell) :: me               !! This GridCell instance
        real(dp)        :: bedArea          !! Bed sediment area [m2]
        integer         :: i                ! Iterator
        bedArea = 0.0_dp
        do i = 1, me%nReaches
            bedArea = bedArea + me%colRiverReaches(i)%item%bedArea
        end do
    end function

    !> Get the total mass of sediment [kg] in this grid cell
    function getBedSedimentMassGridCell(me) result(sedimentMass)
        class(GridCell) :: me               !! This GridCell instance
        real(dp)        :: sedimentMass     !! Bed sediment mass [kg]
        integer         :: i                ! Iterator
        sedimentMass = 0.0_dp
        do i = 1, me%nReaches
            sedimentMass = sedimentMass &
                + me%colRiverReaches(i)%item%bedSediment%Mf_bed_all() &             ! Sediment mass in this reach, kg/m2
                * me%colRiverReaches(i)%item%bedArea                                ! Multiply by bed area to get total mass in this reach 
        end do
    end function

    !> Get the average dissolved contaminant concentration in the grid cell, weighted by water volume
    function get_C_dissolved_waterGridCell(me) result(C_dissolved_water)
        class(GridCell) :: me
        real(dp)        :: C_dissolved_water
        real(dp), allocatable :: C_dissolved_water_w(:)
        real(dp)        :: volumes(me%nReaches)
        integer         :: i
        type(Contaminant) :: tmp_cont

        allocate(C_dissolved_water_w(me%nReaches))
        do i = 1, me%nReaches
            tmp_cont = me%colRiverReaches(i)%item%get_m_contaminant()
            if (me%colRiverReaches(i)%item%volume > 0.0_dp) then
                C_dissolved_water_w(i) = tmp_cont%m_dissolved / me%colRiverReaches(i)%item%volume
            else
                C_dissolved_water_w(i) = 0.0_dp
            end if
            volumes(i) = me%colRiverReaches(i)%item%volume
        end do
        C_dissolved_water = weightedAverage(C_dissolved_water_w, volumes)
    end function
end module
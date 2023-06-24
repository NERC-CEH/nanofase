!> Module container for the DataOutput class
module DataOutputModule
    use DefaultsModule, only: iouOutputSummary, iouOutputWater, &
        iouOutputSediment, iouOutputSoil, iouOutputSSD, iouOutputStats
    use GlobalsModule, only: C, dp
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    use AbstractEnvironmentModule
    use EnvironmentModule
    use AbstractGridCellModule
    use RiverReachModule
    use EstuaryReachModule
    use UtilModule
    use datetime_module
    use mo_netcdf
    use NetCDFOutputModule
    use NetCDFAggregatedOutputModule
    implicit none

    !> The DataOutput class is responsible for writing output data to disk
    type, public :: DataOutput
        character(len=256)                  :: outputPath               !! Path to the output directory
        type(EnvironmentPointer)            :: env                      !! Pointer to the environment, to retrieve state variables
        class(NetCDFOutput), allocatable    :: ncout                    !! NetCDF output class
        ! Storing variables across timesteps for dynamics calculations
        real(dp), allocatable               :: previousSSDByLayer(:,:)
        real(dp), allocatable               :: previousSSD(:)
      contains
        procedure, public :: init => initDataOutput
        procedure, public :: initSedimentSizeDistribution => initSedimentSizeDistributionDataOutput
        procedure, public :: update => updateDataOutput
        procedure, public :: finalise => finaliseDataOutput
        procedure, public :: newChunk => newChunkDataOutput
        procedure, public :: finaliseChunk => finaliseChunkDataOutput
        procedure, private :: writeHeaders => writeHeadersDataOutput
        procedure, private :: writeHeadersSimulationSummary => writeHeadersSimulationSummaryDataOutput
        procedure, private :: writeHeadersWater => writeHeadersWaterDataOutput
        procedure, private :: writeHeadersSediment => writeHeadersSedimentDataOutput
        procedure, private :: writeHeadersSoil => writeHeadersSoilDataOutput
        procedure, private :: writeHeadersStats => writeHeadersStatsDataOutput
        procedure, private :: updateWater => updateWaterDataOutput
        procedure, private :: updateSediment => updateSedimentDataOutput
        procedure, private :: updateSoil => updateSoilDataOutput
        procedure, public :: updateSedimentSizeDistribution => updateSedimentSizeDistributionDataOutput
    end type

  contains
    
    !> Initialise the data output be creating the relevant output files and writing
    !! their headers and metadata
    subroutine initDataOutput(me, env)
        class(DataOutput)           :: me
        type(Environment), target   :: env
        
        ! Point the Environment object to that passed in
        me%env%item => env
        ! Allocate the appropriate NetCDF output object, depending on whether we're aggregating
        ! to grid cell or not
        if (C%includeWaterbodyBreakdown) then
            allocate(NetCDFOutput :: me%ncout)
        else 
            allocate(NetCDFAggregatedOutput :: me%ncout)
        end if

        if (C%writeNetCDF) then
            call me%ncout%init(env, 1)
        end if

        ! Open the files to write to
        open(iouOutputSummary, file=trim(C%outputPath) // 'summary' // trim(C%outputHash) // '.md')
        if (C%writeCSV) then
            open(iouOutputWater, &
                 file=trim(C%outputPath) // 'output_water' // trim(C%outputHash) // '.csv')
            open(iouOutputSediment, &
                 file=trim(C%outputPath) // 'output_sediment' // trim(C%outputHash) // '.csv')
            open(iouOutputSoil, &
                 file=trim(C%outputPath) // 'output_soil' // trim(C%outputHash) // '.csv')
        end if
        if (C%writeCompartmentStats) then
            open(iouOutputStats, &
                 file=trim(C%outputPath) // 'stats' // trim(C%outputHash) // '.csv')
        end if

        ! Write the headers for the files
        call me%writeHeaders()
    end subroutine

    !> Initialise the sediment size distribution steady state run output data file
    subroutine initSedimentSizeDistributionDataOutput(me)
        class(DataOutput)           :: me
        integer                     :: i, j

        ! Sediment begins with distribution given in the input data
        allocate(me%previousSSD, source=DATASET%sedimentInitialMass)
        allocate(me%previousSSDByLayer(C%nSedimentLayers, C%nSizeClassesSpm))
        do i = 1, C%nSedimentLayers
            me%previousSSDByLayer(i,:) = DATASET%sedimentInitialMass
        end do

        ! Open the SSD file and write the headers
        open(iouOutputSSD, file=trim(C%outputPath) // 'output_ssd' // trim(C%outputHash) // '.csv')
        if (C%writeMetadataAsComment) then
            write(iouOutputSSD, '(a)') "# NanoFASE model output data - SEDIMENT SIZE DISTRIBUTION."
            write(iouOutputSSD, '(a)') "# Output file for when running the model until sediment size " // &
                "distribution is at steady state."
            write(iouOutputSSD, '(a)') "# Each row represents a complete model run (as defined by the config/batch config file)."
            write(iouOutputSSD, '(a)') "#\ti: model run index (number of iterations of the same input data)"
            write(iouOutputSSD, '(a)') "#\tssd_sci_all_layers: sediment size distribution across size classes i, " // &
                "averaged across sediment layers"
            write(iouOutputSSD, '(a)') "#\tssd_sci_lj: sediment size distribution across size classes i, for layer j"
            write(iouOutputSSD, '(a)') "#\tdelta_max_lj: maximum difference between size distribution bins for layer j"
            write(iouOutputSSD, '(a)') "#\tdelta_max_all_layers: maximum difference between size " // &
                "distribution bins for size distribution averaged across sediment layers"
        end if
        write(iouOutputSSD, '(a)', advance='no') "i,"
        write(iouOutputSSD, '(*(a))', advance='no') ('ssd_sc'//trim(str(i))//'_all_layers,', i=1, C%nSizeClassesSpm)
        write(iouOutputSSD, '(*(a))', advance='no') (('ssd_sc'//trim(str(i))//'_l'//trim(str(j))//',', &
            i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))', advance='no') ('delta_max_l'//trim(str(i))//',', i=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))') 'delta_max_all_layers'
    end subroutine

    !> Save the output from the current timestep to the output files
    subroutine updateDataOutput(me, t, tInChunk)
        class(DataOutput)   :: me       !! The DataOutput instance
        integer             :: t        !! The current timestep in the batch
        integer             :: tInChunk !! The timestep in the current chunk
        integer             :: x, y     ! Iterators
        type(datetime)      :: date
        character(len=100)  :: dateISO
        real                :: easts, norths

        ! Get the date for this timestep
        date = C%batchStartDate + timedelta(t - 1)
        dateISO = date%isoformat()
        
        ! Loop through the grid cells and update each compartment
        do y = 1, size(me%env%item%colGridCells, dim=2)
            do x = 1, size(me%env%item%colGridCells, dim=1)
                ! Only write data if cell isn't masked
                if (DATASET%simulationMask(x,y)) then
                    easts = DATASET%x(x)
                    norths = DATASET%y(y)
                    call me%updateWater(t, tInChunk, x, y, dateISO, easts, norths)
                    call me%updateSediment(t, tInChunk, x, y, dateISO, easts, norths)
                    call me%updateSoil(t, tInChunk, x, y, dateISO, easts, norths)
                    ! Are we writing to a NetCDF file?
                    if (C%writeNetCDF) then
                        call me%ncout%updateWater(t, tInChunk, x, y)
                        call me%ncout%updateSediment(t, tInChunk, x, y)
                        call me%ncout%updateSoil(t, tInChunk, x, y)
                    end if
                end if
            end do
        end do
    end subroutine

    !> Update the water output file for the current timestep
    subroutine updateWaterDataOutput(me, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: tInChunk         !! Timestep in the current chunk
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: i, w             ! Iterators
        character(len=3)    :: reachType        ! Is this a river of estuary?
        real(dp)            :: m_spm(C%nSizeClassesSpm) ! SPM masses
        real(dp)            :: C_spm(C%nSizeClassesSpm) ! SPM concs
        if (C%writeCSV) then
            ! Do we want to output waterbody breakdown or aggregate to grid cell level?
            if (C%includeWaterbodyBreakdown) then
                ! Loop through the waterbodies in this cell. Only loops if nReaches > 0, hence we don't check explicitly
                do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
                    associate (reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                        ! Is it a reach or an estuary?
                        select type (reach)
                            type is (RiverReach)
                                reachType = 'riv'
                            type is (EstuaryReach)
                                reachType = 'est'
                        end select
                        ! Write the data
                        write(iouOutputWater, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(w)) // "," // reachType // "," // &
                            trim(str(sum(reach%m_np))) // "," // trim(str(sum(reach%C_np))) // "," // &
                            trim(str(sum(reach%m_transformed))) // "," // trim(str(sum(reach%C_transformed))) // "," // &
                            trim(str(reach%m_dissolved)) // "," // trim(str(reach%C_dissolved)) // "," // &
                            trim(str(sum(reach%j_nm%deposition))) // "," // &
                            trim(str(sum(reach%j_nm_transformed%deposition))) // "," // &
                            trim(str(sum(reach%j_nm%resuspension))) // "," // &
                            trim(str(sum(reach%j_nm_transformed%resuspension))) // "," // &
                            trim(str(sum(reach%j_nm%outflow))) // "," // &
                            trim(str(sum(reach%j_nm_transformed%outflow))) // "," // &
                            trim(str(reach%j_dissolved%outflow)) // "," // &
                            trim(str(sum(reach%m_spm))) // "," // &
                            trim(str(sum(reach%C_spm))) // ","
                        if (C%includeSpmSizeClassBreakdown) then
                            write(iouOutputWater, '(*(a))', advance='no') (trim(str(reach%m_spm(i))) // "," // &
                                trim(str(reach%C_spm(i))) // ",", i=1, C%nSizeClassesSpm)
                        end if
                        if (C%includeSedimentFluxes) then
                            write(iouOutputWater, '(a)', advance='no') trim(str(sum(reach%j_spm%soilErosion))) // "," // &
                                trim(str(sum(reach%j_spm%deposition))) // "," // &
                                trim(str(sum(reach%j_spm%resuspension))) // "," // &
                                trim(str(sum(reach%j_spm%inflow))) // "," // trim(str(sum(reach%j_spm%outflow))) // "," // &
                                trim(str(sum(reach%j_spm%bankErosion))) // ","
                        end if
                        write(iouOutputWater, '(a)') trim(str(reach%volume)) // "," // trim(str(reach%depth)) // "," // &
                            trim(str(reach%Q%outflow / C%timeStep))
                    end associate
                end do
            else
                ! We're not including waterbody breakdown, so just output the grid cell aggregated values. Here we check
                ! that there are reaches in the cell, and if not, don't print a row for this cell. There is slightly different
                ! to checking if the cell is empty (i.e. doesn't have a soil profile either)
                associate (cell => me%env%item%colGridCells(x,y)%item)
                    if (cell%nReaches > 0) then
                        ! Write the data
                        write(iouOutputWater, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // cell%aggregatedReachType // "," // &
                            trim(str(sum(cell%get_m_np_water()))) // "," // trim(str(sum(cell%get_C_np_water()))) // "," // &
                            trim(str(sum(cell%get_m_transformed_water()))) // "," // &
                            trim(str(sum(cell%get_C_transformed_water()))) // "," // &
                            trim(str(cell%get_m_dissolved_water())) // "," // &
                            trim(str(cell%get_C_dissolved_water())) // "," // &
                            trim(str(sum(cell%get_j_nm_deposition()))) // "," // &
                            trim(str(sum(cell%get_j_transformed_deposition()))) // "," // &
                            trim(str(sum(cell%get_j_nm_resuspension()))) // "," // &
                            trim(str(sum(cell%get_j_transformed_resuspension()))) // "," // &
                            trim(str(sum(cell%get_j_nm_outflow()))) // "," // &
                            trim(str(sum(cell%get_j_transformed_outflow()))) // "," // &
                            trim(str(cell%get_j_dissolved_outflow())) // ","
                        m_spm = cell%get_m_spm()
                        C_spm = cell%get_C_spm()
                        write(iouOutputWater, '(a)', advance='no') trim(str(sum(m_spm))) // "," // &
                            trim(str(sum(C_spm))) // ","
                        if (C%includeSpmSizeClassBreakdown) then
                            write(iouOutputWater, '(*(a))', advance='no') (trim(str(m_spm(i))) // "," // &
                                trim(str(C_spm(i))) // ",", i=1, C%nSizeClassesSpm)
                        end if
                        if (C%includeSedimentFluxes) then
                            write(iouOutputWater, '(a)', advance='no') trim(str(sum(cell%get_j_spm_soilErosion()))) // "," // &
                                trim(str(sum(cell%get_j_spm_deposition()))) // "," // &
                                trim(str(sum(cell%get_j_spm_resuspension()))) // "," // &
                                trim(str(sum(cell%get_j_spm_inflow()))) // "," // &
                                trim(str(sum(cell%get_j_spm_outflow()))) // "," // &
                                trim(str(sum(cell%get_j_spm_bankErosion()))) // ","
                        end if
                        write(iouOutputWater, '(a)') trim(str(cell%getWaterVolume())) // "," // &
                            trim(str(cell%getWaterDepth())) // "," // &
                            trim(str(cell%get_Q_outflow() / C%timeStep))
                    end if
                end associate
            end if
        end if

    end subroutine

    !> Update the current sediment output file on the current timestep
    subroutine updateSedimentDataOutput(me, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: tInChunk         !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: w, l             ! Iterators
        character(len=3)    :: reachType        ! Is this a river of estuary?

        if (C%writeCSV) then
            if (C%includeWaterbodyBreakdown) then
                ! Loop through the waterbodies in this cell
                do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
                    associate (reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                        ! Is this a river or estuary?
                        select type (reach)
                            type is (RiverReach)
                                reachType = 'riv'
                            type is (EstuaryReach)
                                reachType = 'est'
                        end select
                        ! Write the data
                        write(iouOutputSediment, '(a)', advance='no') trim(str(t)) // "," &
                            // trim(date) // "," // trim(str(x)) // "," // trim(str(y)) &
                            // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // &
                            trim(str(w)) // "," // reachType // "," // &
                            trim(str(sum(reach%bedSediment%get_m_np()) * reach%bedArea)) // "," // &    ! Converting from kg/m2 to kg
                            trim(str(sum(reach%bedSediment%get_C_np()))) // "," // &
                            trim(str(sum(reach%bedSediment%get_C_np_byMass()))) // ","
                        ! Only include layer-by-layer breakdown if we've been asked to
                        if (C%includeSedimentLayerBreakdown) then
                            write(iouOutputSediment, '(*(a))', advance='no') &
                                (trim(str(sum(reach%bedSediment%get_C_np_l(l))))  // "," // &
                                trim(str(sum(reach%bedSediment%get_C_np_l_byMass(l)))) // ",", l=1, C%nSedimentLayers)
                        end if
                        write(iouOutputSediment, '(a)') &
                            trim(str(sum(reach%bedSediment%get_m_np_buried()) * reach%bedArea)) // "," // &
                            trim(str(reach%bedArea)) // "," // trim(str(reach%bedSediment%Mf_bed_all() * reach%bedArea)) &
                            // "," // trim(str(reach%bedSediment%Mf_bed_all() / sum(C%sedimentLayerDepth)))
                    end associate
                end do
            else
                ! We're not including waterbody breakdown, so just output the grid cell aggregated values. Here we check
                ! that there are reaches in the cell, and if not, don't print a row for this cell. There is slightly different
                ! to checking if the cell is empty (i.e. doesn't have a soil profile either)
                associate (cell => me%env%item%colGridCells(x,y)%item)
                    if (cell%nReaches > 0) then
                        ! Write the data
                        write(iouOutputSediment, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // cell%aggregatedReachType // "," // &
                            trim(str(sum(cell%get_m_np_sediment()))) // "," // &
                            trim(str(sum(cell%get_C_np_sediment_byVolume()))) // "," // &
                            trim(str(sum(cell%get_C_np_sediment()))) // ","
                        ! Only include layer-by-layer breakdown if we've been asked to
                        if (C%includeSedimentLayerBreakdown) then
                            write(iouOutputSediment, '(*(a))', advance='no') &
                                (trim(str(sum(cell%get_C_np_sediment_l_byVolume(l)))) // "," // &
                                trim(str(sum(cell%get_C_np_sediment_l(l)))) // ",", l=1, C%nSedimentLayers)
                        end if
                        write(iouOutputSediment, '(a)') &
                            trim(str(sum(cell%get_m_np_buried_sediment()))) // "," // &
                            trim(str(cell%getBedSedimentArea())) // "," // trim(str(cell%getBedSedimentMass())) &
                            // "," // trim(str(cell%getBedSedimentMass() / &
                                ((cell%getBedSedimentArea() * sum(C%sedimentLayerDepth)))))
                    end if
                end associate
            end if
        end if
    end subroutine

    !> Update the sediment output file on the current timestep
    subroutine updateSoilDataOutput(me, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! This DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: tInChunk         !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: i, l             ! Iterators
        ! Loop through soil profiles and write row for each one
        do i = 1, me%env%item%colGridCells(x,y)%item%nSoilProfiles
            associate (profile => me%env%item%colGridCells(x,y)%item%colSoilProfiles(i)%item)
                write(iouOutputSoil, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                    trim(str(x)) // "," // trim(str(y)) // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // &
                    trim(str(i)) // "," // trim(profile%dominantLandUseName) // "," // &
                    trim(str(sum(profile%get_m_np()))) // "," // trim(str(sum(profile%get_m_transformed()))) // "," // &
                    trim(str(profile%get_m_dissolved())) // "," // trim(str(sum(profile%get_C_np()))) // "," // &
                    trim(str(sum(profile%get_C_transformed()))) // "," // trim(str(profile%get_C_dissolved())) // ","
                if (C%includeSoilStateBreakdown) then
                    write(iouOutputSoil, '(a)', advance='no') trim(str(sum(freeNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(freeNM(profile%get_C_transformed())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_transformed())))) // ","
                end if
                if (C%includeSoilLayerBreakdown) then
                    write(iouOutputSoil, '(*(a))', advance='no') &
                        (trim(str(sum(profile%colSoilLayers(l)%item%C_np))) // "," // &
                         trim(str(sum(profile%colSoilLayers(l)%item%C_transformed))) // "," // &
                         trim(str(profile%colSoilLayers(l)%item%C_dissolved)) // ",", l = 1, C%nSoilLayers)
                    if (C%includeSedimentLayerBreakdown) then
                        write(iouOutputSoil, '(*(a))', advance='no') &
                            (trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                        write(iouOutputSoil, '(*(a))', advance='no') &
                            (trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                    end if
                end if
                ! Should we include soil erosion?
                if (C%includeSoilErosionYields) then
                    write(iouOutputSoil, '(a)', advance='no') trim(str(sum(profile%erodedSediment) * profile%area)) &
                        // "," // trim(str(sum(profile%m_np_eroded(:,:,2)))) // "," // &
                        trim(str(sum(profile%m_transformed_eroded(:,:,2)))) // ","
                end if
                write(iouOutputSoil, '(a)') trim(str(sum(profile%m_np_buried))) // "," // &
                    trim(str(sum(profile%m_transformed_buried))) // "," // &
                    trim(str(profile%m_dissolved_buried)) // "," // &
                    trim(str(profile%bulkDensity))
            end associate
        end do
    end subroutine

    function updateSedimentSizeDistributionDataOutput(me, i_model) result(delta_max)
        class(DataOutput)   :: me                           !! This DataOutput instance
        integer             :: i_model                      !! Current model iteration
        real(dp)            :: m_sediment_byLayer(C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp)            :: sedimentSizeDistributionByLayer(C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp)            :: sedimentSizeDistribution(C%nSizeClassesSpm)
        integer             :: i, j
        real(dp)            :: delta_max
        real(dp)            :: delta_max_l(C%nSedimentLayers)
        ! Get the current mass of sediment in each layer
        m_sediment_byLayer = me%env%item%get_m_sediment_byLayer()
        ! Calculate the sediment size distribution across all layers
        sedimentSizeDistribution = sum(m_sediment_byLayer, dim=1) / sum(m_sediment_byLayer)
        ! Calculate the sediment size distribution for each layer
        do j = 1, C%nSedimentLayers
            sedimentSizeDistributionByLayer(j,:) = m_sediment_byLayer(j,:) / sum(m_sediment_byLayer(j,:))
            delta_max_l = maxval(abs(me%previousSSDByLayer(j,:) - sedimentSizeDistributionByLayer(j,:)))
        end do
        delta_max = maxval(abs(me%previousSSD - sedimentSizeDistribution))
        ! Write the values to file
        write(iouOutputSSD, '(a)', advance='no') trim(str(i_model)) // ","
        write(iouOutputSSD, '(*(a))', advance='no') (trim(str(sedimentSizeDistribution(i))) // &
            ",", i=1, C%nSizeClassesSpm)
        write(iouOutputSSD, '(*(a))', advance='no') ((trim(str(sedimentSizeDistributionByLayer(j,i))) // &
            ",", i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))', advance='no') (trim(str(delta_max_l(i)))//',', i=1, C%nSedimentLayers)
        write(iouOutputSSD, '(a)') trim(str(delta_max))
        ! Update the previous SSDs to use on the next model iteration
        me%previousSSD = sedimentSizeDistribution
        me%previousSSDByLayer = sedimentSizeDistributionByLayer
    end function

    ! Finalise the data output by adding PECs to the simulation summary file and closing output files
    subroutine finaliseDataOutput(me, iSteadyState)
        class(DataOutput)   :: me
        integer             :: iSteadyState
        real(dp)            :: timeUntilSteadyState
        ! Write the final model summary info to the simulation summary file
        if (.not. C%runToSteadyState) then
            write(iouOutputSummary, *) "\n## PECs"
        else
            write(iouOutputSummary, *) "\n## PECs (final model iteration)"
        end if
        write(iouOutputSummary, *) "- Soil, spatial mean on final timestep: " // &
            trim(str(sum(me%env%item%get_C_np_soil()))) // " kg/kg soil"
        write(iouOutputSummary, *) "- Water, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_water_t, dim=1)) / size(me%env%item%C_np_water_t, dim=1))) // " kg/m3"
        write(iouOutputSummary, *) "- Sediment, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_sediment_t, dim=1)) / size(me%env%item%C_np_sediment_t, dim=1))) // &
            " kg/kg sediment"
       
        timeUntilSteadyState = iSteadyState * C%timeStep * C%nTimestepsInBatch
        if (C%runToSteadyState) then
            write(iouOutputSummary, *) "\n## Steady state"
            write(iouOutputSummary, *) "- Iterations until steady state: " // trim(str(iSteadyState))
            write(iouOutputSummary, *) "- Time until steady state: " &
                // trim(str(iSteadyState * C%timeStep * C%nTimestepsInBatch)) // " s"
        end if

        ! Close the files
        close(iouOutputSummary); close(iouOutputWater); close(iouOutputSediment); close(iouOutputSoil)
        close(iouOutputSSD); close(iouOutputStats)
        
        ! Log that we've written output data files
        call LOGR%add('Model output written to ' // trim(C%outputPath), COLOR_GREEN)
    end subroutine

    !> Tell the NetCDF output class to reallocate memory for the new chunk,
    !! if we're in write-at-end mode
    subroutine newChunkDataOutput(me, k)
        class(DataOutput)  :: me       !! This DataOutput instance
        integer             :: k        !! This chunk index
        ! Only bother calling this if we need to reallocate memory
        if (C%writeNetCDF .and. C%netCDFWriteMode == 'end') then
            call me%ncout%newChunk(k)
        end if
    end subroutine

    !> Tell the NetCDF output class that we're at the end of a chunk, so that
    !! it writes to the NetCDF file if in write-at-end mode
    subroutine finaliseChunkDataOutput(me, tStart, isFinalChunk)
        class(DataOutput)  :: me               !! This DataOutput instance
        integer             :: tStart           !! Timestep index at the start of this chunk
        logical             :: isFinalChunk     !! Is this the final chunk?
        ! Only bother calling this if we need to write to the NetCDF file
        if (C%writeNetCDF .and. C%netCDFWriteMode == 'end') then
            call me%ncout%finaliseChunk(tStart)
        end if
        ! If this is the final chunk, close the NetCDF file
        if (C%writeNetCDF .and. isFinalChunk) then
            call me%ncout%close()
        end if
    end subroutine

    ! Write the headers for the output files
    subroutine writeHeadersDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        ! Write headers all of the output files
        call me%writeHeadersSimulationSummary()
        if (C%writeCSV) then
            call me%writeHeadersWater()
            call me%writeHeadersSediment()
            call me%writeHeadersSoil()
        end if
        if (C%writeCompartmentStats) then
            call me%writeHeadersStats()
        end if
    end subroutine

    !> Write headers for the simulation summary file, including basic info about the model run
    subroutine writeHeadersSimulationSummaryDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        type(datetime)      :: simDatetime          ! Datetime the simulation was run 

        ! Parse some datetimes
        simDatetime = simDatetime%now()

        ! Summary file headers
        write(iouOutputSummary, '(a)') "# NanoFASE model simulation summary"
        write(iouOutputSummary, '(a)') " - Description: " // trim(C%runDescription)
        write(iouOutputSummary, '(a)') " - Simulation datetime: " // simDatetime%isoformat()
        write(iouOutputSummary, '(a)') " - Model version: " // C%modelVersion
        write(iouOutputSummary, '(a)') " - Is batch run? " // trim(str(C%isBatchRun))
        write(iouOutputSummary, '(a)') " - Number of batches: " // trim(str(C%nChunks))
        write(iouOutputSummary, '(a)', advance='no') " - Is steady state run? " // trim(str(C%runToSteadyState))
        if (C%runToSteadyState) then
            write(iouOutputSummary, '(a)') " (" // trim(C%steadyStateMode) // " mode)"
        else
            write(iouOutputSummary, '(a)') ""
        end if
        write(iouOutputSummary, *) "\n## Temporal domain"
        write(iouOutputSummary, *) "- Start date: " // C%batchStartDate%strftime('%Y-%m-%d')
        write(iouOutputSummary, *) "- End date: " // C%batchEndDate%strftime('%Y-%m-%d')
        write(iouOutputSummary, *) "- Timestep length: " // trim(str(C%timeStep)) // " s"
        write(iouOutputSummary, *) "- Number of timesteps: " // trim(str(C%nTimestepsInBatch))
        
        write(iouOutputSummary, *) "\n## Spatial domain"
        write(iouOutputSummary, *) "- Grid resolution: " // trim(str(DATASET%gridRes(1))) // ", " // &
            trim(str(DATASET%gridRes(2))) // " m" 
        write(iouOutputSummary, *) "- Grid bounds: " // trim(str(DATASET%gridBounds(1))) // ", " // &
            trim(str(DATASET%gridBounds(2))) // &
            ", " // trim(str(DATASET%gridBounds(3))) // ", " // trim(str(DATASET%gridBounds(4))) // " m"
        write(iouOutputSummary, *) "- Grid shape: " // trim(str(DATASET%gridShape(1))) // ", " // trim(str(DATASET%gridShape(2)))
        write(iouOutputSummary, *) "- Number of non-empty grid cells: " // trim(str(me%env%item%nGridCells))
        write(iouOutputSummary, *) "- Is simulation masked? " // trim(str(C%hasSimulationMask))
        write(iouOutputSummary, *) "- Number of non-masked grid cells: " // trim(str(DATASET%nNonMaskedCells))
    end subroutine

    !> Write the headers for the compartment stats file
    subroutine writeHeadersStatsDataOutput(me)
        class(DataOutput)  :: me

        ! ! Write metadata, if we're meant to
        ! if (C%writeMetadataAsComment) then
        !     write(iouOutputStats, '(a)') "# NanoFASE model output data - COMPARTMENT STATS.\n"
        !     write(iouOutputStats, '(a)') "# This file contains summary statistics for each environmental compartment.\n"
        ! end if
        ! write(iouOutputStats '(a)')
    end subroutine

    !> Write the headers for the water output file
    subroutine writeHeadersWaterDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Size class iterator
        
        ! Write metadata, if we're meant to 
        if (C%writeMetadataAsComment) then
            write(iouOutputWater, '(a)') "# NanoFASE model output data - WATER.\n# See summary.md for model run metadata."
            write(iouOutputWater, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(iouOutputWater, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputWater, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            if (C%includeWaterbodyBreakdown) write(iouOutputWater, '(a)') "#\tw: waterbody index within this grid cell"
            if (C%includeWaterbodyBreakdown) then
                write(iouOutputWater, '(a)') "#\twaterbody_type: what type (river, estuary etc) is this waterbody?"
            else
                write(iouOutputWater, '(a)') "#\twaterbody_type: what is the dominant waterbody type in this cell?"
            end if
            write(iouOutputWater, '(a)') "#\tm_np(kg), m_transformed(kg), m_dissolved(kg): " // &
                "NM mass (pristine, transformed and dissolved, kg)"
            write(iouOutputWater, '(a)') "#\tC_np(kg/m3), C_transformed(kg/m3), C_dissolved(kg/m3): NM concentration (kg/m3)"
            write(iouOutputWater, '(a)') "#\tm_np_outflow(kg), m_transformed_outflow(kg), m_dissolved_outflow(kg): " // &
                "downstream outflow NM masses (kg)"
            write(iouOutputWater, '(a)') "#\tm_np_deposited(kg), m_transformed_deposited(kg): mass of NM deposited (kg)"
            write(iouOutputWater, '(a)') "#\tm_np_resuspended(kg), m_transformed_resuspended(kg): mass of NM resuspended (kg)"
            write(iouOutputWater, '(a)') "#\tm_spm(kg), C_spm(kg/m3): mass and concentration of SPM (kg, kg/m3)"
            if (C%includeSpmSizeClassBreakdown) then
                write(iouOutputWater, '(a)') "#\tm_spm_sci(kg), C_spm_sci(kg/m3): mass aond concentration of SPM in " // &
                    "size class i (kg, kg/m3)"
            end if
            if (C%includeSedimentFluxes) then
                write(iouOutputWater, '(a)') "#\tm_spm_erosion(kg), m_spm_dep(kg), m_spm_res(kg), m_spm_inflow(kg), " // &
                    "m_spm_outflow(kg), m_spm_bank_erosion(kg): SPM fluxes from erosion, deposition, resuspension, " // &
                    "inflows, outflow and bank erosion on this timestep (kg)"
            end if
            write(iouOutputWater, '(a)') "#\tvolume(m3), depth(m), flow(m3/s): volume (m3), " // &
                "depth (m) and flow rate (m3/s) of this waterbody"
        end if
        ! Write the actual headers
        write(iouOutputWater, '(a)', advance='no') "t,datetime,x,y,easts,norths,"
        if (C%includeWaterbodyBreakdown) write(iouOutputWater, '(a)', advance='no') "w,"
        write(iouOutputWater, '(a)', advance='no') "waterbody_type,m_np(kg),C_np(kg/m3)," // &
            "m_transformed(kg),C_transformed(kg/m3),m_dissolved(kg),C_dissolved(kg/m3)," // &
            "m_np_deposited(kg),m_transformed_deposited(kg)," // &
            "m_np_resuspended(kg),m_transformed_resuspended(kg),m_np_outflow(kg),m_transformed_outflow(kg)," // &
            "m_dissolved_outflow(kg),m_spm(kg),C_spm(kg/m3),"
        if (C%includeSpmSizeClassBreakdown) then
            write(iouOutputWater, '(*(a))', advance="no") &
                ("m_spm_sc" // trim(str(i)) // "(kg),C_spm_sc" // trim(str(i)) // "(kg/m3),", i=1, C%nSizeClassesSpm) 
        end if
        if (C%includeSedimentFluxes) then
            write(iouOutputWater, '(a)', advance='no') "m_spm_erosion(kg),m_spm_dep(kg),m_spm_res(kg)," // &
                "m_spm_inflow(kg),m_spm_outflow(kg),m_spm_bank_erosion(kg),"
        end if
        write(iouOutputWater, '(a)') "volume(m3),depth(m),flow(m3/s)"
    end subroutine

    !> Write the headers for the sediment output file
    subroutine writeHeadersSedimentDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        ! Write metadata, if we're meant to
        if (C%writeMetadataAsComment) then
            write(iouOutputSediment, '(a)') "# NanoFASE model output data - SEDIMENT."
            write(iouOutputSediment, '(a)') "# See summary.md for model run metadata."
            write(iouOutputSediment, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputSediment, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            if (C%includeWaterbodyBreakdown) write(iouOutputSediment, '(a)') "#\tw: waterbody index within this grid cell"
            if (C%includeWaterbodyBreakdown) then
                write(iouOutputSediment, '(a)') "#\twaterbody_type: what type (river, estuary etc) " // &
                    "of waterbody is this sediment in?"
            else
                write(iouOutputSediment, '(a)') "#\twaterbody_type: what is the dominant waterbody type in this cell?"
            end if
            write(iouOutputSediment, '(a)') "#\tm_np_total(kg), C_np_total(kg/m3), C_np_total(kg/kg): " &
                // "NM mass (kg) and concentration (kg/m3 and kg/kg dry weight) for all layers"
            if (C%includeSedimentLayerBreakdown) then
                write(iouOutputSediment, '(a)') "#\tC_np_li(kg/m3), C_np_li(kg/kg): NM conc for layer i (kg/m3 and kg/kg)"
            end if
            write(iouOutputSediment, '(a)') "#\tm_np_buried(kg): NM mass buried on this timestep (kg)"
            write(iouOutputSediment, '(a)') "#\tbed_area(m2): area of this bed sediment (m2)"
            write(iouOutputSediment, '(a)') "#\tsediment_mass(kg): total mass of fine sediment " // &
                "in this bed sediment (kg, *not* kg/m2)"
            write(iouOutputSediment, '(a)') "#\tsediment_density(kg): average density of the sediment (kg/m3)"
        end if
        ! Write the actual headers
        write(iouOutputSediment, '(a)', advance="no") "t,datetime,x,y,easts,norths," 
        if (C%includeWaterbodyBreakdown) write(iouOutputSediment, '(a)', advance='no') "w,"
        write(iouOutputSediment, '(a)', advance='no') "waterbody_type,m_np_total(kg),C_np_total(kg/m3),C_np_total(kg/kg),"
        ! Should we include sediment layer breakdown?
        if (C%includeSedimentLayerBreakdown) then
            write(iouOutputSediment, '(*(a))', advance="no") &
                ("C_np_l" // trim(str(i)) // "(kg/m3),C_np_l" // trim(str(i)) // "(kg/kg),", i = 1, C%nSedimentLayers) 
        end if
        write(iouOutputSediment, '(a)') "m_np_buried(kg),bed_area(m2),sediment_mass(kg),sediment_density(kg/m3)"
    end subroutine

    !> Write the headers for the soil output file
    subroutine writeHeadersSoilDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        if (C%writeMetadataAsComment) then
            write(iouOutputSoil, '(a)') "# NanoFASE model output data - SOIL."
            write(iouOutputSoil, '(a)') "# See summary.md for model run metadata."
            write(iouOutputSoil, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(iouOutputSoil, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputSoil, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(iouOutputSoil, '(a)') "#\tp: soil profile index within this cell"
            write(iouOutputSoil, '(a)') "#\tland_use: dominant land use of this soil profile"
            write(iouOutputSoil, '(a)') "#\tm_np_total(kg), m_transformed_total(kg), m_dissolved_total(kg): " // &
                "NM mass (pristine, transformed and dissolved) in whole soil profile, sum of free and attached NM"
            write(iouOutputSoil, '(a)') "#\tC_np_total(" // C%soilPECUnits // "), C_transformed_total(" // C%soilPECUnits // &
                "), C_dissolved_total(" // C%soilPECUnits // "): " // &
                "NM concentration averaged over all soil layers, sum of free and attached NM"
            ! Should we include a breakdown of NM state (free vs attached)?
            if (C%includeSoilStateBreakdown) then
                write(iouOutputSoil, '(a)') "#\tC_np_free(" // C%soilPECUnits // "), C_transformed_free(" // C%soilPECUnits // &
                    "): free NM concentration averaged over all soil layers"
                write(iouOutputSoil, '(a)') "#\tC_np_att(" // C%soilPECUnits // "), C_transformed_att(" // C%soilPECUnits // &
                    "): attached NM concentration averaged over all soil layers"
            end if
            ! Should we include a breakdown across the soil layers?
            if (C%includeSedimentLayerBreakdown) then
                write(iouOutputSoil, '(a)') "#\tC_np_li(" // C%soilPECUnits // "), C_transformed_li(" // C%soilPECUnits // &
                "), C_dissolved_li(" // C%soilPECUnits // "): NM concentration for layer i, sum of free and attached"
                if (C%includeSoilStateBreakdown) then
                    write(iouOutputSoil, '(a)') "#\tC_np_free_li("//C%soilPECUnits//"), C_transformed_free_li("// &
                        C%soilPECUnits//"): free NM concentration for layer i"
                    write(iouOutputSoil, '(a)') "#\tC_np_att_li("// C%soilPECUnits//"), C_transformed_att_li("// &
                        C%soilPECUnits//"): attached NM concentration for layer i"
                end if
            end if
            if (C%includeSoilErosionYields) then
                write(iouOutputSoil, '(a)') "#\tm_soil_eroded(kg), m_np_eroded(kg), m_transformed_eroded(kg): " // &
                    "mass of soil and NM eroded on this timestep"
            end if
            write(iouOutputSoil, '(a)') "#\tm_np_buried(kg), m_transformed_buried(kg), m_dissolved_buried(kg): " // &
                "mass of NM buried on this timestep"
            write(iouOutputSoil, '(a)') "#\tbulk_density(kg/m3): bulk density of this soil profile"
        end if
        ! Write the actual headers
        write(iouOutputSoil, '(a)', advance="no") "t,datetime,x,y,easts,norths,p,land_use," // &
            "m_np_total(kg),m_transformed_total(kg),m_dissolved_total(kg)," // &
            "C_np_total(" // C%soilPECUnits // "),C_transformed_total(" // C%soilPECUnits // &
            "),C_dissolved_total(" // C%soilPECUnits // "),"
        ! Should we include state breakdown - free vs attached?
        if (C%includeSoilStateBreakdown) then
            write(iouOutputSoil, '(a)', advance="no") "C_np_free("//C%soilPECUnits//"),C_transformed_free("// &
                C%soilPECUnits//"),C_np_att("//C%soilPECUnits//"),C_transformed_att("//C%soilPECUnits//"),"
        end if
        ! Should we include soil layer breakdown?
        if (C%includeSoilLayerBreakdown) then
            write(iouOutputSoil, '(*(a))', advance="no") &
                ("C_np_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_l"//trim(str(i))//"("//C%soilPECUnits//"),"// &
                 "C_dissolved_l"//trim(str(i))//"("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            if (C%includeSoilStateBreakdown) then
                write(iouOutputSoil, '(*(a))', advance="no") &
                    ("C_np_free_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_free_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
                write(iouOutputSoil, '(*(a))', advance="no") &
                    ("C_np_att_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_att_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            end if
        end if
        ! Should we include eroded soil and NM?
        if (C%includeSoilErosionYields) then
            write(iouOutputSoil, '(a)', advance='no') "m_soil_eroded(kg),m_np_eroded(kg),m_transformed_eroded(kg),"
        end if
        write(iouOutputSoil, '(a)', advance='no') "m_np_buried(kg),m_transformed_buried(kg),"
        write(iouOutputSoil, '(a)') "m_dissolved_buried(kg),bulk_density(kg/m3)"
    end subroutine

end module
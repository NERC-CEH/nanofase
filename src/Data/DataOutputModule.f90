!> Module container for the DataOutput class
module DataOutputModule
    use DefaultsModule, only: ioUnitOutputSummary, ioUnitOutputWater, &
        ioUnitOutputSediment, ioUnitOutputSoil, ioUnitOutputSSD
    use Globals, only: C, dp
    use classDatabase, only: DATASET
    use spcEnvironment
    use classEnvironment1
    use spcGridCell
    use classRiverReach
    use classEstuaryReach
    use UtilModule
    use datetime_module
    implicit none

    !> The DataOutput class is responsible for writing output data to disk
    type, public :: DataOutput
        character(len=256) :: outputPath                    !! Path to the output directory
        type(EnvironmentPointer) :: env                     !! Pointer to the environment, to retrieve state variables
        ! Storing variables across timesteps for dynamics calculations
        real(dp), allocatable :: previousSSDByLayer(:,:)
        real(dp), allocatable :: previousSSD(:)
      contains
        procedure, public :: init => initDataOutput
        procedure, public :: initSedimentSizeDistribution => initSedimentSizeDistributionDataOutput
        procedure, public :: update => updateDataOutput
        procedure, public :: finalise => finaliseDataOutput
        procedure, private :: writeHeaders => writeHeadersDataOutput
        procedure, private :: writeHeadersSimulationSummary => writeHeadersSimulationSummaryDataOutput
        procedure, private :: writeHeadersWater => writeHeadersWaterDataOutput
        procedure, private :: writeHeadersSediment => writeHeadersSedimentDataOutput
        procedure, private :: writeHeadersSoil => writeHeadersSoilDataOutput
        procedure, private :: updateWater => updateWaterDataOutput
        procedure, private :: updateSediment => updateSedimentDataOutput
        procedure, private :: updateSoil => updateSoilDataOutput
        procedure, public :: updateSedimentSizeDistribution => updateSedimentSizeDistributionDataOutput
    end type

  contains
    
    subroutine initDataOutput(me, env)
        class(DataOutput)           :: me
        type(Environment1), target  :: env
        
        ! Point the Environment object to that passed in
        me%env%item => env

        ! Open the files to write to
        open(ioUnitOutputSummary, file=trim(C%outputPath) // 'summary.md')
        if (C%writeCSV) then
            open(ioUnitOutputWater, file=trim(C%outputPath) // 'output_water.csv')
            open(ioUnitOutputSediment, file=trim(C%outputPath) // 'output_sediment.csv')
            open(ioUnitOutputSoil, file=trim(C%outputPath) // 'output_soil.csv')
            ! open(unit=104, file=trim(C%outputPath) // 'n_output_soil_biota.csv')
            ! open(unit=105, file=trim(C%outputPath) // 'n_output_water_biota.csv')
            ! open(unit=106, file=trim(C%outputPath) // 'n_output_rate_constants.csv')
        end if

        ! Write the headers for the files
        call me%writeHeaders()
    end subroutine

    subroutine initSedimentSizeDistributionDataOutput(me)
        class(DataOutput)           :: me
        integer                     :: i, j

        ! Sediment begins with equal distribution
        allocate(me%previousSSD(C%nSizeClassesSpm), &
            me%previousSSDByLayer(C%nSedimentLayers, C%nSizeClassesSpm))
        me%previousSSD = 1.0_dp / C%nSizeClassesSpm
        me%previousSSDByLayer = 1.0_dp / C%nSizeClassesSpm

        ! Open the SSD file and write the headers
        open(ioUnitOutputSSD, file=trim(C%outputPath) // 'output_ssd.csv')
        if (C%writeMetadataAsComment) then
            write(ioUnitOutputSSD, '(a)') "# NanoFASE model output data - SEDIMENT SIZE DISTRIBUTION."
            write(ioUnitOutputSSD, '(a)') "# Output file running the model until sediment size distribution is at steady state."
            write(ioUnitOutputSSD, '(a)') "# Each row represents a complete model run (as defined by the config/batch config file)."
            write(ioUnitOutputSSD, '(a)') "#\ti: model run index (number of iterations of the same input data)"
            write(ioUnitOutputSSD, '(a)') "#\tssd_sci_all_layers: sediment size distribution across size classes i, " // &
                "averaged across sediment layers"
            write(ioUnitOutputSSD, '(a)') "#\tssd_sci_lj: sediment size distribution across size classes i, for layer j"
            write(ioUnitOutputSSD, '(a)') "#\tdelta_max_lj: maximum difference between size distribution bins for layer j"
            write(ioUnitOutputSSD, '(a)') "#\tdelta_max_all_layers: maximum difference between size " // &
                "distribution bins for size distribution averaged across sediment layers"
        end if
        write(ioUnitOutputSSD, '(a)', advance='no') "i,"
        write(ioUnitOutputSSD, '(*(a))', advance='no') ('ssd_sc'//trim(str(i))//'_all_layers,', i=1, C%nSizeClassesSpm)
        write(ioUnitOutputSSD, '(*(a))', advance='no') (('ssd_sc'//trim(str(i))//'_l'//trim(str(j))//',', &
            i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(ioUnitOutputSSD, '(*(a))', advance='no') ('delta_max_l'//trim(str(i))//',', i=1, C%nSedimentLayers)
        write(ioUnitOutputSSD, '(*(a))') 'delta_max_all_layers'

    end subroutine

    !> Save the output from the current timestep to the output files
    subroutine updateDataOutput(me, t)
        class(DataOutput)   :: me       !! The DataOutput instance
        integer             :: t        !! The current timestep
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
                easts = DATASET%x(x)
                norths = DATASET%y(y)
                if (C%writeCSV) then
                    call me%updateWater(t, x, y, dateISO, easts, norths)
                    call me%updateSediment(t, x, y, dateISO, easts, norths)
                    call me%updateSoil(t, x, y, dateISO, easts, norths)
                end if
            end do
        end do
    end subroutine

    !> Update the water output file for the current timestep
    subroutine updateWaterDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: w                ! Waterbody iterator
        character(len=3)    :: reachType        ! Is this a river of estuary?
        
        ! Loop through the waterbodies in this cell
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
                write(ioUnitOutputWater, *) trim(str(t)), ",", trim(date), ",", trim(str(x)), ",", trim(str(y)), ",", &
                    trim(str(easts)), ",", trim(str(norths)), ",", trim(str(w)), ",", reachType, ",", &
                    trim(str(sum(reach%m_np))), ",", trim(str(sum(reach%C_np))), ",", &
                    trim(str(sum(reach%m_transformed))), ",", trim(str(sum(reach%C_transformed))), ",", &
                    trim(str(reach%m_dissolved)), ",", trim(str(reach%C_dissolved)), ",", &
                    trim(str(sum(reach%j_np_deposit()))), ",", &
                    trim(str(sum(reach%j_transformed_deposit()))), ",", &
                    trim(str(sum(reach%j_np_outflow()))), ",", &
                    trim(str(sum(reach%j_transformed_outflow()))), ",", &
                    trim(str(reach%j_dissolved_outflow())), ",", &
                    trim(str(sum(reach%m_spm))), ",", &
                    trim(str(sum(reach%C_spm))), ",", &
                    trim(str(reach%volume)), ",", &
                    trim(str(reach%depth)), ",", &
                    trim(str(reach%Q(1)/C%timeStep))
            end associate
        end do
    end subroutine

    !> Update the current sediment output file on the current timestep
    subroutine updateSedimentDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! The DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: w, l             ! Iterators
        character(len=3)    :: reachType        ! Is this a river of estuary?
       
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
                write(ioUnitOutputSediment, '(a)', advance='no') trim(str(t)) // "," &
                    // trim(date) // "," // trim(str(x)) // "," // trim(str(y)) &
                    // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(w)) // "," // reachType // "," // &
                    trim(str(sum(reach%bedSediment%get_m_np()) * reach%bedArea)) // "," // &            ! Converting from kg/m2 to kg
                    trim(str(sum(reach%bedSediment%get_C_np()))) // "," // &
                    trim(str(sum(reach%bedSediment%get_C_np_byMass()))) // ","
                ! Only include layer-by-layer breakdown if we've been asked to
                if (C%includeSedimentLayerBreakdown) then
                    write(ioUnitOutputSediment, '(*(a))', advance='no') (trim(str(sum(reach%bedSediment%get_C_np_l(l)))) // "," // &
                        trim(str(sum(reach%bedSediment%get_C_np_l_byMass(l)))) // ",", l=1, C%nSedimentLayers)
                end if
                write(ioUnitOutputSediment, '(a)') trim(str(sum(reach%bedSediment%get_m_np_buried() * reach%bedArea))) // "," // &
                    trim(str(reach%bedArea)) // "," // trim(str(reach%bedSediment%Mf_bed_all() * reach%bedArea))
            end associate
        end do
    end subroutine

    !> Update the sediment output file on the current timestep
    subroutine updateSoilDataOutput(me, t, x, y, date, easts, norths)
        class(DataOutput)   :: me               !! This DataOutput instance
        integer             :: t                !! The current timestep
        integer             :: x, y             !! Grid cell indices
        character(len=*)    :: date             !! Datetime of this timestep
        real                :: easts, norths    !! Eastings and northings of this grid cell
        integer             :: i, l             ! Iterators
        ! Loop through soil profiles and write row for each one
        do i = 1, me%env%item%colGridCells(x,y)%item%nSoilProfiles
            associate (profile => me%env%item%colGridCells(x,y)%item%colSoilProfiles(i)%item)
                write(ioUnitOutputSoil, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                    trim(str(x)) // "," // trim(str(y)) // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // &
                    trim(str(i)) // "," // trim(profile%dominantLandUseName) // "," // &
                    trim(str(sum(profile%get_m_np()))) // "," // trim(str(sum(profile%get_m_transformed()))) // "," // &
                    trim(str(profile%get_m_dissolved())) // "," // trim(str(sum(profile%get_C_np()))) // "," // &
                    trim(str(sum(profile%get_C_transformed()))) // "," // trim(str(profile%get_C_dissolved())) // ","
                if (C%includeSoilStateBreakdown) then
                    write(ioUnitOutputSoil, '(a)', advance='no') trim(str(sum(freeNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(freeNM(profile%get_C_transformed())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_np())))) // "," // &
                        trim(str(sum(attachedNM(profile%get_C_transformed())))) // ","
                end if
                if (C%includeSoilLayerBreakdown) then
                    write(ioUnitOutputSoil, '(*(a))', advance='no') &
                        (trim(str(sum(profile%colSoilLayers(l)%item%C_np))) // "," // &
                         trim(str(sum(profile%colSoilLayers(l)%item%C_transformed))) // "," // &
                         trim(str(profile%colSoilLayers(l)%item%C_dissolved)) // ",", l = 1, C%nSoilLayers)
                    if (C%includeSedimentLayerBreakdown) then
                        write(ioUnitOutputSoil, '(*(a))', advance='no') &
                            (trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(freeNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                        write(ioUnitOutputSoil, '(*(a))', advance='no') &
                            (trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_np)))) // "," // &
                             trim(str(sum(attachedNM(profile%colSoilLayers(l)%item%C_transformed)))) // ",", &
                             l = 1, C%nSoilLayers)
                    end if
                end if
                write(ioUnitOutputSoil, '(a)') trim(str(sum(profile%m_np_eroded(:,:,2)))) // "," // &
                    trim(str(sum(profile%m_transformed_eroded(:,:,2)))) // "," // &
                    trim(str(sum(profile%m_np_buried))) // "," // &
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
        write(ioUnitOutputSSD, '(a)', advance='no') trim(str(i_model)) // ","
        write(ioUnitOutputSSD, '(*(a))', advance='no') (trim(str(sedimentSizeDistribution(i))) // &
            ",", i=1, C%nSizeClassesSpm)
        write(ioUnitOutputSSD, '(*(a))', advance='no') ((trim(str(sedimentSizeDistributionByLayer(j,i))) // &
            ",", i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(ioUnitOutputSSD, '(*(a))', advance='no') (trim(str(delta_max_l(i)))//',', i=1, C%nSedimentLayers)
        write(ioUnitOutputSSD, '(a)') trim(str(delta_max))
        ! Update the previous SSDs to use on the next model iteration
        me%previousSSD = sedimentSizeDistribution
        me%previousSSDByLayer = sedimentSizeDistributionByLayer
    end function

    ! Finalise the data output by adding PECs to the simulation summary file and closing output files
    subroutine finaliseDataOutput(me, iSteadyState)
        class(DataOutput)   :: me
        integer             :: iSteadyState
        ! Write the final model summary info to the simulation summary file
        if (.not. C%runToSteadyState) then
            write(ioUnitOutputSummary, *) "\n## PECs"
        else
            write(ioUnitOutputSummary, *) "\n## PECs (final model iteration)"
        end if
        write(ioUnitOutputSummary, *) "- Soil, spatial mean on final timestep: " // &
            trim(str(sum(me%env%item%get_C_np_soil()))) // " kg/kg soil"
        write(ioUnitOutputSummary, *) "- Water, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_water_t, dim=1)) / size(me%env%item%C_np_water_t, dim=1))) // " kg/m3"
        write(ioUnitOutputSummary, *) "- Sediment, spatiotemporal mean: " // &
            trim(str(sum(sum(me%env%item%C_np_sediment_t, dim=1)) / size(me%env%item%C_np_sediment_t, dim=1))) // &
            " kg/kg sediment"
        if (C%runToSteadyState) then
            write(ioUnitOutputSummary, *) "\n## Steady state"
            write(ioUnitOutputSummary, *) "- Iterations until steady state: " // trim(str(iSteadyState))
            write(ioUnitOutputSummary, *) "- Time until steady state: " &
                // trim(str(iSteadyState * C%timeStep * C%nTimestepsInBatch)) // " s"
        end if
        ! Close the files
        close(ioUnitOutputSummary); close(ioUnitOutputWater); close(ioUnitOutputSediment); close(ioUnitOutputSoil)
    end subroutine

    ! Write the headers for the output files
    subroutine writeHeadersDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        ! Write headers all of the output files
        call me%writeHeadersSimulationSummary()
        call me%writeHeadersWater()
        call me%writeHeadersSediment()
        call me%writeHeadersSoil()
    end subroutine

    !> Write headers for the simulation summary file, including basic info about the model run
    !! TODO currently doesn't work with batch runs
    subroutine writeHeadersSimulationSummaryDataOutput(me)
        class(DataOutput)   :: me                   !! This DataOutput instance
        type(datetime)      :: simDatetime          ! Datetime the simulation was run 

        ! Parse some datetimes
        simDatetime = simDatetime%now()

        ! Summary file headers
        write(ioUnitOutputSummary, '(a)') "# NanoFASE model simulation summary"
        write(ioUnitOutputSummary, '(a)') " - Description: " // trim(C%runDescription)
        write(ioUnitOutputSummary, '(a)') " - Simulation datetime: " // simDatetime%isoformat()
        write(ioUnitOutputSummary, '(a)') " - Is batch run? " // trim(str(C%isBatchRun))
        write(ioUnitOutputSummary, '(a)') " - Number of batches: " // trim(str(C%nChunks))
        write(ioUnitOutputSummary, '(a)', advance='no') " - Is steady state run? " // trim(str(C%runToSteadyState))
        if (C%runToSteadyState) then
            write(ioUnitOutputSummary, '(a)') " (" // trim(C%steadyStateMode) // " mode)"
        else
            write(ioUnitOutputSummary, '(a)') ""
        end if
        write(ioUnitOutputSummary, *) "\n## Temporal domain"
        write(ioUnitOutputSummary, *) "- Start date: " // C%batchStartDate%strftime('%Y-%m-%d')
        write(ioUnitOutputSummary, *) "- End date: " // C%batchEndDate%strftime('%Y-%m-%d')
        write(ioUnitOutputSummary, *) "- Timestep length: " // trim(str(C%timeStep)) // " s"
        write(ioUnitOutputSummary, *) "- Number of timesteps: " // trim(str(C%nTimestepsInBatch))
        
        write(ioUnitOutputSummary, *) "\n## Spatial domain"
        write(ioUnitOutputSummary, *) "- Grid resolution: " // trim(str(DATASET%gridRes(1))) // ", " // &
            trim(str(DATASET%gridRes(2))) // " m" 
        write(ioUnitOutputSummary, *) "- Grid bounds: " // trim(str(DATASET%gridBounds(1))) // ", " // &
            trim(str(DATASET%gridBounds(2))) // &
            ", " // trim(str(DATASET%gridBounds(3))) // ", " // trim(str(DATASET%gridBounds(4))) // " m"
        write(ioUnitOutputSummary, *) "- Grid shape: " // trim(str(DATASET%gridShape(1))) // ", " // trim(str(DATASET%gridShape(2)))
        write(ioUnitOutputSummary, *) "- Number of non-empty grid cells: " // trim(str(me%env%item%nGridCells))
    end subroutine

    !> Write the headers for the water output file
    subroutine writeHeadersWaterDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        
        ! Write metadata, if we're meant to 
        if (C%writeMetadataAsComment) then
            write(ioUnitOutputWater, '(a)') "# NanoFASE model output data - WATER.\n# see summary.md for model run metadata."
            write(ioUnitOutputWater, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(ioUnitOutputWater, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(ioUnitOutputWater, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(ioUnitOutputWater, '(a)') "#\tw: waterbody index within this grid cell"
            write(ioUnitOutputWater, '(a)') "#\twaterbody_type: what type (river, estuary etc) is this waterbody?"
            write(ioUnitOutputWater, '(a)') "#\tm_np(kg), m_transformed(kg), m_dissolved(kg): " // &
                "NM mass (untransformed, transformed and dissolved, kg)"
            write(ioUnitOutputWater, '(a)') "#\tC_np(kg/m3), C_transformed(kg/m3), C_dissolved(kg/m3): NM concentration (kg/m3)"
            write(ioUnitOutputWater, '(a)') "#\tm_np_outflow(kg), m_transformed_outflow(kg), m_dissolved_outflow(kg): " // &
                "downstream outflow NM masses (kg)"
            write(ioUnitOutputWater, '(a)') "#\tm_np_deposited(kg), m_transformed_deposited(kg): mass of NM deposited (kg)"
            write(ioUnitOutputWater, '(a)') "#\tm_spm(kg), C_spm(kg/m3): mass and concentration of SPM (kg, kg/m3)"
            write(ioUnitOutputWater, '(a)') "#\tvolume(m3), depth(m), flow(m3/s): volume (m3), " // &
                "depth (m) and flow rate (m3/s) of this waterbody"
        end if
        ! Write the actual headers
        write(ioUnitOutputWater, '(a)') "t,datetime,x,y,easts,norths,w,waterbody_type,m_np(kg),C_np(kg/m3)," // &
            "m_transformed(kg),C_transformed(kg/m3),m_dissolved(kg),C_dissolved(kg/m3)," // &
            "m_np_deposited(kg),m_transformed_deposited(kg),m_np_outflow(kg),m_transformed_outflow(kg)," // &
            "m_dissolved_outflow(kg),m_spm(kg),C_spm(kg/m3),volume(m3),depth(m),flow(m3/s)"
    end subroutine

    !> Write the headers for the sediment output file
    subroutine writeHeadersSedimentDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        ! Write metadata, if we're meant to
        if (C%writeMetadataAsComment) then
            write(ioUnitOutputSediment, '(a)') "# NanoFASE model output data - SEDIMENT."
            write(ioUnitOutputSediment, '(a)') "# See summary.md for model run metadata."
            write(ioUnitOutputSediment, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(ioUnitOutputSediment, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(ioUnitOutputSediment, '(a)') "#\tw: waterbody index within this grid cell"
            write(ioUnitOutputSediment, '(a)') "#\twaterbody_type: what type (river, estuary etc) of waterbody is this sediment in?"
            write(ioUnitOutputSediment, '(a)') "#\tm_np_total(kg), C_np_total(kg/m3), C_np_total(kg/kg): " &
                // "NM mass (kg) and concentration (kg/m3 and kg/kg dry weight) for all layers"
            if (C%includeSedimentLayerBreakdown) then
                write(ioUnitOutputSediment, '(a)') "#\tC_np_li(kg/m3), C_np_li(kg/kg): NM conc for layer i (kg/m3 and kg/kg)"
            end if
            write(ioUnitOutputSediment, '(a)') "#\tm_np_buried(kg): NM mass buried on this timestep (kg)"
            write(ioUnitOutputSediment, '(a)') "#\tbed_area(m2): area of this bed sediment (m2)"
            write(ioUnitOutputSediment, '(a)') "#\tsediment_mass(kg): total mass of fine sediment " // &
                "in this bed sediment (kg, *not* kg/m2)"
        end if
        ! Write the actual headers
        write(ioUnitOutputSediment, '(a)', advance="no") "t,datetime,x,y,easts,norths,w,waterbody_type," // &
            "m_np_total(kg),C_np_total(kg/m3),C_np_total(kg/kg),"
        ! Should we include sediment layer breakdown?
        if (C%includeSedimentLayerBreakdown) then
            write(ioUnitOutputSediment, '(*(a))', advance="no") &
                ("C_np_l" // trim(str(i)) // "(kg/m3),C_np_l" // trim(str(i)) // "(kg/kg),", i = 1, C%nSedimentLayers) 
        end if
        write(ioUnitOutputSediment, '(a)') "m_np_buried(kg),bed_area(m2),sediment_mass(kg)"
    end subroutine

    !> Write the headers for the soil output file
    subroutine writeHeadersSoilDataOutput(me)
        class(DataOutput)   :: me           !! This DataOutput instance
        integer             :: i            ! Iterator

        if (C%writeMetadataAsComment) then
            write(ioUnitOutputSoil, '(a)') "# NanoFASE model output data - SOIL."
            write(ioUnitOutputSoil, '(a)') "# See summary.md for model run metadata."
            write(ioUnitOutputSoil, '(a)') "# Columns:\n#\tt: timestep index\n#\tdatetime: datetime of this timestep"
            write(ioUnitOutputSoil, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(ioUnitOutputSoil, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(ioUnitOutputSoil, '(a)') "#\tp: soil profile index within this cell"
            write(ioUnitOutputSoil, '(a)') "#\tland_use: dominant land use of this soil profile"
            write(ioUnitOutputSoil, '(a)') "#\tm_np_total(kg), m_transformed_total(kg), m_dissolved_total(kg): " // &
                "NM mass (untransformed, transformed and dissolved) in whole soil profile, sum of free and attached NM"
            write(ioUnitOutputSoil, '(a)') "#\tC_np_total(" // C%soilPECUnits // "), C_transformed_total(" // C%soilPECUnits // &
                "), C_dissolved_total(" // C%soilPECUnits // "): " // &
                "NM concentration averaged over all soil layers, sum of free and attached NM"
            ! Should we include a breakdown of NM state (free vs attached)?
            if (C%includeSoilStateBreakdown) then
                write(ioUnitOutputSoil, '(a)') "#\tC_np_free(" // C%soilPECUnits // "), C_transformed_free(" // C%soilPECUnits // &
                    "): free NM concentration averaged over all soil layers"
                write(ioUnitOutputSoil, '(a)') "#\tC_np_att(" // C%soilPECUnits // "), C_transformed_att(" // C%soilPECUnits // &
                    "): attached NM concentration averaged over all soil layers"
            end if
            ! Should we include a breakdown across the soil layers?
            if (C%includeSedimentLayerBreakdown) then
                write(ioUnitOutputSoil, '(a)') "#\tC_np_li(" // C%soilPECUnits // "), C_transformed_li(" // C%soilPECUnits // &
                "), C_dissolved_li(" // C%soilPECUnits // "): NM concentration for layer i, sum of free and attached"
                if (C%includeSoilStateBreakdown) then
                    write(ioUnitOutputSoil, '(a)') "#\tC_np_free_li("//C%soilPECUnits//"), C_transformed_free_li("// &
                        C%soilPECUnits//"): free NM concentration for layer i"
                    write(ioUnitOutputSoil, '(a)') "#\tC_np_att_li("// C%soilPECUnits//"), C_transformed_att_li("// &
                        C%soilPECUnits//"): attached NM concentration for layer i"
                end if
            end if
            write(ioUnitOutputSoil, '(a)') "#\tm_np_eroded(kg), m_transformed_eroded(kg): mass of NM eroded on this timestep"
            write(ioUnitOutputSoil, '(a)') "#\tm_np_buried(kg), m_transformed_buried(kg), m_dissolved_buried(kg): " // &
                "mass of NM buried on this timestep"
            write(ioUnitOutputSoil, '(a)') "#\tbulk_density(kg/m3): bulk density of this soil profile"
        end if
        ! Write the actual headers
        write(ioUnitOutputSoil, '(a)', advance="no") "t,datetime,x,y,easts,norths,p,land_use," // &
            "m_np_total(kg),m_transformed_total(kg),m_dissolved_total(kg)," // &
            "C_np_total(" // C%soilPECUnits // "),C_transformed_total(" // C%soilPECUnits // &
            "),C_dissolved_total(" // C%soilPECUnits // "),"
        ! Should we include state breakdown - free vs attached?
        if (C%includeSoilStateBreakdown) then
            write(ioUnitOutputSoil, '(a)', advance="no") "C_np_free("//C%soilPECUnits//"),C_transformed_free("// &
                C%soilPECUnits//"),C_np_att("//C%soilPECUnits//"),C_transformed_att("//C%soilPECUnits//"),"
        end if
        ! Should we include soil layer breakdown?
        if (C%includeSoilLayerBreakdown) then
            write(ioUnitOutputSoil, '(*(a))', advance="no") &
                ("C_np_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_l"//trim(str(i))//"("//C%soilPECUnits//"),"// &
                 "C_dissolved_l"//trim(str(i))//"("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            if (C%includeSoilStateBreakdown) then
                write(ioUnitOutputSoil, '(*(a))', advance="no") &
                    ("C_np_free_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_free_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
                write(ioUnitOutputSoil, '(*(a))', advance="no") &
                    ("C_np_att_l"//trim(str(i))//"("//C%soilPECUnits//"),C_transformed_att_l"//trim(str(i))// &
                     "("//C%soilPECUnits//"),", i = 1, C%nSoilLayers)
            end if
        end if
        write(ioUnitOutputSoil, '(a)', advance="no") "m_np_eroded(kg),m_transformed_eroded(kg),"// &
            "m_np_buried(kg),m_transformed_buried(kg),"
        write(ioUnitOutputSoil, '(a)') "m_dissolved_buried(kg),bulk_density(kg/m3)"
    end subroutine

end module
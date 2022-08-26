module Globals
    use mo_netcdf
    use datetime_module
    use mod_strptime, only: f_strptime
    use VersionModule, only: MODEL_VERSION
    use DefaultsModule, only: iouConfig, iouBatchConfig, iouVersion, configDefaults
    use ErrorCriteriaModule
    use ErrorInstanceModule
    use ResultModule, only: Result
    implicit none
    
    type(ErrorCriteria)             :: ERROR_HANDLER                        ! Global error handling
    integer, parameter              :: dp = selected_real_kind(15, 307)     ! Double precision
    character(len=*), parameter     :: ESC = char(27)                       ! Terminal escape character
    character(len=*), parameter     :: COLOR_BLUE = ESC // "[94m"           ! Escape sequence for blue text
    character(len=*), parameter     :: COLOR_LIGHT_BLUE = ESC // "[39m"     ! Escape sequence for light blue text
    character(len=*), parameter     :: COLOR_GREEN = ESC // "[32m"          ! Escape sequence for green text
    character(len=*), parameter     :: COLOR_YELLOW = ESC // "[33m"         ! Escape sequence for yellow text
    character(len=*), parameter     :: COLOR_RED = ESC // "[91m"            ! Escape sequence for red text
    character(len=*), parameter     :: COLOR_RESET = ESC // "[0m"           ! Escape sequence to reset text color

    type, public :: GlobalsType
        ! Get model version from the version module (which our build script should modify)
        character(len=16)   :: modelVersion = MODEL_VERSION

        ! Data input
        character(len=256)  :: inputFile
        character(len=256)  :: constantsFile
        
        ! Data output 
        character(len=256)  :: outputPath                       !! Path to directory to store output data
        character(len=32)   :: outputHash                       !! Hash to append to output file names. Useful for parallel runs.
        logical             :: writeCSV                         !! Should output data be written as CSV file?
        logical             :: writeNetCDF                      !! Should output data be written as NetCDF file?
        character(len=3)    :: netCDFWriteMode                  !! Should NetCDF output be written on each timestep or at the end of the chunk?
        logical             :: writeCompartmentStats            !! Should a file with summary stats for each compartment (soil, water, sediment) be output?
        logical             :: writeMetadataAsComment           !! Should CSV files be prepended with metadata as a #-delimited comment?
        logical             :: includeWaterbodyBreakdown        !! Should the surface water output include breakdown of waterbodies?
        logical             :: includeSedimentLayerBreakdown    !! Include breakdown of data over sediment layers?
        logical             :: includeSoilLayerBreakdown        !! Include breakdown of data over soil layers?
        character(len=5)    :: soilPECUnits                     !! What units to use for soil PEC - kg/m3 or kg/kg dw?
        character(len=5)    :: sedimentPECUnits                 !! What units to use for sediment PEC - kg/m4 or kg/kg dw?
        logical             :: includeSoilStateBreakdown        !! Should the breakdown of NM state (free vs attached) be included?
        logical             :: includeSedimentFluxes            !! Should sediment fluxes to/from waterbodies be included?
        logical             :: includeSpmSizeClassBreakdown     !! Should the breakdown of SPM size classes be included?
        logical             :: includeSoilErosionYields         !! Should sediment fluxes to/from waterbodies be included?

        ! Run
        character(len=256)  :: runDescription                   !! Short description of model run
        character(len=256)  :: logFilePath                      !! Log file path
        logical             :: writeToLog                       !! Should a log file be used?
        character(len=256)  :: configFilePath                   !! Config file path
        type(datetime)      :: startDate                        !! Datetime object representing the start date
        integer             :: timeStep                         !! The timestep to run the model on [s]
        integer             :: nTimeSteps                       !! The number of timesteps
        real(dp)            :: epsilon = 1e-10                  !! Used as proximity to check whether variable as equal
        integer             :: warmUpPeriod                     !! How long before we start inputting NM (to give flows to reach steady state)?
        logical             :: triggerWarnings                  !! Should error warnings be printed to the console?
        logical             :: hasSimulationMask = .false.      !! Are we meant to mask the simulation (i.e. only use a subset of the input dataset)?
        character(len=256)  :: simulationMaskPath = ""          !! Path to NetCDF simulation mask
        logical             :: ignoreNM                         !! If .true., miss out costly NM calculations. Useful for sediment calibration, NM PECs will be invalid
        logical             :: bashColors                       !! Should output to the console use ANSI color codes?

        ! Checkpointing
        character(len=256)  :: checkpointFile                   !! Path to checkpoint file, to save to and/or read from
        logical             :: saveCheckpoint                   !! Should a checkpoint be saved when the run finishes?
        logical             :: saveCheckpointAfterWarmUp        !! Should a checkpoint be saved after the warm up period?
        logical             :: reinstateCheckpoint              !! Should a checkpoint be reinstated before the run starts?
        logical             :: preserveTimestep                 !! Should the final timestep from the checkpoint be used to start the reinstated run?

        ! Steady state
        logical             :: runToSteadyState                 !! Should the model be run until steady state by iterating over current simulation input data?
        character(len=50)   :: steadyStateMode                  !! Mode defines what variable will be used to assess steady state
        real(dp)            :: steadyStateDelta                 !! Delta value used to test whether we're at steady state

        ! Compartments
        real, allocatable   :: soilLayerDepth(:)                !! Soil layer depth [m]
        real, allocatable   :: sedimentLayerDepth(:)            !! Sediment layer depth [m]
        logical             :: includeBioturbation              !! Should bioturbation be modelled?
        logical             :: includePointSources              !! Should point sources be included?
        logical             :: includeBedSediment               !! Should the bed sediment be included?
        logical             :: includeAttachment                !! Should attachment to soil be included?
        logical             :: includeSoilErosion               !! Should soil erosion be included?
        logical             :: includeClayEnrichment            !! Should clay enrichment be included?
        integer             :: nSoilLayers                      !! Number of soil layers to be modelled
        integer             :: nSedimentLayers                  !! Number of sediment layers to be modelled
        real                :: minStreamSlope                   !! Minimum stream slope, imposed where calculated stream slope is less than this value [m/m]
        integer             :: minEstuaryTimestep               !! Minimum timestep (displacement) length for modelling estuarine dynamics [s]
        logical             :: includeEstuary                   !! Should we simulate an estuary, or treat everything as a river?
        logical             :: includeBankErosion               !! Should we simulate the inflow of sediment from bank erosion?

        ! Batch run
        integer                         :: nChunks = 1          !! Numbers of chunks to run
        logical                         :: isBatchRun = .false. !! Are we batch running?
        character(len=256), allocatable :: batchInputFiles(:)   !! Paths to input files for each chunk
        character(len=256), allocatable :: batchConstantFiles(:) !! Paths to constants files for each chunk
        type(datetime), allocatable     :: batchStartDates(:)   !! Start dates for each chunk
        integer, allocatable            :: batchNTimesteps(:)   !! Number of timesteps for each chunk
        character(len=256), allocatable :: batchConfigFiles(:)  !! Paths to config files for batches
        integer                         :: nTimestepsInBatch    !! Total number of timesteps in batch run
        type(datetime)                  :: batchStartDate       !! Start date of batch run
        type(datetime)                  :: batchEndDate         !! End date of batch run

        ! Checkpointing
        integer                         :: t0 = 1               !! Used to preserve timestep between checkpoints. Timestep number at start of run

        ! General
        type(NcDataset) :: dataset                          !! The NetCDF dataset

        ! Physical constants
        real(dp) :: g = 9.80665_dp          !! Gravitational acceleration [m/s^2]
        real(dp) :: k_B = 1.38064852e-23    !! Boltzmann constant [m2 kg s-2 K-1]
        real(dp) :: pi = 4*atan(1.0_dp)     !! Pi [-]
        real(dp) :: n_river = 0.035_dp      !! Manning's roughness coefficient, for natural streams and major rivers.
                                            !! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).

        ! Temp
        real(dp) :: T = 15.0_dp             !! Temperature [C]

        ! Size class distributions
        real, allocatable :: d_spm(:)                       !! Suspended particulate matter size class diameters [m]
        real, allocatable :: d_spm_low(:)                   !! Lower bound when treating each size class as distribution [m]
        real, allocatable :: d_spm_upp(:)                   !! Upper bound when treating each size class as distribution [m]
        real, allocatable :: d_nm(:)                        !! Nanomaterial size class diameters [m]
        real, allocatable :: sedimentParticleDensities(:)   !! Sediment particle densities [kg m-3]
        integer :: nSizeClassesSpm                          !! Number of sediment particle size classes
        integer :: nSizeClassesNM                           !! Number of nanoparticle size classes
        integer :: nFracCompsSpm                            !! Number of sediment fractional compositions
        integer :: nFormsNM                                 !! Number of NM forms (e.g. pristine, transformed, etc)
        integer :: nExtraStatesNM                           !! Number of NM states other than heteroaggregated to SPM
        integer, allocatable :: defaultDistributionSediment(:) !! Default imposed size distribution for sediment
        integer, allocatable :: defaultDistributionNP(:)    !! Default imposed size distribution for NPs
        integer :: npDim(3)                                 !! Default dimensions for arrays of NM
        integer :: ionicDim                                 !! Default dimensions for ionic metal

      contains
        procedure :: rho_w      ! Density of water
        procedure :: nu_w       ! Kinematic viscosity of water
        procedure :: mu_w       ! Dynamic viscosity of water
        procedure :: audit      ! Audit the config options
    end type

    type(GlobalsType) :: C

  contains

    !> Initialise global variables, such as `ERROR_HANDLER`
    subroutine GLOBALS_INIT()
        integer :: n, i                                     ! Iterators
        integer :: nmlIOStat                                ! IO status for namelist reading
        type(ErrorInstance) :: errors(17)                   ! ErrorInstances to be added to ErrorHandler
        character(len=256) :: configFilePath, batchRunFilePath
        integer :: configFilePathLength, batchRunFilePathLength
        ! Values from config file
        character(len=256) :: input_file, constants_file, output_path, log_file_path, start_date, &
            startDateStr, description, checkpoint_file, batch_description, simulation_mask
        character(len=50) :: mode
        character(len=256), allocatable :: input_files(:), constants_files(:), start_dates(:)
        character(len=5) :: soil_pec_units, sediment_pec_units
        character(len=3) :: netcdf_write_mode
        character(len=32) :: output_hash
        integer, allocatable :: n_timesteps_per_chunk(:)
        integer :: n_nm_size_classes, n_nm_forms, n_nm_extra_states, warm_up_period, n_spm_size_classes, &
            n_fractional_compositions, n_chunks
        integer :: timestep, n_timesteps, n_soil_layers, n_sediment_layers, min_estuary_timestep
        real :: min_stream_slope
        real(dp) :: epsilon, delta
        real, allocatable :: soil_layer_depth(:), nm_size_classes(:), spm_size_classes(:), &
            sediment_particle_densities(:), sediment_layer_depth(:)
        logical :: error_output, include_bioturbation, include_attachment, include_point_sources, include_bed_sediment, &
            write_csv, write_netcdf, write_metadata_as_comment, include_sediment_layer_breakdown, &
            include_soil_layer_breakdown, include_soil_state_breakdown, save_checkpoint, reinstate_checkpoint, &
            preserve_timestep, trigger_warnings, run_to_steady_state, include_sediment_fluxes, include_soil_erosion_yields, &
            write_to_log, include_spm_size_class_breakdown, include_clay_enrichment, include_waterbody_breakdown, &
            write_compartment_stats, ignore_nm, include_estuary, bash_colors, save_checkpoint_after_warm_up, include_bank_erosion, &
            include_soil_erosion
        
        ! Config file namelists
        namelist /allocatable_array_sizes/ n_soil_layers, n_nm_size_classes, n_spm_size_classes, &
            n_fractional_compositions, n_sediment_layers
        namelist /nanomaterial/ n_nm_forms, n_nm_extra_states, nm_size_classes
        namelist /data/ input_file, constants_file, output_path
        namelist /output/ write_metadata_as_comment, include_sediment_layer_breakdown, include_soil_layer_breakdown, &
            soil_pec_units, sediment_pec_units, include_soil_state_breakdown, write_csv, include_sediment_fluxes, &
            include_soil_erosion_yields, include_spm_size_class_breakdown, include_waterbody_breakdown, write_compartment_stats, &
            write_netcdf, netcdf_write_mode
        namelist /run/ timestep, n_timesteps, epsilon, error_output, log_file_path, start_date, warm_up_period, &
            description, trigger_warnings, simulation_mask, write_to_log, output_hash, ignore_nm, bash_colors
        namelist /checkpoint/ checkpoint_file, save_checkpoint, reinstate_checkpoint, preserve_timestep, &
            save_checkpoint_after_warm_up
        namelist /steady_state/ run_to_steady_state, mode, delta
        namelist /soil/ soil_layer_depth, include_bioturbation, include_attachment, include_clay_enrichment, include_soil_erosion
        namelist /sediment/ spm_size_classes, include_bed_sediment, sediment_particle_densities, sediment_layer_depth
        namelist /water/ min_stream_slope, min_estuary_timestep, include_estuary, include_bank_erosion
        namelist /sources/ include_point_sources

        ! Batch config namelists
        namelist /batch_config/ n_chunks, batch_description
        namelist /chunks/ input_files, constants_files, start_dates, n_timesteps_per_chunk

        ! Defaults, which will be overwritten if present in config file
        ! TODO move all defaults to DefaultsModule.f90
        write_to_log = configDefaults%writeToLog                                ! True
        write_csv = configDefaults%writeCSV                                     ! True
        write_netcdf = configDefaults%writeNetCDF                               ! False
        netcdf_write_mode = configDefaults%netCDFWriteMode                      ! 'end'
        output_hash = configDefaults%outputHash                                 ! ''
        description = configDefaults%description                                ! 'NanoFASE model run'
        batch_description = configDefaults%description                          ! 'NanoFASE model run'
        write_metadata_as_comment = configDefaults%writeMetadataAsComment       ! True
        include_sediment_layer_breakdown = configDefaults%includeSedimentLayerBreakdown  ! True
        include_soil_layer_breakdown = configDefaults%includeSoilLayerBreakdown  ! True
        include_soil_state_breakdown = configDefaults%includeSoilStateBreakdown ! False
        include_sediment_fluxes = configDefaults%includeSedimentFluxes          ! False
        include_spm_size_class_breakdown = configDefaults%includeSpmSizeClassBreakdown  ! False
        include_soil_erosion_yields = configDefaults%includeSoilErosionYields   ! False
        include_clay_enrichment = configDefaults%includeClayEnrichment          ! False
        soil_pec_units = configDefaults%soilPECUnits                            ! kg/kg
        sediment_pec_units = configDefaults%sedimentPECUnits                    ! kg/kg
        save_checkpoint = configDefaults%saveCheckpoint                         ! False
        save_checkpoint_after_warm_up = configDefaults%saveCheckpointAfterWarmUp ! False
        checkpoint_file = configDefaults%checkpointFile                         ! ./checkpoint.dat
        reinstate_checkpoint = configDefaults%reinstateCheckpoint               ! False
        preserve_timestep = configDefaults%preserveTimeStep                     ! False
        run_to_steady_state = configDefaults%runToSteadyState                   ! False
        delta = configDefaults%steadyStateDelta                                 ! 1e-5
        mode = configDefaults%steadyStateMode                                   ! 'sediment_size_distribution'
        simulation_mask = configDefaults%simulationMask                         ! ''
        min_stream_slope = configDefaults%minStreamSlope                        ! 0.001
        min_estuary_timestep = configDefaults%minEstuaryTimestep                ! 3600
        include_waterbody_breakdown = configDefaults%includeWaterbodyBreakdown  ! True
        write_compartment_stats = configDefaults%writeCompartmentStats          ! False
        ignore_nm = configDefaults%ignoreNM                                     ! False
        include_estuary = configDefaults%includeEstuary                         ! True
        include_bank_erosion = configDefaults%includeBankErosion                ! True
        warm_up_period = configDefaults%warmUpPeriod                            ! 0
        bash_colors = configDefaults%bashColors                                 ! True
        include_soil_erosion = configDefaults%includeSoilErosion                ! True

        ! Has a path to the config path been provided as a command line argument?
        call get_command_argument(1, configFilePath, configFilePathLength)
        call get_command_argument(2, batchRunFilePath, batchRunFilePathLength)

        ! Open the config file, or try and find one at config/config.nml if it can't be found 
        if (configFilePathLength > 0) then
            open(iouConfig, file=trim(configFilePath), status="old")
            C%configFilePath = configFilePath
        else
            open(iouConfig, file="config/config.nml", status="old")
            C%configFilePath = "config/config.nml"
        end if

        ! If this is a batch run, then open the batch run config file and store the data from it
        if (batchRunFilePathLength > 0) then
            C%isBatchRun = .true.
            ! Open and read the namelists
            open(iouBatchConfig, file=trim(batchRunFilePath), status="old")
            read(iouBatchConfig, nml=batch_config); rewind(iouBatchConfig)
            C%nChunks = n_chunks
            ! Allocate variables based on the number of batches
            allocate(input_files(C%nChunks), &
                constants_files(C%nChunks), &
                start_dates(C%nChunks), &
                n_timesteps_per_chunk(C%nChunks))
            ! Now we can read the other variables in
            read(iouBatchConfig, nml=chunks)
            ! Store these in config variables
            allocate(C%batchInputFiles, source=input_files)
            allocate(C%batchConstantFiles, source=constants_files)
            allocate(C%batchStartDates(C%nChunks))
            allocate(C%batchNTimesteps, source=n_timesteps_per_chunk)
            ! Turn the datetime string into a datetime object
            do i = 1, C%nChunks
                C%batchStartDates(i) = f_strptime(start_dates(i))
            end do
            ! Close the file
            close(iouBatchConfig)
        end if

        read(iouConfig, nml=allocatable_array_sizes); rewind(iouConfig)
        ! Use the allocatable array sizes to allocate those arrays (allocatable arrays
        ! must be allocated before being read in to)
        allocate(soil_layer_depth(n_soil_layers))
        allocate(sediment_layer_depth(n_sediment_layers))
        allocate(nm_size_classes(n_nm_size_classes))
        allocate(spm_size_classes(n_spm_size_classes))
        allocate(sediment_particle_densities(n_fractional_compositions))
        ! Carry on reading in the different config groups
        read(iouConfig, nml=nanomaterial); rewind(iouConfig)
        read(iouConfig, nml=data); rewind(iouConfig)
        read(iouConfig, nml=output); rewind(iouConfig)
        read(iouConfig, nml=run); rewind(iouConfig)
        ! Checkpoint and steady state - check if groups exist before reading
        read(iouConfig, nml=checkpoint, iostat=nmlIOStat); rewind(iouConfig)
        if (nmlIOStat .ge. 0) read(iouConfig, nml=checkpoint); rewind(iouConfig)
        read(iouConfig, nml=steady_state, iostat=nmlIOStat); rewind(iouConfig)
        if (nmlIOStat .ge. 0) read(iouConfig, nml=steady_state); rewind(iouConfig)
        read(iouConfig, nml=soil); rewind(iouConfig)
        read(iouConfig, nml=sediment); rewind(iouConfig)
        read(iouConfig, nml=water, iostat=nmlIOStat); rewind(iouConfig)
        if (nmlIOStat .ge. 0) read(iouConfig, nml=water); rewind(iouConfig)
        read(iouConfig, nml=sources)
        close(iouConfig)
        
        ! Store this data in the Globals variable
        ! Nanomaterial
        C%nSizeClassesNM = n_nm_size_classes
        C%nFormsNM = n_nm_forms
        C%nExtraStatesNM = n_nm_extra_states
        allocate(C%d_nm, source=nm_size_classes)
        ! Data
        C%inputFile = input_file
        C%constantsFile = constants_file
        C%outputPath = output_path
        C%outputHash = output_hash
        ! Output
        C%writeCSV = write_csv
        C%writeNetCDF = write_netcdf
        C%netCDFWriteMode = netcdf_write_mode
        C%writeMetadataAsComment = write_metadata_as_comment
        C%writeCompartmentStats = write_compartment_stats
        C%includeWaterbodyBreakdown = include_waterbody_breakdown
        C%includeSedimentLayerBreakdown = include_sediment_layer_breakdown
        C%includeSoilLayerBreakdown = include_soil_layer_breakdown
        C%soilPECUnits = soil_pec_units
        C%sedimentPECUnits = sediment_pec_units
        C%includeSoilStateBreakdown = include_soil_state_breakdown
        C%includeSedimentFluxes = include_sediment_fluxes
        C%includeSoilErosionYields = include_soil_erosion_yields
        C%includeSpmSizeClassBreakdown = include_spm_size_class_breakdown
        ! Run
        if (.not. C%isBatchRun) then
            C%runDescription = description
        else
            C%runDescription = batch_description
        end if
        C%logFilePath = log_file_path
        C%writeToLog = write_to_log
        C%timeStep = timestep
        C%nTimeSteps = n_timesteps
        C%epsilon = epsilon
        startDateStr = start_date
        C%startDate = f_strptime(startDateStr)
        C%triggerWarnings = trigger_warnings
        if (.not. trim(simulation_mask) == "") then
            C%hasSimulationMask = .true.
            C%simulationMaskPath = simulation_mask
        end if
        C%ignoreNM = ignore_nm
        C%warmUpPeriod = warm_up_period
        C%bashColors = bash_colors
        ! Checkpointing
        C%checkpointFile = checkpoint_file
        C%saveCheckpoint = save_checkpoint
        C%saveCheckpointAfterWarmUp = save_checkpoint_after_warm_up
        C%reinstateCheckpoint = reinstate_checkpoint
        C%preserveTimestep = preserve_timestep
        ! Steady state
        C%runToSteadyState = run_to_steady_state
        C%steadyStateMode = mode
        C%steadyStateDelta = delta
        ! Sediment
        C%sedimentLayerDepth = sediment_layer_depth
        C%nSizeClassesSpm = n_spm_size_classes
        C%includeBedSediment = include_bed_sediment
        C%nSedimentLayers = n_sediment_layers
        allocate(C%d_spm, source=spm_size_classes)
        C%nFracCompsSpm = n_fractional_compositions
        allocate(C%sedimentParticleDensities, source=sediment_particle_densities)
        ! Soil
        C%nSoilLayers = n_soil_layers
        C%soilLayerDepth = soil_layer_depth
        C%includeBioturbation = include_bioturbation
        C%includeAttachment = include_attachment
        C%includeClayEnrichment = include_clay_enrichment
        C%includeSoilErosion = include_soil_erosion
        ! Water
        C%minStreamSlope = min_stream_slope
        C%minEstuaryTimestep = min_estuary_timestep
        C%includeEstuary = include_estuary
        C%includeBankErosion = include_bank_erosion
        ! Sources
        C%includePointSources = include_point_sources

        ! If this is batch run, then use the config options in the batch config file
        ! to override those given in the config file. Also set some variables about the
        ! whole batch run
        if (C%isBatchRun) then
            C%inputFile = C%batchInputFiles(1)
            C%constantsFile = C%batchConstantFiles(1)
            C%startDate = C%batchStartDates(1)
            C%nTimeSteps = C%batchNTimesteps(1)
            C%nTimestepsInBatch = sum(C%batchNTimesteps)
            C%batchStartDate = C%batchStartDates(1)
            C%batchEndDate = C%batchStartDates(C%nChunks) + timedelta(C%batchNTimesteps(C%nChunks) - 1)
        else
            C%nTimestepsInBatch = C%nTimesteps
            C%batchStartDate = C%startDate
            C%batchEndDate = C%startDate + timedelta(C%nTimeSteps - 1)
            allocate(C%batchNTimesteps(1))
            C%batchNTimesteps(1) = C%nTimeSteps
        end if

        allocate(C%d_spm_low(C%nSizeClassesSpm))
        allocate(C%d_spm_upp(C%nSizeClassesSpm))
        ! Set the upper and lower bounds of each size class, if treated as a distribution
        do n = 1, C%nSizeClassesSpm
            ! Set the upper and lower limit of the size class's distributions
            if (n == C%nSizeClassesSpm) then
                C%d_spm_upp(n) = 1                                              ! failsafe overall upper size limit
            else
                C%d_spm_upp(n) = C%d_spm(n+1) - (C%d_spm(n+1)-C%d_spm(n))/2     ! Halfway between d_1 and d_2
            end if                
        end do
        do n = 1, C%nSizeClassesSpm
            if (n == 1) then
                C%d_spm_low(n) = 0                                              ! Particles can be any size below d_upp,1
            else
                C%d_spm_low(n) = C%d_spm_upp(n-1)                               ! lower size boundary equals upper size boundary of lower size class
            end if
        end do        

        ! Array to store default NM and ionic array dimensions. NM:
        !   1: NP size class
        !   2: form (core, shell, coating, corona)
        !   3: state (free, bound, heteroaggregated)
        ! Ionic: Form (free ion, solution, adsorbed)
        C%npDim = [C%nSizeClassesNM, C%nFormsNM, C%nSizeClassesSpm + C%nExtraStatesNM]
        
        ! General
        errors(1) = ErrorInstance(code=110, message="Invalid object type index in data file.")
        ! File operations
        errors(2) = ErrorInstance(code=200, message="File not found.")
        errors(3) = ErrorInstance(code=201, message="Variable not found in input file.")
        ! Numerical calculations
        errors(6) = ErrorInstance(code=300, message="Newton's method failed to converge.")
        ! Grid and geography
        errors(7) = ErrorInstance(code=401, &
            message="Invalid RiverReach inflow reference. Inflow must be from a neighbouring RiverReach.")
        errors(8) = ErrorInstance(code=402, &
            message="Invalid RiverReach inflow reference. If multiple inflows are specified, they must " // &
                        "be inflows to the GridCell and all come from the same GridCell.")
        errors(9) = ErrorInstance(code=403, &
            message="RiverReach cannot have more than 5 inflows.")
        errors(10) = ErrorInstance(code=404, &
            message="RiverReach outflow could not be determined. Reaches must either be specified as " // &
                        "inflow to downstream reach, or have a model domain outflow specified.")
        errors(11) = ErrorInstance(code=405, &
            message="RiverReach lengths specified in input data sum to greater than straight-line river branch " // &
                        "length. Are you sure this is intended?", isCritical=.false.)
        ! River routing
        errors(11) = ErrorInstance(code=500, &
            message="All SPM advected from RiverReach.", isCritical=.false.)
        errors(12) = ErrorInstance(code=501, &
            message="No input data provided for required SubRiver - check nSubRivers is correct.")
        ! Soil
        errors(13) = ErrorInstance(code=600, message="All water removed from SoilLayer.", isCritical=.false.)
        ! General
        errors(14) = ErrorInstance(code=901, message="Invalid RiverReach type index provided.")
        errors(15) = ErrorInstance(code=902, message="Invalid Biota index provided.")
        errors(16) = ErrorInstance(code=903, message="Invalid Reactor index provided.")
        errors(17) = ErrorInstance(code=904, message="Invalid BedSedimentLayer index provided.")

        ! Add custom errors to the error handler
        call ERROR_HANDLER%init(errors=errors, triggerWarnings=C%triggerWarnings, on=error_output)
        
        ! Auditing the config. Must be done after error handler and logger
        ! have been initialised
        call C%audit()

    end subroutine

    !> Audit the config file options
    subroutine audit(me)
        class(GlobalsType), intent(in)  :: me
        type(Result)                    :: rslt

        ! Steady state mode
        if (me%runToSteadyState) then
            if (trim(me%steadyStateMode) /= 'sediment_size_distribution') then
                call rslt%addError(ErrorInstance( &
                    message='Invalid or non-present config file value for &steady_state > mode.' &
                ))
            end if
        end if
        ! NetCDF write mode must be itr or end
        if (me%netCDFWriteMode /= 'itr' .and. me%netCDFWriteMode /= 'end') then
            call rslt%addError(ErrorInstance( &
                message='Invalid config file value for &output > netcdf_write_mode. Should be "itr" or "end"'))
        end if
        ! Warm up period must be smaller than number of time steps
        if (me%warmUpPeriod > me%nTimeSteps) then
            call rslt%addError(ErrorInstance(message='Warm up period must be less than or equal to the number of ' // &
                'time steps in the model run (or first chunk).'))
        end if

        ! CHECKPOINT
        ! Add warning if saving checkpoint at warm up and end of run
        if (C%saveCheckpoint .and. C%saveCheckpointAfterWarmUp) then
            call rslt%addError(ErrorInstance(message='You have specified to save a checkpoint after warm up ' // &
                'and at the end of the model run. Only the latter will be saved to file.', isCritical=.false.))
        end if
        
        ! Trigger the errors, if there were any
        call rslt%addToTrace('Auditing config file')
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
    end subroutine

    !> Calculate the density of water at a given temperature \( T \):
    !! $$
    !!      \rho_{\text{w}}(T) = 1000 \left( 1 - \frac{T + 288.9414}{508929.2 (T + 68.12963) (T - 3.9863^2)} \right)
    !! $$
    !! and optionally with a given salinity \( S \):
    !! $$
    !!      \rho_{\text{w,s}}(T,S) = \rho_w + AS + BS^{3/2} + CS^2
    !! $$
    !! where \( A = 0.824493 - 0.0040899T + 0.000076438T^2 -0.00000082467T^3 + 0.0000000053675T^4 \),
    !! \( B = -0.005724 + 0.00010227T - 0.0000016546T^2 \) and \( C = 4.8314 \times 10^{-4} \).
    !! Reference:
    !! [D. R. Maidment, Handbook of Hydrology (2012)](https://books.google.co.uk/books/about/Handbook_of_hydrology.html?id=4_9OAAAAMAAJ)
    function rho_w(me, T, S)
        class(GlobalsType), intent(in) :: me                    !! This `Constants` instance
        real, intent(in) :: T                                   !! Temperature \( T \) [deg C]
        real(dp), intent(in), optional :: S                     !! Salinity \( S \) [g/kg]
        real(dp) :: rho_w                                       !! Density of water \( \rho_w \) [kg/m**3].
        if (present(S)) then
            rho_w = 1000.0_dp*(1-(T+288.9414_dp)/(508929.2_dp*(T+68.12963_dp))*(T-3.9863_dp)**2) &
                    + (0.824493_dp - 0.0040899_dp*T + 0.000076438_dp*T**2 - 0.00000082467_dp*T**3 + 0.0000000053675_dp*T**4)*S &
                    + (-0.005724_dp + 0.00010227_dp*T - 0.0000016546_dp*T**2)*S**(3.0_dp/2.0_dp) &
                    + 0.00048314_dp*S**2
        else
            rho_w = 1000.0_dp*(1-(T+288.9414_dp)/(508929.2_dp*(T+68.12963_dp))*(T-3.9863_dp)**2)
        end if
    end function

    !> Calculate the kinematic viscosity of water \( \nu_w \) at given temperature \( T \)
    !! and optionally salinity \( S \):
    !! $$
    !!      \nu_{\text{w}}(T,S) = \frac{1}{\rho_w(T,S)} 2.414\times 10^{-5} \cdot 10^{\frac{247.8}{(T+273.15)-140.0}}
    !! $$
    !! Reference: [T. Al-Shemmeri](http://varunkamboj.typepad.com/files/engineering-fluid-mechanics-1.pdf)
    function nu_w(me, T, S)
        class(GlobalsType), intent(in) :: me                    !! This Globals instance
        real, intent(in) :: T                                   !! Temperature \( T \) [deg C]
        real(dp), intent(in), optional :: S                     !! Salinity \( S \) [g/kg]
        real(dp) :: nu_w                                        !! Kinematic viscosity of water \( \nu_{\text{w}} \)
        if (present(S)) then
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T,S)
        else
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T)
        end if
    end function
    
    !> Calculate the dynamic viscosity of water \( \mu_w \) at a given temperature \( T \)
    !! $$
    !!      \nu_{\text{w}}(T,S) = 2.414\times 10^{-5} \cdot 10^{\frac{247.8}{(T+273.15)-140.0}}
    !! $$
    !! Reference: [T. Al-Shemmeri](http://varunkamboj.typepad.com/files/engineering-fluid-mechanics-1.pdf)
    function mu_w(me, T)
        class(GlobalsType), intent(in) :: me
        real, intent(in) :: T
        real(dp) :: mu_w
        mu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))
    end function
end module

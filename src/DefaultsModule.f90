!> The DefaultsModule holds default values used throughout the model, such as those
!! used in input data or config
module DefaultsModule
    implicit none

    ! Double precision reals. Private for the moment as it's also in Globals
    integer, private, parameter :: dp = selected_real_kind(15, 307)

    ! Config file IO units
    integer, parameter :: iouConfig = 1
    integer, parameter :: iouBatchConfig = 2
    integer, parameter :: iouVersion = 3
    ! Constants file IO units
    integer, parameter :: iouConstants = 10
    ! Output file IO units
    integer, parameter :: iouOutputSummary = 100
    integer, parameter :: iouOutputWater = 101
    integer, parameter :: iouOutputSediment = 102
    integer, parameter :: iouOutputSoil = 103
    integer, parameter :: iouOutputSSD = 104
    integer, parameter :: iouOutputStats = 105
    ! Checkpoint and logging
    integer, parameter :: iouCheckpoint = 500
    integer, parameter :: iouLog = 501

    ! Defaults for config, encapsulated in a type
    type, public :: ConfigDefaultsType
        ! Run
        character(len=256)  :: description = "NanoFASE model run"
        logical             :: runToSteadyState = .false.
        character(len=50)   :: steadyStateMode = 'sediment_size_distribution'   ! Run to steady state of which variable?
        real(dp)            :: steadyStateDelta = 1e-5
        real                :: minStreamSlope = 0.0001                  ! [m/m]
        integer             :: minEstuaryTimestep = 3600                ! 1 hour [s] 
        logical             :: writeToLog = .true.
        integer             :: warmUpPeriod = 0                         ! How many time steps to warm the model up for
        logical             :: bashColors = .true.                      ! Should we print colored output to the console?
        ! Output
        logical             :: writeCSV = .true.                        ! Should output data be written to CSV files?
        logical             :: writeNetCDF = .false.                    ! Should output data be written to NetCDF?
        character(len=3)    :: netCDFWriteMode = 'end'                  ! When to write the NetCDF file - every time step ('itr') or at the end ('end')
        logical             :: writeCompartmentStats = .false.          ! Should a summary stats file for each compartment be written?
        logical             :: includeWaterbodyBreakdown = .true.       ! For surface water breakdown, should breakdown over waterbodies be included?
        logical             :: includeSedimentFluxes = .false.          ! Include sediment fluxes in output?
        logical             :: includeSedimentLayerBreakdown = .true.   ! Output breakdown across sediment layers?
        logical             :: includeSoilLayerBreakdown = .true.       ! Output breakdown across soil layers?
        logical             :: includeSoilStateBreakdown = .false.      ! Output breakdown of different soil states?
        logical             :: includeSoilErosionYields = .false.       ! Should we output soil erosion yields?
        logical             :: includeSpmSizeClassBreakdown = .false.
        logical             :: includeClayEnrichment = .false.
        character(len=5)    :: soilPECUnits = 'kg/kg'                   ! Should soil PECS be kg/kg or kg/m3?
        character(len=5)    :: sedimentPECUnits = 'kg/kg'               ! Should sediment PECs be kg/kg or kg/m3?
        logical             :: writeMetadataAsComment = .true.          ! Should metadata be added to the top of CSV files as # comments
        ! Checkpoint
        logical             :: saveCheckpoint = .false.                 ! Should we save a checkpoint file after the model run?
        logical             :: saveCheckpointAfterWarmUp = .false.      ! Should we save a checkpoint file after the warm up period?
        character(len=256)  :: checkpointFile = './checkpoint.dat'      ! Where to save the checkpoint
        logical             :: reinstateCheckpoint = .false.            ! Should we be reinstating a checkpoint file?
        logical             :: preserveTimeStep = .false.               ! Should the time step be preserved when reinstating a checkpoint?
        ! Run
        character(len=32)   :: outputHash = ''                          ! Hash to append to output file names
        logical             :: ignoreNM = .false.                       ! If .true., costly NM calculations are missed out. Useful for sediment calibation
        character(len=256)  :: simulationMask = ''                      ! Path to model simulation mask (or empty if there isn't one)
        ! Water
        logical             :: includeEstuary = .true.                  ! Should we model estuaries, or treat them as rivers?
        logical             :: includeBankErosion = .true.              ! Should we include the inflow of sediment from bank erosion?
        ! Soil
        logical             :: includeSoilErosion = .true.              ! Should we model soil erosion?
    end type
    ! Object to exposre the config defaults
    type(ConfigDefaultsType) :: configDefaults

    ! Defaults for constants
    real, parameter :: defaultSoilAttachmentEfficiency = 0.0
    real, parameter :: default_k_diss_pristine = 0.0
    real, parameter :: default_k_diss_transformed = 0.0
    real, parameter :: default_k_transform_pristine = 0.0
    real, parameter :: defaultEstuaryMeanderingFactor = 1.0
    real, parameter :: defaultRiverMeanderingFactor = 1.0
    real, parameter :: defaultShearRate = 10.0                      ! Arvidsson et al, 2009: https://doi.org/10.1080/10807039.2011.538639
    real, parameter :: defaultMinWaterTemperature = 4.0             ! Thames River
    real, parameter :: defaultMaxWaterTemperature = 21.0            ! Thames River
    integer, parameter :: defaultMinWaterTemperatureDayOfYear = 32  ! Thames River
    real(dp), parameter :: defaultSedimentTransport_a = 2.0e-9_dp
    real(dp), parameter :: defaultSedimentTransport_b = 0.0_dp
    real(dp), parameter :: defaultSedimentTransport_c = 0.2_dp
    real(dp), parameter :: defaultSedimentEnrichment_k = 1.0_dp
    real(dp), parameter :: defaultSedimentEnrichment_a = 0.0_dp
    real(dp), parameter :: defaultSlope = 0.0005_dp
    real(dp), parameter :: defaultDepositionAlpha = 38.1_dp         ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real(dp), parameter :: defaultDepositionBeta =  0.93_dp         ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real(dp), parameter :: defaultBankErosionAlpha = 1.0e-9_dp      ! [kg/m5] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
    real(dp), parameter :: defaultBankErosionBeta = 1.0_dp          ! [-] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
end module

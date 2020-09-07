!> The DefaultsModule holds default values used throughout the model, such as those
!! used in input data or config
module DefaultsModule
    implicit none

    ! Double precision reals. Private for the moment as it's also in Globals
    integer, private, parameter :: dp = selected_real_kind(15, 307)

    ! Config file IO units
    integer, parameter :: iouConfig = 1
    integer, parameter :: iouBatchConfig = 2
    ! Constants file IO units
    integer, parameter :: iouConstants = 10
    integer, parameter :: iouCalibrationSites = 11
    ! Output file IO units
    integer, parameter :: iouOutputSummary = 100
    integer, parameter :: iouOutputWater = 101
    integer, parameter :: iouOutputSediment = 102
    integer, parameter :: iouOutputSoil = 103
    integer, parameter :: iouOutputSSD = 104
    ! Checkpoint and logging
    integer, parameter :: iouCheckpoint = 500
    integer, parameter :: iouLog = 501

    ! Defaults for config, encapsulated in a type
    type, public :: ConfigDefaultsType
        ! Run
        logical             :: runToSteadyState = .false.
        character(len=50)   :: steadyStateMode = ''
        real(dp)            :: steadyStateDelta = 1e-5
        character(len=7)    :: calibrationMode = 'mean'
        real                :: minimumStreamSlope = 0.0001
        logical             :: writeToLog = .true.
        ! Output
        logical             :: includeSedimentFluxes = .false.
        logical             :: includeSoilErosion = .false.
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
    real, parameter :: defaultSedimentWashload = 0.0
    real(dp), parameter :: defaultSedimentTransport_a = 5.0_dp
    real(dp), parameter :: defaultSedimentTransport_b = 0.0_dp
    real(dp), parameter :: defaultSedimentTransport_c = 0.8_dp
    real(dp), parameter :: defaultSlope = 0.0005_dp

end module

!> The DefaultsModule holds default values used throughout the model, such as those
!! used in input data or config
module DefaultsModule
    implicit none

    ! Double precision reals. Private for the moment as it's also in Globals
    integer, private, parameter :: dp = selected_real_kind(15, 307)

    ! Config file IO units
    integer, parameter :: ioUnitConfig = 1
    integer, parameter :: ioUnitBatchConfig = 2
    ! Constants file IO units
    integer, parameter :: ioUnitConstants = 10
    integer, parameter :: ioUnitCalibrationSites = 11
    ! Output file IO units
    integer, parameter :: ioUnitOutputSummary = 100
    integer, parameter :: ioUnitOutputWater = 101
    integer, parameter :: ioUnitOutputSediment = 102
    integer, parameter :: ioUnitOutputSoil = 103
    integer, parameter :: ioUnitOutputSSD = 104
    ! Checkpoint and logging
    integer, parameter :: ioUnitCheckpoint = 500
    integer, parameter :: ioUnitLog = 501

    ! Defaults for config, encapsulation in a type
    type, public :: ConfigDefaultsType
        logical             :: runToSteadyState = .false.
        character(len=50)   :: steadyStateMode = ''
        real(dp)            :: steadyStateDelta = 1e-5
        character(len=7)    :: calibrationMode = 'mean'
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

end module

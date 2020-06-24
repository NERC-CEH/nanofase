module DefaultsModule
    implicit none

    ! Config file IO units
    integer, parameter :: ioUnitConfig = 1
    integer, parameter :: ioUnitBatchConfig = 2
    ! Constants file IO units
    integer, parameter :: ioUnitConstants = 10
    ! Output file IO units
    integer, parameter :: ioUnitOutputSummary = 100
    integer, parameter :: ioUnitOutputWater = 101
    integer, parameter :: ioUnitOutputSediment = 102
    integer, parameter :: ioUnitOutputSoil = 103

    ! Defaults for constants
    real, parameter :: defaultSoilAttachmentEfficiency = 0.0
    real, parameter :: default_k_diss_pristine = 0.0
    real, parameter :: default_k_diss_transformed = 0.0
    real, parameter :: default_k_transform_pristine = 0.0
    real, parameter :: defaultEstuaryMeanderingFactor = 1.0
    real, parameter :: defaultRiverMeanderingFactor = 1.0

end module

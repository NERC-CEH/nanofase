!> The ConstantsDefaultsModule holds default values for constants used in the model
module ConstantsDefaultsModule
    implicit none

    ! Defaults for constants
    real, parameter :: defaultSoilAttachmentEfficiency = 0.0
    real, parameter :: defaultRiverAttachmentEfficiency = 0.0  ! Attachment efficiency for NM to SPM in rivers
    real, parameter :: defaultSoilDarcyVelocity = 9e-6      ! [m/s] Tufenkji et al, 2004: https://doi.org/10.1021/es034049r 
    real, parameter :: default_k_diss_pristine = 0.0        ! Dissolution rate for pristine contaminant [s-1]
    real, parameter :: default_k_diss_transformed = 0.0     ! Dissolution rate for transformed contaminant [s-1]
    real, parameter :: default_k_transform_pristine = 0.0   ! Transformation rate for pristine contaminant [s-1]
    real, parameter :: default_rho_contaminant = 1000.0     ! Density of contaminant [kg/m3]
    real, parameter :: defaultShearRate = 10.0              ! Arvidsson et al, 2009: https://doi.org/10.1080/10807039.2011.538639
    real, parameter :: defaultMinWaterTemperature = 4.0     ! Thames River
    real, parameter :: defaultMaxWaterTemperature = 21.0    ! Thames River
    integer, parameter :: defaultMinWaterTemperatureDayOfYear = 32  ! Thames River
    real, parameter :: defaultSedimentTransport_a = 2.0e-9
    real, parameter :: defaultSedimentTransport_b = 0.0
    real, parameter :: defaultSedimentTransport_c = 0.2
    real, parameter :: defaultSedimentEnrichment_k = 1.0
    real, parameter :: defaultSedimentEnrichment_a = 0.0
    real, parameter :: defaultSlope = 0.0005
    real, parameter :: defaultDepositionAlpha = 38.1        ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real, parameter :: defaultDepositionBeta = 0.93         ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real, parameter :: defaultBankErosionAlpha = 1.0e-9     ! [kg/m5] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
    real, parameter :: defaultBankErosionBeta = 1.0         ! [-] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
    ! Default for contaminant form distribution (pristine, transformed, dissolved)
    real, parameter :: defaultContaminantFormDistribution(3) = [1.0, 0.0, 0.0]  ! All in pristine form
end module
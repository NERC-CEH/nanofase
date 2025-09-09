!> The ConstantsDefaultsModule holds default values for constants used in the model
module ConstantsDefaultsModule
    implicit none

    integer, private, parameter :: dp = selected_real_kind(15, 307)

    ! Defaults for constants
    real(dp), parameter :: defaultSoilAttachmentEfficiency = 0.0_dp
    real(dp), parameter :: defaultRiverAttachmentEfficiency = 0.0_dp  ! Attachment efficiency for NM to SPM in rivers
    real(dp), parameter :: defaultEstuaryAttachmentEfficiency = 0.0_dp
    real(dp), parameter :: defaultSoilDarcyVelocity = 9.0e-6_dp      ! [m/s] Tufenkji et al, 2004: https://doi.org/10.1021/es034049r 
    real(dp), parameter :: default_k_diss_pristine = 0.0_dp        ! Dissolution rate for pristine contaminant [s-1]
    real(dp), parameter :: default_k_diss_transformed = 0.0_dp     ! Dissolution rate for transformed contaminant [s-1]
    real(dp), parameter :: default_k_transform_pristine = 0.0_dp   ! Transformation rate for pristine contaminant [s-1]
    real(dp), parameter :: default_rho_contaminant = 1000.0_dp     ! Density of contaminant [kg/m3]
    real(dp), parameter :: defaultShearRate = 10.0_dp              ! Arvidsson et al, 2009: https://doi.org/10.1080/10807039.2011.538639
    real(dp), parameter :: defaultMinWaterTemperature = 4.0_dp     ! Thames River
    real(dp), parameter :: defaultMaxWaterTemperature = 21.0_dp    ! Thames River
    integer, parameter  :: defaultMinWaterTemperatureDayOfYear = 32  ! Thames River
    real(dp), parameter :: defaultSedimentTransport_a = 2.0e-9_dp
    real(dp), parameter :: defaultSedimentTransport_b = 0.0_dp
    real(dp), parameter :: defaultSedimentTransport_c = 0.2_dp
    real(dp), parameter :: defaultSedimentEnrichment_k = 1.0_dp
    real(dp), parameter :: defaultSedimentEnrichment_a = 0.0_dp
    real(dp), parameter :: defaultSlope = 0.0005_dp
    real(dp), parameter :: defaultDepositionAlpha = 38.1_dp        ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real(dp), parameter :: defaultDepositionBeta = 0.93_dp         ! Zhiyao et al, 2008: https://doi.org/10.1016/S1674-2370(15)30017-X
    real(dp), parameter :: defaultBankErosionAlpha = 1.0e-9_dp     ! [kg/m5] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
    real(dp), parameter :: defaultBankErosionBeta = 1.0_dp         ! [-] Loosely based on Lazar et al, 2010: https://doi.org/10.1016/j.scitotenv.2010.02.030
    ! Default for contaminant form distribution (pristine, transformed, dissolved)
    real(dp), parameter :: defaultContaminantFormDistribution(3) = [1.0_dp, 0.0_dp, 0.0_dp]  ! All in pristine form
end module
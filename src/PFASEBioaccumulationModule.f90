module PFASEBioaccumulationModule
    use GlobalsModule, only: dp
    implicit none
contains
    pure function pfase_bio_uptake_fraction(k_uptake, dt) result(frac)
        real(dp), intent(in) :: k_uptake, dt
        real(dp) :: frac
        frac = 0.0_dp
        if (dt <= 0.0_dp) return
        frac = max(0.0_dp, min(1.0_dp, 1.0_dp - exp(-max(0.0_dp, k_uptake) * dt)))
    end function pfase_bio_uptake_fraction

    pure function pfase_body_burden_step(C_old, C_env, k_uptake, k_elim, k_growth, k_death, dt_days) result(C_new)
        real(dp), intent(in) :: C_old, C_env, k_uptake, k_elim, k_growth, k_death, dt_days
        real(dp) :: C_new, k_loss, C_ss
        k_loss = max(0.0_dp, k_elim + k_growth + k_death)
        if (k_loss <= 0.0_dp) then
            C_new = C_old + max(0.0_dp, k_uptake) * max(0.0_dp, C_env) * max(0.0_dp, dt_days)
        else
            C_ss = max(0.0_dp, k_uptake) * max(0.0_dp, C_env) / k_loss
            C_new = C_ss + (C_old - C_ss) * exp(-k_loss * max(0.0_dp, dt_days))
        end if
        C_new = max(0.0_dp, C_new)
    end function pfase_body_burden_step
end module 
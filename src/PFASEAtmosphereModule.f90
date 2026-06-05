module PFASEAtmosphereModule
    use GlobalsModule, only: dp
    implicit none
contains
    pure function pfase_first_order_fraction(k, dt) result(frac)
        real(dp), intent(in) :: k, dt
        real(dp) :: frac
        frac = 0.0_dp
        if (dt <= 0.0_dp) return
        frac = max(0.0_dp, min(1.0_dp, 1.0_dp - exp(-max(0.0_dp, k) * dt)))
    end function pfase_first_order_fraction

    pure function pfase_deposition_mass(concentration_air, deposition_velocity, area, dt) result(mass)
        real(dp), intent(in) :: concentration_air, deposition_velocity, area, dt
        real(dp) :: mass
        mass = max(0.0_dp, concentration_air) * max(0.0_dp, deposition_velocity) * &
               max(0.0_dp, area) * max(0.0_dp, dt)
    end function pfase_deposition_mass
end module 

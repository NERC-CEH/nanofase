module PFASEFoamModule
    use GlobalsModule, only: dp
    implicit none
contains
    pure function pfase_foam_fraction(wind_speed, turbulence, foam_coeff, dt) result(frac)
        real(dp), intent(in) :: wind_speed, turbulence, foam_coeff, dt
        real(dp) :: frac, rate
        if (dt <= 0.0_dp) then
            frac = 0.0_dp
            return
        end if
        rate = max(0.0_dp, foam_coeff) * max(0.0_dp, wind_speed) * max(0.0_dp, turbulence)
        frac = max(0.0_dp, min(1.0_dp, 1.0_dp - exp(-rate * dt)))
    end function pfase_foam_fraction

    pure function pfase_foam_enriched_mass(surface_mass, foam_fraction, enrichment_factor) result(mass)
        real(dp), intent(in) :: surface_mass, foam_fraction, enrichment_factor
        real(dp) :: mass
        mass = max(0.0_dp, surface_mass) * max(0.0_dp, min(1.0_dp, foam_fraction)) * &
               max(0.0_dp, enrichment_factor)
        mass = min(max(0.0_dp, surface_mass), mass)
    end function pfase_foam_enriched_mass
end module

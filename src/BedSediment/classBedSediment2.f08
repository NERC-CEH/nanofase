module classBedSediment2                                             ! class definition for BedSediment2
    use Globals
    use ResultModule
    use spcBedSediment                                               ! use BedSediment superclass
    implicit none                                                    ! force declaration of all variables
    private

    type, public, extends(BedSediment) :: &
        BedSediment2                                                 ! type declaration for class - extends abstract superclass

      contains
        procedure, public :: Resuspension => calculateResuspensionBedSediment2
        procedure, public :: StreamPower => calculateStreamPowerBedSediment2
    end type

  contains
    !> Calculate resuspension from bed sediment using
    !! [Bussi](http://www.sciencedirect.com/science/article/pii/S0022169416305625):
    !! $$
    !!      m_{\text{ent}} = a m_{\text{bed}} \alpha \omega \frac{R_\text{h}}{R_{\text{h,max}}}
    !! $$
    function calculateResuspensionBedSediment2(me, a, m_bed, alpha, omega, R_h, R_hmax) result(r)
        class(BedSediment2) :: me
        real(dp) :: a                                   !! Calibration factor [s2/kg]
        real(dp) :: m_bed                               !! Bed mass per unit area [kg/m2]
        real(dp) :: alpha                               !! Proportion of size class that can be resuspended [-]
        real(dp) :: omega                               !! Stream power per unit area of stream bed [J/s/m2]
        real(dp) :: R_h                                 !! Actual hydraulic radius [m]
        real(dp) :: R_hmax                              !! Maximum hydraulic radius [m]
        real(dp) :: f                                   !! Friction factor [-]
        type(Result0D) :: r
        f = R_h/R_hmax                                  ! Calculate the friction factor
        r = Result( &
            data = a * m_bed * alpha * omega * f &       ! Calculate the resuspension
        )
    end function

    !> Calculate the stream power (per unit area of stream bed) using Bagnold's
    !! stream power equation:
    !! $$
    !!      \omega = \frac{\rho g Q S}{W}
    !! $$
    !! Reference: [Bagnold, 1966](https://www.uvm.edu/~wbowden/Teaching/Stream_Geomorph_Assess/Resources/Private/Documents/1966_Bagnold_river_sediments.pdf)
    function calculateStreamPowerBedSediment2(me, rho_water, g, Q, W, S) result(r)
        class(BedSediment2) :: me
        real(dp) :: rho_water                           !! Density of water [kg/m3]
        real(dp) :: g                                   !! Gravitational acceleration [m/s]
        real(dp) :: Q                                   !! Discharge [m3/s]
        real(dp) :: W                                   !! River width [m]
        real(dp) :: S                                   !! River slope [m/m]
        type(Result0D) :: r
        r = Result( &
            data = rho_water * g * Q * S / W &
        )
    end function


end module

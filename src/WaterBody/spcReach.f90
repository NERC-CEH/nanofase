!> Module containing definition of abstract base class `Reach`.
module spcReach
    use Globals
    use ResultModule, only: Result
    use ErrorInstanceModule
    use spcWaterBody
    use mo_netcdf               ! See below TODO re me%ncGroup
    implicit none

    !> `ReachPointer` used for `Reach` inflows array, so the elements within can
    !! point to other `Reach`'s colReach elements
    type ReachPointer
        class(Reach), pointer :: item => null()                  !! Pointer to polymorphic `Reach` object
    end type

    !> Abstract base class for `Reach`, which represents any flowing water body
    !! (rivers, estuaries).
    type, abstract, public, extends(WaterBody) :: Reach
        ! Linking water bodies
        type(WaterBodyRef), allocatable :: inflowRefs(:)            !! References to inflow reaches
        type(ReachPointer), allocatable :: inflows(:)               !! Array of points to inflow reaches
        type(ReachPointer) :: outflow                               !! Pointer to the outflow from this reach
        integer, allocatable :: domainOutflow(:)
            !! If this `RiverReach` flows out of the gridded domain, this array is used to specify where to,
            !! for reach length calculations
        integer :: nInflows                                         !! Integer to store the number of inflows to this reach in
        logical :: isHeadwater = .false.                            !! Is this reach a headwater (no inflows)?
        logical :: isGridCellInflow = .false.                       !! Is this reach the inflow the `GridCell` its in
        logical :: isGridCellOutflow = .false.                      !! Does the reach outflow to another cell?
        logical :: isDomainOutflow = .false.                        !! Does the reach flow out of the gridded domain?
        integer :: branch = 0                                       !! Which branch is this reach on in the GridCell? 0 = not processed yet
        ! Physical properties
        real(dp) :: slope                                           !! Slope of reach [m/m]
        real(dp) :: width                                           !! Width of the reach [m]
        real(dp) :: xsArea                                          !! The cross-sectional area of water in the reach [m2]
        real(dp) :: f_m                                             !! Meandering factor used for calculating river volume. Default to 1 (no meandering).
        real(dp) :: length                                          !! Length of the river, without meandering factor [m]
        real(dp) :: velocity                                        !! Water velocity [m/s]
        real(dp) :: alpha_resus                                     !! Maximum resuspendable particle size calibration param [-]
        real(dp) :: beta_resus                                      !! Resuspension calibration factor [s2 kg-1]
        real(dp) :: n                                               !! Manning's roughness coefficient [-]
        ! Transformation properties
        real(dp) :: alpha_hetero                                    !! Heteroaggregation attachment efficiency, 0-1 [-]
        ! TODO NetCDF group needed to pass to bed sediment. Need to deprecate this eventually to save memory
        type(NcGroup) :: ncGroup

      contains
        ! Simulators
        procedure(resuspensionReach), deferred :: resuspension
        procedure(settlingReach), deferred :: settling
        procedure(depositToBedReach), deferred :: depositToBed
        ! Data handlers
        procedure(setDefaultsReach), deferred :: setDefaults
        ! Calculators
        procedure :: calculateSettlingVelocity => calculateSettlingVelocity
        procedure :: calculateResuspension => calculateResuspension
        ! Getters
        procedure :: getVolume => getVolumeReach
        procedure :: getQOut => getQOutReach
        procedure :: get_j_spm_out => get_j_spm_outReach
    end type

    !> Container type for `class(Reach)`, the actual type of the `Reach` class.
    !! a variable of type `ReachElement` can be of any object type inheriting from the
    !! `Reach` abstract base class.
    type ReachElement                                          
        class(Reach), allocatable :: item                      !! Polymorphic `Reach` object
    end type

    abstract interface
        !> Resuspend sediment based on current river flow
        function resuspensionReach(me) result(r)
            use ResultModule, only: Result
            import Reach
            class(Reach) :: me                                      !! This `Reach` instance
            type(Result) :: r                                       !! The `Result` object to return
        end function

        !> Set settling rate \( k_{\text{settle}} \) for this time step
        function settlingReach(me) result(r)
            use ResultModule, only: Result
            import Reach
            class(Reach) :: me                                      !! This `Reach` instance
            type(Result) :: r                                       !! The `Result` object to return
        end function

        function depositToBedReach(me, spmDep) result(r)
            use Globals
            use ResultModule, only: Result
            import Reach
            class(Reach) :: me                                      !! This `Reach` instance
            real(dp) :: spmDep(C%nSizeClassesSpm)                   !! The SPM to deposit
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function

        subroutine setDefaultsReach(me)
            import Reach
            class(Reach) :: me                                      !! This `Reach` instance
        end subroutine

    end interface

  contains

    !> Calculate the settling velocity of sediment particles for an individual
    !! size class:
    !! $$
    !!      W_{\text{spm}} = \frac{\nu}{d} d_{*}^3 (38.1 + 0.93 d_{*}^{12/7})^{-7/8}
    !! $$
    !! where
    !! $$
    !!      d_{*} = \left( \frac{\Delta g}{\nu^2} \right)^{1/3} d
    !! $$
    !! and
    !! $$
    !!      \Delta = \frac{\rho_{\text{spm}}}{\rho} - 1
    !! $$
    !! Reference: [Zhiyao et al, 2008](https://doi.org/10.1016/S1674-2370(15)30017-X).
    function calculateSettlingVelocity(Me, d, rho_particle, T) result(W)
        class(Reach), intent(in) :: me                          !! The `Reach` instance
        real(dp), intent(in) :: d                               !! Sediment particle diameter [m]
        real(dp), intent(in) :: rho_particle                    !! Sediment particulate density [kg/m3]
        real(dp), intent(in) :: T                               !! Temperature [C]
        real(dp) :: W                                           !! Calculated settling velocity [m/s]
        real(dp) :: dStar                                       ! Dimensionless particle diameter.
        ! Settling only occurs if SPM particle density is greater than density of water
        if (rho_particle > C%rho_w(T)) then
            dStar = ((rho_particle/C%rho_w(T) - 1)*C%g/C%nu_w(T)**2)**(1.0_dp/3.0_dp) * d   ! Calculate the dimensional particle diameter
            W = (C%nu_w(T)/d) * dStar**3 * (38.1_dp + 0.93_dp &                             ! Calculate the settling velocity
                * dStar**(12.0_dp/7.0_dp))**(-7.0_dp/8.0_dp)
        else
            W = 0.0_dp
        end if
    end function

    !> Calculate the resuspension flux of sediment particles
    function calculateResuspension(me, beta, L, W, M_prop, omega, f_fr) result(k_res)
        class(Reach), intent(in) :: me                          !! This `Reach` instance
        real(dp), intent(in) :: beta                            !! Calibration parameter \( \beta \) [s2 kg-1]
        real(dp), intent(in) :: L                               !! Reach length \( L = lf_{\text{m}} \) [m]
        real(dp), intent(in) :: W                               !! Reach width \( W \) [m]
        real(dp), intent(in) :: M_prop(C%nSizeClassesSPM)       !! Proportion of this size class that is resuspenable \( M_{\text{prop}} \) [-]
        real(dp), intent(in) :: omega                           !! Stream power per unit bed area \( \omega \) [kg m-2]
        real(dp), intent(in) :: f_fr                            !! Friction factor \( f \) [-]
        real(dp) :: k_res(C%nSizeClassesSpm)                    !! Calculated resuspension flux \( j_{\text{res}} \) [s-1]
        k_res = beta*L*W*M_prop*omega*f_fr
    end function

    !> Return the volume of the `Reach`.
    function getVolumeReach(me) result(volume)
        class(Reach) :: me
        real(dp) :: volume
        volume = me%volume
    end function

    !> Return the outflow.
    function getQOutReach(me) result(Q_out)
        class(Reach) :: me
        real(dp) :: Q_out
        if (allocated(me%Q)) then
            Q_out = me%Q(1)                 ! First element of Q is the outflow
        end if
    end function

    !> Return the SPM discahrge.
    function get_j_spm_outReach(me) result(j_spm_out)
        class(Reach) :: me
        real(dp) :: j_spm_out(size(me%j_spm,2))
        j_spm_out = me%j_spm(1,:)           ! First element of j_spm is the outflow
    end function


end module

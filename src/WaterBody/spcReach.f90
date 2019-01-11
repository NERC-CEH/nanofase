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
        procedure :: resuspension => resuspensionReach
        procedure :: settling => settlingReach
        procedure :: depositToBed => depositToBedReach
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

  contains

    function settlingReach(me) result(rslt)
        class(Reach) :: me                      !! This `Reach` instance
        type(Result) :: rslt                    !! The `Result` object to return any errors in
        integer :: i                            ! Size class iterator
        ! SPM: Loop through the size classes and calculate settling velocity
        do i = 1, C%nSizeClassesSpm
            me%W_settle_spm(i) = me%calculateSettlingVelocity( &
                C%d_spm(i), &
                sum(C%rho_spm)/C%nFracCompsSpm, &       ! Average of the fractional comps. TODO: Change to work with actual fractional comps.
                me%T_water &
            )
        end do
        me%k_settle = me%W_settle_spm/me%depth

        ! NP: Calculate this to pass to Reactor
        do i = 1, C%nSizeClassesNP
            me%W_settle_np(i) = me%calculateSettlingVelocity( &
                C%d_np(i), &
                4230.0_dp, &       ! HACK: rho_np, change this to account for different NP materials
                me%T_water &
            )
        end do
    end function


    function depositToBedReach(me, spmDep) result(rslt)
        class(Reach)        :: me                           !! This Reach instance
        real(dp)            :: spmDep(C%nSizeClassesSpm)    !! The SPM to deposit [kg/reach]
        type(Result)        :: rslt                         !! The data object to return any errors in
        type(Result0D)      :: depositRslt                  !! Result from the bed sediment's deposit procedure
        real(dp)            :: V_water_toDeposit            !! Volume of water to deposit to bed sediment [m3/m2]
        type(FineSediment1) :: fineSediment(C%nSizeClassesSpm) ! FineSediment object to pass to BedSediment
        integer             :: n                            ! Loop iterator
        ! Create the FineSediment object and add deposited SPM to it
        ! (converting units of Mf_in to kg/m2), then give that object
        ! to the BedSediment
        ! TODO: What f_comp should be input? SL: THAT OF THE DEPOSITING SEDIMENT
        do n = 1, C%nSizeClassesSpm
            !HACK
            call rslt%addErrors(.errors. fineSediment(n)%create("FineSediment_class_" // trim(str(n)), 4))
            !TODO: allow number of compositional fractions to be set 
            call rslt%addErrors(.errors. fineSediment(n)%set( &
                Mf_in=spmDep(n)/me%bedArea, &
                f_comp_in=C%defaultFractionalComp/100.0_dp &
            ))
        end do
        
        ! COMMENTED BEDSEDIMENT STUFF OUT TO GET MODEL RUNNING WITHOUT FPE ERRORS
        ! AND WITH REASONABLE RUNTIME

        if (C%includeBedSediment) then
            ! Deposit the fine sediment to the bed sediment
            depositRslt = Me%bedSediment%deposit(fineSediment)
            call rslt%addErrors(.errors. depositRslt)
            if (rslt%hasCriticalError()) then
                ! print *, "Error in DepositSediment"
                ! call rslt%addToTrace("Depositing SPM to BedSediment")
                return
            end if
        end if
        ! TODO add error handling to line above as it causes a crash if there is a critical error in the called method
        ! Retrieve the amount of water to be taken from the reach
        V_water_toDeposit = .dp. depositRslt                ! [m3/m2]
        ! Subtract that volume for the reach (as a depth)
        ! TODO: Subtracting the water doesn't have any effect at the moment,
        ! since the depth is recalculated based on hydrology at the start
        ! of every time step.
        me%depth = me%depth - V_water_toDeposit

        ! Add any errors that occured in the deposit procedure
        call rslt%addToTrace("Depositing SPM to BedSediment")
    end function

    !> Compute the resuspension rate [s-1] for a time step
    !! Reference: [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub)
    function resuspensionReach(me) result(rslt)
        class(Reach) :: me                      !! This `Reach` instance
        type(Result) :: rslt                    !! `Result` object to return with any errors
        !---
        real(dp) :: d_max                       ! Maximum resuspendable particle size [m]
        integer :: i                            ! Iterator
        real(dp) :: M_prop(C%nSizeClassesSpm)   ! Proportion of size class that can be resuspended [-]
        real(dp) :: omega                       ! Stream power per unit bed area [W m-2]
        real(dp) :: f_fr                        ! Friction factor [-]
        real(dp) :: mbed(C%nSizeClassesSpm)     ! mass of fine material in the sediment [kg]

        ! There must be inflow for there to be resuspension
        if (me%Q_in_total > 0) then
            ! Calculate maximum resuspendable particle size and proportion of each
            ! size class that can be resuspended. Changes on each timestep as dependent
            ! on river depth
            d_max = 9.994*sqrt(me%alpha_resus*C%g*me%depth*me%slope)**2.5208 
            ! Calculate proportion of each size class that can be resuspended
            do i = 1, C%nSizeClassesSpm
                ! Calculate the proportion of size class that can be resuspended
                if (d_max < C%d_spm_low(i)) then
                    M_prop(i) = 0                                    ! None can be resuspended
                else if (d_max > C%d_spm_upp(i)) then
                    M_prop(i) = 1                                    ! All can be resuspended
                else
                    M_prop(i) = (d_max - C%d_spm_low(i)) &           ! Only some can be resuspended
                        / (C%d_spm_upp(i) - C%d_spm_low(i))     
                end if
            end do
            ! Calculate the stream power per unit bed area
            omega = C%rho_w(C%T)*C%g*(me%Q_in_total/C%timeStep)*me%slope/me%width
            f_fr = 4*me%depth/(me%width+2*me%depth)
            ! Calculate the resuspension
            ! TODO: [DONE REQUIRES CHECKING] Get masses of bed sediment by size fraction
            ! r1D = Me%bedSediment%Mf_bed_by_size()                    ! retrieve bed masses [kg/m2] by size class
            ! call r%addErrors(.errors. r1D)                           ! add any errors to trace
            ! if (r%hasCriticalError()) then                           ! if call throws a critical error
            !     call r%addToTrace(trim(Me%ref // "Getting bed sediment mass"))   ! add trace to all errors
            !     return                                               ! and exit
            ! end if
            ! mbed = .dp. r1D                                          ! extract mbed from Result object (1D array => 1D array
            
            me%k_resus = me%calculateResuspension( &
                beta = me%beta_resus, &
                L = me%length*me%f_m, &
                W = me%width, &
                M_prop = M_prop, &
                omega = omega, &
                f_fr = f_fr &
            )
        else
            me%k_resus = 0                                ! If there's no inflow
        end if
        call rslt%addToTrace('Calculating resuspension mass for ' // trim(me%ref))
    end function

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

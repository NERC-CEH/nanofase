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
        integer :: streamOrder = 0                                  !! Shreve stream order of this reach [-]
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
        ! Data
        procedure :: allocateAndInitialise => allocateAndInitialiseReach
        ! Simulators
        procedure :: setResuspensionRate => setResuspensionRateReach
        procedure :: setSettlingRate => setSettlingRateReach
        procedure :: depositToBed => depositToBedReach
        ! Calculators
        procedure :: calculateSettlingVelocity => calculateSettlingVelocity
        procedure :: calculateResuspension => calculateResuspension
        ! Getters
        procedure :: Q_outflow_final => Q_outflow_finalReach
        procedure :: j_spm_outflow_final => j_spm_outflow_finalReach
        procedure :: j_np_outflow_final => j_np_outflow_finalReach
        procedure :: Q_outflow
        procedure :: Q_inflows
        procedure :: Q_runoff
        procedure :: Q_transfers
        procedure :: j_spm_outflow
        procedure :: j_spm_inflows
        procedure :: j_spm_runoff
        procedure :: j_spm_transfers
        procedure :: j_spm_deposit
        procedure :: j_np_outflow
        procedure :: j_np_inflows
        procedure :: j_np_runoff
        procedure :: j_np_transfer
        procedure :: j_np_deposit
        procedure :: j_np_diffusesource
        procedure :: j_np_pointsource
        ! Setters
        procedure :: set_Q_outflow
        procedure :: set_Q_inflow
        procedure :: set_Q_inflows
        procedure :: set_Q_runoff
        procedure :: set_Q_transfers
        procedure :: set_j_spm_outflow
        procedure :: set_j_spm_inflow
        procedure :: set_j_spm_inflows
        procedure :: set_j_spm_runoff
        procedure :: set_j_spm_transfers
        procedure :: set_j_spm_deposit
        procedure :: set_j_np_outflow
        procedure :: set_j_np_inflow
        procedure :: set_j_np_inflows
        procedure :: set_j_np_runoff
        procedure :: set_j_np_transfers
        procedure :: set_j_np_deposit
        procedure :: set_j_np_diffusesource
        procedure :: set_j_np_pointsource
        ! Static methods
        procedure, nopass :: Q_outflowStatic
        procedure, nopass :: j_spm_outflowStatic
        procedure, nopass :: j_np_outflowStatic
        generic, public :: out => Q_outflowStatic, j_spm_outflowStatic, j_np_outflowStatic
        procedure, nopass :: Q_inflowsStatic
        procedure, nopass :: j_spm_inflowsStatic
        procedure, nopass :: j_np_inflowsStatic
        generic, public :: in => Q_inflowsStatic, j_spm_inflowsStatic, j_np_inflowsStatic
        procedure, nopass :: Q_runoffStatic
        procedure, nopass :: j_spm_runoffStatic
        procedure, nopass :: j_np_runoffStatic
        generic, public :: run => Q_runoffStatic, j_spm_runoffStatic, j_np_runoffStatic
    end type

    !> Container type for `class(Reach)`, the actual type of the `Reach` class.
    !! a variable of type `ReachElement` can be of any object type inheriting from the
    !! `Reach` abstract base class.
    type ReachElement                                          
        class(Reach), allocatable :: item                      !! Polymorphic `Reach` object
    end type

  contains

    !> Allocate memory for arrays and set any initial values
    subroutine allocateAndInitialiseReach(me)
        class(Reach) :: me
        ! WaterBody initialises the variables common to all water bodies
        call me%WaterBody%allocateAndInitialise()
        ! Allocate flow arrays, which depend on the number of inflows and sources.The 1st dimension of
        ! the flow arrays represent the compartment the flow is to/from, and for reaches this is indexed as so:
        !   1. outflow
        !   2 -> 1+nInflows: inflows
        !   2+nInflows: runoff
        !   3+nInflows: transfers
        !   (SPM & NM only) 4+nInflows: settling & resuspension
        !   (NM only)   5+nInflows -> 4+nInflows+nDiffuseSources: diffuse sources
        !   (NM only)   5+nInflows+nDiffuseSources -> 4+nInflows+nDiffuseSources+nPointSources: point sources
        allocate(me%Q(me%nInflows + 3), &
            me%Q_final(me%nInflows + 3), &
            me%j_spm(me%nInflows + 4, C%nSizeClassesSpm), &
            me%j_spm_final(me%nInflows + 4, C%nSizeClassesSpm), &
            me%j_np(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_np_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_ionic(me%nInflows + 3, C%ionicDim), &
            me%j_ionic_final(me%nInflows + 3, C%ionicDim) &
        )
        me%Q = 0
        me%Q_final = 0
        me%j_spm = 0
        me%j_spm_final = 0
        me%j_np = 0
        me%j_np_final = 0
        me%j_ionic = 0
        me%j_ionic_final = 0
        ! Defaults
        me%n = C%n_river
        me%f_m = C%defaultMeanderingFactor
    end subroutine

    !> Set the settling rate
    subroutine setSettlingRateReach(me)
        class(Reach) :: me                      !! This `Reach` instance
        integer :: i                            ! Size class iterator
        ! SPM: Loop through the size classes and calculate settling velocity
        do i = 1, C%nSizeClassesSpm
            me%W_settle_spm(i) = me%calculateSettlingVelocity( &
                C%d_spm(i), &
                sum(C%rho_spm)/C%nFracCompsSpm, &       ! Average of the fractional comps. TODO: Change to work with actual fractional comps.
                me%T_water &
            )
        end do
        if (.not. isZero(me%depth)) then
            me%k_settle = me%W_settle_spm/me%depth
        else
            me%k_settle = 0.0_dp
        end if

        ! NP: Calculate this to pass to Reactor
        do i = 1, C%nSizeClassesNP
            me%W_settle_np(i) = me%calculateSettlingVelocity( &
                C%d_np(i), &
                4230.0_dp, &       ! HACK: rho_np, change this to account for different NP materials
                me%T_water &
            )
        end do
    end subroutine


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
    subroutine setResuspensionRateReach(me)
        class(Reach) :: me                      !! This `Reach` instance
        !--- Locals ---!
        real(dp) :: Q_in_total                  ! Total inflow to this reach
        real(dp) :: d_max                       ! Maximum resuspendable particle size [m]
        integer :: i                            ! Iterator
        real(dp) :: M_prop(C%nSizeClassesSpm)   ! Proportion of size class that can be resuspended [-]
        real(dp) :: omega                       ! Stream power per unit bed area [W m-2]
        real(dp) :: f_fr                        ! Friction factor [-]
        real(dp) :: mbed(C%nSizeClassesSpm)     ! mass of fine material in the sediment [kg]

        ! There must be flow for there to be resuspension
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
            ! Set k_resus using the above
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
    end subroutine

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

!-------------!
!-- GETTERS --!
!-------------!

    function Q_outflow_finalReach(me) result(Q_outflow_final)
        class(Reach) :: me
        real(dp) :: Q_outflow_final
        Q_outflow_final = me%Q_final(1)
    end function

    !> Return the SPM discahrge.
    function j_spm_outflow_finalReach(me) result(j_spm_outflow_final)
        class(Reach) :: me
        real(dp) :: j_spm_outflow_final(C%nSizeClassesSpm)
        j_spm_outflow_final = me%j_spm_final(1,:)
    end function

    !> Return the SPM discahrge.
    function j_np_outflow_finalReach(me) result(j_np_outflow_final)
        class(Reach) :: me
        real(dp) :: j_np_outflow_final(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_outflow_final = me%j_np_final(1,:,:,:)
    end function

    function Q_outflow(me)
        class(Reach) :: me
        real(dp) :: Q_outflow
        Q_outflow = me%Q(1)
    end function

    function Q_inflows(me)
        class(Reach) :: me
        real(dp) :: Q_inflows
        Q_inflows = sum(me%Q(2:1+me%nInflows))
    end function

    function Q_runoff(me)
        class(Reach) :: me
        real(dp) :: Q_runoff
        Q_runoff = me%Q(2+me%nInflows)
    end function

    function Q_transfers(me)
        class(Reach) :: me
        real(dp) :: Q_transfers
        Q_transfers = me%Q(3+me%nInflows)
    end function

    function j_spm_outflow(me)
        class(Reach) :: me
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)
        j_spm_outflow = me%j_spm(1,:)
    end function

    function j_spm_inflows(me)
        class(Reach) :: me
        real(dp) :: j_spm_inflows(C%nSizeClassesSpm)
        if (me%nInflows > 0) then
            j_spm_inflows = sum(me%j_spm(2:1+me%nInflows,:), dim=1)
        else
            j_spm_inflows = 0
        end if
    end function

    function j_spm_runoff(me)
        class(Reach) :: me
        real(dp) :: j_spm_runoff(C%nSizeClassesSpm)
        j_spm_runoff = me%j_spm(2+me%nInflows,:)
    end function

    function j_spm_transfers(me)
        class(Reach) :: me
        real(dp) :: j_spm_transfers(C%nSizeClassesSpm)
        j_spm_transfers = me%j_spm(3+me%nInflows,:)
    end function

    function j_spm_deposit(me)
        class(Reach) :: me
        real(dp) :: j_spm_deposit(C%nSizeClassesSpm)
        j_spm_deposit = me%j_spm(4+me%nInflows,:)
    end function

    !> Get the outflow from NM flux array
    function j_np_outflow(me)
        class(Reach) :: me
        real(dp) :: j_np_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_outflow = me%j_np(1,:,:,:)
    end function

    !> Get the inflowing NM from NM flux array
    function j_np_inflows(me)
        class(Reach) :: me
        real(dp) :: j_np_inflows(C%npDim(1), C%npDim(2), C%npDim(3))
        if (me%nInflows > 0) then
            j_np_inflows = sum(me%j_np(2:1+me%nInflows,:,:,:), dim=1)
        else
            j_np_inflows = 0
        end if
    end function

    !> Get the total runoff from NM flux array
    function j_np_runoff(me)
        class(Reach) :: me
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_runoff = me%j_np(2+me%nInflows,:,:,:)
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_np_transfer(me)
        class(Reach) :: me
        real(dp) :: j_np_transfer(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_transfer = me%j_np(3+me%nInflows,:,:,:)
    end function

    !> Get the total deposited NM (settling + resus) from NM flux array
    function j_np_deposit(me)
        class(Reach) :: me
        real(dp) :: j_np_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_deposit = me%j_np(4+me%nInflows,:,:,:)
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_np_diffusesource(me)
        class(Reach) :: me
        real(dp) :: j_np_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_diffusesource = sum(me%j_np(5+me%nInflows:4+me%nInflows+me%nDiffuseSources,:,:,:), dim=1)
    end function

    !> Get the total point source fluxes from NM flux array
    function j_np_pointsource(me)
        class(Reach) :: me
        real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_pointsource &
            = sum(me%j_np(5+me%nInflows+me%nDiffuseSources:4+me%nInflows+me%nDiffuseSources+me%nPointSources,:,:,:), dim=1)
    end function

!-------------!
!-- SETTERS --!
!-------------!

!-- WATER --!

    subroutine set_Q_outflow(me, Q_outflow)
        class(Reach) :: me                      !! This `Reach` instance
        real(dp) :: Q_outflow                   !! Outflow value to set
        me%Q(1) = Q_outflow
    end subroutine

    subroutine set_Q_inflow(me, Q_inflow, i)
        class(Reach) :: me                      !! This `Reach` instance
        real(dp) :: Q_inflow                    !! Inflow value to set
        integer :: i                            !! Inflow index
        me%Q(i+1) = Q_inflow
    end subroutine

    subroutine set_Q_inflows(me, Q_inflows)
        class(Reach) :: me                      !! This `Reach` instance
        real(dp) :: Q_inflows(me%nInflows)      !! Inflow array to set
        if (me%nInflows > 0) then
            me%Q(2:1+me%nInflows) = Q_inflows
        end if
    end subroutine

    subroutine set_Q_runoff(me, Q_runoff)
        class(Reach) :: me                      !! This `Reach` instance
        real(dp) :: Q_runoff                    !! Runoff value to set
        me%Q(2+me%nInflows) = Q_runoff
    end subroutine

    subroutine set_Q_transfers(me, Q_transfers)
        class(Reach) :: me                      !! This `Reach` instance
        real(dp) :: Q_transfers                 !! Transfer value to set
        me%Q(3+me%nInflows) = Q_transfers
    end subroutine

!-- SPM --!

    subroutine set_j_spm_outflow(me, j_spm_outflow)
        class(Reach) :: me
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)
        me%j_spm(1,:) = j_spm_outflow
    end subroutine

    subroutine set_j_spm_inflow(me, j_spm_inflow, i)
        class(Reach) :: me
        real(dp) :: j_spm_inflow(C%nSizeClassesSpm)
        integer :: i
        me%j_spm(1+i,:) = j_spm_inflow
    end subroutine

    subroutine set_j_spm_inflows(me, j_spm_inflows)
        class(Reach) :: me
        real(dp) :: j_spm_inflows(me%nInflows, C%nSizeClassesSpm)
        if (me%nInflows > 0) then
            me%j_spm(2:1+me%nInflows,:) = j_spm_inflows
        end if
    end subroutine

    !> Set the runoff flux of the SPM flux array
    subroutine set_j_spm_runoff(me, j_spm_runoff)
        class(Reach) :: me
        real(dp) :: j_spm_runoff(C%nSizeClassesSpm)
        me%j_spm(2+me%nInflows,:) = j_spm_runoff
    end subroutine

    !> Set the transfer flux of the SPM flux array
    subroutine set_j_spm_transfers(me, j_spm_transfers)
        class(Reach) :: me
        real(dp) :: j_spm_transfers(C%nSizeClassesSpm)
        me%j_spm(3+me%nInflows,:) = j_spm_transfers
    end subroutine

    !> Set the settling/resuspension flux of the SPM flux array
    subroutine set_j_spm_deposit(me, j_spm_deposit)
        class(Reach) :: me
        real(dp) :: j_spm_deposit(C%nSizeClassesSpm)
        me%j_spm(4+me%nInflows,:) = j_spm_deposit
    end subroutine

!-- NM --!

    subroutine set_j_np_outflow(me, j_np_outflow)
        class(Reach) :: me
        real(dp) :: j_np_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_np(1,:,:,:) = j_np_outflow
    end subroutine

    subroutine set_j_np_inflow(me, j_np_inflow, i)
        class(Reach) :: me
        real(dp) :: j_np_inflow(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_np(1+i,:,:,:) = j_np_inflow
    end subroutine

    subroutine set_j_np_inflows(me, j_np_inflows)
        class(Reach) :: me
        real(dp) :: j_np_inflows(me%nInflows, C%npDim(1), C%npDim(2), C%npDim(3))
        if (me%nInflows > 0) then
            me%j_np(2:1+me%nInflows,:,:,:) = j_np_inflows
        end if
    end subroutine

    !> Set the runoff flux of the SPM flux array
    subroutine set_j_np_runoff(me, j_np_runoff)
        class(Reach) :: me
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_np(2+me%nInflows,:,:,:) = j_np_runoff
    end subroutine

    !> Set the transfer flux of the SPM flux array
    subroutine set_j_np_transfers(me, j_np_transfers)
        class(Reach) :: me
        real(dp) :: j_np_transfers(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_np(3+me%nInflows,:,:,:) = j_np_transfers
    end subroutine

    !> Set the settling/resuspension flux of the SPM flux array
    subroutine set_j_np_deposit(me, j_np_deposit)
        class(Reach) :: me
        real(dp) :: j_np_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_np(4+me%nInflows,:,:,:) = j_np_deposit
    end subroutine

    !> Set the diffuse source flux of the SPM flux array
    subroutine set_j_np_diffusesource(me, j_np_diffusesource, i)
        class(Reach) :: me
        real(dp) :: j_np_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_np(4+me%nInflows+i,:,:,:) = j_np_diffusesource
    end subroutine

    !> Set the point source flux of the SPM flux array
    subroutine set_j_np_pointsource(me, j_np_pointsource, i)
        class(Reach) :: me
        real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_np(4+me%nInflows+me%nDiffuseSources+i,:,:,:) = j_np_pointsource
    end subroutine


!---------------------!
!-- STATIC METHODS ---!
!---------------------!

    function Q_outflowStatic(Q) result(Q_outflow)
        real(dp) :: Q(:)
        real(dp) :: Q_outflow
        Q_outflow = Q(1)
    end function

    function j_spm_outflowStatic(j_spm) result(j_spm_outflow)
        real(dp) :: j_spm(:,:)
        real(dp) :: j_spm_outflow(C%nSizeClassesSpm)
        j_spm_outflow = j_spm(1,:)
    end function

    function j_np_outflowStatic(j_np) result(j_np_outflow)
        real(dp) :: j_np(:,:,:,:)
        real(dp) :: j_np_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_outflow = j_np(1,:,:,:)
    end function

    function Q_inflowsStatic(Q) result(Q_inflows)
        real(dp) :: Q(:)
        real(dp), allocatable :: Q_inflows(:)
        integer :: nInflows
        nInflows = size(Q) - 3
        allocate(Q_inflows(nInflows))
        if (nInflows > 0) then
            Q_inflows = Q(2:1+nInflows)
        end if
    end function

    function j_spm_inflowsStatic(j_spm) result(j_spm_inflows)
        real(dp) :: j_spm(:,:)
        real(dp), allocatable :: j_spm_inflows(:,:)
        integer :: nInflows
        nInflows = size(j_spm, 1) - 4
        allocate(j_spm_inflows(nInflows, C%nSizeClassesSpm))
        if (nInflows > 0) then
            j_spm_inflows = j_spm(2:1+nInflows,:)
        end if
    end function

    function j_np_inflowsStatic(j_np, nInflows) result(j_np_inflows)
        real(dp) :: j_np(:,:,:,:)
        real(dp) :: j_np_inflows(nInflows, C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: nInflows
        if (nInflows > 0) then
            j_np_inflows = j_np(2:1+nInflows,:,:,:)
        end if
    end function

    function Q_runoffStatic(Q) result(Q_runoff)
        real(dp) :: Q(:)
        real(dp), allocatable :: Q_runoff
        integer :: nInflows
        nInflows = size(Q) - 3
        Q_runoff = Q(2+nInflows)
    end function

    function j_spm_runoffStatic(j_spm) result(j_spm_runoff)
        real(dp) :: j_spm(:,:)
        real(dp) :: j_spm_runoff(C%nSizeClassesSpm)
        integer :: nInflows
        nInflows = size(j_spm, 1) - 4
        j_spm_runoff = j_spm(2+nInflows,:)
    end function

    function j_np_runoffStatic(j_np, nInflows) result(j_np_runoff)
        real(dp) :: j_np(:,:,:,:)
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: nInflows
        j_np_runoff = j_np(2+nInflows,:,:,:)
    end function


end module
!> Module containing definition of abstract base class `Reach`.
module spcReach
    use Globals
    use ResultModule, only: Result
    use ErrorInstanceModule
    use spcWaterBody
    ! use mo_netcdf               ! See below TODO re me%ncGroup
    use netcdf
    use classDatabase, only: DATASET
    use DefaultsModule, only: defaultSlope
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
        integer, allocatable :: inflowsArr(:,:)
        integer :: outflowArr(3)
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
        integer :: streamOrder = 0                                  !! Stream order of this reach [-]
        ! Physical properties
        real(dp) :: slope                                           !! Slope of reach [m/m]
        real(dp) :: width                                           !! Width of the reach [m]
        real(dp) :: xsArea                                          !! The cross-sectional area of water in the reach [m2]
        real(dp) :: f_m                                             !! Meandering factor used for calculating river volume [-]
        real(dp) :: length                                          !! Length of the river, without meandering factor [m]
        real(dp) :: velocity                                        !! Water velocity [m/s]
        real(dp) :: alpha_resus                                     !! Maximum resuspendable particle size calibration param [-]
        real(dp) :: beta_resus                                      !! Resuspension calibration factor [s2 kg-1]
        real(dp) :: n                                               !! Manning's roughness coefficient [-]
        ! Transformation properties
        real(dp) :: alpha_hetero                                    !! Heteroaggregation attachment efficiency, 0-1 [-]
        ! TODO NetCDF group needed to pass to bed sediment. Need to deprecate this eventually to save memory
        ! type(NcGroup) :: ncGroup
        ! Boundary conditions for calibration
        logical :: isBoundary = .false.                             !! Is this a sampling site for calibrating?
        real(dp) :: boundary_C_spm                                  !! Boundary condition for C_spm
        real(dp), allocatable :: boundary_C_spm_timeseries(:)
        real(dp), allocatable :: boundary_Q_timeseries(:)
        type(datetime), allocatable :: boundary_dates(:)
        real(dp) :: boundary_Q                                      !! Boundary condition for Q
        character(len=20) :: calibrationSiteRef                     !! Reference of the calibration site

      contains
        ! Data
        procedure :: allocateAndInitialise => allocateAndInitialiseReach
        procedure :: parseInflowsAndOutflow => parseInflowsAndOutflowReach
        procedure :: setReachLengthAndSlope => setReachLengthAndSlopeReach
        procedure :: parseNewBatchData => parseNewBatchDataReach
        ! Simulators
        procedure :: setResuspensionRate => setResuspensionRateReach
        procedure :: setSettlingRate => setSettlingRateReach
        procedure :: setSedimentTransportCapacity => setSedimentTransportCapacityReach
        procedure :: scaleErosionBySedimentTransportCapacity => scaleErosionBySedimentTransportCapacityReach
        procedure :: depositToBed => depositToBedReach
        ! Calculators
        procedure :: calculateSettlingVelocity => calculateSettlingVelocity
        procedure :: calculateResuspension => calculateResuspension
        procedure :: calculateBankErosionRate => calculateBankErosionRateReach
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
        procedure :: j_transformed_outflow
        procedure :: j_transformed_deposit
        procedure :: j_transformed_diffusesource
        procedure :: j_transformed_pointsource
        procedure :: j_dissolved_outflow
        procedure :: j_dissolved_diffusesource
        procedure :: j_dissolved_pointsource
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
        procedure :: set_j_transformed_outflow
        procedure :: set_j_transformed_runoff
        procedure :: set_j_transformed_inflow
        procedure :: set_j_transformed_deposit
        procedure :: set_j_transformed_diffusesource
        procedure :: set_j_transformed_pointsource
        procedure :: set_j_dissolved_outflow
        procedure :: set_j_dissolved_inflow
        procedure :: set_j_dissolved_diffusesource
        procedure :: set_j_dissolved_pointsource
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
            me%j_transformed(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
                C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_transformed_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
                C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_dissolved(me%nInflows + me%nPointSources + me%nDiffuseSources + 4), &
            me%j_dissolved_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4) &
        )
        me%Q = 0.0_dp
        me%Q_final = 0.0_dp
        me%j_spm = 0.0_dp
        me%j_spm_final = 0.0_dp
        me%j_np = 0.0_dp
        me%j_np_final = 0.0_dp
        ! Defaults
        me%n = C%n_river
    end subroutine

    !> Parse the input data for this reach. This function is called at the start of every
    !! chunk for batch runs.
    subroutine parseNewBatchDataReach(me)
        class(Reach) :: me
        real(dp), allocatable :: tmp_j(:,:,:,:)
        real(dp), allocatable :: tmp_j_dissolved(:)
        ! NM
        call move_alloc(me%j_np, tmp_j)
        allocate(me%j_np(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
            C%npDim(1), C%npDim(2), C%npDim(3)))
        me%j_np = 0.0_dp
        me%j_np(:4+me%nInflows,:,:,:) = tmp_j(:4+me%nInflows,:,:,:)         ! Only copy over stuff that isn't sources, as those will be changed anyway
        ! Transformed
        call move_alloc(me%j_transformed, tmp_j)
        allocate(me%j_transformed(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
            C%npDim(1), C%npDim(2), C%npDim(3)))
        me%j_transformed = 0.0_dp
        me%j_transformed(:4+me%nInflows,:,:,:) = tmp_j(:4+me%nInflows,:,:,:) 
        ! Dissolved
        call move_alloc(me%j_dissolved, tmp_j_dissolved)
        allocate(me%j_dissolved(me%nInflows + me%nPointSources + me%nDiffuseSources + 4))
        me%j_dissolved = 0.0_dp
        me%j_dissolved(:4+me%nInflows) = tmp_j_dissolved(:4+me%nInflows)
        ! Final NM
        call move_alloc(me%j_np_final, tmp_j)
        allocate(me%j_np_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
            C%npDim(1), C%npDim(2), C%npDim(3)))
        me%j_np_final = 0.0_dp
        me%j_np_final(:4+me%nInflows,:,:,:) = tmp_j(:4+me%nInflows,:,:,:)
        ! Final transformed
        call move_alloc(me%j_transformed_final, tmp_j)
        allocate(me%j_transformed_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, &
            C%npDim(1), C%npDim(2), C%npDim(3)))
        me%j_transformed_final = 0.0_dp
        me%j_transformed_final(:4+me%nInflows,:,:,:) = tmp_j(:4+me%nInflows,:,:,:) 
        ! Final dissolved
        call move_alloc(me%j_dissolved_final, tmp_j_dissolved)
        allocate(me%j_dissolved_final(me%nInflows + me%nPointSources + me%nDiffuseSources + 4))
        me%j_dissolved_final = 0.0_dp
        me%j_dissolved_final(:4+me%nInflows) = tmp_j_dissolved(:4+me%nInflows)
    end subroutine

    !> Set the settling rate
    subroutine setSettlingRateReach(me, T_water_t)
        class(Reach)    :: me                   !! This `Reach` instance
        real            :: T_water_t            !! Water temperature on this timestep
        integer         :: i                    ! Size class iterator

        if (.not. isZero(me%depth)) then
            ! SPM: Loop through the size classes and calculate settling velocity
            do i = 1, C%nSizeClassesSpm
                me%W_settle_spm(i) = me%calculateSettlingVelocity( &
                    C%d_spm(i), &
                    DATASET%spmDensityBySizeClass(i), &       ! Average of the fractional comps. TODO: Change to work with actual fractional comps.
                    T_water_t, &
                    alphaDep=DATASET%depositionAlpha(me%x, me%y), &
                    betaDep=DATASET%depositionBeta(me%x, me%y) &
                )
            end do
            me%k_settle = me%W_settle_spm / me%depth

            ! NP: Calculate this to pass to Reactor
            do i = 1, C%nSizeClassesNM
                me%W_settle_np(i) = me%calculateSettlingVelocity( &
                    C%d_nm(i), &
                    DATASET%nmDensity, &
                    T_water_t, &
                    alphaDep=DATASET%depositionAlpha(me%x, me%y), &
                    betaDep=DATASET%depositionBeta(me%x, me%y) &
                )
            end do
        else
            me%W_settle_spm = 0.0_dp
            me%W_settle_np = 0.0_dp
            me%k_settle = 0.0_dp
        end if
    end subroutine

    !> Set the sediment transport capacity to this reach, based on Lazar et al 2010
    !! (https://doi.org/10.1016/j.scitotenv.2010.02.030). This limits the amount of eroded sediment
    !! from the soil profile that is available to the reach. It is set here, as opposed
    !! to the soil profile, because it depends on the reach length.
    subroutine setSedimentTransportCapacityReach(me, contributingArea, q_overland)
        class(Reach)    :: me                   !! This SoilProfile instance
        real(dp)        :: contributingArea     !! Area over which erosion occurs (e.g. soil profile area) [km2]
        real(dp)        :: q_overland           !! Overland flow [m3/km2/s]
        ! Using units from Lazar, which are a little strange:
        !   a_stc (a4 in Lazar)     [kg/m2/km2]
        !   b_stc (a5 in Lazar)     [m2/s]
        !   c_stc (a6 in Lazar)     [-]
        !   contributingArea        [km2]
        !   q_overland              [m3/s/km2]
        !   length                  [m]
        !   sedimentTransportCapacity   [kg/m2/timestep]
        ! The following also makes sure that b_stc isn't < 0, and that the resulting STC isn't < 0
        me%sedimentTransportCapacity = max(C%timeStep &
            * me%a_stc * max((contributingArea * q_overland / me%length - me%b_stc), 0.0_dp) ** me%c_stc, 0.0_dp)
    end subroutine

    !> Scale the erosion yield by the sediment transport capacity, which is a function of the overland flow.
    function scaleErosionBySedimentTransportCapacityReach(me, erosionYield, q_overland, contributingArea) result(scaledErosionYield)
        class(Reach)    :: me                                       !! This SoilProfile instance
        real(dp)        :: erosionYield(C%nSizeClassesSPM)          !! The un-scaled erosion yield [kg/timestep]
        real(dp)        :: q_overland                               !! Overland flow [m3/m2/timestep]
        real(dp)        :: contributingArea                         !! Area over which erosion occurs (e.g. soil profile area) [m2]
        real(dp)        :: scaledErosionYield(C%nSizeClassesSPM)    !! The scaled erosion yield [kg/timestep]
        ! Set the transport capacity
        call me%setSedimentTransportCapacity( &
            contributingArea=contributingArea / 1e6, &          ! Convert m2 to km2
            q_overland=q_overland * 1e6 / C%timeStep &          ! Convert m3/m2/timestep to m3/km2/s
        )
        ! Where sum of eroded sediment (over size classes) is > STC, scale it proportionally
        if (sum(erosionYield) > me%sedimentTransportCapacity * contributingArea) then
            scaledErosionYield = (erosionYield / sum(erosionYield)) * me%sedimentTransportCapacity * contributingArea
        else
            scaledErosionYield = erosionYield
        end if
    end function

    function depositToBedReach(me, spmDep) result(rslt)
        class(Reach)        :: me                           !! This Reach instance
        real(dp)            :: spmDep(C%nSizeClassesSpm)    !! The SPM to deposit [kg]
        type(Result)        :: rslt                         !! The data object to return any errors in
        real(dp)            :: spmDep_perArea(C%nSizeClassesSpm)    ! The SPM to deposit, per unit area [kg/m2]
        type(Result0D)      :: depositRslt                  !! Result from the bed sediment's deposit procedure
        real(dp)            :: V_water_toDeposit            !! Volume of water to deposit to bed sediment [m3/m2]
        type(FineSediment1) :: fineSediment(C%nSizeClassesSpm) ! FineSediment object to pass to BedSediment
        integer             :: n                            ! Loop iterator
        ! Create the FineSediment object and add deposited SPM to it
        ! (converting units of Mf_in to kg/m2), then give that object
        ! to the BedSediment
        if (isZero(me%bedArea)) then
            spmDep_perArea = 0.0_dp
        else
            spmDep_perArea = spmDep/me%bedArea
        end if
        do n = 1, C%nSizeClassesSpm
            call fineSediment(n)%create("FS", C%nFracCompsSpm)
            call fineSediment(n)%set( &
                Mf_in=spmDep_perArea(n), &
                f_comp_in=real(DATASET%sedimentFractionalComposition, 8) &
            )
        end do

        if (C%includeBedSediment) then
            ! Deposit the fine sediment to the bed sediment
            depositRslt = Me%bedSediment%deposit(fineSediment)
            call rslt%addErrors(.errors. depositRslt)
            if (rslt%hasCriticalError()) then
                return
            end if
        end if
        ! TODO add error handling to line above as it causes a crash if there is a critical error in the called method
        ! Retrieve the amount of water to be taken from the reach
        V_water_toDeposit = .dp. depositRslt                ! [m3/m2]
        ! Subtract that volume for the reach (as a depth). This doesn't have any effect on
        ! the model calculations, as the model recalculates depth depth on hydrology at the
        ! start of every timestep. However, it is this updated depth that is saved to data.
        me%depth = me%depth - V_water_toDeposit

        ! Add any errors that occured in the deposit procedure
        call rslt%addToTrace("Depositing SPM to BedSediment")
    end function

    !> Compute the resuspension rate [s-1] for a time step
    !! Reference: [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub)
    subroutine setResuspensionRateReach(me, Q, T_water_t)
        class(Reach)    :: me                   !! This `Reach` instance
        real(dp)        :: Q                    !! Flow rate to set resuspension rate based on [m/s]
        real            :: T_water_t            !! Water temperature on this timestep [deg C]
        real(dp)        :: d_max                ! Maximum resuspendable particle size [m]
        integer         :: i                    ! Iterator
        real(dp)        :: M_prop(C%nSizeClassesSpm)    ! Proportion of size class that can be resuspended [-]
        real(dp)        :: omega                ! Stream power per unit bed area [W m-2]
        real(dp)        :: f_fr                 ! Friction factor [-]

        ! There must be flow for there to be resuspension
        if (Q > 0) then
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
            omega = C%rho_w(T_water_t) * C%g * Q * me%slope / me%width
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
            me%k_resus = 0.0_dp                                 ! If there's no inflow
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
    function calculateSettlingVelocity(me, d, rho_particle, T, alphaDep, betaDep) result(W)
        class(Reach), intent(in) :: me                          !! The `Reach` instance
        real, intent(in) :: d                                   !! Sediment particle diameter [m]
        real, intent(in) :: rho_particle                        !! Sediment particulate density [kg/m3]
        real, intent(in) :: T                                   !! Temperature [C]
        real, intent(in) :: alphaDep                            !! Alpha calibration parameter
        real, intent(in) :: betaDep                             !! Beta calibration parameter
        real(dp) :: W                                           !! Calculated settling velocity [m/s]
        real(dp) :: dStar                                       ! Dimensionless particle diameter.
        real(dp) :: dStarTerm                                   ! Local storage for d* term, to check if it's < 0
        ! Settling only occurs if SPM particle density is greater than density of water
        if ((rho_particle > C%rho_w(T))) then
            dStar = ((rho_particle/C%rho_w(T) - 1)*C%g/C%nu_w(T)**2)**(1.0_dp/3.0_dp) * d   ! Calculate the dimensionless particle diameter
            dStarTerm = alphaDep + betaDep * dStar ** (1.714285714_dp)
            if (dStarTerm > 0.0) then
                W = max( &
                    (C%nu_w(T)/d) * dStar**3 * (alphaDep + betaDep &                          ! Calculate the settling velocity
                        * dStar**(1.714285714_dp))**(-0.875_dp), &
                    0.0_dp &
                )
            else
                W = 0.0_dp
            end if
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
        k_res = beta * L * W * M_prop * omega * f_fr
    end function

    !> Calculate the bank erosion rate, based on Lazar et al 2010 (https://doi.org/10.1016/j.scitotenv.2010.02.030).
    !! $$
    !!  m_\text{bank} = \alpha_\text{bank} Q^{\beta_\text{bank}}
    !! $$
    !! where $m_\text{bank}$ is in kg/m2/s. To convert to kg/timestep, it is multiplied by the bank area
    !! (assuming a rectangular channel) and the timestep length.
    function calculateBankErosionRateReach(me, Q, alpha_bank, beta_bank, length, depth) result(j_spm_bank)
        class(Reach)        :: me           !! This reach
        real(dp)            :: Q            !! Flow [m3/s]
        real(dp)            :: alpha_bank   !! Bank erosion alpha calibration param [kg/m5]
        real(dp)            :: beta_bank    !! Bank erosion beta calibration param [-]
        real(dp)            :: length       !! Reach length [m]
        real(dp)            :: depth        !! Reach depth [m]
        real(dp)            :: j_spm_bank   !! Bank erosion rate [kg/timestep]
        j_spm_bank = C%timeStep * length * depth * alpha_bank * Q ** beta_bank
    end function

    function parseInflowsAndOutflowReach(me) result(rslt)
        class(Reach) :: me
        type(Result) :: rslt
        integer :: i                                ! Loop iterator
        integer :: inflowCell(2)
        integer :: outflowCell(2)
        integer :: i_out
        ! If we're not in a headwater cell, use the grid cell inflows and outflow to set the
        ! inflows to this reach. This is assuming the model conventions that each branch in the
        ! cell has one reach, and thus the cell outflow is the outflow for all reaches in the cell,
        ! and the inflows are the inflows to each reach.
        if (.not. DATASET%isHeadwater(me%x, me%y)) then
            ! First, use this reach's index to get the inflowing cell (making sure we use the
            ! same convention when setting outflows)
            inflowCell = DATASET%inflows(:, me%w, me%x, me%y)
            ! Then use the number of waterbodies in that cell to get the number of inflowing reaches
            ! to this reach (remember each cell can only have one outflow)
            me%nInflows = DATASET%nWaterbodies(inflowCell(1), inflowCell(2))
            allocate(me%inflowsArr(me%nInflows, 3))
            do i = 1, me%nInflows
                me%inflowsArr(i, :) = [i, inflowCell]
            end do
        else
            ! If we're in a headwater cell, this must be the only reach and it won't have an inflow
            me%isHeadwater = .true.
            me%nInflows = 0
            allocate(me%inflowsArr(0,0))
        end if

        ! Allocate inflows() array (the array of pointers) to the correct size
        allocate(me%inflows(me%nInflows))
        ! Now we have to look at the outflow cell's inflow cells (from DATASET) to figure
        ! which waterbody index to use for this cell's outflow - the above creates reaches
        ! simply by looping through the indices starting with the first it encounters in
        ! DATASET%inflows, so the outflow reach ref will follow this convention
        outflowCell = DATASET%outflow(:, me%x, me%y)
        ! Check if the outflow is in the model domain first
        if (.not. DATASET%inModelDomain(outflowCell(1), outflowCell(2))) then
            me%isDomainOutflow = .true.
            ! Set outflow array with waterbody index of 1 if this outflow is out of the
            ! model domain. The waterbody index isn't used, so this isn't important that
            ! it might be wrong.
            me%outflowArr = [1, outflowCell(1), outflowCell(2)]
        else
            ! Loop through outflow cell reaches and find index where inflow (x,y) equals
            ! this cell (x,y)
            do i = 1, DATASET%nWaterbodies(outflowCell(1),outflowCell(2))
                if (DATASET%inflows(1, i, outflowCell(1), outflowCell(2)) == outflowCell(1) &
                    .and. DATASET%inflows(2, i, outflowCell(1), outflowCell(2)) == outflowCell(2)) then
                    i_out = i
                end if
            end do
            ! Set reach outflow based on this
            me%outflowArr =  [i_out, outflowCell(1), outflowCell(2)]
        end if
    end function

    !> Set the length and slope of this reach, based on inflows and outflow reach locations
    function setReachLengthAndSlopeReach(me) result(rslt)
        class(Reach) :: me
        type(Result) :: rslt
        real(dp) :: dx, dy, dz

        if (me%isHeadwater) then
            ! If headwater, assume reach starts in centre of cell
            dx = (me%x - me%outflowArr(2)) * 0.5 * DATASET%gridRes(1)
            dy = (me%y - me%outflowArr(3)) * 0.5 * DATASET%gridRes(2)
            ! Difference in elevation from start of reach and outflow, converted from dm to m
            if (allocated(DATASET%dem)) then
                dz = real(DATASET%dem(me%x, me%y) - DATASET%dem(me%outflowArr(2), me%outflowArr(3))) / 10.0
            end if
        else
            ! If not headwater, use distance between inflow and outflow to calculate length
            ! All inflows to this reach will be from same cell, so just use first in array
            dx = (me%inflowsArr(1,2) - me%outflowArr(2)) * 0.5 * DATASET%gridRes(1)
            dy = (me%inflowsArr(1,3) - me%outflowArr(3)) * 0.5 * DATASET%gridRes(2)
            ! Difference in elevation from inflow to outflow, converted from dm to m
            if (allocated(DATASET%dem)) then
                dz = real(DATASET%dem(me%inflowsArr(1,2), me%inflowsArr(1,3)) &
                    - DATASET%dem(me%outflowArr(2), me%outflowArr(3))) / 10.0
            end if
        end if
        ! A touch of trig to calculate reach length and slope
        me%length = sqrt(dx**2 + dy**2)
        ! If a DEM was provided, then use the calculated dz to get the slope gradient. Otherwise,
        ! default to what is provided in DefaultsModule
        if (allocated(DATASET%dem)) then
            ! Rivers can't flow uphill, so if dz is negative, the gridding is causing too 
            ! much loss of data to reasonably calculate slope. If this is the case, we assume
            ! the slope must be small and set the minimum slope to that specified in config
            ! (which defaults to 0.0001)
            me%slope = max(divideCheckZero(dz, me%length), C%minStreamSlope)           ! [m/m]
        else
            me%slope = defaultSlope     ! 0.0005 m/m
        end if
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
        real(dp), allocatable :: j_np_outflow(:,:,:)
        allocate(j_np_outflow, source=me%j_np(1,:,:,:))
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
        real(dp), allocatable :: j_np_deposit(:,:,:)
        allocate(j_np_deposit, source=me%j_np(4+me%nInflows,:,:,:))
        !j_np_deposit = me%j_np(4+me%nInflows,:,:,:)
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

    !> Get the outflow from transformed flux array
    function j_transformed_outflow(me)
        class(Reach) :: me
        real(dp), allocatable :: j_transformed_outflow(:,:,:)
        allocate(j_transformed_outflow, source=me%j_transformed(1,:,:,:))
    end function

    function j_transformed_deposit(me)
        class(Reach) :: me
        real(dp), allocatable :: j_transformed_deposit(:,:,:)
        allocate(j_transformed_deposit, source=me%j_transformed(4+me%nInflows,:,:,:))
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_transformed_diffusesource(me)
        class(Reach) :: me
        real(dp) :: j_transformed_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_transformed_diffusesource = &
            sum(me%j_transformed(5+me%nInflows:4+me%nInflows+me%nDiffuseSources,:,:,:), dim=1)
    end function

    !> Get the total point source fluxes from NM flux array
    function j_transformed_pointsource(me)
        class(Reach) :: me
        real(dp) :: j_transformed_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_transformed_pointsource &
            = sum(me%j_transformed(5+me%nInflows+me%nDiffuseSources:4+me%nInflows+me%nDiffuseSources+me%nPointSources,:,:,:), dim=1)
    end function

    !> Get the outflow from dissolved flux array
    function j_dissolved_outflow(me)
        class(Reach) :: me
        real(dp) :: j_dissolved_outflow
        j_dissolved_outflow = me%j_dissolved(1)
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_dissolved_diffusesource(me)
        class(Reach) :: me
        real(dp) :: j_dissolved_diffusesource
        j_dissolved_diffusesource = sum(me%j_dissolved(5+me%nInflows:4+me%nInflows+me%nDiffuseSources))
    end function

    !> Get the total point source fluxes from NM flux array
    function j_dissolved_pointsource(me)
        class(Reach) :: me
        real(dp) :: j_dissolved_pointsource
        j_dissolved_pointsource &
            = sum(me%j_dissolved(5+me%nInflows+me%nDiffuseSources:4+me%nInflows+me%nDiffuseSources+me%nPointSources))
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

!-- TRANSFORMED --!

    subroutine set_j_transformed_outflow(me, j_transformed_outflow)
        class(Reach) :: me
        real(dp) :: j_transformed_outflow(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_transformed(1,:,:,:) = j_transformed_outflow
    end subroutine

    subroutine set_j_transformed_runoff(me, j_transformed_runoff)
        class(Reach) :: me
        real(dp) :: j_transformed_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_transformed(2+me%nInflows,:,:,:) = j_transformed_runoff
    end subroutine

    subroutine set_j_transformed_inflow(me, j_transformed_inflow, i)
        class(Reach) :: me
        real(dp) :: j_transformed_inflow(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_transformed(1+i,:,:,:) = j_transformed_inflow
    end subroutine

    subroutine set_j_transformed_deposit(me, j_transformed_deposit)
        class(Reach) :: me
        real(dp) :: j_transformed_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        me%j_transformed(4+me%nInflows,:,:,:) = j_transformed_deposit
    end subroutine

    !> Set the diffuse source flux of the SPM flux array
    subroutine set_j_transformed_diffusesource(me, j_transformed_diffusesource, i)
        class(Reach) :: me
        real(dp) :: j_transformed_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_transformed(4+me%nInflows+i,:,:,:) = j_transformed_diffusesource
    end subroutine

    !> Set the point source flux of the SPM flux array
    subroutine set_j_transformed_pointsource(me, j_transformed_pointsource, i)
        class(Reach) :: me
        real(dp) :: j_transformed_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        integer :: i
        me%j_transformed(4+me%nInflows+me%nDiffuseSources+i,:,:,:) = j_transformed_pointsource
    end subroutine

!-- DISSOLVED --!

    subroutine set_j_dissolved_outflow(me, j_dissolved_outflow)
        class(Reach) :: me
        real(dp) :: j_dissolved_outflow
        me%j_dissolved(1) = j_dissolved_outflow
    end subroutine

    subroutine set_j_dissolved_inflow(me, j_dissolved_inflow, i)
        class(Reach) :: me
        real(dp) :: j_dissolved_inflow
        integer :: i
        me%j_dissolved(1+i) = j_dissolved_inflow
    end subroutine

    !> Set the diffuse source flux of the SPM flux array
    subroutine set_j_dissolved_diffusesource(me, j_dissolved_diffusesource, i)
        class(Reach) :: me
        real(dp) :: j_dissolved_diffusesource
        integer :: i
        me%j_dissolved(4+me%nInflows+i) = j_dissolved_diffusesource
    end subroutine

    !> Set the point source flux of the SPM flux array
    subroutine set_j_dissolved_pointsource(me, j_dissolved_pointsource, i)
        class(Reach) :: me
        real(dp) :: j_dissolved_pointsource
        integer :: i
        me%j_dissolved(4+me%nInflows+me%nDiffuseSources+i) = j_dissolved_pointsource
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
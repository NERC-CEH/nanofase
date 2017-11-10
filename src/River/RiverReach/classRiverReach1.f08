!> Module responsible for the RiverReach1 object
module classRiverReach1
    use mo_netcdf
    use Globals
    use UtilModule
    use ResultModule
    use ErrorInstanceModule
    use spcRiverReach
    ! use classBedSediment1
    implicit none
    private

    !> RiverReach1 object is responsible for sediment transport along river and
    !! sediment deposition to bed sediment.
    type, public, extends(RiverReach) :: RiverReach1

      contains
        procedure :: create => createRiverReach1
        procedure :: destroy => destroyRiverReach1
        procedure :: update => updateRiverReach1
        procedure :: resuspension => resuspensionRiverReach1
        procedure, private :: calculateWidth => calculateWidth1
        procedure, private :: calculateDepth => calculateDepth1
        procedure, private :: calculateVelocity => calculateVelocity1
        procedure, private :: calculateSettlingVelocity => calculateSettlingVelocity1
        procedure, private :: calculateResuspension => calculateResuspension1
        procedure, private :: calculateArea => calculateArea1
        procedure, private :: calculateVolume => calculateVolume1
    end type

  contains

    !> Create a RiverReach with x, y, s, r coordinates from the datafile.
    function createRiverReach1(me, x, y, s, r, l, QrunoffTimeSeries) result(res)
        class(RiverReach1) :: me                                !! The RiverReach1 instance.
        integer :: x                                            !! Containing GridCell x-position index.
        integer :: y                                            !! Containing GridCell y-position index.
        integer :: s                                            !! Containing SubRiver index.
        integer :: r                                            !! RiverReach index.
        real(dp) :: l                                           !! Length of the RiverReach (without meandering).
        real(dp), allocatable :: QrunoffTimeSeries(:)           !! Any GridCell runoff (that has already been split to the correct RiverReach size)
        type(Result0D) :: D                                     ! Depth [m]
        type(Result0D) :: v                                     ! River velocity [m/s]
        type(Result0D) :: W                                     ! River width [m]
        integer :: n                                            ! Loop iterator for SPM size classes
        integer :: allst                                        ! Allocation status
        type(Result) :: res                                     !! The Result object
        type(NcDataset) :: NC                                   ! NetCDF dataset
        type(NcVariable) :: var                                 ! NetCDF variable
        type(NcGroup) :: grp                                    ! NetCDF group
        type(ErrorInstance) :: error                            ! To return errors
        real(dp), allocatable :: spmDensities(:)                ! Array of sediment particle densities for each size class

        ! First, let's set the RiverReach's reference and the length
        me%ref = trim(ref("RiverReach", x, y, s, r))
        me%l = l
        ! Allocate the arrays of size classes and set SPM to 0 to begin with
        allocate(me%rho_spm(C%nSizeClassesSpm), &
            me%spmIn(C%nSizeClassesSpm), &
            me%spmOut(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            me%j_spm_res(C%nSizeClassesSpm), &
            stat=allst) 
        me%rho_spm = 0                  ! Set SPM density and mass to 0 to begin with
        me%m_spm = 0
        allocate(me%QrunoffTimeSeries, source=QrunoffTimeSeries)    ! This reach's runoff

        ! Get the specific RiverReach parameters from data - only the stuff
        ! that doesn't depend on time
        ! TODO: Check these groups exist (hasGroup()). Move data extraction to database object.
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(trim(ref("GridCell", x, y)))         ! Get the GridCell we're in
        grp = grp%getGroup(trim(ref("SubRiver", x, y, s)))      ! Get the SubRiver we're in
        me%ncGroup = grp%getGroup(trim(me%ref))                 ! Finally, get the actual RiverReach group
        var = me%ncGroup%getVariable("slope")                   ! Get the slope
        call var%getData(me%S)
        if (me%ncGroup%hasVariable("f_m")) then                 ! If there is a meandering factor, get that
            var = me%ncGroup%getVariable("f_m")                 ! If not, it defaults to 1 (no meandering).
            call var%getData(me%f_m)
        end if
        var = me%ncGroup%getVariable("alpha_res")               ! Get the alpha_res calibration factor for resuspension
        call var%getData(me%alpha_res)
        var = me%ncGroup%getVariable("beta_res")                ! Get the beta_res calibration factor for resuspension
        call var%getData(me%beta_res)
        ! TODO: Add checks for the above

        ! Get the time series of SPM inflows
        allocate(me%m_spmTimeSeries(C%nTimeSteps,C%nSizeClassesSpm))
        if (me%ncGroup%hasVariable("spm")) then
            var = me%ncGroup%getVariable("spm")
            call var%getData(me%m_spmTimeSeries)
            me%m_spmTimeSeries = me%m_spmTimeSeries*C%timeStep      ! SPM should be in kg/s, thus need to convert to kg/timestep
        else
            me%m_spmTimeSeries = 0
        end if
        ! Initial density can't be updated in we have volume from Qin, so this is left to the update() procedure

        ! TODO: Where should Manning's n come from? From Constants for the moment:
        me%n = C%n_river

        ! Create the BedSediment for this RiverReach. Hard coded as type 1 for the moment
        ! and filled with dummy parameters
        ! allocate(type(BedSediment1)::me%bedSediment)
        ! res = me%bedSediment%create( &
        !     n = ref('BedSediment',x,y,s,r,1), &
        !     ln = ["BSL1","BSL2","BSL3","BSL4","BSL5"], &
        !     nsc = C%nSizeClassesSpm, &
        !     nl = 5, &
        !     bslType = 1, &
        !     C_tot = [ &
        !         me%bsArea*0.1, &
        !         me%bsArea*0.1, &
        !         me%bsArea*0.1, &
        !         me%bsArea*0.1, &
        !         me%bsArea*0.1 &
        !     ], &
        !     f_comp = [ &
        !         [ &
        !             [0.1],[0.1],[0.1],[0.1],[0.1] &
        !         ], &
        !         [ &
        !             [0.1],[0.1],[0.1],[0.1],[0.1] &
        !         ], &
        !     ], &
        !     pd_comp = [0.01,0.01,0.01,0.01,0.01], &
        !     M_f = [1,1,1,1,1])
        ! call res%addToTrace('Creating ' // trim(me%ref))
    end function

    !> Destroy this RiverReach1
    function destroyRiverReach1(me) result(r)
        class(RiverReach1) :: me                            !! This RiverReach1 instance
        type(Result) :: r                                   !! The Result object
        ! TODO: Write some destroy logic
    end function

    !> Update the RiverReach on this timestep t, based on the inflow Q and SPM provided:
    !! <ul>
    !!  <li>Masses/volumes updated according to inflows</li>
    !!  <li>Reach dimensions updated according to inflows</li>
    !!  <li>Resuspension from BedSediment added</li>
    !!  <li>Deposition to BedSediment removed</li>
    !!  <li>Water and SPM advected from the reach</li>
    !! </ul>
    function updateRiverReach1(me, Qin, spmIn, t) result(r)
        class(RiverReach1) :: me                            !! This RiverReach1 instance
        real(dp) :: Qin                                     !! Inflow to this reach
        real(dp) :: spmIn(C%nSizeClassesSpm)                !! Inflow SPM to this reach
        integer :: t                                        !! What time step are we on?
        type(Result) :: r                                   !! Result object to return
        type(Result0D) :: D                                 ! Result objects for depth
        integer :: n                                        ! Size class loop it  erator
        integer :: nDisp                                    ! Number of displacements to split reach into
        real(dp) :: dt                                      ! Length of each displacement [s]
        integer :: i                                        ! Iterator for displacements
        real(dp) :: dQin                                    ! Qin for each displacement
        real(dp) :: dSpmIn(C%nSizeClassesSpm)               ! spmIn for each displacement
        real(dp) :: settlingVelocity(C%nSizeClassesSpm)     ! Settling velocity for each size class
        real(dp) :: k_settle(C%nSizeClassesSpm)             ! Settling constant for each size class
        real(dp) :: dSpmOut(C%nSizeClassesSpm)              ! SPM outflow for the displacement

        me%Qrunoff = me%QrunoffTimeSeries(t)                ! Get the runoff for this time step.
        me%Qin = Qin + me%Qrunoff                           ! Set this reach's inflow
        me%spmIn = spmIn + me%m_spmTimeSeries(t,:)          ! Inflow SPM from upstream reach + inflow from data file
        ! TODO: m_spm shouldn't really be used as a symbol for what is a mass flux,
        ! change the name to something else, e.g. spmInTimeSeries.

        ! Calculate the depth, velocity, area and volume
        me%W = me%calculateWidth(me%Qin/C%timeStep)
        D = me%calculateDepth(me%W, me%S, me%Qin/C%timeStep)
        me%D = .dp. D                                       ! Get real(dp) data from Result object
        call r%addError(.error. D)                          ! Add any error that occurred
        me%v = me%calculateVelocity(me%D, me%Qin/C%timeStep, me%W)
        me%xsArea = me%calculateArea(me%D, me%W)            ! Calculate the cross-sectional area of the reach
        me%bsArea = me%W*me%l*me%f_m                        ! Calculate the BedSediment area
        me%volume = me%calculateVolume(me%D, me%W, me%l, me%f_m)

        ! Set the resuspension flux me%j_spm_res (but don't acutally resuspend until we're looping
        ! through displacements). This can be done now as resuspension doesn't depend
        ! on anything that changes on each displacement (unlike settling velocity,
        ! which depends on SPM density).
        call r%addErrors(.errors. me%resuspension())

        ! If Qin for this timestep is bigger than the reach volume, then we need to
        ! split into a number of displacements
        nDisp = ceiling(me%Qin/me%volume)                   ! Number of displacements
        dt = C%timeStep/nDisp                               ! Length of each displacement [s]
        dQin = me%Qin/nDisp                                 ! Inflow to the first displacement [m3]
        dSpmIn = me%spmIn/nDisp                             ! SPM inflow to the first displacment [kg]
        me%Qout = 0                                         ! Reset Qout for this timestep
        me%spmOut = 0                                       ! Reset spmOut for this timestep

        ! Loop through the displacements
        do i = 1, nDisp
            ! Update SPM according to inflow for this displacement, then calculate
            ! new SPM density based on this and the dimensions
            me%m_spm = me%m_spm + dSpmIn                    ! Add inflow SPM to SPM already in reach
            me%rho_spm = me%m_spm/me%volume                 ! Update the SPM density

            ! TODO: Resuspended SPM must be taken from BedSediment
            ! Resuspend SPM for this displacment, based on resuspension flux previously calculated
            me%m_spm = me%m_spm + me%j_spm_res*dt         ! SPM resuspended is resuspension flux * displacement length
            me%rho_spm = me%m_spm/me%volume                 ! Update the SPM density

            ! Calculate the settling velocity and rate for each SPM size class. Must be
            ! done for each displacement as it depends on rho_spm
            do n = 1, C%nSizeClassesSpm
                settlingVelocity(n) = me%calculateSettlingVelocity(C%d_spm(n), me%rho_spm(n), C%T)
                k_settle(n) = settlingVelocity(n)/me%D
            end do

            ! Remove settled SPM from the displacement. TODO: This will go to BedSediment eventually
            me%m_spm = me%m_spm - (k_settle*dt)*me%m_spm
            me%rho_spm = me%m_spm/me%volume                     ! Recalculate the density

            ! If we've removed all of the SPM, set to 0
            do n = 1, C%nSizeClassesSpm
                if (me%m_spm(n) < 0) then                       ! If we've removed all of the SPM, set to 0
                    me%m_spm(n) = 0
                    me%rho_spm(n) = 0
                end if
            end do

            ! Other stuff, like abstraction, to go here.

            ! Advect the SPM out of the reach at the outflow rate, until it has all gone
            ! TODO: Set dQout different to dQin based on abstraction etc.
            dSpmOut = dQin*me%rho_spm
            do n = 1, C%nSizeClassesSpm
                if (dSpmOut(n) .le. me%m_spm(n)) then
                    ! Update the SPM mass and density after it has been advected
                    me%m_spm(n) = me%m_spm(n) - dSpmOut(n)
                    me%rho_spm(n) = me%m_spm(n)/me%volume
                else
                    ! If dSpmOut > current SPM mass, then actual spmOut must equal the SPM mass,
                    ! i.e., all of the remaining SPM has been advected out of the reach 
                    dSpmOut(n) = me%m_spm(n)
                    me%m_spm(n) = 0                       ! SPM mass and density must now be zero
                    me%rho_spm(n) = 0
                end if
            end do

            ! Sum the displacement outflows and mass for the final outflow
            ! Currently, Qout = Qin. Maybe abstraction etc will change this
            me%Qout = me%Qout + dQin
            me%spmOut = me%spmOut + dSpmOut
        end do
        ! If there's no SPM left, add the "all SPM advected" warning
        do n = 1, C%nSizeClassesSpm
            if (me%m_spm(n) == 0 .and. me%spmIn(n) /= 0) then
                call r%addError(ErrorInstance( &
                    code=500, &
                    message="All SPM in size class " // trim(str(n)) // " (" // trim(str(C%d_spm(n)*1e6)) // &
                            " um) advected from RiverReach.", &
                    isCritical=.false.) &
                )
            end if 
        end do
        
        ! Set the final SPM density
        me%rho_spm = me%m_spm/me%volume
        ! Add what we're doing here to the error trace
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function

    !> Perform the resuspension simulation for a time step
    function resuspensionRiverReach1(me) result(r)
        class(RiverReach1) :: me                !! This RiverReach1 instance
        type(Result) :: r                       !! The Result object to return
        real(dp) :: d_max                       ! Maximum resuspendable particle size
        real(dp) :: M_prop(C%nSizeClassesSpm)   ! Proportion of size class that can be resuspended
        real(dp) :: omega                       ! Stream power per unit bed area [W m-2]
        real(dp) :: f_fr                        ! Friction factor [-]
        integer :: n                            ! Iterator for size classes
        ! There must be inflow for there to be resuspension
        if (me%Qin > 0) then
            ! Calculate maximum resuspendable particle size and proportion of each
            ! size class that can be resuspended. Changes on each timestep as dependent
            ! on river depth
            d_max = 9.994*sqrt(me%alpha_res*C%g*me%D*me%S)**2.5208 
            ! Calculate proportion of each size class that can be resuspended
            do n = 1, C%nSizeClassesSpm
                ! Calculate the proportion of size class that can be resuspended
                if (d_max < C%d_spm_low(n)) then
                    M_prop(n) = 0                               ! None can be resuspended
                else if (d_max > C%d_spm_upp(n)) then
                    M_prop(n) = 1                               ! All can be resuspended
                else
                    M_prop(n) = (d_max - C%d_spm_low(n)) &      ! Only some can be resuspended
                        / (C%d_spm_upp(n) - C%d_spm_low(n))     
                end if
            end do
            ! Calculate the stream power per unit bed area
            omega = C%rho_w(C%T)*C%g*(me%Qin/C%timeStep)*me%S/me%W
            f_fr = 4*me%D/(me%W+2*me%D)
            ! Calculate the resuspension
            me%j_spm_res = me%calculateResuspension( &
                beta = me%beta_res, &
                L = me%l*me%f_m, &
                W = me%W, &
                m_bed = 1.0_dp, &
                M_prop = M_prop, &
                omega = omega, &
                f_fr = f_fr &
            )
        else
            me%j_spm_res = 0                                ! If there's no inflow
        end if
        call r%addToTrace('Calculating resuspension for' // trim(me%ref))
    end function

    !> Calculate the width \( W \) of the river based on the discharge:
    !! $$
    !!      W = 1.22Q^{0.557}
    !! $$
    !! References:
    !! <ul>
    !!  <li>[Dumont et al., 2012](https://doi.org/10.1080/02626667.2012.715747)</li>
    !!  <li>[Allen et al., 1994](https://doi.org/10.1111/j.1752-1688.1994.tb03321.x)</li>
    !! </ul>
    pure function calculateWidth1(me, Q) result(W)
        class(RiverReach1), intent(in) :: me    !! The RiverReach1 instance
        real(dp), intent(in) :: Q               !! Grid cell discharge \( Q \) [m**3/s]
        real(dp) :: W                           !! The calculated width \( W \) [m]
        W = 1.22*Q**0.557                       ! Calculate the width
    end function

    !> Calculate water depth from Manning's roughness coefficient,
    !! using Newton's method:
    !! $$
    !!      D_i = D_{i-1} - \frac{f(D_{i-1})}{f'(D_{i-1})}
    !! $$
    !! where
    !! $$
    !!      f(D) = WD \left( \frac{WD}{W+2D} \right)^{2/3} \frac{\sqrt{S}}{n} - Q = 0
    !! $$
    !! and
    !! $$
    !!      f'(D) = \frac{\sqrt{S}}{n} \frac{(DW)^{5/3}(6D + 5W)}{3D(2D + W)^{5/3}}
    !! $$
    pure function calculateDepth1(me, W, S, Q) result(r)
        class(RiverReach1), intent(in) :: me    !! The RiverReach1 instance.
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        real(dp), intent(in) :: S               !! River slope \( S \) [-].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m3/s].
        real(dp) :: D_i                         ! The iterative river depth \( D_i \) [m].
        real(dp) :: f                           ! The function to find roots for \( f(D) \).
        real(dp) :: df                          ! The derivative of \( f(D) \) with respect to \( D \).
        real(dp) :: alpha                       ! Constant extracted from f and df
        integer :: i                            ! Loop iterator to make sure loop isn't endless.
        integer :: iMax                         ! Maximum number of iterations before error.
        real(dp) :: epsilon                     ! Proximity to zero allowed.
        type(ErrorInstance) :: error            ! Variable to store error in.
        character(len=100) :: iChar             ! Loop iterator as character (for error message).
        character(len=100) :: fChar             ! f(D) value as character (for error message).
        character(len=100) :: epsilonChar       ! Proximity of f(D) to zero as character (for error message).
        type(Result0D) :: r                     !! The Result object.

        ! TODO: Allow user (e.g., data file) to specify max iterations and precision?
        D_i = 1.0_dp                                                            ! Take a guess at D being 1m to begin
        i = 1                                                                   ! Iterator for Newton solver
        iMax = 10000                                                            ! Allow 10000 iterations
        epsilon = 1.0e-9_dp                                                     ! Proximity to zero allowed
        alpha = W**(5.0_dp/3.0_dp) * sqrt(S)/me%n                               ! Extract constant to simplify f and df.
        f = alpha*D_i*((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                    ! First value for f, based guessed D_i

        ! Loop through and solve until f(D) is within e-9 of zero, or max iterations reached
        do while (abs(f) > epsilon .and. i <= iMax)
            f = alpha * D_i * ((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q            ! f(D) based on D_{m-1}
            df = alpha * ((D_i)**(5.0_dp/3.0_dp) * (6*D_i + 5*W))/(3*D_i * (2*D_i + W)**(5.0_dp/3.0_dp))
            D_i = D_i - f/df                                                    ! Calculate D_i based on D_{m-1}
            i = i+1
        end do

        if (isnan(D_i)) then                                                    ! If method diverges (results in NaN)
            write(iChar,*) i
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method diverged to NaN after " // trim(adjustl(iChar)) // " iterations." &
            )
        else if (i > iMax) then                                                 ! If max number of iterations reached
            write(iChar,*) iMax
            write(fChar,*) f
            write(epsilonChar,*) epsilon
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method failed to converge - maximum number of iterations (" &
                    // trim(adjustl(iChar)) // ") exceeded. " &
                    // "Precision (proximity to zero) required: " // trim(adjustl(epsilonChar)) &
                    // ". Final value: " // trim(adjustl(fChar)) // "." &
            )
        else
            error = ERROR_HANDLER%getNoError()                                  ! Otherwise, no error occurred
        end if
        r = Result( &                                                           ! Return the resulting data and error (or no error)
            data = D_i, &
            error = error &
        )
        call r%addToTrace("Calculating river depth")
    end function

    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    pure function calculateVelocity1(me, D, Q, W) result(v)
        class(RiverReach1), intent(in) :: me    !! This RiverReach1 instance
        real(dp), intent(in) :: D               !! River depth \( D \) [m]
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m**3/s]
        real(dp), intent(in) :: W               !! River width \( W \) [m]
        real(dp) :: v                           !! The calculated velocity \( v \) [m/s]
        v = Q/(W*D)
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
    function calculateSettlingVelocity1(me, d, rho_spm, T) result(W_spm)
        class(RiverReach1), intent(in) :: me        !! The RiverReach1 instance.
        real(dp), intent(in) :: d                   !! Sediment particle diameter [m].
        real(dp), intent(in) :: rho_spm             !! Sediment particle density [kg/m**3].
        real(dp), intent(in) :: T                   !! Temperature [C].
        real(dp) :: dStar                           ! Dimensionless particle diameter.
        real(dp) :: W_spm                           !! Calculated settling velocity [m/s].
        ! Settling only occurs if density of SPM is greater than density of water
        if (rho_spm > C%rho_w(T)) then
            dStar = ((rho_spm/C%rho_w(T) - 1)*C%g/C%nu_w(T)**2)**(1.0_dp/3.0_dp) * d    ! Calculate the dimensional particle diameter
            W_spm = (C%nu_w(T)/d) * dStar**3 * (38.1_dp + 0.93_dp &                     ! Calculate the settling velocity
                * dStar**(12.0_dp/7.0_dp))**(-7.0_dp/8.0_dp)
        else
            W_spm = 0.0_dp
        end if
    end function

    !> Calculate the resuspension flux [kg/s] reach with BedSediment area \( W*L \):
    !! $$
    !!      \mathbf{j}_{\text{res}} = \beta L W m_{\text{bed}} \mathbf{M}_{\text{prop}} \omega f
    !! $$
    pure function calculateResuspension1(me, beta, L, W, m_bed, M_prop, omega, f_fr) result(j_res)
        class(RiverReach1), intent(in) :: me            !! This RiverReach1 instance
        real(dp), intent(in) :: beta                    !! Calibration parameter \( \beta \) [s2 kg-1]
        real(dp), intent(in) :: L                       !! Reach length \( L = lf_{\text{m}} \) [m]
        real(dp), intent(in) :: W                       !! Reach width \( W \) [m]
        real(dp), intent(in) :: m_bed                   !! BedSediment mass per unit area \( m_{\text{bed}} \) [kg m-2]
        real(dp), intent(in) :: M_prop(C%nSizeClassesSpm) !! Proportion of this size class that is resuspenable \( M_{\text{prop}} \) [-]
        real(dp), intent(in) :: omega                   !! Stream power per unit bed area \( \omega \) [kg m-2]
        real(dp), intent(in) :: f_fr                    !! Friction factor \( f \) [-]
        real(dp) :: j_res(C%nSizeClassesSpm)            !! Calculated resuspension flux \( j_{\text{res}} \) [kg/s]
        j_res = beta*L*W*m_bed*M_prop*omega*f_fr
    end function

    !> Calculate the volume of a RiverReach, assuming a rectangular profile:
    !! $$
    !!      \text{volume} = DWlf_m
    !! $$
    pure function calculateVolume1(me, D, W, l, f_m) result(volume)
        class(RiverReach1), intent(in) :: me        !! The RiverReach1 instance
        real(dp), intent(in) :: D                   !! River depth [m]
        real(dp), intent(in) :: W                   !! River width [m]
        real(dp), intent(in) :: l                   !! River length, without meandering [m]
        real(dp), intent(in) :: f_m                 !! Meandering factor [-]
        real(dp) :: volume                          !! The calculated volume [m3]
        volume = D*W*l*f_m
    end function

    !> Calculate the area of a cross-section of the RiverReach, assuming
    !! a rectangular profile:
    !! $$
    !!      \text{area} = DW
    !! $$
    pure function calculateArea1(me, D, W) result(area)
        class(RiverReach1), intent(in) :: me        !! The RiverReach1 instance
        real(dp), intent(in) :: D                   !! River depth [m]
        real(dp), intent(in) :: W                   !! River width [m]
        real(dp) :: area                            !! The calculated area [m3]
        area = D*W
    end function

end module
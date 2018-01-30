!> Module containing the `RiverReach1` class definition
module classRiverReach1
    use mo_netcdf
    use Globals
    use UtilModule
    use ResultModule
    use ErrorInstanceModule
    use spcRiverReach
    use classBedSediment1
    use classReactor1
    implicit none

    !> `RiverReach1` object is responsible for sediment transport along river and
    !! sediment deposition to bed sediment.
    type, public, extends(RiverReach) :: RiverReach1
      contains
        ! Create/destroy
        procedure :: create => createRiverReach1
        procedure :: destroy => destroyRiverReach1
        ! Simulators
        procedure :: update => updateRiverReach1
        procedure :: resuspension => resuspensionRiverReach1
        procedure :: settling => settlingRiverReach1
        procedure :: depositToBed => depositToBedRiverReach1
        ! Data handlers
        procedure :: setDefaults => setDefaultsRiverReach1
        procedure :: parseInputData => parseInputDataRiverReach1
        ! Calculators
        procedure :: calculateWidth => calculateWidth1
        procedure :: calculateDepth => calculateDepth1
        procedure :: calculateVelocity => calculateVelocity1
        procedure :: calculateSettlingVelocity => calculateSettlingVelocity1
        procedure :: calculateResuspension => calculateResuspension1
        procedure :: calculateArea => calculateArea1
        procedure :: calculateVolume => calculateVolume1
    end type

  contains

    !> Create this `RiverReach1`, parse input data and set up contained `BedSediment`
    function createRiverReach1(me, x, y, s, r, l, Q_runoff_timeSeries) result(res)
        class(RiverReach1) :: me                                !! The `RiverReach1` instance.
        integer :: x                                            !! Containing `GridCell` x-position index.
        integer :: y                                            !! Containing `GridCell` y-position index.
        integer :: s                                            !! Containing `SubRiver` index.
        integer :: r                                            !! `RiverReach` index.
        real(dp) :: l                                           !! Length of the `RiverReach` (without meandering).
        real(dp), allocatable :: Q_runoff_timeSeries(:)         !! Any `GridCell` runoff (that has already been split to the correct `RiverReach` size)
        type(Result) :: res                                     !! The Result object
        type(Result0D) :: D                                     ! Depth [m]
        type(Result0D) :: v                                     ! River velocity [m/s]
        type(Result0D) :: W                                     ! River width [m]
        integer :: n                                            ! Loop iterator for SPM size classes
        integer :: allst                                        ! Allocation status
        type(ErrorInstance) :: error                            ! To return errors

        ! First, let's set the RiverReach's reference and the length
        me%x = x
        me%y = y
        me%s = s
        me%r = r
        me%ref = trim(ref("RiverReach", x, y, s, r))
        me%l = l
        ! Allocate the arrays of size classes and set SPM to 0 to begin with
        allocate(me%spmIn(C%nSizeClassesSpm), &
            me%spmOut(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            me%j_spm_res(C%nSizeClassesSpm), &
            me%C_spm(C%nSizeClassesSpm), &
            me%k_settle(C%nSizeClassesSpm), &
            me%spmDep(C%nSizeClassesSpm), &
            stat=allst)
        me%C_spm = 0                    ! Set SPM concentration and mass to 0 to begin with
        me%m_spm = 0
        allocate(me%Q_runoff_timeSeries, source=Q_runoff_timeSeries)    ! This reach's runoff
        
        ! Set any defaults (no errors to be thrown)
        call me%setDefaults()
        
        ! Get data from the input data file
        call res%addErrors( &
            .errors. me%parseInputData() &    
        )

        ! TODO: Where should Manning's n come from? From Constants for the moment:
        me%n = C%n_river

        ! Create the BedSediment for this RiverReach
        ! TODO: Get the type of BedSediment from the data file, and check for allst
        allocate(BedSediment1::me%bedSediment)
        call res%addErrors(.errors. me%bedSediment%create(me%ncGroup))
        
        ! Create the Reactor object to deal with nanoparticle transformations
        allocate(Reactor1::me%reactor)
        call res%addErrors(.errors. me%reactor%create(me%x, me%y))
        
        call res%addToTrace('Creating ' // trim(me%ref))
    end function

    !> Destroy this `RiverReach1`
    function destroyRiverReach1(me) result(r)
        class(RiverReach1) :: me                            !! This `RiverReach1` instance
        type(Result) :: r                                   !! The `Result` object
        ! TODO: Write some destroy logic
    end function

    !> Update the RiverReach on this time step t, based on the inflow Q and SPM provided:
    !! <ul>
    !!  <li>Masses/volumes updated according to inflows</li>
    !!  <li>Reach dimensions updated according to inflows</li>
    !!  <li>Resuspension from BedSediment added</li>
    !!  <li>Deposition to BedSediment removed</li>
    !!  <li>Water and SPM advected from the reach</li>
    !! </ul>
    function updateRiverReach1(me, Q_in, spmIn, t, j_spm_runoff) result(r)
        class(RiverReach1) :: me                            !! This `RiverReach1` instance
        real(dp) :: Q_in                                    !! Inflow to this reach
        real(dp) :: spmIn(C%nSizeClassesSpm)                !! Inflow SPM to this reach
        real(dp) :: j_spm_runoff(:)		                    !! Eroded sediment runoff to this reach
        integer :: t                                        !! Current time step [s]
        type(Result) :: r                                   !! `Result` object to return
        type(Result0D) :: D                                 ! Result object for depth
        integer :: n                                        ! Size class loop it  erator
        integer :: nDisp                                    ! Number of displacements to split reach into
        real(dp) :: dt                                      ! Length of each displacement [s]
        integer :: i                                        ! Iterator for displacements
        real(dp) :: dQ_in                                   ! Q_in for each displacement
        real(dp) :: dSpmIn(C%nSizeClassesSpm)               ! spmIn for each displacement
        real(dp) :: dSpmDep(C%nSizeClassesSpm)              ! Deposited SPM for each displacement
        real(dp) :: settlingVelocity(C%nSizeClassesSpm)     ! Settling velocity for each size class
        real(dp) :: k_settle(C%nSizeClassesSpm)             ! Settling constant for each size class
        real(dp) :: dSpmOut(C%nSizeClassesSpm)              ! SPM outflow for the displacement

        me%Q_runoff = me%Q_runoff_timeSeries(t)             ! Hydrological runoff for this time step [m3/timestep - TODO change to m/s]
        me%j_spm_runoff = j_spm_runoff                      ! Eroded soil runoff [kg/timestep]
        me%Q_in = Q_in + me%Q_runoff                        ! Set this reach's inflow
        me%spmIn = spmIn + me%j_spm_runoff                  ! Inflow SPM from upstream reach + eroded soil runoff [kg/timestep]

        ! Calculate the depth, velocity, area and volume
        me%W = me%calculateWidth(me%Q_in/C%timeStep)
        D = me%calculateDepth(me%W, me%slope, me%Q_in/C%timeStep)
        me%D = .dp. D                                       ! Get real(dp) data from Result object
        call r%addError(.error. D)                          ! Add any error that occurred
        me%v = me%calculateVelocity(me%D, me%Q_in/C%timeStep, me%W)
        me%xsArea = me%calculateArea(me%D, me%W)            ! Calculate the cross-sectional area of the reach
        me%bedArea = me%W*me%l*me%f_m                       ! Calculate the BedSediment area
        me%volume = me%calculateVolume(me%D, me%W, me%l, me%f_m)

        ! Set the resuspension flux me%j_spm_res and settling rate me%k_settle
        ! (but don't acutally resuspend/settle until we're looping through
        ! displacements). This can be done now as settling/resuspension rates
        ! don't depend on anything that changes on each displacement
        call r%addErrors([ &
            .errors. me%resuspension(), &                   ! Also removes SPM from BedSediment
            .errors. me%settling() &                        ! Doesn't remove SPM from BedSediment - done later
        ])

        ! If Q_in for this timestep is bigger than the reach volume, then we need to
        ! split into a number of displacements
        nDisp = ceiling(me%Q_in/me%volume)                  ! Number of displacements
        dt = C%timeStep/nDisp                               ! Length of each displacement [s]
        dQ_in = me%Q_in/nDisp                               ! Inflow to the first displacement [m3]
        dSpmIn = me%spmIn/nDisp                             ! SPM inflow to the first displacment [kg]
        me%Q_out = 0                                        ! Reset Q_out for this time step
        me%spmOut = 0                                       ! Reset spmOut for this time step
        me%spmDep = 0                                       ! Reset deposited SPM for this time step

        ! Loop through the displacements
        do i = 1, nDisp
            ! Update SPM according to inflow for this displacement, then calculate
            ! new SPM concentration based on this and the dimensions
            me%m_spm = me%m_spm + dSpmIn                    ! Add inflow SPM to SPM already in reach
            me%C_spm = me%m_spm/me%volume                   ! Update the SPM concentration

            ! TODO: Resuspended SPM must be taken from BedSediment
            ! Resuspend SPM for this displacment, based on resuspension flux previously calculated
            me%m_spm = me%m_spm + me%j_spm_res*dt           ! SPM resuspended is resuspension flux * displacement length
            me%C_spm = me%m_spm/me%volume                   ! Update the SPM concentration

            ! Remove settled SPM from the displacement. TODO: This will go to BedSediment eventually
            ! If we've removed all of the SPM, set to 0
            dSpmDep = min(me%k_settle*dt*me%m_spm, me%m_spm)    ! Up to a maximum of the mass of SPM currently in reach
            me%m_spm = me%m_spm - dSpmDep
            me%spmDep = me%spmDep + dSpmDep                 ! Keep track of deposited SPM for this time step
            me%C_spm = max(me%m_spm/me%volume, 0.0)         ! Recalculate the concentration

            ! Other stuff, like abstraction, to go here.

            ! Advect the SPM out of the reach at the outflow rate, until it has all gone
            ! TODO: Set dQ_out different to dQ_in based on abstraction etc.
            dSpmOut = min(dQ_in*me%C_spm, me%m_spm)             ! Maximum of m_spm can be advected
            me%m_spm = me%m_spm - dSpmOut                       ! Update the SPM mass after advection
            me%C_spm = me%m_spm/me%volume                       ! Update the concentration

            ! Sum the displacement outflows and mass for the final outflow
            ! Currently, Q_out = Q_in. Maybe abstraction etc will change this
            me%Q_out = me%Q_out + dQ_in
            me%spmOut = me%spmOut + dSpmOut
        end do

        ! Now add the settled SPM to the BedSediment
        ! TODO: Fractional composition errors trigger when calling BedSediment%deposit.
        ! Need to sort these out.
        ! call r%addErrors(.errors. me%depositToBed(me%spmDep))

        ! If there's no SPM left, add the "all SPM advected" warning
        do n = 1, C%nSizeClassesSpm
            if (isZero(me%m_spm(n)) .and. me%spmIn(n) /= 0) then
                call r%addError(ErrorInstance( &
                    code = 500, &
                    message = "All SPM in size class " // trim(str(n)) // " (" // trim(str(C%d_spm(n)*1e6)) // &
                            " um) advected from RiverReach.", &
                    isCritical = .false.) &
                )
            end if 
        end do
        
        ! Set the final SPM concentration
        me%C_spm = me%m_spm/me%volume
        ! Add what we're doing here to the error trace
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function

    !> Perform the resuspension simulation for a time step
    !! Reference: [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub)
    function resuspensionRiverReach1(me) result(r)
        class(RiverReach1) :: me                                !! This `RiverReach1` instance
        type(Result) :: r                                       !! The Result object to return
        real(dp) :: d_max                                       ! Maximum resuspendable particle size
        real(dp) :: M_prop(C%nSizeClassesSpm)                   ! Proportion of size class that can be resuspended
        real(dp) :: omega                                       ! Stream power per unit bed area [W m-2]
        real(dp) :: f_fr                                        ! Friction factor [-]
        integer :: n                                            ! Iterator for size classes
        ! There must be inflow for there to be resuspension
        if (me%Q_in > 0) then
            ! Calculate maximum resuspendable particle size and proportion of each
            ! size class that can be resuspended. Changes on each timestep as dependent
            ! on river depth
            d_max = 9.994*sqrt(me%alpha_res*C%g*me%D*me%slope)**2.5208 
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
            omega = C%rho_w(C%T)*C%g*(me%Q_in/C%timeStep)*me%slope/me%W
            f_fr = 4*me%D/(me%W+2*me%D)
            ! Calculate the resuspension
            ! TODO: Get actually mass of bed sediment
            me%j_spm_res = me%calculateResuspension( &
                beta = me%beta_res, &
                L = me%l*me%f_m, &
                W = me%W, &
                m_bed = 1.0_dp, &
                M_prop = M_prop, &
                omega = omega, &
                f_fr = f_fr &
            )
            ! Remove the material from the bed sediment
            ! TODO: Not working - BedSediment throws memory errors
            ! TODO: Double check M_resus param for resuspend is really /m2
            ! print *, me%j_spm_res*C%timeStep/me%bedArea
            ! call r%addErrors(.errors. me%bedSediment%resuspend(me%j_spm_res*C%timeStep/me%bedArea))
        else
            me%j_spm_res = 0                                ! If there's no inflow
        end if
        call r%addToTrace('Calculating resuspension for ' // trim(me%ref))
    end function

    !> Perform the settling simulation for a time step
    function settlingRiverReach1(me) result(r)
        class(RiverReach1) :: me                            !! This `RiverReach1` instance
        type(Result) :: r                                   !! The `Result` object to return any errors
        type(FineSediment1) :: fineSediment                 ! Object to pass SPM to BedSediment in
        integer :: n                                        ! Size class iterator
        real(dp) :: settlingVelocity(C%nSizeClassesSpm)     ! Settling velocity for each size class
        ! Loop through the size classes and calculate settling velocity
        do n = 1, C%nSizeClassesSpm
            settlingVelocity(n) = me%calculateSettlingVelocity( &
                C%d_spm(n), &
                sum(C%rho_spm)/C%nFracCompsSpm, &       ! Average of the fractional comps. TODO: Change to work with actual fractional comps.
                C%T &
            )
        end do
        me%k_settle = settlingVelocity/me%D
    end function

    !> Send the given mass of settled SPM to the BedSediment
    function depositToBedRiverReach1(me, spmDep) result(r)
        class(RiverReach1)  :: me                           !! This RiverReach1 instance
        real(dp)            :: spmDep(C%nSizeClassesSpm)    !! The SPM to deposit [kg/reach]
        type(Result)        :: r                            !! The data object to return any errors in
        type(FineSediment1) :: fineSediment(C%nSizeClassesSpm) ! FineSediment object to pass to BedSediment
        integer             :: n                            ! Loop iterator
        ! Create the FineSediment object and add deposited SPM to it
        ! (converting units of Mf_in to kg/m2), then give that object
        ! to the BedSediment
        ! TODO: Check units of deposited FS, are they /m2?
        do n = 1, C%nSizeClassesSpm
            call r%addErrors([ &
                .errors. fineSediment(n)%create("FineSediment_class_" // trim(str(n))), &
                .errors. fineSediment(n)%set(Mf_in=spmDep(n)/me%bedArea) &
            ])
        end do
        call r%addErrors(.errors. me%bedSediment%deposit(fineSediment))
        call r%addToTrace("Depositing SPM to BedSediment")
    end function
    
    !> Set any defaults from config.nml file (accessed in Globals). Should be
    !! called before input data in parsed.
    subroutine setDefaultsRiverReach1(me)
        class(RiverReach1) :: me            !! This `RiverReach` instance
        me%f_m = C%defaultMeanderingFactor
    end subroutine
    
    !> Obtain and store in object properties data from the input file
    function parseInputDataRiverReach1(me) result(r)
        class(RiverReach1) :: me            !! This `RiverReach1` instance
        type(Result) :: r                   !! The `Result` object to return, with any errors
        type(NcDataset) :: NC               ! NetCDF dataset
        type(NcVariable) :: var             ! NetCDF variable
        type(NcGroup) :: grp                ! NetCDF group
        
        ! Get the specific RiverReach parameters from data - only the stuff
        ! that doesn't depend on time
        ! TODO: Check these groups exist (hasGroup()). Move data extraction to database object.
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(trim(ref("GridCell", me%x, me%y)))       ! Get the GridCell we're in
        grp = grp%getGroup(trim(ref("SubRiver", me%x, me%y, me%s))) ! Get the SubRiver we're in
		me%ncGroup = grp%getGroup(trim(me%ref))                 ! Store the NetCDF group in a variable
        
        var = me%ncGroup%getVariable("slope")                   ! Get the slope
        call var%getData(me%slope)
        if (me%ncGroup%hasVariable("f_m")) then                 ! If there is a meandering factor, get that
            var = me%ncGroup%getVariable("f_m")                 ! If not, it defaults to 1 (no meandering) without warning
            call var%getData(me%f_m)
        end if
        var = me%ncGroup%getVariable("alpha_res")               ! Get the alpha_res calibration factor for resuspension
        call var%getData(me%alpha_res)
        var = me%ncGroup%getVariable("beta_res")                ! Get the beta_res calibration factor for resuspension
        call var%getData(me%beta_res)
        ! TODO: Add checks for the above
        
        
        call r%addToTrace('Parsing input data')             ! Add this procedure to the trace
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
        class(RiverReach1), intent(in) :: me    !! The `RiverReach1` instance
        real(dp), intent(in) :: Q               !! `GridCell` discharge \( Q \) [m**3/s]
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
        class(RiverReach1), intent(in) :: me    !! The `RiverReach1` instance.
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        real(dp), intent(in) :: S               !! River slope \( S \) [-].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m3/s].
        type(Result0D) :: r
            !! The Result object, containing the calculated depth [m] and any numerical error.
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
        call r%setData(D_i)
        call r%addError(error)
        call r%addToTrace("Calculating river depth")
    end function

    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    pure function calculateVelocity1(me, D, Q, W) result(v)
        class(RiverReach1), intent(in) :: me    !! This `RiverReach1` instance
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
        class(RiverReach1), intent(in) :: me        !! The `RiverReach1` instance.
        real(dp), intent(in) :: d                   !! Sediment particle diameter [m].
        real(dp), intent(in) :: rho_spm             !! Sediment particle density [kg/m**3].
        real(dp), intent(in) :: T                   !! Temperature [C].
        real(dp) :: W_spm                           !! Calculated settling velocity [m/s].
        real(dp) :: dStar                           ! Dimensionless particle diameter.
        ! Settling only occurs if SPM particle density is greater than density of water
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
        class(RiverReach1), intent(in) :: me            !! This `RiverReach1` instance
        real(dp), intent(in) :: beta                    !! Calibration parameter \( \beta \) [s2 kg-1]
        real(dp), intent(in) :: L                       !! Reach length \( L = lf_{\text{m}} \) [m]
        real(dp), intent(in) :: W                       !! Reach width \( W \) [m]
        real(dp), intent(in) :: m_bed                   !! `BedSediment` mass per unit area \( m_{\text{bed}} \) [kg m-2]
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
        class(RiverReach1), intent(in) :: me        !! The `RiverReach1` instance
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
        class(RiverReach1), intent(in) :: me        !! The `RiverReach1` instance
        real(dp), intent(in) :: D                   !! River depth [m]
        real(dp), intent(in) :: W                   !! River width [m]
        real(dp) :: area                            !! The calculated area [m3]
        area = D*W
    end function

end module
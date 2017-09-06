module classRiverReach1
    use netcdf                  ! TODO: Check if netcdf library is needed - don't think so
    use mo_netcdf
    use Globals
    use UtilModule
    use ResultModule
    use ErrorInstanceModule
    use spcRiverReach
    implicit none
    private

    !> RiverReach1 object is responsible for sediment transport along river and
    !! sediment deposition to bed sediment.
    type, public, extends(RiverReach) :: RiverReach1

      contains
        procedure, public :: create => createRiverReach1
        procedure, public :: destroy => destroyRiverReach1
        procedure, public :: updateDimensions => updateDimensions1
        procedure, public :: simulate => simulate1
        procedure :: calculateWidth => calculateWidth1
        procedure :: calculateDepth => calculateDepth1
        procedure :: calculateVelocity => calculateVelocity1
        procedure :: calculateSettlingVelocity => calculateSettlingVelocity1
        procedure :: calculateVolume => calculateVolume1
    end type

  contains

    !> Create a RiverReach with x, y, s, r coordinates from the datafile.
    function createRiverReach1(me, x, y, s, r, l) result(res)
        class(RiverReach1) :: me                                !! The RiverReach1 instance.
        integer :: x, y, s, r                                   !! Grid, SubRiver and RiverReach references.
        real(dp) :: l                                           !! Length of the RiverReach (without meandering).
        type(Result0D) :: D                                     !! Depth [m].
        type(Result0D) :: v                                     !! River velocity [m/s].
        type(Result0D) :: W                                     !! River width [m].
        integer :: i                                            !! Loop iterator.
        type(Result) :: res                                     !! The Result object.
        type(NcDataset) :: NC                                   !! NetCDF dataset
        type(NcVariable) :: var                                 !! NetCDF variable
        type(NcGroup) :: grp                                    !! NetCDF group
        type(ErrorInstance) :: error                            !! To return errors
        real(dp), allocatable :: spmDensities(:)                !! Array of sediment particle densities for each size class

        ! First, let's set the RiverReach's reference and the length
        me%ref = trim(ref(x,y,s,r))
        me%l = l

        !*************************************************!
        !*************************************************!
        ! NCDF GROUP REFERENCES NOT CORRECT AT THE MOMENT !
        !*************************************************!
        !*************************************************!

        ! Get the specific RiverReach parameters from data - only the stuff
        ! that doesn't depend on time
        ! TODO: Check these groups exist (hasGroup())
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(trim(ref(x,y)))                      ! Get the GridCell we're in
        grp = grp%getGroup(trim(ref(x,y,s)))                    ! Get the SubRiver we're in
        me%ncGroup = grp%getGroup(trim(ref(x,y,s,r)))           ! Finally, get the actual RiverReach group
        var = me%ncGroup%getVariable("slope")                   ! Get the slope
        call var%getData(me%S)
        if (me%ncGroup%hasVariable("f_m")) then                 ! If there is a meandering factor, get that
            var = me%ncGroup%getVariable("f_m")                 ! If not, it defaults to 1 (no meandering).
            call var%getData(me%f_m)
        end if
        ! var = grp%getVariable("runoff")                         ! Get the flow
        ! call var%getData(me%Q_runoff)
        ! var = grp%getVariable("spm_densities")                  ! Sediment particle densities
        ! call var%getData(spmDensities)

        !* DO SOMETHING TO GET SPMDENSITIES FROM SPMIN (SOURCES),
        !* WHICH SHOULD BE IN THE DATAFILE. SPMDENSITIES BELOW
        !* WILL CAUSE INVALID MEMORY REF UNTIL ITS GOT FROM SOMEWHERE.

        ! Check the sediment particle density array is the same size of nSizeClassesSPM
        ! error = ERROR_HANDLER%equal( &
        !     value = size(spmDensities), &
        !     criterion = C%nSizeClassesSpm, &
        !     message = "Sediment particle density array size must equal number of size classes." &
        ! )
        ! allocate(me%rho_spm, source=spmDensities)    ! Allocate to class variable

        ! Allocate size of settling velocity arrays
        allocate(me%W_spm(C%nSizeClassesSpm))
        allocate(me%k_settle(C%nSizeClassesSpm))

        ! TODO: Get the temperature from somewhere
        me%T = 15.0_dp

        ! TODO: Where should Manning's n come from? From Constants for the moment:
        me%n = C%n_river

        ! Calculate the initial dimensions, based on Q_in
        ! r = me%initDimensions()

        ! Only loop through size classes if there wasn't a array size mismatch error,
        ! otherwise a runtime error will be triggered before our custom error.
        ! if (error%notError()) then
        !     ! Calculate sediment particle settling velocity for each size class,
        !     ! then set the settling rate (W_spm/D) accordingly
        !     do i=1, C%nSizeClassesSPM
        !         ! Currently no error checking in this procedure, but if that changes
        !         ! we'll need to check for them here
        !         me%W_spm(i) = .dp. me%calculateSettlingVelocity(C%d_spm(i), me%rho_spm(i), me%T)
        !         me%k_settle(i) = me%W_spm(i)/me%D
        !     end do
        ! end if

        ! Return any errors there might have been, add this procedure to trace
        ! res = Result(errors=[error,.errors.W,.errors.D,.errors.v])
        ! call res%addToTrace("Creating River Reach")
    end function

    !> Destroy this RiverReach1
    function destroyRiverReach1(me) result(r)
        class(RiverReach1) :: me
        type(Result) :: r
        ! TODO: Write some destroy logic
    end function

    !> Updates the dimensions for each time step, based on Q_in
    function updateDimensions1(me, Q_in) result(r)
        class(RiverReach1) :: me            !! This RiverReach1 instance
        real(dp) :: Q_in                    !! Inflow to this reach
        type(Result) :: r                   !! Result object to return
        type(Result0D) :: W, D, v, volume

        me%Q_in = Q_in                      ! Set this reach's inflow
        ! Calculate the depth and velocities and set the instance's variables
        W = me%calculateWidth(me%Q_in)
        me%W = .dp. W
        D = me%calculateDepth(me%W, me%S, me%Q_in)
        me%D = .dp. D
        v = me%calculateVelocity(me%D, me%Q_in, me%W)
        me%v = .dp. v
        volume = me%calculateVolume(me%D, me%W, me%l, me%f_m)
        me%volume = .dp. volume
        r = Result( &
            errors = [.error. W, .error. D, .error. v, .error. volume] &
        )
    end function

    !> DUMMY FUNCTION
    function simulate1(me, dQ, dSPM) result(r)
        class(RiverReach1) :: me
        real(dp) :: dQ
        real(dp) :: dSPM(:)
        type(Result1D) :: r
        ! Do nothing for the moment
        r = Result( &
            data = [dQ, dSPM] &
        )
    end function

    !> Calculate the width \( W \) of the river based on the discharge:
    !! $$
    !!      W = 1.22Q^0.557
    !! $$
    !! References:
    !!  - [Dumont et al., 2012](https://doi.org/10.1080/02626667.2012.715747)
    !!  - [Allen et al., 1994](https://doi.org/10.1111/j.1752-1688.1994.tb03321.x)
    pure function calculateWidth1(me, Q) result(r)
        class(RiverReach1), intent(in) :: me     !! The RiverReach1 instance.
        real(dp), intent(in) :: Q               !! Grid cell discharge \( Q \) [m**3/s].
        type(ErrorInstance) :: error            !! Variable to store error in.
        type(Result0D) :: r                     !! Result object to return.
        ! Make sure the flow is positive
        error = ERROR_HANDLER%positive(Q, "Flow rate Q must be positive.")
        ! Calculate the width and return as Result object
        r = Result( &
            data = 1.22*Q**0.557, &
            error = error &
        )
        call r%addToTrace("Calculating river width")
    end function

    !> Calculate water depth from Manning's roughness coefficient,
    !! using Newton's method:
    !! $$
    !!      D_i = D_{i-1} - \frac{f(D_{i-1})}{f'(D_{i-1})}
    !! $$
    !! where
    !! $$
    !!      f(D) = WD \left( \frac{WD}{w+2D} \right)**{2/3} \frac{\sqrt{S}}{n} - Q = 0
    !! $$
    !! and
    !! $$
    !!      f'(D) = \frac{\sqrt{S}}{n} \frac{(DW)^{5/3}(6D + 5W)}{3D(2D + W)^{5/3}}
    !! $$
    pure function calculateDepth1(me, W, S, Q) result(r)
        class(RiverReach1), intent(in) :: me     !! The RiverReach1 instance.
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        real(dp), intent(in) :: S               !! River slope \( S \) [-].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m**3/s].
        real(dp) :: D_i                         !! The iterative river depth \( D_i \) [m].
        real(dp) :: f                           !! The function to find roots for \( f(D) \).
        real(dp) :: df                          !! The derivative of \( f(D) \) with respect to \( D \).
        real(dp) :: alpha                       !! Constant extracted from f and df
        integer :: i                            !! Loop iterator to make sure loop isn't endless.
        integer :: iMax                         !! Maximum number of iterations before error.
        type(ErrorInstance) :: error            !! Variable to store error in.
        character(len=100) :: iChar             !! Loop iterator as character (for error message).
        character(len=100) :: fChar             !! f(D) value as character (for error message).
        type(Result0D) :: r                     !! The result object.

        ! TODO: Allow user (e.g., data file) to specify max iterations, max iterations
        ! and precision?
        D_i = 1.0_dp                                                            ! Take a guess at D being 1m to begin
        i = 1                                                                   ! Iterator for Newton solver
        iMax = 10000                                                            ! Allow 10000 iterations before solving
        alpha = W**(5.0_dp/3.0_dp) * sqrt(S)/me%n                               ! Extract constant to simplify f and df.
        f = alpha*D_i*((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                    ! First value for f, based guessed D_i

        ! Loop through and solve until f(D) is within e-9 of zero.
        do while (abs(f) > 1.0e-9_dp .and. i <= iMax)
            f = alpha * D_i * ((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q            ! f(D) based on D_{m-1}
            df = alpha * ((D_i)**(5.0_dp/3.0_dp) * (6*D_i + 5*W))/(3*D_i * (2*D_i + W)**(5.0_dp/3.0_dp))
            D_i = D_i - f/df                                                    ! Calculate D_i based on D_{m-1}
            i = i+1
        end do

        if (isnan(D_i)) then                                                    ! If method diverges (results in NaN)
            write(iChar,*) i
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method diverged to NaN after " // trim(adjustl(iChar)) // " iterations.", &
                trace = ["Calculating river depth"] &
            )
        else if (i > iMax) then                                                 ! If max number of iterations reached
            write(iChar,*) iMax
            write(fChar,*) f
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method failed to converge - maximum number of iterations (" &
                    // trim(adjustl(iChar)) // ") exceeded." &
                    // "Precision (proximity to zero) required: 1e-9. Final value: " // trim(fChar) // ".", &
                trace = ["Calculating river depth"] &
            )
        else
            error = ERROR_HANDLER%getNoError()                                  ! Otherwise, no error occurred
        end if
        r = Result( &                                                           ! Return the resulting data and error (or no error)
            data = D_i, &
            error = error &
        )
    end function

    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    pure function calculateVelocity1(me, D, Q, W) result(r)
        class(RiverReach1), intent(in) :: me    !! The RiverReach1 instance.
        real(dp), intent(in) :: D               !! River depth \( D \) [m].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m**3/s].
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        type(Result0D) :: r                     !! The result object.
        r = Result( &                           ! Return the velocity. No error can occur here.
            data = Q/(W*D) &
        )
    end function

    !> Calculate the settling velocity of sediment particles for an individual
    !! size class:
    !! $$
    !!      W_spm = \frac{\nu}{d} d_{*}^3 (38.1 + 0.93 d_{*}^{12/7})^{-7/8}
    !! $$
    !! where
    !! $$
    !!      d_{*} = \left( \frac{\Delta g}{\nu^2} \right)^{1/3} d
    !! $$
    !! and
    !! $$
    !!      \Delta = \frac{\rho_spm}{\rho} - 1
    !! $$
    !! Reference: [Zhiyao et al, 2008](https://doi.org/10.1016/S1674-2370(15)30017-X)
    pure function calculateSettlingVelocity1(me, d, rho_spm, T) result(r)
        class(RiverReach1), intent(in) :: me        !! The RiverReach1 instance.
        real(dp), intent(in) :: d                   !! Sediment particle diameter [m].
        real(dp), intent(in) :: rho_spm             !! Sediment particle density [kg/m**3].
        real(dp), intent(in) :: T                   !! Temperature [C].
        real(dp) :: dStar                           !! Dimensionless particle diameter.
        real(dp) :: W_spm                           !! Calculated settling velocity [m/s].
        type(Result0D) :: r                         !! The Result object.
        dStar = ((rho_spm/C%rho_w(T) - 1)*C%g/C%nu_w(T)**2)**(1.0_dp/3.0_dp) * d  ! Calculate the dimensional particle diameter
        W_spm = (C%nu_w(T)/d) * dStar**3 * (38.1_dp + 0.93_dp &                   ! Calculate the settling velocity
            * dStar**(12.0_dp/7.0_dp))**(-7.0_dp/8.0_dp)
        r = Result(data = W_spm)
    end function

    !> Calculate the volume of a RiverReach:
    !! $$
    !!      \text{volume} = DWlf_m
    !! $$
    pure function calculateVolume1(me, D, W, l, f_m) result(r)
        class(RiverReach1), intent(in) :: me        !! The RiverReach1 instance
        real(dp), intent(in) :: D                   !! River depth
        real(dp), intent(in) :: W                   !! River width
        real(dp), intent(in) :: l                   !! River length, without meandering
        real(dp), intent(in) :: f_m                 !! Meandering factor
        type(Result0D) :: r                         !! The Result object
        r = Result(data = D*W*l*f_m)
    end function

end module

module classRiverReach
    use netcdf
    use mo_netcdf
    use Globals
    use ResultModule
    use ErrorInstanceModule
    implicit none
    private

    !> RiverReach object is responsible for sediment transport along river and
    !! sediment deposition to bed sediment.
    !!
    !! TODO: Sediment size classes hard coded for the moment. Change this so
    !! it can be altered (e.g. through data file, or this%App that everything extends).
    type, public :: RiverReach
        real(dp) :: W               !> Width [m]
        real(dp) :: S               !> Slope [m/m]
        real(dp) :: Q               !> Flow rate [m3/s]
        real(dp), allocatable :: d_s(:)          !> Sediment particle diameter array, for different size classes [m].
        real(dp) :: rho_s(5)        !> Sediment particle density, for different size classes [kg/m3].
        real(dp) :: k_settle(5)     !> Settling rates, for different size classes [s-1]
        real(dp) :: D               !> Depth [m]
        real(dp) :: v               !> River velocity [m/s]
        real(dp) :: W_s(5)          !> Sediment particle settling velocity array, for different size classes [m/s]
        real(dp) :: n = 0.035       !> Manning's roughness coefficient, for natural streams and major rivers.
                                    !! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).

      contains
        procedure, public :: create => createRiverReach
        procedure :: calculateDepth
        procedure :: calculateVelocity
        procedure :: calculateSettlingVelocity
    end type

  contains

    !> Create a river reach by reading data in from file and calculating
    !! properties such as depth and velocity.
    function createRiverReach(me) result(r)
        class(RiverReach) :: me                 !> The RiverReach instance.
        type(Result0D) :: D                     !> Depth [m].
        type(Result0D) :: v                     !> River velocity [m/s].
        integer :: i                            !> Loop iterator.
        type(Result) :: r                       !> The Result object.
        type(NcDataset) :: NC                   !> NetCDF dataset
        type(NcVariable) :: var                 !> NetCDF variable
        type(NcGroup) :: grp                    !> NetCDF group
        real(dp), allocatable :: sedimentSizeClasses(:)         !> Array of sediment particle sizes
        real(dp), allocatable :: nanoparticleSizeClasses(:)     !> Array of nanoparticle particle sizes

        ! Get the sediment and nanoparticle size classes from data file
        nc = NcDataset("data.nc", "r")                          ! Open dataset as read-only
        grp = nc%getGroup("root")                             ! Get the global variables group
        grp = grp%getGroup("global")
        var = grp%getVariable("sediment_size_classes")          ! Get the sediment size classes variable
        call var%getData(sedimentSizeClasses)                   ! Get the variable's data
        var = grp%getVariable("nanoparticle_size_classes")      ! Get the sediment size classes variable
        call var%getData(nanoparticleSizeClasses)               ! Get the variable's data

        ! Here we should read data in from a file and set W, S, Q, d_s and rho_s accordingly.
        ! But, for the moment...
        me%W = 50.0_dp                                                          ! Width
        me%S = 0.0005_dp                                                        ! Slope
        me%Q = 300.0_dp                                                         ! Flow
        allocate(me%d_s, source=sedimentSizeClasses)                            ! Sediment particle diameter [m]
        me%rho_s = [2120, 2120, 2120, 2120, 2120]                               ! Sediment particle density [kg/m^3]

        ! Calculate the depth and velocities and set the instance's variables
        ! TODO: Should we check for errors (e.g., negative river width) here,
        ! or in calculateDepth and calculateVelocity?
        D = me%calculateDepth(me%W, me%S, me%Q)
        v = me%calculateVelocity(me%D, me%Q, me%W)
        me%D = .dp. D
        me%v = .dp. v

        ! Calculate sediment particle settling velocity for each size class,
        ! then set the settling rate (W_s/D) accordingly
        ! TODO: This is arbitrary at the moment, change so size classes
        ! aren't hard coded. Perhaps they're got from data file?
        do i=1, 5
            ! Currently no errors checking in this procedure, but if that changes
            ! we'll need to check for them here
            me%W_s(i) = .dp. me%calculateSettlingVelocity(me%d_s(i), me%rho_s(i))
            me%k_settle(i) = me%W_s(i)/me%D
        end do

        ! Return any errors there might have been, add this procedure to trace
        r = Result(errors=[.errors.D,.errors.v])
        call r%addToTrace("Creating River Reach")
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
    pure function calculateDepth(me, W, S, Q) result(r)
        class(RiverReach), intent(in) :: me     !> The RiverReach instance.
        real(dp), intent(in) :: W               !> River width \( W \) [m].
        real(dp), intent(in) :: S               !> River slope \( S \) [-].
        real(dp), intent(in) :: Q               !> Flow rate \( Q \) [m**3/s].
        real(dp) :: D_i                         !> The iterative river depth \( D_i \) [m].
        real(dp) :: f                           !> The function to find roots for \( f(D) \).
        real(dp) :: df                          !> The derivative of \( f(D) \) with respect to \( D \).
        real(dp) :: alpha                       !> Constant extracted from f and df
        integer :: i                            !> Loop iterator to make sure loop isn't endless.
        integer :: iMax                         !> Maximum number of iterations before error.
        type(ErrorInstance) :: error            !> Variable to store error in.
        character(len=100) :: iChar             !> Loop iterator as character (for error message).
        character(len=100) :: fChar             !> f(D) value as character (for error message).
        type(Result0D) :: r                     !> The result object.

        ! TODO: Allow user (e.g., data file) to specify max iterations? And D0?
        D_i = 1.0_dp                                                            ! Take a guess at D being 1m to begin
        i = 1                                                                   ! Iterator for Newton solver
        iMax = 10000                                                            ! Allow 10000 iterations before solving
        alpha = W**(5.0_dp/3.0_dp) * sqrt(S)/me%n                               ! Extract constant to simplify f and df.
        f = alpha*D_i*((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                    ! First value for f, based guessed D_i

        ! Loop through and solve until f(D) is within e-9 of zero.
        do while (abs(f) > 1.0e-9_dp .and. i <= iMax)
            f = alpha * D_i * ((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                ! f(D) based on D_{m-1}
            df = alpha * ((D_i)**(5.0_dp/3.0_dp) * (6*D_i + 5*W))/(3*D_i * (2*D_i + W)**(5.0_dp/3.0_dp))
            D_i = D_i - f/df                                                    ! Calculate D_i based on D_{m-1}
            i = i+1
        end do

        if (isnan(D_i)) then                                                    ! If method diverges (results in NaN)
            write(iChar,*) i
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method diverged to NaN after " // trim(adjustl(iChar)) // " iterations.", &
                trace = ["Calculating water depth"] &
            )
        else if (i > iMax) then                                                 ! If max number of iterations reached
            write(iChar,*) iMax
            write(fChar,*) f
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method failed to converge - maximum number of iterations (" &
                    // trim(adjustl(iChar)) // ") exceeded." &
                    // "Precision (proximity to zero) required: 1e-9. Final value: " // trim(fChar) // ".", &
                trace = ["Calculating water depth"] &
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
    pure function calculateVelocity(me, D, Q, W) result(r)
        class(RiverReach), intent(in) :: me     !> The RiverReach instance.
        real(dp), intent(in) :: D               !> River depth \( D \) [m].
        real(dp), intent(in) :: Q               !> Flow rate \( Q \) [m**3/s].
        real(dp), intent(in) :: W               !> River width \( W \) [m].
        type(Result0D) :: r                     !> The result object.
        r = Result( &                           ! Return the velocity. No error can occur here.
            data = Q/(W*D) &
        )
    end function

    !> Calculate the settling velocity of sediment particles for an individual
    !! size class:
    !! $$
    !!      W_s = \frac{\nu}{d} d_{*}^3 (38.1 + 0.93 d_{*}^{12/7})^{-7/8}
    !! $$
    !! where
    !! $$
    !!      d_{*} = \left( \frac{\Delta g}{\nu^2} \right)^{1/3} d
    !! $$
    !! and
    !! $$
    !!      \Delta = \frac{\rho_s}{\rho} - 1
    !! $$
    !! Reference: [Zhiyao et al, 2008](https://doi.org/10.1016/S1674-2370(15)30017-X)
    !! TODO: Make rho and nu dependent on temp
    pure function calculateSettlingVelocity(me, d, rho_s) result(r)
        class(RiverReach), intent(in) :: me         !> The RiverReach instance.
        real(dp), intent(in) :: d                   !> Sediment particle diameter.
        real(dp), intent(in) :: rho_s               !> Sediment particle density.
        real(dp) :: dStar                           !> Dimensionless particle diameter.
        real(dp) :: W_s                             !> Calculated settling velocity.
        type(Result0D) :: r                         !> The Result object.
        dStar = ((rho_s/C%rho - 1)*C%g/C%nu**2)**(1.0_dp/3.0_dp) * d        ! Calculate the dimensional particle diameter
        W_s = (C%nu/d) * dStar**3 * (38.1_dp + 0.93_dp &                    ! Calculate the settling velocity
            * dStar**(12.0_dp/7.0_dp))**(-7.0_dp/8.0_dp)
        r = Result(data = W_s)
    end function

end module
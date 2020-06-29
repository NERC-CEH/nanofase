!> Module with a handful of useful globally-available procedures
module UtilModule
    use Globals
    use netcdf
    implicit none

    !> Return a string from an integer or real number
    interface str
        module procedure strFromInteger
        module procedure strFromReal
        module procedure strFromReal1D
        module procedure strFromDp
        module procedure strFromLogical
    end interface

    interface ref
        module procedure ref1
        module procedure ref2p1
        module procedure ref2
        module procedure ref3
        module procedure ref4
        module procedure ref5
    end interface

    interface isZero
        module procedure isZeroReal
        module procedure isZeroDp
        module procedure isZeroDp3D
    end interface
    
    interface isLessThanZero
        module procedure isLessThanZeroReal
        module procedure isLessThanZeroDp
    end interface

    interface divideCheckZero
        module procedure divideCheckZeroReal
        module procedure divideCheckZeroDp
        module procedure divideCheckZeroRealNumeratorIntegerDenominator
        module procedure divideCheckZeroDpNumeratorIntegerDenominator
    end interface

    interface weightedAverage
        module procedure weightedAverageDp
        module procedure weightedAverageDp1D
        module procedure weightedAverageDp2D
        module procedure weightedAverageDp3D
    end interface

    contains
        !> print a set of r x c matrices to the console
        subroutine print_matrix(m)
            real(dp), allocatable :: m(:,:,:)                        !! the passed matrix as a 2D array
            real(dp), allocatable :: mm(:)                           ! LOCAL 1D temproary array
            integer :: r                                             ! LOCAL number of rows in array
            integer :: c                                             ! LOCAL number of columns in array
            integer :: s                                             ! LOCAL third array dimension
            integer :: n                                             ! LOCAL loop counter 
            integer :: p                                             ! LOCAL loop counter 
            r = size(m, 1)                                           ! number of rows
            c = size(m, 2)                                           ! number of columns
            s = size(m, 3)                                           ! third dimension
            allocate(mm(c))             
            do p = 1, s
                do n = 1, r
                    mm = m(n, 1:c, p)
                    print '(25f20.15)', mm
                end do
                print *, ""
            end do
        end subroutine

        !> Convert a signed integer to a logical value
        elemental function lgcl(i)
            integer, intent(in) :: i
            logical :: lgcl
            if (i .le. 0) then
                lgcl = .false.
            else
                lgcl = .true.
            end if
        end function

        !> Convert an unsigned integer (uint1, ubyte) to a logical value
        elemental function ulgcl(i)
            integer, intent(in) :: i
            logical :: ulgcl
            if (i .le. 0 .or. i .ge. nf90_fill_uint1) then
                ulgcl = .false.
            else
                ulgcl = .true.
            end if
        end function
    
        !> Convert an integer to a string
        function strFromInteger(i) result(str)
            integer, intent(in) :: i        !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)i
            str = trim(adjustl(str))
        end function

        !> Convert a real to a string
        function strFromReal(r) result(str)
            real, intent(in) :: r           !! The real to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function
        
        !> Convert a real 1D array to a string
        function strFromReal1D(r) result(str)
            real, intent(in) :: r(:)        !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            ! write(str, *) (trim(str(r(i))) // ", ", i=1, size(r - 1))
            ! str = trim(str) // trim(str(r(size(r))))
        end function

        !> Convert a double-precision real to a string
        function strFromDp(r) result(str)
            real(dp), intent(in) :: r           !! The dp real to convert to a string
            character(len=256) :: str           !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        function strFromLogical(l) result(str)
            logical, intent(in) :: l
            character(len=5) :: str
            if (l) then
                str = "true"
            else
                str = "false"
            end if
        end function

        !> Generate an object reference from a prefix (e.g., "GridCell")
        !! and one integers
        function ref1(prefix, a)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            character(len=256) :: ref1
            ref1 = trim(prefix) // "_" // trim(str(a))
        end function

        !> Generate an object reference from two character prefixes and
        !! one integer
        function ref2p1(prefix1, prefix2, a)
            character(len=*), intent(in) :: prefix1
            character(len=*), intent(in) :: prefix2
            integer, intent(in) :: a
            character(len=256) :: ref2p1
            ref2p1 = trim(prefix1) // "_" // trim(prefix2) // "_" // trim(str(a))
        end function

        !> Generate an object reference from a prefix (e.g., "GridCell")
        !! and two integers
        function ref2(prefix, a, b)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            integer, intent(in) :: b
            character(len=256) :: ref2
            ref2 = trim(prefix) // "_" // trim(str(a)) // "_" // trim(str(b))
        end function

        !> Generate an object reference from a prefix (e.g., "RiverReach")
        !! and three integers
        function ref3(prefix, a, b, c)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: c
            character(len=256) :: ref3
            ref3 = trim(prefix) // "_" // trim(str(a)) // "_" // trim(str(b)) // &
                    "_" // trim(str(c))
        end function

        !> Generate an object reference from a prefix (e.g., "BedSediment")
        !! and four integers
        function ref4(prefix, a, b, c, d)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: c
            integer, intent(in) :: d
            character(len=256) :: ref4
            ref4 = trim(prefix) // "_" // trim(str(a)) // "_" // trim(str(b)) // &
                    "_" // trim(str(c)) // "_" // trim(str(d))
        end function

        !> Generate an object reference from a prefix and five integers
        function ref5(prefix, a, b, c, d, e)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            integer, intent(in) :: b
            integer, intent(in) :: c
            integer, intent(in) :: d
            integer, intent(in) :: e
            character(len=256) :: ref5
            ref5 = trim(prefix) // "_" // trim(str(a)) // "_" // trim(str(b)) // &
                    "_" // trim(str(c)) // "_" // trim(str(d)) // "_" // trim(str(e))
        end function

        !> Check whether a real value is within epsilon of zero
        pure function isZeroReal(value, epsilon)
            real, intent(in)                :: value        !! Value to check
            real(dp), intent(in), optional  :: epsilon      !! Proximity to zero permitted
            real(dp)                        :: e            !! Internal epsilon
            logical :: isZeroReal
            isZeroReal = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (abs(value) < epsilon) isZeroReal = .true.
        end function

        !> Check whether a real(dp) value is within epsilon of zero
        pure function isZeroDp(value, epsilon)
            real(dp), intent(in)            :: value        !! Value to check
            real(dp), intent(in), optional  :: epsilon      !! Proximity to zero permitted
            real(dp)                        :: e            !! Internal epsilon
            logical                         :: isZeroDp     !! The logical to return
            isZeroDp = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (abs(value) < e) isZeroDp = .true.
        end function

        pure function isZeroDp3D(value, epsilon)
            real(dp), intent(in)            :: value(:,:,:)     !! Value to check
            real(dp), intent(in), optional  :: epsilon          !! Proximity to zero permitted
            real(dp)                        :: e                !! Internal epsilon
            integer                         :: i, j, k          !! Iterators
            logical                         :: isZeroDp3D       !! The logical to return
            isZeroDp3D = .true.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            outer: do k = 1, size(value, dim=3)
                do j = 1, size(value, dim=2)
                    do i = 1, size(value, dim=1)
                        if (abs(value(i,j,k)) > e) then
                            isZeroDp3D = .false.
                            exit outer
                        end if
                    end do
                end do
            end do outer
        end function
        
        function isLessThanZeroReal(value, epsilon)
            real, intent(in) :: value
            real(dp), intent(in), optional :: epsilon
            real(dp) :: e
            logical :: isLessThanZeroReal
            isLessThanZeroReal = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (value <= -e) isLessThanZeroReal = .true.
        end function
        
        function isLessThanZeroDp(value, epsilon)
            real(dp), intent(in) :: value
            real(dp), intent(in), optional :: epsilon
            real(dp) :: e
            logical :: isLessThanZeroDp
            isLessThanZeroDp = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (value <= -e) isLessThanZeroDp = .true.
        end function

        !> Divide a number by another, check if the numerator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroReal(numerator, denominator)
            real, intent(in) :: numerator
            real, intent(in) :: denominator
            real :: divideCheckZeroReal
            if (isZero(numerator)) then
                divideCheckZeroReal = 0.0
            else
                divideCheckZeroReal = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroDp(numerator, denominator)
            real(dp), intent(in) :: numerator
            real(dp), intent(in) :: denominator
            real(dp) :: divideCheckZeroDp
            if (isZero(numerator)) then
                divideCheckZeroDp = 0.0_dp
            else
                divideCheckZeroDp = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroRealNumeratorIntegerDenominator(numerator, denominator)
            real, intent(in) :: numerator
            integer, intent(in) :: denominator
            real :: divideCheckZeroRealNumeratorIntegerDenominator
            if (isZero(numerator)) then
                divideCheckZeroRealNumeratorIntegerDenominator = 0.0_dp
            else
                divideCheckZeroRealNumeratorIntegerDenominator = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroDpNumeratorIntegerDenominator(numerator, denominator)
            real(dp), intent(in) :: numerator
            integer, intent(in) :: denominator
            real(dp) :: divideCheckZeroDpNumeratorIntegerDenominator
            if (isZero(numerator)) then
                divideCheckZeroDpNumeratorIntegerDenominator = 0.0_dp
            else
                divideCheckZeroDpNumeratorIntegerDenominator = numerator / denominator
            end if
        end function

        function weightedAverageDp(x, w) result(x_w)
            real(dp), intent(in) :: x(:)
            real(dp), intent(in) :: w(:)
            real(dp) :: x_w
            x_w = divideCheckZero(sum(x * w), sum(w))
        end function

        !> Calculate the weighted average of an array of 1D variables (i.e. a 2D array)
        !! using the provided weights
        function weightedAverageDp1D(x, w) result(x_w)
            real(dp), intent(in) :: x(:,:)
            real(dp), intent(in) :: w(:)
            real(dp), allocatable :: x_w(:)
            integer :: i
            allocate(x_w(size(x, dim=2)))
            do i = 1, size(x, dim=2)
                x_w(i) = weightedAverage(x(:,i), w)
            end do
        end function

        !> Calculate the weighted average of an array of 2D variables (i.e. a 3D array)
        !! using the provided weights
        function weightedAverageDp2D(x, w) result(x_w)
            real(dp), intent(in) :: x(:,:,:)
            real(dp), intent(in) :: w(:)
            real(dp), allocatable :: x_w(:,:)
            integer :: i, j
            allocate(x_w(size(x, dim=2), size(x, dim=3)))
            do j = 1, size(x, dim=3)    
                do i = 1, size(x, dim=2)
                    x_w(i,j) = weightedAverage(x(:,i,j), w)
                end do
            end do
        end function

        !> Calculate the weighted average of an array of 3D variables (i.e. a 4D array)
        !! using the provided weights
        function weightedAverageDp3D(x, w) result(x_w)
            real(dp), intent(in) :: x(:,:,:,:)
            real(dp), intent(in) :: w(:)
            real(dp), allocatable :: x_w(:,:,:)
            integer :: i, j, k
            allocate(x_w(size(x, dim=2), size(x, dim=3), size(x, dim=4)))
            do k = 1, size(x, dim=4)
                do j = 1, size(x, dim=3)
                    do i = 1, size(x, dim=2)
                        x_w(i,j,k) = weightedAverage(x(:,i,j,k), w)
                    end do
                end do
            end do
        end function

        function freeNM(x) result(free)
            real(dp), intent(in)    :: x(C%npDim(1), C%npDim(2), C%npDim(3))
            real(dp)                :: free(C%nSizeClassesNM)
            free = x(:,1,1)
        end function

        function attachedNM(x) result(attached)
            real(dp), intent(in)    :: x(C%npDim(1), C%npDim(2), C%npDim(3))
            real(dp)                :: attached(C%nSizeClassesNM)
            attached = x(:,1,2)
        end function

        function heteroaggregatedNM(x) result(heteroaggregated)
            real(dp), intent(in)    :: x(C%npDim(1), C%npDim(2), C%npDim(3))
            real(dp)                :: heteroaggregated(C%nSizeClassesNM)
            heteroaggregated = sum(x(:,1,3:), dim=1)
        end function

        subroutine progress(j)
            integer :: j,k
            character(len=118) :: bar="\r???% |                                          "//&
                "                                                          |"
             
            ! Updates the fraction of calculation done
            write(unit=bar(2:4), fmt="(i3)") j
            do k = 1, j
                bar(7+k:7+k)="*"
            end do
             
            ! Print the progress bar.
            write(*,'(a)', advance='no') bar
             
        end subroutine

end module
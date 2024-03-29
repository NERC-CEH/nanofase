!> Module with a handful of useful globally-available procedures
module UtilModule
    use GlobalsModule
    use netcdf
    implicit none

    interface printMatrix
        module procedure printMatrix2D
        module procedure printMatrix3D
    end interface

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

    interface flushToZero
        module procedure flushToZeroReal
        module procedure flushToZeroDp
    end interface

    interface weightedAverage
        module procedure weightedAverageDp
        module procedure weightedAverageDp1D
        module procedure weightedAverageDp2D
        module procedure weightedAverageDp3D
    end interface

    contains

        !> Print a welcome to the model message to the console.
        subroutine printWelcome()
            write(*,'(A)') "_____________________________________________________________________"
            write(*,'(A)') "                _   _                   _____ _    ____  _____ "
            write(*,'(A)') "               | \ | | __ _ _ __   ___ |  ___/ \  / ___|| ____|"
            write(*,'(A)') "               |  \| |/ _` | '_ \ / _ \| |_ / _ \ \___ \|  _|  "
            write(*,'(A)') "               | |\  | (_| | | | | (_) |  _/ ___ \ ___) | |___ "
            write(*,'(A)') "Welcome to the |_| \_|\__,_|_| |_|\___/|_|/_/   \_\____/|_____| model"
            write(*,'(A)') "...version: " // C%modelVersion
            write(*,'(A)') "_____________________________________________________________________"
            write(*,'(A)') ""
        end subroutine

        !> Print a 3D array as a set of 2D matrices to the console
        subroutine printMatrix3D(m)
            real(dp), allocatable :: m(:,:,:)                   !! The 3D array to print
            real(dp), allocatable :: mm(:)                      ! 1D temproary array
            integer :: i, j                                     ! Iterators 
            allocate(mm(size(m, 2)))             
            do j = 1, size(m, 3)
                do i = 1, size(m, 1)
                    mm = m(i, 1:size(m, 2), j)
                    print '(25f20.15)', mm
                end do
                print *, ""
            end do
        end subroutine

        !> Print a 2D array as a matrix
        subroutine printMatrix2D(m)
            real(dp), allocatable :: m(:,:)                     !! The 2D array to print
            real(dp), allocatable :: mm(:)                      ! 1D temproary array
            integer :: i                                        ! Iterator
            allocate(mm(size(m, 2)))             
            do i = 1, size(m, 1)
                mm = m(i, 1:size(m, 2))
                print '(25f20.15)', mm
            end do
        end subroutine

        !> Convert a singular to a plural, if i is greater than 1
        pure function pluralize(singular, i)
            character(len=*), intent(in) :: singular
            integer, intent(in)          :: i
            character(len=256)          :: pluralize
            if (i == 1) then
                pluralize = singular
            else
                pluralize = trim(singular) // "s"
            end if
        end function

        !> Convert a signed integer to a logical value
        pure elemental function lgcl(i)
            integer, intent(in) :: i
            logical :: lgcl
            if (i .le. 0) then
                lgcl = .false.
            else
                lgcl = .true.
            end if
        end function

        !> Convert an unsigned integer (uint1, ubyte) to a logical value
        pure elemental function ulgcl(i)
            integer, intent(in) :: i
            logical :: ulgcl
            if (i .le. 0 .or. i .ge. nf90_fill_uint1) then
                ulgcl = .false.
            else
                ulgcl = .true.
            end if
        end function
    
        !> Convert an integer to a string
        pure function strFromInteger(i) result(str)
            integer, intent(in) :: i        !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)i
            str = trim(adjustl(str))
        end function

        !> Convert a real to a string
        pure function strFromReal(r) result(str)
            real, intent(in) :: r           !! The real to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function
        
        !> Convert a real 1D array to a string
        pure function strFromReal1D(r) result(string)
            real, intent(in) :: r(:)        !! The integer to convert to a string
            character(len=256) :: string       !! The string to return
            integer         :: i
            write(string, *) (trim(str(r(i))) // ", ", i=1, size(r) - 1)
            string = trim(string) // " " // trim(str(r(size(r))))
        end function

        !> Convert a double-precision real to a string
        pure function strFromDp(r) result(str)
            real(dp), intent(in) :: r           !! The dp real to convert to a string
            character(len=256) :: str           !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        pure function strFromLogical(l) result(str)
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
            if (abs(value) < e) isZeroReal = .true.
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

        !> Divide a number by another, check if the numerator or denominator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroReal(numerator, denominator)
            real, intent(in) :: numerator
            real, intent(in) :: denominator
            real :: divideCheckZeroReal
            if (isZero(numerator) .or. isZero(denominator)) then
                divideCheckZeroReal = 0.0
            else
                divideCheckZeroReal = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator or denominator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroDp(numerator, denominator)
            real(dp), intent(in) :: numerator
            real(dp), intent(in) :: denominator
            real(dp) :: divideCheckZeroDp
            if (isZero(numerator) .or. isZero(denominator)) then
                divideCheckZeroDp = 0.0_dp
            else
                divideCheckZeroDp = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator or denominator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroRealNumeratorIntegerDenominator(numerator, denominator)
            real, intent(in) :: numerator
            integer, intent(in) :: denominator
            real :: divideCheckZeroRealNumeratorIntegerDenominator
            if (isZero(numerator) .or. denominator == 0) then
                divideCheckZeroRealNumeratorIntegerDenominator = 0.0_dp
            else
                divideCheckZeroRealNumeratorIntegerDenominator = numerator / denominator
            end if
        end function

        !> Divide a number by another, check if the numerator or denominator is zero
        !! first, and if so, setting the result to zero
        elemental function divideCheckZeroDpNumeratorIntegerDenominator(numerator, denominator)
            real(dp), intent(in) :: numerator
            integer, intent(in) :: denominator
            real(dp) :: divideCheckZeroDpNumeratorIntegerDenominator
            if (isZero(numerator) .or. denominator == 0) then
                divideCheckZeroDpNumeratorIntegerDenominator = 0.0_dp
            else
                divideCheckZeroDpNumeratorIntegerDenominator = numerator / denominator
            end if
        end function

        pure elemental function flushToZeroReal(x) result(y)
            real, intent(in)    :: x
            real                :: y
            if (abs(x) < C%epsilon) then
                y = 0.0
            else
                y = x
            end if
        end function

        pure elemental function flushToZeroDp(x) result(y)
            real(dp), intent(in)    :: x
            real(dp)                :: y
            if (abs(x) < C%epsilon) then
                y = 0.0_dp
            else
                y = x
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

! Functions without interfaces

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

end module
!> Module with a handful of useful globally-available procedures
module UtilModule
    use Globals
    implicit none

    !> Return a string from an integer or real number
    interface str
        module procedure strFromInteger
        module procedure strFromReal
        module procedure strFromDp
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
    end interface
    
    interface isLessThanZero
        module procedure isLessThanZeroReal
        module procedure isLessThanZeroDp
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
                    print '(20f15.10)', mm
                end do
                print *, ""
            end do
        end subroutine

        !> Convert an integer to a logical value
        elemental function lgcl(i)
            integer, intent(in) :: i
            logical :: lgcl
            if (i .le. 0) then
                lgcl = .false.
            else
                lgcl = .true.
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
            real, intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        !> Convert a double-precision real to a string
        function strFromDp(r) result(str)
            real(dp), intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
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
        function isZeroReal(value, epsilon)
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
        function isZeroDp(value, epsilon)
            real(dp), intent(in)            :: value        !! Value to check
            real(dp), intent(in), optional  :: epsilon      !! Proximity to zero permitted
            real(dp)                        :: e            !! Internal epsilon
            logical :: isZeroDp
            isZeroDp = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (abs(value) < e) isZeroDp = .true.
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


end module
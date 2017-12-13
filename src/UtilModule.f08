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

  contains
        !> Convert an integer to a string
        pure function strFromInteger(i) result(str)
            integer, intent(in) :: i        !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)i
            str = trim(adjustl(str))
        end function

        !> Convert a real to a string
        pure function strFromReal(r) result(str)
            real, intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        !> Convert a double-precision real to a string
        pure function strFromDp(r) result(str)
            real(dp), intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        !> Generate an object reference from a prefix (e.g., "GridCell")
        !! and one integers
        pure function ref1(prefix, a)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            character(len=256) :: ref1
            ref1 = trim(prefix) // "_" // trim(str(a))
        end function

        !> Generate an object reference from two character prefixes and
        !! one integer
        pure function ref2p1(prefix1, prefix2, a)
            character(len=*), intent(in) :: prefix1
            character(len=*), intent(in) :: prefix2
            integer, intent(in) :: a
            character(len=256) :: ref2p1
            ref2p1 = trim(prefix1) // "_" // trim(prefix2) // "_" // trim(str(a))
        end function

        !> Generate an object reference from a prefix (e.g., "GridCell")
        !! and two integers
        pure function ref2(prefix, a, b)
            character(len=*), intent(in) :: prefix
            integer, intent(in) :: a
            integer, intent(in) :: b
            character(len=256) :: ref2
            ref2 = trim(prefix) // "_" // trim(str(a)) // "_" // trim(str(b))
        end function

        !> Generate an object reference from a prefix (e.g., "RiverReach")
        !! and three integers
        pure function ref3(prefix, a, b, c)
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
        pure function ref4(prefix, a, b, c, d)
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
        pure function ref5(prefix, a, b, c, d, e)
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
            logical :: isZeroDp
            isZeroDp = .false.
            if (.not. present(epsilon)) then
                e = C%epsilon
            else
                e = epsilon
            end if
            if (abs(value) < e) isZeroDp = .true.
        end function


end module
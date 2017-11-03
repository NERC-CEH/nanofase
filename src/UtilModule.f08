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
        module procedure ref2
        module procedure ref3
        module procedure ref4
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
end module
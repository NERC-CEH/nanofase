!>
!! Module with a handful of useful procedures
module UtilModule
    use Globals
    implicit none

    !> Return a string from an integer or real number
    interface str
        module procedure strFromInteger
        module procedure strFromReal
        module procedure strFromDp
    end interface

  contains
        !> Convert an integer to a string
        pure function strFromInteger(i) result(str)
            integer, intent(in) :: i        !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)i
            str = trim(adjustl(str))
        end function

        !> Convert an integer to a string
        pure function strFromReal(r) result(str)
            real, intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function

        !> Convert an integer to a string
        pure function strFromDp(r) result(str)
            real(dp), intent(in) :: r           !! The integer to convert to a string
            character(len=256) :: str       !! The string to return
            write(str, *)r
            str = trim(adjustl(str))
        end function
end module
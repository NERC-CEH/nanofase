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
        module procedure gridCellRef
        module procedure subRiverRef
        module procedure riverReachRef
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

        !> Return GridCell reference from x and y coordinates
        pure function gridCellRef(x, y)
            integer, intent(in) :: x, y
            character(len=256) :: gridCellRef
            gridCellRef = "GridCell_" // trim(str(x)) // "_" // trim(str(y))
        end function

        !> Return SubRiver reference from x, y and s coordinates
        pure function subRiverRef(x, y, s)
            integer, intent(in) :: x, y, s
            character(len=256) :: subRiverRef
            subRiverRef = "SubRiver_" // trim(str(x)) // "_" // trim(str(y)) // "_" // trim(str(s))
        end function

        !> Return RiverReach reference from x, y, s and r coordinates
        pure function riverReachRef(x, y, s, r)
            integer, intent(in) :: x, y, s, r
            character(len=256) :: riverReachRef
            riverReachRef = "RiverReach_" // trim(str(x)) // "_" // trim(str(y)) &
                // "_" // trim(str(s)) // "_" // trim(str(r))
        end function
end module
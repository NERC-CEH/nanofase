module classReactor2                                                 ! class definition for Reactor2
    use spcReactor                                                   ! use spcReactor superclass
    implicit none                                                    ! force declaration of all variables
    type, public, extends(Reactor) :: objReactor2                    ! type declaration for class - extends interface
        contains
            procedure :: new => newReactor2
            procedure :: destroy => destroyReactor2
    end type

    contains
        subroutine newReactor2(Me, lname)                                       ! constructor method
            class(objReactor2) :: Me                                    ! correct?
            type(character) :: lname                                    ! object name
        end subroutine

        subroutine destroyReactor2(Me)                                          ! finaliser method
            class(objReactor2) :: Me                                    ! correct?
        end subroutine
end module

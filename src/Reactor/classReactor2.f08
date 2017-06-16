module classReactor2                                                ! class definition for Reactor2
    use spcReactor                                                  ! use spcReactor superclass
    implicit none                                                   ! force declaration of all variables
    type, public, extends(Reactor) :: objReactor2                   ! type declaration for class - extends interface
        contains
            procedure :: new => newObjReactor2
            procedure :: destroy => destroyObjReactor2
    end type

    contains
        subroutine newObjReactor2(Me, lname)                        ! constructor method
            class(objReactor2) :: Me                                ! correct?
            type(character) :: lname                                ! object name
        end subroutine

        subroutine destroyObjReactor2(Me)                           ! finaliser method
            class(objReactor2) :: Me                                ! correct?
        end subroutine
end module

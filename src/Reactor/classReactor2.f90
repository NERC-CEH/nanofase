module classReactor2                                                ! class definition for Reactor2
    use spcReactor                                                  ! use spcReactor superclass
    implicit none                                                   ! force declaration of all variables
    type, public, extends(Reactor) :: objReactor2                   ! type declaration for class - extends interface
        contains
            procedure :: create => newObjReactor2
            procedure :: destroy => destroyObjReactor2
    end type

    contains
        subroutine newObjReactor2(Me)                               ! constructor method
            class(objReactor2) :: Me                                ! correct?
            Me%name = "Reactor 2"
        end subroutine

        subroutine destroyObjReactor2(Me)                           ! finaliser method
            class(objReactor2) :: Me                                ! correct?
        end subroutine
end module

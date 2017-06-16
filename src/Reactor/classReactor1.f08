module classReactor1                                                ! class definition for Reactor1
    use spcReactor                                                  ! use spcReactor superclass
    implicit none                                                   ! force declaration of all variables
    type, public, extends(Reactor) :: objReactor1                   ! type declaration for class - extends interface
        contains
            procedure :: new => newObjReactor1
            procedure :: destroy => destroyObjReactor1
    end type

    contains
        subroutine newObjReactor1(Me, lname)                        ! constructor method
            class(objReactor1) :: Me                                ! correct?
            type(character) :: lname                                ! object name
        end subroutine
        subroutine destroyObjReactor1(Me)                           ! finaliser method
            class(objReactor1) :: Me                                ! correct?
        end subroutine
end module

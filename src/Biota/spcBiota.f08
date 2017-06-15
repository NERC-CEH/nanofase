module spcBiota                                                      ! superclass definition for Biota
    implicit none                                                    ! force declaration of all variables
    type, abstract, public :: Biota                                  ! type declaration for class
                                                                     ! class properties
        character(len=256) :: name                                   ! a name for the object
        contains                                                       ! METHODS - all declared deferred
            procedure(createInterface), public, deferred :: create       ! constructor method. Interface name (in brackets)
                                                                         ! and procedure name must be given
            procedure(destroyInterface), public, deferred :: destroy     ! finaliser method
                                                                     ! any other private subroutines or functions go here
    end type

    abstract interface                                               ! abstract defintion for the constructor method
        subroutine createInterface(Me)                                        ! constructor method
            import Biota
            class(Biota) :: Me                                       ! implemented as a function returning a Biota type
        end subroutine
    end interface
    abstract interface                                               ! interface definition for the finaliser method
        subroutine destroyInterface(Me)                                       ! finaliser method
            import Biota
            class(Biota) :: Me
        end subroutine
    end interface
end module

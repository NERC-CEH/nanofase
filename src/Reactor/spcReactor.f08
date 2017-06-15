module spcReactor                                            ! class definition for Reactor1
    implicit none                                            ! force declaration of all variables
    type, abstract, public :: Reactor                        ! type declaration for interface
                                                             ! class properties
        character :: name                                    ! a name for the object
      contains                                               ! METHODS - all declared deferred
        procedure(newInterface), public, deferred :: new                   ! constructor method
        procedure(destroyInterface), public, deferred :: destroy               ! finaliser method
                                                             ! any other subroutines or functions go here
    end type
    abstract interface                                       ! interface defintion for the constructor method
        subroutine newInterface(Me, &
                       lname)                                ! constructor method
            import Reactor
            class(Reactor) :: Me                             ! implemented as a function returning a Reactor type                                                             ! Reactor type
            type(character) :: lname                         ! object name
        end subroutine
    end interface
    abstract interface                                       ! interface definition for the finaliser method
        subroutine destroyInterface(Me)                               ! finaliser method
            import Reactor
            class(Reactor) :: Me
        end subroutine
    end interface
end module

module spcReactor                                               ! class definition for Reactor1
    implicit none                                               ! force declaration of all variables
    type, abstract, public :: Reactor                           ! type declaration for interface
                                                                ! class properties
        character(len=256) :: name                              ! a name for the object
      contains                                                  ! METHODS - all declared deferred
        procedure, public :: create => createReactor            ! constructor method
        procedure, public :: destroy => destroyReactor          ! finaliser method
                                                                ! any other subroutines or functions go here
    end type
    
  contains

    subroutine createReactor(me)
        class(Reactor) :: me
        ! Do stuff to create reactor
    end subroutine

    subroutine destroyReactor(me)
        class(Reactor) :: me
        ! Do stuff to destroy reactor
    end subroutine
end module

module spcBiota                                                     !! superclass definition for Biota
    implicit none                                                   ! force declaration of all variables
    type, abstract, public :: Biota                                 !! type declaration for class
                                                                    ! class properties
        character(len=256) :: name                                  ! a name for the object
        contains                                                    ! METHODS - all declared deferred
            procedure, public :: create => createBiota              ! constructor method
            procedure, public :: destroy => destroyBiota            ! finaliser method
                                                                    ! any other private subroutines or functions go here
    end type

  contains

    subroutine createBiota(me)
        class(Biota) :: me
        ! Do some stuff to create biota
    end subroutine

    subroutine destroyBiota(me)
        class(Biota) :: me
        ! Do some stuff to destory biota
    end subroutine
end module

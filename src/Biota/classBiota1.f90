module classBiota1                                                      !! class definition for Biota1
    use spcBiota                                                        ! use spcBiota interface
    implicit none                                                       ! force declaration of all variables
    type, public, extends(Biota) :: objBiota1                           !! type declaration for class - extends interface
        contains
            procedure :: create => createObjBiota1                      ! Different procedure name (createObjBiota1) need for each
            procedure :: destroy => destroyObjBiota1                    ! class if overloading the binding name create.
    end type
    contains
        subroutine createObjBiota1(Me)                                  !! constructor method
            class(objBiota1) :: Me                                      !! This Biota1 instance
            Me%name = "Biota 1"                                         ! Hard code name for testing purposes (see main.f08)
        end subroutine
        subroutine destroyObjBiota1(Me)                                 !! finaliser method
            class(objBiota1) :: Me
        end subroutine
end module

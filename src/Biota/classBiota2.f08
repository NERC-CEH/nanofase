module classBiota2                                                      !! class definition for Biota2
    use spcBiota                                                        ! use spcBiota interface
    implicit none                                                       ! force declaration of all variables

    type, public, extends(Biota) :: objBiota2                           !! type declaration for class - extends interface
        contains
            procedure :: create => createObjBiota2                      ! Bind createObjBiota2 to create to overload create interface
            procedure :: destroy => destroyObjBiota2
    end type

    contains
        subroutine createObjBiota2(Me)                                  !! constructor method
            class(objBiota2) :: Me                                      !! This Biota2 instance
            Me%name = "Biota 2"                                         ! Hard code name for testing purposes (see main.f08)
        end subroutine
        subroutine destroyObjBiota2(Me)                                 !! finaliser method
            class(objBiota2) :: Me                                      !! This Biota2 instance
        end subroutine
end module

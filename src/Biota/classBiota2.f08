module classBiota2                                                   ! class definition for Biota2
    use spcBiota                                                     ! use spcBiota interface
    implicit none                                                    ! force declaration of all variables

    type, public, extends(Biota) :: objBiota2                        ! type declaration for class - extends interface
        character(len=256) :: test
        contains
            procedure :: create => createClassBiota2
            procedure :: destroy => destroyClassBiota2
    end type

    contains
        subroutine createClassBiota2(Me)                                          ! constructor method
            class(objBiota2) :: Me                                     ! *** CORRECT? Or must this be class(Biota)?
            Me%name = "Biota 2"
        end subroutine
        subroutine destroyClassBiota2(Me)                                         ! finaliser method
            class(objBiota2) :: Me                                     ! *** CORRECT? Or must this be class(Biota)?
        end subroutine
end module

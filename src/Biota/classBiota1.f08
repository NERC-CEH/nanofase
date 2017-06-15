module classBiota1                                                   ! class definition for Biota1
    use spcBiota                                                     ! use spcBiota interface
    implicit none                                                    ! force declaration of all variables

    type, public, extends(Biota) :: objBiota1                        ! type declaration for class - extends interface
      contains
        procedure :: create => createClassBiota1
        procedure :: destroy => destroyClassBiota1
    end type
    
  contains
      subroutine createClassBiota1(Me)                                          ! constructor method
          class(objBiota1) :: Me                                     ! *** CORRECT? Or must this be class(Biota)? I think this is OK.
          Me%name = "Biota 1"
      end subroutine
      subroutine destroyClassBiota1(Me)                                         ! finaliser method
          class(objBiota1) :: Me                                     ! *** CORRECT? Or must this be class(Biota)?
      end subroutine
end module

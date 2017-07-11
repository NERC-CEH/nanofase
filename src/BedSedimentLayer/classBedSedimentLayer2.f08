module classBedSedimentLayer2                                        ! class definition for BedSedimentLayer2
    use spcBedSedimentLayer                                          ! use intBedSedimentLayer superclass
    implicit none                                                    ! force declaration of all variables
    type, public, extends(BedSedimentLayer) :: &
        objBedSedimentLayer2                                         ! type declaration for class - extends abstract superclass
    end type
end module

module classBedSediment2                                             ! class definition for BedSediment2
    use spcBedSediment                                               ! use BedSediment superclass
    implicit none                                                    ! force declaration of all variables
    type, public, extends(BedSediment) :: &
        objBedSediment2                                              ! type declaration for class - extends abstract superclass
    end type
end module

module classBedSedimentLayer1                                        ! class definition for BedSedimentLayer1
    use spcBedSedimentLayer                                          ! use intBedSedimentLayer superclass
    implicit none                                                    ! force declaration of all variables

    type, public, extends(BedSedimentLayer) :: &
        objBedSedimentLayer1                                         ! type declaration for class - extends abstract superclass
    end type                                                         
end module

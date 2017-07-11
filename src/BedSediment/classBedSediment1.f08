module classBedSediment1                                             ! class definition for BedSediment1
    use spcBedSediment                                               ! use BedSediment superclass
    implicit none                                                    ! force declaration of all variables
    type, public, extends(BedSediment) :: &
        objBedSediment1                                              ! type declaration for class - extends abstract superclass
    end type
end module

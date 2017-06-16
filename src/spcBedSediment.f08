module spcBedSediment                                               ! abstract superclass definition for BedSediment
                                                                    ! defines the properties and methods shared by all BedSediment objects
                                                                    ! objects of this class cannot be instantiated, only objects of its subclasses
    use spcBedSedimentLayer                                         ! USEs the spcBedSedimentLayer superclass
    implicit none                                                   ! force declaration of all variables
    type, abstract, public :: BedSediment                           ! type declaration for superclass
        ! properties
        class(BedSedimentLayer), allocatable :: &
                    colBedSedimentLayer(:)                          ! collection of BedSedimentLayer objects
        integer, private :: allst                                   ! array allocation status
        integer, private :: err                                     ! success/failure code
                                                                    ! any private variable declarations go here
      contains
        ! methods
        procedure(new), public, deferred :: new                     ! constructor method
        procedure(destroy), public, deferred :: destroy             ! finaliser method
                                                                    ! NON-DEFERRED METHODS: defined here. Can be overwritten in subclasses
        procedure :: nLayers => nl                                  ! property function to return number of BedSedimentLayer objects
        procedure :: Depth => d                                     ! property function to return total depth of BedSediment
        ! any other subroutines or functions go here
    end type
    private :: nl                                                   ! nLayers           method and property private names
    private :: d                                                    ! Depth
    abstract interface                                              ! interface definition for the deferred constructor method
        subroutine new(Me, lnlayers)
            import BedSediment
            implicit none                                           ! force declaration of all variables
            class(BedSediment) :: Me                                ! BedSediment type
            type(integer) :: lnlayers                               ! number of sediment layers
        end subroutine
                                                                    ! interface definition for the deferred finaliser method
        subroutine destroy(Me)                                      ! finaliser method
            import BedSediment
            class(BedSediment) :: Me
        end subroutine
    end interface

    contains
    integer function nl(Me)                                         ! property function, returns number of BedSedimentLayer objects
        implicit none                                               ! force declaration of all variables
        class(BedSediment) :: Me
        nl = size(Me%colBedSedimentLayer)
    end function
    real function d(Me)                                             ! property function, returns total depth of sediment
        implicit none                                               ! force declaration of all variables
        class(BedSediment) :: Me
        type(integer) :: x                                          ! loop counter
        d = 0                                                       ! initialise the function return value
        do x = 1, nl(Me)                                            ! loop through all the layers
            d = d + Me%colBedSedimentLayer(x)%Depth                 ! adding up the depth of each layer
        end do                                                      ! to return the total sediment depth
    end function
end module

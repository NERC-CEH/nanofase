module spcBedSediment                                               ! abstract superclass definition for BedSediment
                                                                    ! defines the properties and methods shared by all BedSediment objects
                                                                    ! objects of this class cannot be instantiated, only objects of its subclasses
    use spcBedSedimentLayer                                         ! USEs the spcBedSedimentLayer superclass and subclasses
    use classBedSedimentLayer1
    use classBedSedimentLayer2
    implicit none                                                   ! force declaration of all variables
    type BedSedimentLayerElement
        class(BedSedimentLayer), allocatable :: item                ! Storing polymorphic class(BedSedimentLayer) in derived type so that a collection of
    end type                                                        ! different extended types of BedSedimentLayer can be stored in an array.
    type, abstract, public :: BedSediment                           ! type declaration for superclass
        character(len=256) :: name                                  ! a name for the object
                                                                    ! define variables for 'has a' objects: BedSedimentLayer
        class(BedSedimentLayerElement), allocatable :: &
                    colBedSedimentLayer(:)                          ! collection of BedSedimentLayer objects
                                                                    ! properties
        integer :: nLayers                                          ! number of BedSedimentLayer objects
        integer, private :: allst                                   ! array allocation status
        integer, private :: err                                     ! success/failure code
                                                                    ! any private variable declarations go here
      contains
                                                                    ! deferred methods: must be defined in all subclasses
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: create                                 ! constructor method
        procedure, public :: destroy                                ! finaliser method
        procedure, public :: Depth                                  ! property function to return total depth of BedSediment
                                                                    ! any other subroutines or functions go here
    end type
  contains
    subroutine create(Me, &
                      lname, &
                      ltBSL)                                        ! constructor method
                                                                    ! dummy variables
        class(BedSediment) :: Me                                    ! reference to this object, using the type of the abstract superclass
        character(len=256) :: lname                                 ! the name of this object
        type(integer) :: ltBSL(:)                                   ! array of integers representing BedSedimentLayer types to create
                                                                    ! internal variables
        type(integer) :: x                                          ! loop counter
        type(objBedSedimentLayer1), allocatable :: BSL1             ! object of type BedSedimentLayer1
        type(objBedSedimentLayer2), allocatable :: BSL2             ! object of type BedSedimentLayer2
        Me%name = lname                                             ! the name of this object
        Me%nLayers = size(ltBSL)                                    ! number of BedSedimentLayer objects to create
        ! The next block of code creates the required number of BedSedimentLayer objects
        ! and stores them in the colBedSedimentLayer collection
        ! the collections are allocatable arrays of user-defined types BedSedimentLayerElement
        ! SH: the best method to do this is to store the class(BedSedimentLayer) in a derived type (BedSedimentLayerElement) and
        ! then have an array of that derived type as the colBedSedimentLayer property. That way,
        ! Fortran won't complain that elements of the array are of different types, which
        ! is why allocating individual array elements of the class(BedSedimentLayer) array won't work.
        ! implemented here and seems to be working okay.
        ! Reference:
        ! https://stackoverflow.com/questions/31106539/polymorphism-in-an-array-of-elements.
        if (lnBSL > 0) then
            allocate(Me%colBedSedimentLayer(lnBSL), stat=Me%allst)  ! Set colBedSedimentLayer to be of size lnBSL
            do x = 1, nLayers
                select case (ltBSL(x))
                    case (1)
                        allocate (BSL1, stat=Me%allst)              ! objBedSedimentLayer1 type - create the object
                        call BSL1%create()                          ! call the object constructor
                        call move_alloc(BSL1, &
                        Me%colBedSedimentLayer(x)%item)             ! move the object to the yth element of the BedSedimentLayer collection
                                                                    ! SH: Technically, Fortran's specification allows assignment to polymorphic
                                                                    ! variable: Me%colBiota(x)%item = Me%b1. However, GFortran doesn't support this yet.
                                                                    ! deallocating bsl1 isn't necessary and actually throws up
                                                                    ! a runtime error because it's already been deallocated
                    case (2)
                        allocate (BSL2, stat=Me%allst)              ! objBedSedimentLayer2 type - create the object
                        call BSL2%create()                          ! call the object constructor
                        call move_alloc(BSL2, &
                        Me%colBedSedimentLayer(x)%item)             ! move the object to the yth element of colBiota
                    case default
                                                                    ! error - ltBSL(y) points to an invalid number. Need to abort and report.
                end select
            end do
        else if (lnBSL == 0) then
                                                                    ! code here for actions if no BedSedimentLayer objects required
        else
                                                                    ! code here for invalid (negative) value of lnBSL
        end if
    end subroutine
    subroutine destroy(Me)                                          ! finaliser method
        class(BedSediment)  :: Me                                   ! reference to this object, using the type of the abstract superclass
        integer :: x                                                ! loop counter
        do x = 1, Me%nLayers
            call Me%colBedSedimentLayer(x)%item%destroy()           ! do any cleanup required in BedSedimentLayer objects
        end do
    end subroutine
    integer function nLayers(Me)                                    ! property function, returns number of BedSedimentLayer objects
        class(BedSediment) :: Me
        nLayers = size(Me%colBedSedimentLayer)
    end function
    real function Depth(Me)                                         ! property function, returns total depth of sediment
        class(BedSediment) :: Me
        type(integer) :: x                                          ! loop counter
        Depth = 0                                                   ! initialise the function return value
        do x = 1, Me%nLayers()                                      ! loop through all the layers
            Depth = Depth + Me%colBedSedimentLayer(x)%item%Depth    ! adding up the depth of each layer
        end do                                                      ! to return the total sediment depth
    end function
end module

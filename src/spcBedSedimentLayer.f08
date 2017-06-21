module spcBedSedimentLayer                                          ! abstract superclass definition for BedSedimentLayer
                                                                    ! defines the properties and methods shared by all BedSedimentLayer objects
                                                                    ! objects of this class cannot be instantiated, only objects of its subclasses
    use spcBiota                                                    ! USEs the spcBiota superclass and subclasses
    use classBiota1
    use classBiota2
    use spcReactor                                                  ! USEs the spcReactor superclass and subclasses
    use classReactor1
    use classReactor2
                                                                    ! *** does it also have to USE all the subclasses of each superclass?
                                                                    ! ^ if it uses them in this module, then yes. E.g., type(objBiota1) wouldn't
                                                                    ! work without use classBiota1
    implicit none                                                   ! force declaration of all variables
    type BiotaElement                                               ! Storing polymorphic class(Biota) in derived type so that a collection of
        class(Biota), allocatable :: item                           ! different extended types of Biota can be stored in an array. Simply storing
    end type                                                        ! class(Biota) in an array means you can't allocate each separate element to
                                                                    ! a different extended type (e.g., Biota1, Biota2), as the array must be of
                                                                    ! all the same type.
    type ReactorElement                                             ! Same as for class(Biota).
        class(Reactor), allocatable :: item
    end type
    type, abstract, public :: BedSedimentLayer                      ! type declaration for superclass
        character(len=256) :: name                                  ! a name for the object
                                                                    ! define variables for 'has a' objects: Biota and Reactor
        type(BiotaElement), allocatable :: colBiota(:)              ! collection of objects of type BiotaElement, each containing a class(Biota) item
        type(ReactorElement), allocatable :: colReactor(:)          ! collection of objects of type ReactorElement, each containing a class(Reactor) item
                                                                    ! properties
        real :: Depth                                               ! depth of layer (m)
        real :: Pdens                                               ! particle density of solid phase (g/cm3)
        real :: Porosity                                            ! porosity of sediment layer (m3 water/m3 total)
        integer :: nBiota                                           ! number of Biota objects
        integer :: nReactor                                         ! number of Reactor objects
        integer, private :: allst                                   ! array allocation status
        integer, private :: err                                     ! success/failure code
      contains
                                                                    ! deferred methods: must be defined in all subclasses
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: create => createBSL                    ! constructor method
        procedure, public :: destroy => destroyBSL                  ! finaliser method
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        procedure, public :: SetDepth                               ! procedure to set the depth at initialisation
        procedure, public :: SetPdens                               ! procedure to set the particle density at initialisation
        procedure, public :: SetPorosity                            ! procedure to set the porosity at initialisation
                                                                    ! any other subroutines or functions go here
    end type
  contains
    subroutine createBSL(Me, &
                         lname, &
                         ldepth, &
                         lpdens, &
                         lporosity, &
                         ltbiota, &
                         ltreactor)                                 ! constructor method
                                                                    ! dummy variables
        class(BedSedimentLayer) :: Me                               ! reference to this object, using the type of the abstract superclass
        character(len=*) :: lname                                   ! the name of this object
        type(real) :: ldepth                                        ! layer depth
        type(real) :: lpdens                                        ! layer particle density
        type(real) :: lporosity                                     ! layer porosity
        type(integer) :: ltbiota(:)                                 ! array of integers representing Biota types to create.
        type(integer) :: ltreactor(:)                               ! array of integers representing Reactor types to create.
                                                                    ! internal variables
        type(integer) :: x                                          ! loop counter
        type(objBiota1), allocatable :: b1                          ! object of type Biota1
        type(objBiota2), allocatable :: b2                          ! object of type Biota2
        type(objReactor1), allocatable :: r1                        ! object of type Reactor1
        type(objReactor2), allocatable :: r2                        ! object of type Reactor2
        Me%name = lname                                             ! the name of this object
        Me%nBiota = size(ltbiota)                                   ! number of Biota objects to create
        Me%nReactor = size(ltreactor)                               ! number of Reactor objects to create
        Me%err = Me%SetDepth(ldepth)                                ! set depth, return success/failure code
        Me%err = Me%SetPdens(lpdens)                                ! set particle density, return success/failure code
        Me%err = Me%SetPorosity(lporosity)                          ! set porosity, return success/failure code
        ! The next block of code creates the required number of Biota and Reactor objects
        ! and stores them in the colBiota and colReactor collections
        ! the collections are allocatable arrays of user-defined types BiotaElement and ReactorElement respectively
        ! SH: the best method to do this is to store the class(Biota) in a derived type (BiotaElement) and
        ! then have an array of that derived type as the colBiota property. That way,
        ! Fortran won't complain that elements of the array are of different types, which
        ! is why allocating individual array elements of the class(Biota) array won't work.
        ! implemented here and seems to be working okay.
        ! Reference:
        ! https://stackoverflow.com/questions/31106539/polymorphism-in-an-array-of-elements.
        if (Me%nBiota > 0) then
            allocate(Me%colBiota(Me%nBiota), stat=Me%allst)         ! Set colBiota to be of size nBiota
            do x = 1, Me%nBiota
                select case (ltbiota(x))
                    case (1)
                        allocate (b1, stat=Me%allst)                ! objBiota1 type - create the object
                        call b1%create()                            ! call the object constructor
                        call move_alloc(b1, Me%colBiota(x)%item)    ! move the object to the yth element of the Biota collection
                                                                    ! SH: Technically, Fortran's specification allows assignment to polymorphic
                                                                    ! variable: Me%colBiota(x)%item = Me%b1. However, GFortran doesn't support this yet.
                                                                    ! deallocate isn't necessary and actually throws up
                                                                    ! a runtime error because it's already been deallocated
                    case (2)
                        allocate (b2, stat=Me%allst)                ! objBiota2 type - create the object
                        call b2%create()                            ! call the object constructor
                        call move_alloc(b2, Me%colBiota(x)%item)    ! move the object to the yth element of colBiota
                    case default
                        ! error - ltbiota(y) points to an invalid number. Need to abort and report.
                end select
            end do
        else if (Me%nBiota == 0) then
                                                                    ! code here for actions if no Biota objects required
        else
                                                                    ! code here for invalid (negative) value of nBiota
        end if
        if (Me%nReactor > 0) then
            allocate(Me%colReactor(Me%nReactor), stat=Me%allst)     ! allocate colReactor array
            do x = 1, Me%nReactor
              select case (ltreactor(x))
                  case (1)
                      allocate (r1, stat=Me%allst)                  ! objReactor1 type - create the object
                      call r1%create()                              ! call the object constructor
                      call move_alloc(r1, Me%colReactor(x)%item)    ! move the object to the yth element of the Reactor collection
                  case (2)
                      allocate (r2, stat=Me%allst)                  ! objReactor2 type - create the object
                      call r2%create()                              ! call the object constructor
                      call move_alloc(r2, Me%colReactor(x)%item)    ! move the object to the yth element of the Reactor collection
                  case default
                      ! error - ltbiota(y) points to an invalid number. Need to abort and report.
              end select
          end do
      elseif (Me%nReactor == 0) then
                                                                    ! code here for actions if no Reactor objects required
      else
                                                                    ! code here for invalid (negative) value of nReactor
      end if
    end subroutine
    subroutine destroyBSL(Me)                                       ! finaliser method
        class(BedSedimentLayer) :: Me                               ! reference to this object, using the type of the abstract superclass
        integer :: x                                                ! loop iterator
        do x = 1, Me%nBiota
            call Me%colBiota(x)%item%destroy()                      ! do any cleanup required in Biota objects
        end do
        deallocate(Me%colBiota)                                     ! destroy objects in colBiota
        do x = 1, Me%nReactor
            call Me%colReactor(x)%item%destroy()                    ! do any cleanup required in Reactor objects
        end do
        deallocate(Me%colReactor)                                   ! destroy objects in colReactor
    end subroutine
    integer function SetDepth(Me, ld)                               ! Set depth property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: ld                                                  ! depth to which layer to be set
        if (ld < 0) then
                                                                    ! invalid value of depth - set return error code
        else
            Me%Depth = ld                                           ! Assign depth property
            SetDepth = 0                                            ! return success code
        end if
    end function
    integer function SetPdens(Me, lpd)                              ! set particle density property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: lpd                                                 ! particle density for layer
        if (lpd < 0) then
                                                                    ! invalid value of particle density - set return error code
        else
            Me%Pdens = lpd                                          ! Assign particle density property
            SetPdens = 0                                            ! return success code
        end if
    end function
    integer function SetPorosity(Me, lp)                            ! set porosity property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: lp                                                  ! porosity for layer
        if (lp <= 0 .or. lp > 1) then
                                                                    ! invalid value of porosity - set return error code
        else
            Me%Porosity = lp                                        ! Assign porosity property
            SetPorosity = 0                                         ! return success code
        end if
    end function
end module

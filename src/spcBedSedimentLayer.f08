module spcBedSedimentLayer                                           ! abstract superclass definition for BedSedimentLayer
                                                                     ! defines the properties and methods shared by all BedSedimentLayer objects
                                                                     ! objects of this class cannot be instantiated, only objects of its subclasses
    use spcBiota                                                     ! USEs the spcBiota superclass and subclasses
    use classBiota1
    use classBiota2
    use spcReactor                                                   ! USEs the spcReactor superclass and subclasses
    use classReactor1
    use classReactor2
                                                                     ! *** does it also have to USE all the subclasses of each superclass?
    implicit none                                                    ! force declaration of all variables

    ! Storing polymorphic class(Biota) in derived type so that a collection of
    ! different extended types of Biota can be stored in an array. Simply storing
    ! class(Biota) in an array means you can't allocate each separate element to 
    ! a different extended type (e.g., Biota1, Biota2).
    type BiotaElement
        class(Biota), allocatable :: item
    end type

    ! Same as for class(Biota).
    type ReactorElement
        class(Reactor), allocatable :: item
    end type

    type, abstract, public :: BedSedimentLayer                       ! type declaration for superclass
        ! define variables for 'has a' objects: Biota and Reactor
        type(BiotaElement), allocatable :: colBiota(:)               ! collection of objects of type Biota
        type(ReactorElement), allocatable :: colReactor(:)          ! collection of objects of type Reactor
        integer :: nBiotaTypes = 2                                   ! constant - number of Biota object types.
        type(objBiota1), allocatable :: b1                                 ! object of type Biota1
        type(objBiota2), allocatable :: b2                                 ! object of type Biota2
        integer :: nReactorTypes = 2                                 ! constant - number of Reactor object types.
        type(objReactor1), allocatable :: r1                               ! object of type Reactor1
        type(objReactor2), allocatable :: r2                               ! object of type Reactor2
        ! properties
        real :: Depth                                                ! depth of layer (m)
        real :: Pdens                                                ! particle density of solid phase (g/cm3)
        real :: Porosity                                             ! porosity of sediment layer (m3 water/m3 total)
        integer :: allst                                             ! array allocation status
        integer :: err                                               ! success/failure code
      contains
        ! deferred methods: must be defined in all subclasses
        ! nothing here (yet)
        ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure :: create => cr                                    ! constructor method
        procedure :: destroy => fi                                   ! finaliser method
        procedure :: nBiota => nb                                    ! property function to return number of Biota objects
        procedure :: nReactor => nr                                  ! property function to return number of Reactor objects
        procedure :: SetDepth => sd                                  ! procedure to set the depth at initialisation
        procedure :: SetPdens => spd                                 ! procedure to set the particle density at initialisation
        procedure :: SetPorosity => sp                               ! procedure to set the porosity at initialisation
        ! any other subroutines or functions go here
    end type

    Private :: cr                                                    ! create           method and property private names
    Private :: fi                                                    ! destroy
    Private :: nb                                                    ! nBiota
    Private :: nr                                                    ! nReactor
    Private :: sd                                                    ! SetDepth
    Private :: spd                                                   ! SetPdens
    Private :: sp                                                    ! SetPorosity

    contains
    subroutine cr(Me, &
                  ldepth, &
                  lpdens, &
                  lporosity, &
                  lnbiota, &
                  ltbiota, &
                  lnreactor, &
                  ltreactor)                                         ! constructor method
        implicit none                                                ! force declaration of all variables
                                                                     ! dummy variables
        class(BedSedimentLayer) :: Me                                ! reference to this object, using the type of the abstract superclass
        type(real) :: ldepth                                         ! layer depth
        type(real) :: lpdens                                         ! layer particle density
        type(real) :: lporosity                                      ! layer porosity
        type(integer) :: lnbiota                                     ! number of objBiota objects to create
        type(integer) :: ltbiota(:)                                  ! array of integers representing Biota types to create. Must have lnbiota elements.
        type(integer) :: lnreactor                                   ! number of objReactor objects to create
        type(integer) :: ltreactor(:)                                ! array of integers representing Reactor types to create. Must have lnreactor elements.
                                                                     ! internal variables
        type(integer) :: x                                           ! loop counter
        
        Me%err = Me%SetDepth(ldepth)                             ! set depth, return success/failure code
        Me%err = Me%SetPdens(lpdens)                             ! set particle density, return success/failure code
        Me%err = Me%SetPorosity(lporosity)                       ! set porosity, return success/failure code
        if (size(ltbiota) /= lnbiota) then
            ! code here to handle error where the number of elements in ltbiota() does not equal lnbiota
        end if
        if (size(ltreactor) /= lnreactor) then
            ! code here to handle error where the number of elements in ltreactor() does not equal lnreactor
        end if

        allocate(Me%colBiota(lnbiota))                         ! Set colBiota to be of size lnbiota
        allocate(Me%colReactor(lnreactor))                     ! Set colReactor to be of size lnreactor

        if (lnbiota > 0) then
            do x = 1, lnBiota
                ! here we want to use the list of integers in ltbiota() to
                ! create objects with the type of the specified subclass of Biota, where
                ! 1 --> type objBiota1, 2 --> type objBiota2 etc.,
                ! and point the corresponding element of colBiota() to them.
                ! For example, if ltbiota(1) = 1, we want to make the first
                ! element of colBiota, colBiota(1), a pointer to an object of type objBiota1
                ! in doing this we must trap the possible error that an element of ltbiota()
                ! refers to a non-existent subclass of Biota.
                ! the code below is a first guess at how to do this.
                !
                ! Sam's comments:
                ! ---------------
                ! You're right that move_alloc() won't work with Me%colBiota(x) as the array
                ! elements aren't allocatable. It seems a tricky problem to solve:
                ! https://stackoverflow.com/questions/31106539/polymorphism-in-an-array-of-elements.
                ! I think the best method is to store the class(Biota) in a derived type and
                ! then have an array of that derived type as the colBiota property. That way,
                ! Fortran won't complain that elements of the array are of different types, which
                ! is why allocating individual array elements of the class(Biota) array won't work.
                ! I've implemented that here and it seems to be working okay.
                !
                ! As an aside, it might be worth considering declaring Me%b1 in this procedure and not as a
                ! derived-type property, seeing as it's only used here. That way we're ensuring
                ! that it only sticks around for the duration of this procedure and the compiler
                ! will make sure it's deallocated when the procedure is done.
                select case (ltbiota(x))
                    case (1)
                        allocate (Me%b1, stat=Me%allst)              ! objBiota1 type - create the object
                        call Me%b1%create()                          ! call the object constructor - note arguments omitted for now
                        call move_alloc(Me%b1, Me%colBiota(x)%item)       ! move the object to the yth element of colBiota *** not sure this assignment will work
                        ! You're right, deallocate isn't necessary and actually throws up
                        ! a runtime error because it's already been deallocated
                        ! deallocate (Me%b1)                             ! not sure that this is necessary - b1 may be automatically deallocated in move_alloc
                    case (2)
                        allocate (Me%b2, stat=Me%allst)              ! objBiota2 type - create the object
                        call Me%b2%create()                          ! call the object constructor - note arguments omitted for now
                        call move_alloc(Me%b2, Me%colBiota(x)%item)       ! move the object to the yth element of colBiota *** not sure this assignment will work
                        ! deallocate (Me%b2)                             ! not sure that this is necessary - b2 may be automatically deallocated in move_alloc
                    case default
                        ! error - ltbiota(y) points to an invalid number. Need to abort and report.
                end select
            end do
        else if (lnBiota == 0) then
                                                                     ! code here for actions if no objBiota objects required
        else
                                                                     ! code here for invalid (negative) value of lnBiota
        end if
        if (lnreactor > 0) then
            allocate(Me%colReactor(lnreactor), stat=Me%allst)        ! allocate colReactor array
            do x = 1, lnReactor
              ! here we want to use the list of integers in ltreactor() to
              ! create objects with the type of the specified subclass of Reactor, where
              ! 1 --> type objReactor1, 2 --> type objReactor2 etc.,
              ! and point the corresponding element of colReactor() to them.
              ! For example, if ltreactor(1) = 1, we want to make the first
              ! element of colReactor, colReactor(1), a pointer to an object of type objReactor1
              ! in doing this we must trap the possible error that an element of ltreactor()
              ! refers to a non-existent subclass of Reactor.
              ! the code below is a first guess at how to do this.
              select case (ltreactor(x))
                  case (1)
                      allocate (Me%r1, stat=Me%allst)                ! objReactor1 type - create the object
                      call Me%r1%new('name')                            ! call the object constructor - note arguments omitted for now
                      call move_alloc(Me%r1, Me%colReactor(x)%item)       ! move the object to the yth element of colReactor *** not sure this assignment will work
                      ! deallocate(Me%r1)                               ! not sure that this is necessary - r1 may be automatically deallocated in move_alloc
                  case (2)
                      allocate (Me%r2, stat=Me%allst)                ! objReactor2 type - create the object
                      call Me%r1%new('name')                            ! call the object constructor - note arguments omitted for now
                      call move_alloc(Me%r2, Me%colReactor(x)%item)       ! move the object to the yth element of colReactor *** not sure this assignment will work
                      ! deallocate(Me%r2)                               ! not sure that this is necessary - r2 may be automatically deallocated in move_alloc
                  case default
                      ! error - ltbiota(y) points to an invalid number. Need to abort and report.
              end select
          end do
      elseif (lnReactor == 0) then
                                                                     ! code here for actions if no objReactor objects required
      else
                                                                     ! code here for invalid (negative) value of lnreactor
      end if
    end subroutine
    subroutine fi(Me)                                                ! finaliser method
        class(BedSedimentLayer) :: Me                                ! reference to this object, using the type of the abstract superclass
        integer :: x                                                  ! loop iterator
        !implicit none                                                ! Not needed if also declared for module
        do x = 1, Me%nBiota()
            ! TODO: This causes a compiler error: "Name ‘destroyinterface’ is an ambiguous reference to
            ! ‘destroyinterface’ from module ‘spcbiota’". Not sure if this error makes sense or not,
            ! as colBiota should contain classBiota1 or classBiota2 objects (thus destroy isn't
            ! ambiguous), but the compiler doesn't know that.
            ! call Me%colBiota(x)%destroy()                            ! do any cleanup required in Biota objects
        end do
        deallocate(Me%colBiota)                                      ! destroy objects in colBiota
        do x = 1, Me%nReactor()
            ! TODO: Same as above
            ! call Me%colReactor(x)%destroy()                          ! do any cleanup required in Reactor objects
        end do
        deallocate(Me%colReactor)                                    ! destroy objects in colReactor
    end subroutine
    integer function nb(Me)                                          ! Property function, returns number of Biota objects
        implicit none                                                ! force declaration of all variables
        class(BedSedimentLayer) :: Me                                ! BedSedimentLayer type
        nb = size(Me%colBiota)
    end function
    integer function nr(Me)                                          ! Property function, returns number of Reactor objects
        implicit none                                                ! force declaration of all variables
        class(BedSedimentLayer) :: Me                                ! BedSedimentLayer type
        nr = size(Me%colReactor)
    end function
    integer function sd(Me, ld)                                      ! Set depth property of layer
        implicit none
        class(BedSedimentLayer) :: Me                                ! BedSedimentLayer type
        real :: ld                                                   ! depth to which layer to be set
        if (ld < 0) then
                                                                     ! invalid value of depth - set return error code
        else
            Me%Depth = ld                                            ! Assign depth property
            sd = 0                                                   ! return success code
        end if
    end function
    integer function spd(Me, lpd)                                    ! set particle density property of layer
        implicit none
        class(BedSedimentLayer) :: Me                                ! BedSedimentLayer type
        real :: lpd                                                  ! particle density for layer
        if (lpd < 0) then
                                                                     ! invalid value of particle density - set return error code
        else
            Me%Pdens = lpd                                           ! Assign particle density property
            spd = 0                                                  ! return success code
        end if
    end function
    integer function sp(Me, lp)                                      ! set porosity property of layer
        implicit none
        class(BedSedimentLayer) :: Me                                ! BedSedimentLayer type
        real :: lp                                                   ! porosity for layer
        if (lp <= 0 .or. lp > 1) then
                                                                     ! invalid value of porosity - set return error code
        else
            Me%Porosity = lp                                         ! Assign porosity property
            sp = 0                                                   ! return success code
        end if
    end function
end module

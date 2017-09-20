module spcBedSedimentLayer                                          ! abstract superclass definition for BedSedimentLayer
                                                                    ! defines the properties and methods shared by all BedSedimentLayer objects
                                                                    ! objects of this class cannot be instantiated, only objects of its subclasses
    use Globals
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    implicit none                                                   ! force declaration of all variables
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
        integer, private :: err
        type(Result), private :: r                                  ! Result object from function, for error checking

      contains
                                                                    ! deferred methods: must be defined in all subclasses
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: create => createBedSedimentLayer       ! constructor method
        procedure, public :: destroy => destroyBedSedimentLayer     ! finaliser method
        procedure, public :: SetDepth                               ! procedure to set the depth at initialisation
        procedure, public :: SetPdens                               ! procedure to set the particle density at initialisation
        procedure, public :: SetPorosity                            ! procedure to set the porosity at initialisation
                                                                    ! any other subroutines or functions go here
    end type

  contains

    subroutine createBedSedimentLayer(Me, &
                      lname, &
                      ldepth, &
                      lpdens, &
                      lporosity, &
                      ltbiota, &
                      ltreactor)                                    ! constructor method
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
        ! Plenty of different ways the below could be done, this is just an example.
        ! More succinctly, could do something like:
        ! call ERROR_HANDLER%trigger( &
        !   errors=[ &
        !       .error. Me%SetDepth(ldepth), &
        !       .error. Me%SetPdens(lpdens), &
        !       .error. Me%SetPorosity(lporosity) &
        !   ] &
        ! )
        ! This would print out all of the errors encountered in those three procedures as well,
        ! not just the first one encountered.
        Me%r = Me%SetDepth(ldepth)                                  ! set depth
        call ERROR_HANDLER%trigger(error=Me%r%getError())           ! trigger an error, if there was one
        Me%r = Me%SetPdens(lpdens)                                  ! set particle density, return success/failure code
        call Me%r%addToTrace("BedSedimentLayer%create")             ! Example of adding to error trace
        call ERROR_HANDLER%trigger(error=Me%r%getError())           ! trigger an error, if there was one
        Me%r = Me%SetPorosity(lporosity)                            ! set porosity, return success/failure code
        call ERROR_HANDLER%trigger(error=Me%r%getError())           ! trigger an error, if there was one
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
                        call ERROR_HANDLER%trigger(999)             ! error - ltbiota(y) points to an invalid number. Need to abort and report.
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
                    allocate (r1, stat=Me%allst)                    ! objReactor1 type - create the object
                    call r1%create()                                ! call the object constructor
                    call move_alloc(r1, Me%colReactor(x)%item)      ! move the object to the yth element of the Reactor collection
                case (2)
                    allocate (r2, stat=Me%allst)                    ! objReactor2 type - create the object
                    call r2%create()                                ! call the object constructor
                    call move_alloc(r2, Me%colReactor(x)%item)      ! move the object to the yth element of the Reactor collection
                case default
                    call ERROR_HANDLER%trigger(998)                 ! error - ltreactor(y) points to an invalid number. Need to abort and report.
              end select
          end do
      elseif (Me%nReactor == 0) then
                                                                    ! code here for actions if no Reactor objects required
      else
                                                                    ! code here for invalid (negative) value of nReactor
      end if
    end subroutine

    subroutine destroyBedSedimentLayer(Me)                          ! finaliser method
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

    function SetDepth(Me, ld) result(r)                             ! Set depth property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: ld                                                  ! depth to which layer to be set
        type(Result) :: r                                           ! Result object to return, including any error
        type(ErrorInstance) :: error                                ! Add to return after criterion check
        ! The next two lines could equally be written r = Result(error=ERROR_HANDLER%positive(ld))
        error = ERROR_HANDLER%positive( &                           ! Enforce that ld must be positive
            ld, &
            message="Bed Sediment layer depth must be positive." &
        )
        r = Result(error=error)                                     ! Construct the result to return
        if (error%notError()) Me%Depth = ld                         ! If no error was returned
    end function

    function SetPdens(Me, lpd) result(r)                            ! set particle density property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: lpd                                                 ! particle density for layer
        type(Result) :: r                                           ! Result object to return
        Me%Pdens = lpd                                              ! Assign particle density property, doesn't matter if < 0 and error should be triggered anywhere
        r = Result(error=ERROR_HANDLER%positive( &                  ! Return the result
            lpd, &
            message="Bed sediment particle density must be positive.", &
            traceMessage="BedSedimentLayer%setPdens" &
        ))
    end function

    function SetPorosity(Me, lp) result(r)                           ! set porosity property of layer
        implicit none
        class(BedSedimentLayer) :: Me                               ! BedSedimentLayer type
        real :: lp                                                  ! porosity for layer
        type(Result) :: r                                           ! Result object to return
        Me%Porosity = lp                                            ! Assign porosity property
        r = Result(error=ERROR_HANDLER%limit(lp,0.0,1.0, &          ! Limit to between 0 and 1
            message="Bed sediment porosity must be between 0 and 1." &
        ))
        ! TODO: Properly enforce >= 0 or < 1
    end function

end module

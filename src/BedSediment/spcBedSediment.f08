module spcBedSediment                                               ! abstract superclass definition for BedSediment
                                                                    ! defines the properties and methods shared by all BedSediment objects
                                                                    ! objects of this class cannot be instantiated, only objects of its subclasses
    use spcBedSedimentLayer                                         ! USEs the spcBedSedimentLayer superclass and subclasses
    use classBedSedimentLayer1
    use classBedSedimentLayer2
    use Globals
    use ResultModule                                                ! Error handling
    use ErrorInstanceModule
    use ErrorCriteriaModule
    implicit none                                                   ! force declaration of all variables
    private

    type BedSedimentLayerElement
        class(BedSedimentLayer), allocatable :: item                ! Storing polymorphic class(BedSedimentLayer) in derived type so that a collection of
    end type                                                        ! different extended types of BedSedimentLayer can be stored in an array.

    type, abstract, public :: BedSediment                           ! type declaration for superclass
        private
        character(len=256) :: name                                  ! a name for the object
                                                                    ! define variables for 'has a' objects: BedSedimentLayer
        class(BedSedimentLayerElement), allocatable :: &
                    colBedSedimentLayer(:)                          ! collection of BedSedimentLayer objects
                                                                    ! properties
        integer :: nLayers                                          ! number of BedSedimentLayer objects
        integer :: allst                                            ! array allocation status
        integer :: err                                              ! success/failure code
                                                                    ! any private variable declarations go here
      contains
                                                                    ! deferred methods: must be defined in all subclasses
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: create => createBedSediment            ! constructor method
        procedure, public :: destroy => destroyBedSediment          ! finaliser method
        procedure(calculateResuspensionBedSediment), deferred, public :: calculateResuspension ! calculuate resuspension
        procedure(calculateStreamPowerBedSediment), deferred, public ::  calculateStreamPower ! calculate stream power per unit area of stream bed
        procedure, public :: getNLayers                             ! property function, returns number of bed sediment layers
        procedure, public :: Depth                                  ! property function to return total depth of BedSediment
                                                                    ! any other subroutines or functions go here
    end type

    abstract interface
        function calculateResuspensionBedSediment(me, a, m_bed, alpha, omega, R_h, R_hmax) result(r)
            use Globals
            import BedSediment, Result0D
            class(BedSediment) :: me
            real(dp) :: a                                   !! Calibration factor [s2/kg]
            real(dp) :: m_bed                               !! Bed mass per unit area [kg/m2]
            real(dp) :: alpha                               !! Proportion of size class that can be resuspended [-]
            real(dp) :: omega                               !! Stream power per unit area of stream bed [J/s/m2]
            real(dp) :: R_h                                 !! Actual hydraulic radius [m]
            real(dp) :: R_hmax                              !! Maximum hydraulic radius [m]
            type(Result0D) :: r
        end function

        function calculateStreamPowerBedSediment(me, rho_water, g, Q, W, S) result(r)
            use Globals
            import BedSediment, Result0D
            class(BedSediment) :: me
            real(dp) :: rho_water                           !! Density of water [kg/m3]
            real(dp) :: g                                   !! Gravitational acceleration [m/s]
            real(dp) :: Q                                   !! Discharge [m3/s]
            real(dp) :: W                                   !! River width [m]
            real(dp) :: S                                   !! River slope [m/m]
            type(Result0D) :: r
        end function
    end interface
  contains
    subroutine createBedSediment(Me, &
                      lname, &
                      ltBSL)                                        ! constructor method
                                                                    ! dummy variables
        class(BedSediment) :: Me                                    ! reference to this object, using the type of the abstract superclass
        character(len=*) :: lname                                   ! the name of this object
                                                                    ! SH: Changed to assumed-length character string create() procedure
                                                                    ! will accept character strings less than 256.
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
        if (Me%nLayers > 0) then
            allocate(Me%colBedSedimentLayer(Me%nLayers), &
                stat=Me%allst)                                      ! Set colBedSedimentLayer size to number of layers
            do x = 1, Me%nLayers
                select case (ltBSL(x))
                    case (1)
                        allocate (BSL1, stat=Me%allst)              ! objBedSedimentLayer1 type - create the object
                                                                    ! SH: create() filled with arbitrary values for the moment
                        call BSL1%create('name',1.0,1.0,1.0,[1],[1])! call the object constructor
                        call move_alloc(BSL1, &
                            Me%colBedSedimentLayer(x)%item)         ! move the object to the xth element of the BedSedimentLayer collection
                    case (2)
                        allocate (BSL2, stat=Me%allst)              ! objBedSedimentLayer2 type - create the object
                        call BSL2%create('name',1.0,1.0,1.0,[1],[1])! call the object constructor
                        call move_alloc(BSL2, &
                            Me%colBedSedimentLayer(x)%item)         ! move the object to the xth element of colBiota
                    case default
                        call ERROR_HANDLER%trigger(997)             ! error - ltBSL(y) points to an invalid number. Need to abort and report.
                end select
            end do
        else
            call ERROR_HANDLER%trigger(996)                         ! If no BSLs have been provided (can't be negative as nLayer deduced from array size)
        end if
    end subroutine

    subroutine destroyBedSediment(Me)                               ! finaliser method
        class(BedSediment)  :: Me                                   ! reference to this object, using the type of the abstract superclass
        integer :: x                                                ! loop counter
        do x = 1, Me%nLayers
            call Me%colBedSedimentLayer(x)%item%destroy()           ! do any cleanup required in BedSedimentLayer objects
        end do
    end subroutine

    integer function getNLayers(Me) result(nLayers)                 ! property function, returns number of BedSedimentLayer objects
        class(BedSediment) :: Me
        nLayers = size(Me%colBedSedimentLayer)
    end function

    real function Depth(Me)                                         ! property function, returns total depth of sediment
        class(BedSediment) :: Me
        type(integer) :: x                                          ! loop counter
        Depth = 0                                                   ! initialise the function return value
        do x = 1, Me%nLayers                                        ! loop through all the layers
            Depth = Depth + Me%colBedSedimentLayer(x)%item%Depth    ! adding up the depth of each layer
        end do                                                      ! to return the total sediment depth
    end function
end module

!> abstract superclass definition for BedSediment
!! defines the properties and methods shared by all BedSediment objects
!! objects of this class cannot be instantiated, only objects of its subclasses
module spcBedSediment
    use Globals                                                      !  global declarations
    use mo_netcdf                                                    ! input/output handling
    use ResultModule                                                 ! error handling classes, required for
    use ResultFineSedimentModule
    use ErrorInstanceModule                                          ! generation of trace error messages
    use spcBedSedimentLayer                                          ! uses the spcBedSedimentLayer superclass and subclasses
    use classFineSediment1
    implicit none                                                    ! force declaration of all variables
    type BedSedimentLayerElement
        class(BedSedimentLayer), allocatable :: item                 ! Storing polymorphic class(BedSedimentLayer) in derived type so that a collection of
    end type                                                         ! different extended types of BedSedimentLayer can be stored in an array.
    type, abstract, public :: BedSediment                            ! type declaration for superclass
        character(len=256) :: name                                   ! Name for this object, of the form BedSediment_x_y_s_r
                                                                     ! define variables for 'has a' objects: BedSedimentLayer
        class(BedSedimentLayerElement), allocatable :: &
        colBedSedimentLayers(:)                                      ! collection of BedSedimentLayer objects
                                                                     ! properties
        integer :: nSizeClasses                                      ! number of fine sediment size classes
        integer :: nLayers                                           ! number of layers (BedSedimentLayer objects)
        integer :: nfComp                                            ! number of fractional composition terms for sediment
        type(NcGroup) :: ncGroup                                     !! The NETCDF group for this BedSediment
    contains
                                                                     ! deferred methods: must be defined in all subclasses
        procedure(createBedSediment), public, deferred :: &
            create                                                   ! constructor method
        procedure(destroyBedSediment), public, deferred :: &
            destroy                                                  ! finaliser method
        procedure(DepositSediment), public, deferred :: &
            deposit                                                  ! deposit sediment from water column
        procedure(ResuspendSediment), public, deferred :: &
            resuspend                                                ! resuspend sediment to water column
                                                                     ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: Af_sediment => Get_Af_sediment          ! fine sediment available capacity for size class
        procedure, public :: Cf_sediment => Get_Cf_sediment          ! fine sediment capacity for size class
        procedure, public :: Aw_sediment => Get_Aw_sediment          ! water available capacity for size class
        procedure, public :: Cw_sediment => Get_Cw_sediment          ! water capacity for size class
        procedure, public :: Mf_sediment => Get_Mf_sediment          ! fine sediment mass for size class
        procedure, public :: Mf_sed_all => Get_Mf_sed_all            ! fine sediment mass in all size classes
    end type
    abstract interface
        !> Function purpose
        !! ----------------------------------------------------------------------------------
        !! Initialise a BedSediment object.
        !!
        !!
        !! Function inputs
        !! ----------------------------------------------------------------------------------
        !!
        !!
        !! Function outputs/outcomes
        !! ----------------------------------------------------------------------------------
        !! initialised BedSediment object, including all layers and included FineSediment
        !! objects
        !!
        !! ----------------------------------------------------------------------------------
        function createBedSediment(Me, riverReachGroup) result(r)
!        function createBedSediment(Me, x, y, s, b, riverReachGroup) result(r)
            import BedSediment, Result, NcGroup
            class(BedSediment) :: Me                                 !! self-reference
!            integer :: x, y, s, b                                    !! References to containing GridCell, SubRiver and RiverReaches
            type(NcGroup), intent(in) :: riverReachGroup             !! NetCDF group reference to the RiverReach containing this object
            type(Result) :: r                                        !! returned Result object
        end function
        !> Function purpose
        !! ----------------------------------------------------------------------------------
        !! deallocate all allocatable variables and call destroy methods for all
        !! enclosed objects
        !!
        !! Function inputs
        !! ----------------------------------------------------------------------------------
        !! none
        !!
        !! Function outputs/outcomes
        !! ----------------------------------------------------------------------------------
        !!
        !! returns a warning if any deallocation throws an error
        !! ----------------------------------------------------------------------------------
        function destroyBedSediment(Me) result(r)
            import BedSediment, Result
            class(BedSediment) :: Me                                     !! self-reference
            type(Result) :: r                                            !! returned Result object
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! Deposit specified masses of fine sediment in each size class, and their
        !! associated water. Function buries sediment and shifts remaining sediment down
        !! to make space for deposition, if required
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !! Function takes as inputs:
        !! FS_dep (FineSediment1)   1D array of FineSediment1 objects containing the
        !! depositing fine sediment per size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !!
        !! r (real(dp)) returns water requirement from the water column [m3 m-2] real(dp)
        !! -------------------------------------------------------------------------------
        function depositSediment(Me, FS_dep) result (r)
            use Globals
            import BedSediment, Result0D, FineSediment1
            ! TODO: replace D with real array to represent SPM *masses* only
            class(BedSediment) :: Me                                 !! self-reference
            type(FineSediment1), allocatable :: FS_dep(:)            !! Depositing sediment by size class
            type(Result0D) :: r                                      !! returned Result object
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Deposit specified masses of fine sediment in each size class, and their
            ! associated water. Function buries sediment and shifts remaining sediment down
            ! to make space for deposition, if required
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! D (FineSedimentElement)  FineSediment object representing the depositing fine
            !                          sediment and water
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! 1.    Currently does not account fully for sediment burial, in the sense that
            !       it does not tally mass, volume and composition of buried material. This
            !       will need to be added before burial losses of a chemical vector can be
            !       computed.
            !       ACTION: add code to mix FineSediments together and return a single
            !       FineSediment object. This code can be used to tally up the sediment that
            !       is lost through burial.
            ! 2.    The FineSediment objects in D should not contain any water, but if they
            !       do it is not a problem as it will be overwritten.
            ! -------------------------------------------------------------------------------
        end function
        !> Function purpose
        !! ----------------------------------------------------------------------------------
        !! Resuspend specified masses of fine sediment in each size class, and their
        !! associated water
        !!
        !! Function inputs
        !! ----------------------------------------------------------------------------------
        !!
        !! M_resusp (real, dp)      1D array of fine sediment masses to be resuspended
        !!
        !! Function outputs/outcomes
        !! ----------------------------------------------------------------------------------
        !!
        !! returns a warning if the resuspended mass in a size class exceeds the mass in the
        !! sediment bed
        !!
        !! r (ResultFineSediment2D) returns resuspended fine sediments
        !!
        !! ----------------------------------------------------------------------------------
        function resuspendSediment(Me, M_resusp) result(r)
            use Globals
            import ResultFineSediment2D, BedSediment
            class(BedSediment) :: Me                                     !! self-reference
            real(dp), allocatable :: M_resusp(:)                         !! array of sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
            type(Result2D) :: r                                          !! returned Result object
        end function
    end interface
contains
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return available capacity for fine sediment of a specified size class in the whole
    !! sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! S    size class
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !! throws critical error if size class is invalid
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Af_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Af_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Af_sediment")                       ! LOCAL error trace
        if (S < 0) then &
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then &
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Af_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Af_sediment = Af_sediment + &
                Me%colBedSedimentLayers(L)%item%A_f(S)               ! sum capacities for all layers
        end do
        r = Result(data = Af_sediment)
    end function
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return capacity for fine sediment of a specified size class in the whole
    !! sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! S    size class
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !! throws critical error if size class is invalid
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Cf_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Cf_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Cf_sediment")                       ! LOCAL error trace
        if (S < 0) then &
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then &
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Cf_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Cf_sediment = Cf_sediment + &
                Me%colBedSedimentLayers(L)%item%C_f(S)               ! sum capacities for all layers
        end do
        r = Result(data = Cf_sediment)
    end function
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return available capacity for water associated with a specified size class in the
    !! whole sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! S    size class
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !! throws critical error if size class is invalid
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Aw_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Aw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Aw_sediment")                       ! LOCAL error trace
        if (S < 0) then &
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then &
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Aw_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Aw_sediment = Aw_sediment + &
                Me%colBedSedimentLayers(L)%item%A_w(S)               ! sum capacities for all layers
        end do
        r = Result(data = Aw_sediment)
    end function
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return capacity for water associated with a specified size class in the
    !! whole sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! S    size class
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !! throws critical error if size class is invalid
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Cw_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Cw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Cw_sediment")                       ! LOCAL error trace
        if (S < 0) then &
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then &
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Cw_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Cw_sediment = Cw_sediment + &
                Me%colBedSedimentLayers(L)%item%C_w(S)               ! sum capacities for all layers
        end do
        r = Result(data = Cw_sediment)
    end function
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return fine sediment mass in a specified size class, in the whole sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! S    size class
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !! throws critical error if size class is invalid
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Mf_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Mf_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Mf_sediment")                       ! LOCAL error trace
        if (S < 0) then &
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then &
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Mf_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Mf_sediment = Mf_sediment + &
                Me%colBedSedimentLayers(L)%item%colFineSediment(S)%M_f()
                                                                     ! sum masses for all layers. Not very elegant
        end do
        r = Result(data = Mf_sediment)
    end function
    !> Function purpose
    !! ----------------------------------------------------------------------------------
    !! return fine sediment mass for all size classes, in the whole sediment
    !!
    !! Function inputs
    !! ----------------------------------------------------------------------------------
    !!
    !! none
    !!
    !! Function outputs/outcomes
    !! ----------------------------------------------------------------------------------
    !!
    !! r (Result 0D) returns value required
    !!
    !! ----------------------------------------------------------------------------------
    pure function Get_Mf_sed_all(Me) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        type(Result0D) :: r                                          !! return value
        type(Result0D) :: r_l                                        ! LOCAL internal storage
        real(dp) :: Mf_sed_all                                       ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14), parameter :: tr = &
            trim(Me%name // "Get_Mf_sediment")                       ! LOCAL error trace
        Mf_sed_all = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            r_l = Me%colBedSedimentLayers(L)%item%M_f_layer()        ! get Result object for Layer L
            r%addErrors(.errors. r_l)                                ! pull errors out of Result object
            if (r%hasCriticalError()) return                         ! exit if error thrown
            Mf_sed_all = Mf_sed_all + .real. r_l                     ! sum masses across layers
        end do
        r = Result(data = Mf_sed_all)
    end function
end module

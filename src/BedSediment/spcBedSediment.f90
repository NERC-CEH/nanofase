!> Module containing definition of abstract superclass `BedSediment`.
module spcBedSediment
    use Globals                                                      !  global declarations
    use mo_netcdf                                                    ! input/output handling
    use ResultModule, only: Result, Result0D                         ! error handling classes, required for
    use ErrorInstanceModule                                          ! generation of trace error messages
    use spcBedSedimentLayer                                          ! uses the spcBedSedimentLayer superclass and subclasses
    use classFineSediment1
    implicit none                                                    ! force declaration of all variables
    !> Type definition for polymorphic `BedSedimentLayer` container,
    !! which stores a polymorphic class(BedSedimentLayer) in derived
    !! type so that a collection of different extended types of `BedSedimentLayer`
    !! can be stored in an array.
    type BedSedimentLayerElement
        class(BedSedimentLayer), allocatable :: item                !! The polymorphic `BedSedimentLayer` object
    end type
    !> Abstract superclass `BedSediment`. Defines the properties and methods
    !! shared by all `BedSediment` objects. Objects of this class cannot be instantiated,
    !! only objects of its subclasses
    type, abstract, public :: BedSediment
        character(len=256) :: name                                   !! Name for this object, of the form *BedSediment_x_y_s_r*
                                                                     ! define variables for 'has a' objects: BedSedimentLayer
        class(BedSedimentLayerElement), allocatable ::  &
        colBedSedimentLayers(:)                                      !! Collection of `BedSedimentLayer` objects
                                                                     ! properties
        integer :: nSizeClasses                                      !! Number of fine sediment size classes
        integer :: nLayers                                           !! Number of layers (`BedSedimentLayer` objects)
        integer :: nfComp                                            !! number of fractional composition terms for sediment
        type(NcGroup) :: ncGroup                                     !! The NETCDF group for this `BedSediment`
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
        procedure(ReportBedMassToConsole), public, deferred :: &
            repMass                                                  ! report fine sediment masses to the console
                                                                     ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: Af_sediment => Get_Af_sediment          ! fine sediment available capacity for size class
        procedure, public :: Cf_sediment => Get_Cf_sediment          ! fine sediment capacity for size class
        procedure, public :: Aw_sediment => Get_Aw_sediment          ! water available capacity for size class
        procedure, public :: Cw_sediment => Get_Cw_sediment          ! water capacity for size class
        procedure, public :: Mf_bed_one_size => Get_Mf_bed_one_size  ! fine sediment mass in bed for a single size class
        procedure, public :: Mf_bed_all => Get_Mf_bed_all            ! fine sediment mass in all size classes (single value)
        procedure, public :: Mf_bed_by_size => Get_Mf_bed_by_size    ! fine sediment mass in all size classes (1D array by size)
    end type
    abstract interface
        !> **Function purpose:** <br>
        !! Initialise a `BedSediment` object.
        !!                                                          <br>
        !! **Function outputs/outcomes:** <br>
        !! Initialised `BedSediment` object, including all layers and included `FineSediment`
        !! objects
        function createBedSediment(Me, riverReachGroup) result(r)
            use ResultModule, only: Result
            import BedSediment, NcGroup
            class(BedSediment) :: Me                                !! Self-reference
            type(NcGroup), intent(in) :: riverReachGroup            !! NetCDF group reference to the `RiverReach` containing this object
            type(Result) :: r                                       !! Returned `Result` object
        end function
        !> **Function purpose** <br>
        !! Deallocate all allocatable variables and call destroy methods for all
        !! enclosed objects
        !!                                                          <br>
        !! **Function outputs/outcomes** <br>
        !! Returns a warning if any deallocation throws an error
        function destroyBedSediment(Me) result(r)
            use ResultModule, only: Result
            import BedSediment
            class(BedSediment) :: Me                                !! Self-reference
            type(Result) :: r                                       !! Returned `Result` object
        end function
        !> **Function purpose**                                     <br>
        !! Deposit specified masses of fine sediment in each size class, and their
        !! associated water. Function buries sediment and shifts remaining sediment down
        !! to make space for deposition, if required
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! Function takes as inputs:
        !! <ul><li>
        !! `FS_dep (FineSediment1)`: 1D array of FineSediment1 objects containing the
        !! depositing fine sediment per size class
        !! </li></ul>
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (real(dp))` returns water requirement from the water column [m3 m-2]
        function depositSediment(Me, FS_dep) result (r)
            use Globals
            use ResultModule, only: Result0D
            import BedSediment, FineSediment1
            ! TODO: replace D with real array to represent SPM *masses* only
            class(BedSediment) :: Me                                !! Self-reference
            type(FineSediment1) :: FS_dep(:)                        !! Depositing sediment by size class
            type(Result0D) :: r                                     !! Returned `Result` object
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
            class(BedSediment) :: Me                                     !! Self-reference
            real(dp) :: M_resusp(:)                                      !! Array of sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
            type(ResultFineSediment2D) :: r                              !! Returned `Result` object
        end function
        !> **Function purpose**                                   
        !! 1. Report the mass of fine sediment in each layer to the console
        !! 2. report the total mass of fine sediment in the sediment to the console
        !!                                                          
        !! **Function inputs**                                      
        !! none
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! 
        subroutine ReportBedMassToConsole(Me)
            import BedSediment
            class(BedSediment) :: Me                                     !! The `BedSediment` instance
            integer :: n                                                 !! LOCAL loop counter 
        end subroutine
    end interface
contains
    !> **Function purpose**                                             <br>
    !! Return available capacity for fine sediment of a specified size class in the whole
    !! sediment
    !!                                                                  <br>
    !! **Function inputs**                                              <br>
    !! `S`: size class  
    !!                                                                  <br>
    !! **Function outputs/outcomes**                                    <br>
    !! `r (Result 0D)`: returns value required. Throws critical error if size class is invalid
    function Get_Af_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! the BedSediment instance
        integer, intent(in) :: S                                     !! size class
        type(Result0D) :: r                                          !! return value
        real(dp) :: Af_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace
        tr  = trim(Me%name // "Get_Af_sediment")
        if (S < 0) then
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        end if
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        end if
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Af_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Af_sediment = Af_sediment + &
                .dp. Me%colBedSedimentLayers(L)%item%A_f(S)          ! sum capacities for all layers
        end do
        r = Result(data = Af_sediment)
    end function
    !> **Function purpose**                                         <br>
    !! Return capacity for fine sediment of a specified size class in the whole
    !! sediment
    !!                                                              <br>
    !! **Function inputs**                                          <br>
    !! `S`: size class
    !!                                                              <br>
    !! **Function outputs/outcomes**
    !! `r (Result 0D)`: returns value required. Throws critical error if size class is invalid
    function Get_Cf_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! The `BedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        type(Result0D) :: r                                          !! Return value
        real(dp) :: Cf_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace
        tr = trim(Me%name // "Get_Cf_sediment")
        if (S < 0) then
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        end if
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        end if
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Cf_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Cf_sediment = Cf_sediment + &
                .dp. Me%colBedSedimentLayers(L)%item%C_f(S)          ! sum capacities for all layers
        end do
        r = Result(data = Cf_sediment)
    end function
    !> **Function purpose**                                         <br>
    !! Return available capacity for water associated with a specified size class in the
    !! whole sediment
    !!                                                              <br>
    !! **Function inputs**                                          <br>
    !! `S`: size class
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (Result 0D)`: returns value required. Throws critical error if size class is invalid
    function Get_Aw_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! The `BedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        type(Result0D) :: r                                          !! Return value
        real(dp) :: Aw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace
        tr = trim(Me%name // "Get_Aw_sediment")
        if (S < 0) then
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        end if
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        end if
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Aw_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Aw_sediment = Aw_sediment + &
                .dp. Me%colBedSedimentLayers(L)%item%A_w(S)          ! sum capacities for all layers
        end do
        r = Result(data = Aw_sediment)
    end function
    !> **Function purpose**                                         <br>
    !! Return capacity for water associated with a specified size class in the
    !! whole sediment
    !!                                                              <br>
    !! **Function inputs**                                          <br>
    !! `S`: size class
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (Result 0D)`: returns value required. Throws critical error if size class is invalid
    function Get_Cw_sediment(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! The `BedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        type(Result0D) :: r                                          !! Return value
        real(dp) :: Cw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace  
        tr = trim(Me%name // "Get_Cw_sediment")
        if (S < 0) then
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        end if
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        end if
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Cw_sediment = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Cw_sediment = Cw_sediment + &
                .dp. Me%colBedSedimentLayers(L)%item%C_w(S)          ! sum capacities for all layers
        end do
        r = Result(data = Cw_sediment)
    end function
    !> **Function purpose**                                         <br>
    !! Return fine sediment mass in a specified size class, in the whole sediment
    !!                                                              <br>
    !! **Function inputs**                                          <br>
    !! `S`: size class
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (Result 0D)`: returns value required. Throws critical error if size class is invalid
    function Get_Mf_bed_one_size(Me, S) result(r)
        class(BedSediment), intent(in) :: Me                         !! The `BedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        type(Result0D) :: r                                          !! Return value
        real(dp) :: Mf                                               ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace
        tr = trim(Me%name // "Get_Mf_bed_one_size")
        if (S < 0) then
            call r%addError(ErrorInstance( &
                            code = 103, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S < 0
        end if
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        if (S > Me%nSizeClasses) then
            call r%addError(ErrorInstance( &
                            code = 104, &
                            message = "The size class is invalid", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if S > nSizeClasses
        end if
        if (r%hasCriticalError()) return                             ! exit if error thrown
        Mf  = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            Mf = Mf + &
                Me%colBedSedimentLayers(L)%item%colFineSediment(S)%M_f()
                                                                     ! sum masses for all layers. Not very elegant
        end do
        r = Result(data = Mf)
    end function
    !> **Function purpose**                                         <br>
    !! Return fine sediment mass for all size classes, in the whole sediment, as a single value
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (Result 0D)`: returns value required
    function Get_Mf_bed_all(Me) result(r)
        class(BedSediment), intent(in) :: Me                         !! the `BedSediment` instance
        type(Result0D) :: r                                          !! Return value
        type(Result0D) :: r_l                                        ! LOCAL internal storage
        real(dp) :: Mf                                               ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        character(len=14) :: tr                                      ! LOCAL error trace
        tr = trim(Me%name // "Get_Mf_bed_size")
        Mf = 0
        do L = 1, Me%nLayers                                         ! loop through each layer
            r_l = Me%colBedSedimentLayers(L)%item%M_f_layer()        ! get Result object for Layer L
            call r%addErrors(.errors. r_l)                           ! pull errors out of Result object
            if (r%hasCriticalError()) return                         ! exit if error thrown
            Mf = Mf + .real. r_l                                     ! sum masses across layers
        end do
        r = Result(data = Mf)                                        ! return value
    end function 
    !> **function purpose**
    !!
    !!
    !!
    !!
    function Get_Mf_bed_by_size(Me) result(r)
        class(BedSediment), intent(in) :: Me                         !! the `BedSediment` instance
        type(Result1D) :: r                                          !! Return value
        integer :: L                                                 ! LOCAL loop counter
        integer :: S                                                 ! LOCAL loop counter
        real(dp) :: Mf                                               ! LOCAL internal storage
        real(dp) :: Mf_size(Me%nSizeClasses)                         ! LOCAL 1D array to hold masses by size fraction
        character(len=14) :: tr                                      ! LOCAL error trace
        tr = trim(Me%name // "Get_Mf_bed_by_size")
        do S = 1, Me%nSizeClasses                                    ! for each size class
            Mf = 0                                                   ! initialise sumnation of mass
            do L = 1, Me%nLayers                                     ! loop through each layer
            Mf = Mf + &
                Me%colBedSedimentLayers(L)%item%colFineSediment(S)%M_f()
                                                                     ! sum masses across all layers. Not very elegant
            end do
            Mf_size(S) = Mf                                          ! assign to array for output
        end do
        r = Result(data = Mf_size)                                   ! return value
    end function
    
                                ! Get the FineSediment objects from the Result1D object, temporarily
                                !! store in array and then assign to U and T
                                !tmpFineSediment = .finesediment. r1D
                                !U = tmpFineSediment(1)               ! sediment removed - to be added to Layer L
                                !T = tmpFineSediment(2)               ! sediment not removed - *** should be none ***
                                !r0D = O%addSediment(S, U)            ! add the sediment in U to the "receiving" layer
                                !! SL: r0D returns fine sediment that could not be added - *** should be none ***
                                !call r%addErrors(.errors. r0D)       ! retrieve errors into main Result object
                                !if (r%hasCriticalError()) then       ! if RemoveSediment throws a critical error
                                !    call r%addToTrace(tr)            ! add trace to all errors
                                !    return                           ! and exit
                                !end if

end module

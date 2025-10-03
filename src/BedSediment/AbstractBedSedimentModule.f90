!> Module containing definition of abstract superclass `AbstractBedSediment`.
module AbstractBedSedimentModule
    use GlobalsModule
    use mo_netcdf
    use ResultModule, only: Result, Result0D, Result3D
    use ErrorInstanceModule
    use AbstractBedSedimentLayerModule
    use FineSedimentModule
    use ContaminantModule
    use Spoof
    implicit none                                                    ! force declaration of all variables
    !> Type definition for polymorphic `BedSedimentLayer` container,
    !! which stores a polymorphic class(BedSedimentLayer) in derived
    !! type so that a collection of different extended types of `BedSedimentLayer`
    !! can be stored in an array.
    type BedSedimentLayerElement
        class(AbstractBedSedimentLayer), allocatable :: item            !! The polymorphic `BedSedimentLayer` object
    end type
    !> Abstract superclass `AbstractBedSediment`. Defines the properties and methods
    !! shared by all `AbstractBedSediment` objects. Objects of this class cannot be instantiated,
    !! only objects of its subclasses
    type, abstract, public :: AbstractBedSediment
        character(len=256)              :: name                                 !! Name for this object, of the form *BedSediment_x_y_s_r*
        class(BedSedimentLayerElement), allocatable :: colBedSedimentLayers(:)  !! Collection of `BedSedimentLayer` objects
        integer                         :: x                                    !! x index of the containing water body
        integer                         :: y                                    !! y index of the containing water body
        integer                         :: nSizeClasses                         !! Number of fine sediment size classes
        real(dp), allocatable           :: delta_sed(:,:,:)                     !! mass transfer matrix for sediment deposition and resuspension. dim1=layers+3, dim2=layers+3, dim3=size classes
        integer                         :: n_delta_sed                          !! The order of delta_sed
        type(CSRMatrix), allocatable    :: delta_sed_csr(:)                     !! CSR matrix storage for delta_sed. dim=spm size classes
        integer                         :: nfComp                               !! number of fractional composition terms for sediment
        ! Contaminants
        type(Contaminant), allocatable :: m_contaminant(:)                     !! Contaminant objects for deposition, resuspension, layers 1 to N, and burial
    contains
        procedure(createBedSediment), deferred :: create            ! constructor method
        procedure(destroyBedSediment), deferred :: destroy          ! finaliser method
        procedure(DepositSediment), deferred :: deposit             ! deposit sediment from water column
        procedure(ResuspendSediment), deferred :: resuspend         ! resuspend sediment to water column
        procedure(ReportBedMassToConsole), deferred :: repMass      ! report fine sediment masses to the console
        procedure(FinaliseMTCMatrix), deferred :: getMatrix         ! finalise mass transfer coefficient matrix
        procedure(transferContaminantBedSediment), deferred :: transferContaminant    ! Transfer Contaminant masses between layers and to/from water body, using mass transfer coef matrix
        procedure(deposit_spm),   deferred :: deposit_spm
        procedure(resuspend_spm), deferred :: resuspend_spm
        procedure :: Af_sediment => Get_Af_sediment          ! fine sediment available capacity for size class
        procedure :: Cf_sediment => Get_Cf_sediment          ! fine sediment capacity for size class
        procedure :: Aw_sediment => Get_Aw_sediment          ! water available capacity for size class
        procedure :: Cw_sediment => Get_Cw_sediment          ! water capacity for size class
        procedure :: Mf_bed_one_size => Get_Mf_bed_one_size  ! fine sediment mass in bed for a single size class
        procedure :: Mf_bed_all => Get_Mf_bed_all            ! fine sediment mass in all size classes (single value)
        procedure :: Mf_bed_by_size => Get_Mf_bed_by_size    ! fine sediment mass in all size classes (1D array by size)
        procedure :: Mf_bed_by_layer => get_Mf_bed_by_layer  ! Fine sediment mass for an individual layer
        procedure :: Mf_bed_layer_array => get_Mf_bed_layer_array  ! Fine sediment mass as an array of all layers
        procedure :: V_w_by_layer => get_V_w_by_layer        ! total water volume in each layer
        ! Getters
        procedure :: get_m_contaminant
        procedure :: get_C_contaminant
        procedure :: get_C_contaminant_byMass
        procedure :: get_m_contaminant_l
        procedure :: get_C_contaminant_l
        procedure :: get_C_contaminant_l_byMass
        procedure :: get_m_contaminant_buried
        procedure :: finalise => finaliseBedSediment
    end type

    abstract interface
        !> **Function purpose**                                         <br>
        !! Derive a mass transfer coefficient matrix
        !!                                                              <br>
        !! **Function inputs**
        !! mtcmat: 3D matrix (layers + 3, layers + 3, size classes)
        !! containing the absolute mass transfers for deposition, 
        !! resuspension, layers and burial
        !! djdep: deposition fluxes by size class [kg/m2]
        !! djres: resuspension fluxes by size class [kg/m2]             <br>
        !!
        !! **Function outputs/outcomes**                                <br>
        !! mtcmat: 3D matrix (layers + 3, layers + 3, size classes)
        !! containing the mass transfers coefficients for deposition, 
        !! resuspension, layers and burial
        !! objects
        subroutine FinaliseMTCMatrix(Me, djdep, djres)
            use GlobalsModule, only: dp
            import AbstractBedSediment
            class(AbstractBedSediment) :: me                                 !! Self-reference
            real(dp) :: djdep(:)                                     !! deposition fluxes by size class [kg/m2]
            real(dp) :: djres(:)                                     !! resuspension fluxes by size class [kg/m2]
        end subroutine

        !> **Function purpose:** <br>
        !! Initialise a `AbstractBedSediment` object.
        !!                                                          <br>
        !! **Function outputs/outcomes:** <br>
        !! Initialised `AbstractBedSediment` object, including all layers and included `FineSediment`
        !! objects
        function createBedSediment(Me, x, y, w) result(r)
            use ResultModule, only: Result
            import AbstractBedSediment, NcGroup
            class(AbstractBedSediment) :: Me                                !! Self-reference
            integer :: x                                            !! x index of the containing water body
            integer :: y                                            !! y index of the containing water body
            integer :: w                                            !! w index of the containing water body
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
            import AbstractBedSediment
            class(AbstractBedSediment) :: Me                                !! Self-reference
            type(Result) :: r                                       !! Returned `Result` object
        end function

        function transferContaminantBedSediment(me, j_contaminant_dep) result(r)
            use ResultModule, only: Result
            import AbstractBedSediment, Contaminant
            class(AbstractBedSediment), intent(inout) :: me
            type(Contaminant), intent(in) :: j_contaminant_dep
            type(Result) :: r
        end function

        function deposit_spm(Me, dj_spm_deposit, bedArea, out_deposit, out_resus) result(r)
            use GlobalsModule, only: dp
            import AbstractBedSediment, Contaminant, Result
            class(AbstractBedSediment), intent(inout) :: Me
            real(dp), intent(in) :: dj_spm_deposit(:)
            real(dp), intent(in) :: bedArea
            type(Contaminant), intent(out) :: out_deposit
            type(Contaminant), intent(out) :: out_resus
            type(Result) :: r
        end function

        function resuspend_spm(Me, dj_spm_resus, bedArea, out_resus) result(r)
            use GlobalsModule, only: dp
            import AbstractBedSediment, Contaminant, Result
            class(AbstractBedSediment), intent(inout) :: Me
            real(dp), intent(in) :: dj_spm_resus(:)
            real(dp), intent(in) :: bedArea
            type(Contaminant), intent(out) :: out_resus
            type(Result) :: r
        end function


        !> **Function purpose**                                     <br>
        !! Deposit specified masses of fine sediment in each size class, and their
        !! associated water. Function buries sediment and shifts remaining sediment down
        !! to make space for deposition, if required
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! Function takes as inputs:
        !! <ul><li>
        !! `FS_dep (FineSediment)`: 1D array of FineSediment objects containing the
        !! depositing fine sediment per size class
        !! </li></ul>
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (real(dp))` returns water requirement from the water column [m3 m-2]
        function depositSediment(Me, FS_dep) result (r)
            use GlobalsModule
            use ResultModule, only: Result0D
            import AbstractBedSediment, FineSediment
            class(AbstractBedSediment) :: Me                                !! Self-reference
            type(FineSediment) :: FS_dep(:)                        !! Depositing sediment by size class
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
        function resuspendSediment(Me, FS_resusp) result(r)
            use GlobalsModule
            import ResultFineSediment2D, AbstractBedSediment
            class(AbstractBedSediment) :: Me                                     !! Self-reference
            real(dp) :: FS_resusp(:)                                     !! Array of sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
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
            import AbstractBedSediment
            class(AbstractBedSediment) :: Me                                     !! The `AbstractBedSediment` instance       
        end subroutine

        !> **Function purpose**
        !! initialise the matrix of mass transfer coefficients for sediment deposition and resuspension
        !!                                                          
        !! **Function inputs**                                      
        !! none (uses class-level variable array delta_sed(:,:,:)
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! delta_sed populated with initial values, all zero except for layer(x) ->layer(y) coefficients where x=y; these are set to unity
        function InitialiseMTCMatrix(Me) result(r)
            use ResultModule, only: Result
            import AbstractBedSediment
            class(AbstractBedSediment) :: Me                                     !! The `AbstractBedSediment` instance
            type(Result) :: r                                            !! `Result` object. Returns water requirement from the water column [m3 m-2], real(dp)
        end function
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
    function Get_Af_sediment(Me, S) result(Af_sediment)
        class(AbstractBedSediment), intent(in) :: Me                         !! the AbstractBedSediment instance
        integer, intent(in) :: S                                     !! size class
        real(dp) :: Af_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        ! character(len=14) :: tr                                      ! LOCAL error trace
        ! tr  = trim(Me%name // "Get_Af_sediment")
        ! if (S < 0) then
        !     call r%addError(ErrorInstance( &
        !                     code = 103, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S < 0
        ! end if
        ! if (r%hasCriticalError()) then                               ! if a critical error has been thrown
        !     call r%addToTrace(tr)                                    ! add trace to Result
        !     return                                                   ! and exit
        ! end if
        ! if (S > Me%nSizeClasses) then
        !     call r%addError(ErrorInstance( &
        !                     code = 104, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S > nSizeClasses
        ! end if
        ! if (r%hasCriticalError()) return                             ! exit if error thrown
        Af_sediment = 0
        do L = 1, C%nSedimentLayers                                             ! loop through each layer
            Af_sediment = Af_sediment + Me%colBedSedimentLayers(L)%item%A_f(S)  ! sum capacities for all layers
        end do
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
    function Get_Cf_sediment(Me, S) result(Cf_sediment)
        class(AbstractBedSediment), intent(in) :: Me                         !! The `AbstractBedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        ! type(Result0D) :: r                                          !! Return value
        real(dp) :: Cf_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        ! character(len=14) :: tr                                      ! LOCAL error trace
        ! tr = trim(Me%name // "Get_Cf_sediment")
        ! if (S < 0) then
        !     call r%addError(ErrorInstance( &
        !                     code = 103, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S < 0
        ! end if
        ! if (r%hasCriticalError()) then                               ! if a critical error has been thrown
        !     call r%addToTrace(tr)                                    ! add trace to Result
        !     return                                                   ! and exit
        ! end if
        ! if (S > Me%nSizeClasses) then
        !     call r%addError(ErrorInstance( &
        !                     code = 104, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S > nSizeClasses
        ! end if
        ! if (r%hasCriticalError()) return                             ! exit if error thrown
        Cf_sediment = 0
        do L = 1, C%nSedimentLayers                                         ! loop through each layer
            Cf_sediment = Cf_sediment + Me%colBedSedimentLayers(L)%item%C_f(S)          ! sum capacities for all layers
        end do
        ! r = Result(data = Cf_sediment)
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
    function Get_Aw_sediment(Me, S) result(Aw_sediment)
        class(AbstractBedSediment), intent(in) :: Me                         !! The `AbstractBedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        ! type(Result0D) :: r                                          !! Return value
        real(dp) :: Aw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        ! character(len=14) :: tr                                      ! LOCAL error trace
        ! tr = trim(Me%name // "Get_Aw_sediment")
        ! if (S < 0) then
        !     call r%addError(ErrorInstance( &
        !                     code = 103, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S < 0
        ! end if
        ! if (r%hasCriticalError()) then                               ! if a critical error has been thrown
        !     call r%addToTrace(tr)                                    ! add trace to Result
        !     return                                                   ! and exit
        ! end if
        ! if (S > Me%nSizeClasses) then
        !     call r%addError(ErrorInstance( &
        !                     code = 104, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S > nSizeClasses
        ! end if
        ! if (r%hasCriticalError()) return                             ! exit if error thrown
        Aw_sediment = 0
        do L = 1, C%nSedimentLayers                                         ! loop through each layer
            Aw_sediment = Aw_sediment + Me%colBedSedimentLayers(L)%item%A_w(S)  ! sum capacities for all layers
        end do
        ! r = Result(data = Aw_sediment)
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
    function Get_Cw_sediment(Me, S) result(Cw_sediment)
        class(AbstractBedSediment), intent(in) :: Me                         !! The `AbstractBedSediment` instance
        integer, intent(in) :: S                                     !! Size class
        ! type(Result0D) :: r                                          !! Return value
        real(dp) :: Cw_sediment                                      ! LOCAL internal storage
        integer :: L                                                 ! LOCAL loop counter
        ! character(len=14) :: tr                                      ! LOCAL error trace  
        ! tr = trim(Me%name // "Get_Cw_sediment")
        ! if (S < 0) then
        !     call r%addError(ErrorInstance( &
        !                     code = 103, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S < 0
        ! end if
        ! if (r%hasCriticalError()) then                               ! if a critical error has been thrown
        !     call r%addToTrace(tr)                                    ! add trace to Result
        !     return                                                   ! and exit
        ! end if
        ! if (S > Me%nSizeClasses) then
        !     call r%addError(ErrorInstance( &
        !                     code = 104, &
        !                     message = "The size class is invalid", &
        !                       trace = [tr] &
        !                                  ) &
        !                    )                                         ! CRITICAL ERROR if S > nSizeClasses
        ! end if
        ! if (r%hasCriticalError()) return                             ! exit if error thrown
        Cw_sediment = 0
        do L = 1, C%nSedimentLayers                                         ! loop through each layer
            Cw_sediment = Cw_sediment + Me%colBedSedimentLayers(L)%item%C_w(S)          ! sum capacities for all layers
        end do
        ! r = Result(data = Cw_sediment)
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
        class(AbstractBedSediment), intent(in) :: Me                         !! The `AbstractBedSediment` instance
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
        do L = 1, C%nSedimentLayers                                         ! loop through each layer
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
    function Get_Mf_bed_all(Me) result(Mf)
        class(AbstractBedSediment), intent(in) :: Me                 !! The `AbstractBedSediment` instance
        real(dp) :: Mf                                               ! Internal storage
        integer :: l                                                 ! Iterator
        Mf = 0
        do l = 1, C%nSedimentLayers                                  ! loop through each layer
            Mf = Mf + me%colBedSedimentLayers(l)%item%M_f_layer()    ! sum masses across layers
        end do
    end function

    !> Get mass of fine sediment for layer l [kg/m2]
    function get_Mf_bed_by_layer(me, l) result(Mf)
        class(AbstractBedSediment), intent(in)  :: me
        integer                         :: l
        real(dp)                        :: Mf
        Mf = me%colBedSedimentLayers(l)%item%M_f_layer()
    end function

    !> Get mass of fine sediment as array of layers [kg/m2]
    function get_Mf_bed_layer_array(me) result(Mf)
        class(AbstractBedSediment), intent(in)  :: me
        integer                         :: l            ! Layer
        real(dp)                        :: Mf(C%nSedimentLayers)
        do l = 1, C%nSedimentLayers 
            Mf(l) = me%colBedSedimentLayers(l)%item%M_f_layer()
        end do
    end function

    function get_V_w_by_layer(me) result(V_w)
        class(AbstractBedSediment), intent(in) :: me
        real(dp) :: V_w(C%nSedimentLayers)
        integer :: i
        do i = 1, C%nSedimentLayers
            V_w(i) = me%colBedSedimentLayers(i)%item%V_w_layer()
        end do
    end function
    
    function Get_Mf_bed_by_size(Me) result(Mf_size)
        class(AbstractBedSediment), intent(in) :: Me                         !! The AbstractBedSediment instance
        integer :: L                                                 ! LOCAL loop counter
        integer :: S                                                 ! LOCAL loop counter
        real(dp) :: Mf                                               ! LOCAL internal storage
        real(dp) :: Mf_size(Me%nSizeClasses)                         ! LOCAL 1D array to hold masses by size fraction
        do S = 1, Me%nSizeClasses                                    ! for each size class
            Mf = 0                                                   ! initialise sumnation of mass
            do L = 1, C%nSedimentLayers                                     ! loop through each layer
                Mf = Mf + &
                    Me%colBedSedimentLayers(L)%item%colFineSediment(S)%M_f()
                                                                     ! sum masses across all layers. Not very elegant
            end do
            Mf_size(S) = Mf                                          ! assign to array for output
        end do
    end function

    function get_m_contaminant(me) result(r)
        class(AbstractBedSediment), intent(in) :: me
        type(Result0D) :: r
        type(Contaminant) :: m_contaminant
        type(Result) :: res
        integer :: i
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        res = m_contaminant%create()
        if (res%hasCriticalError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        do i = 3, C%nSedimentLayers+2
            call m_contaminant%add(me%m_contaminant(i))
        end do
        allocate(r%data, source=m_contaminant)
        call r%setErrors()
    end function

    function get_C_contaminant(me) result(r)
        class(AbstractBedSediment), intent(in) :: me
        type(Result3D) :: r
        type(Contaminant) :: m_contaminant
        real(dp), allocatable :: C_contaminant(:,:,:)
        type(Result0D) :: res
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        res = me%get_m_contaminant()
        if (res%hasError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        select type (data => res%getData())
            type is (Contaminant)
                m_contaminant = data
            class default
                call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                return
        end select
        allocate(C_contaminant(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
        if (sum(C%sedimentLayerDepth) > C%epsilon) then
            C_contaminant = m_contaminant%c / sum(C%sedimentLayerDepth)
        else
            C_contaminant = 0.0_dp
        end if
        allocate(r%data, source=C_contaminant)
        call r%setErrors()
        deallocate(C_contaminant)
    end function

    function get_C_contaminant_byMass(me) result(r)
        class(AbstractBedSediment), intent(in) :: me
        type(Result3D) :: r
        real(dp), allocatable :: C_contaminant_byMass(:,:,:)
        real(dp) :: layerMasses(C%nSedimentLayers)
        type(Contaminant) :: m_contaminant_l
        type(Result0D) :: res
        integer :: i
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        allocate(C_contaminant_byMass(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
        C_contaminant_byMass = 0.0_dp
        do i = 1, C%nSedimentLayers
            layerMasses(i) = me%Mf_bed_by_layer(i)
            res = me%get_m_contaminant_l(i)
            if (res%hasError()) then
                call r%addErrors(res%getErrors())
                deallocate(C_contaminant_byMass)
                return
            end if
            select type (data => res%getData())
                type is (Contaminant)
                    m_contaminant_l = data
                class default
                    call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                    deallocate(C_contaminant_byMass)
                    return
            end select
            if (layerMasses(i) > C%epsilon) then
                C_contaminant_byMass = C_contaminant_byMass + m_contaminant_l%c / layerMasses(i)
            end if
        end do
        if (sum(layerMasses) > C%epsilon) then
            C_contaminant_byMass = C_contaminant_byMass / sum(layerMasses)
        else
            C_contaminant_byMass = 0.0_dp
        end if
        allocate(r%data, source=C_contaminant_byMass)
        call r%setErrors()
        deallocate(C_contaminant_byMass)
    end function

    function get_m_contaminant_l(me, l) result(r)
        class(AbstractBedSediment), intent(in) :: me
        integer, intent(in) :: l
        type(Result0D) :: r
        type(Contaminant) :: m_contaminant_l
        type(Result) :: res
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        if (l < 1 .or. l > C%nSedimentLayers) then
            call r%addError(ErrorInstance(code=106, message="Invalid layer index"))
            return
        end if
        res = m_contaminant_l%create()
        if (res%hasCriticalError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        call m_contaminant_l%add(me%m_contaminant(2+l))
        allocate(r%data, source=m_contaminant_l)
        call r%setErrors()
    end function

    function get_C_contaminant_l(me, l) result(r)
        class(AbstractBedSediment), intent(in) :: me
        integer, intent(in) :: l
        type(Result3D) :: r
        real(dp), allocatable :: C_contaminant_l(:,:,:)
        type(Contaminant) :: m_contaminant_l
        type(Result0D) :: res
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        if (l < 1 .or. l > C%nSedimentLayers) then
            call r%addError(ErrorInstance(code=106, message="Invalid layer index"))
            return
        end if
        res = me%get_m_contaminant_l(l)
        if (res%hasError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        select type (data => res%getData())
            type is (Contaminant)
                m_contaminant_l = data
            class default
                call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                return
        end select
        allocate(C_contaminant_l(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
        if (C%sedimentLayerDepth(l) > C%epsilon) then
            C_contaminant_l = m_contaminant_l%c / C%sedimentLayerDepth(l)
        else
            C_contaminant_l = 0.0_dp
        end if
        allocate(r%data, source=C_contaminant_l)
        call r%setErrors()
        deallocate(C_contaminant_l)
    end function

    function get_C_contaminant_l_byMass(me, l) result(r)
        class(AbstractBedSediment), intent(in) :: me
        integer, intent(in) :: l
        type(Result3D) :: r
        real(dp), allocatable :: C_contaminant_l_byMass(:,:,:)
        type(Contaminant) :: m_contaminant_l
        real(dp) :: layerMass
        type(Result0D) :: res
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        if (l < 1 .or. l > C%nSedimentLayers) then
            call r%addError(ErrorInstance(code=106, message="Invalid layer index"))
            return
        end if
        res = me%get_m_contaminant_l(l)
        if (res%hasError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        select type (data => res%getData())
            type is (Contaminant)
                m_contaminant_l = data
            class default
                call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                return
        end select
        allocate(C_contaminant_l_byMass(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
        layerMass = me%Mf_bed_by_layer(l)
        if (layerMass > C%epsilon) then
            C_contaminant_l_byMass = m_contaminant_l%c / layerMass
        else
            C_contaminant_l_byMass = 0.0_dp
        end if
        allocate(r%data, source=C_contaminant_l_byMass)
        call r%setErrors()
        deallocate(C_contaminant_l_byMass)
    end function

    function get_m_contaminant_buried(me) result(r)
        class(AbstractBedSediment), intent(in) :: me
        type(Result0D) :: r
        type(Contaminant) :: m_contaminant_buried
        type(Result) :: res
        if (.not. allocated(me%m_contaminant)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        res = m_contaminant_buried%create()
        if (res%hasCriticalError()) then
            call r%addErrors(res%getErrors())
            return
        end if
        call m_contaminant_buried%add(me%m_contaminant(C%nSedimentLayers+3))
        allocate(r%data, source=m_contaminant_buried)
        call r%setErrors()
    end function

    subroutine finaliseBedSediment(me)
        class(AbstractBedSediment), intent(inout) :: me
        integer :: i
        if (allocated(me%m_contaminant)) then
            do i = 1, size(me%m_contaminant)
                call me%m_contaminant(i)%finalise()
            end do
            deallocate(me%m_contaminant)
        end if
        if (allocated(me%colBedSedimentLayers)) deallocate(me%colBedSedimentLayers)
        if (allocated(me%delta_sed)) deallocate(me%delta_sed)
        if (allocated(me%delta_sed_csr)) deallocate(me%delta_sed_csr)
    end subroutine
end module                     
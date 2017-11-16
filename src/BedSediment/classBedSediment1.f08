module classBedSediment1                                             ! class definition for BedSediment1
    use Globals
    use ResultModule
    use ResultFineSedimentModule
    use spcBedSediment                                               ! use BedSediment superclass
    use classBedSedimentLayer1
    implicit none                                                    ! force declaration of all variables
    private

    type, public, extends(BedSediment) :: &
        BedSediment1                                                 ! type declaration for class - extends abstract superclass
      contains
        procedure, public :: create => createBedSediment1            ! constructor method
        procedure, public :: destroy => destroyBedSediment1          ! finaliser method
        procedure, public :: deposit => DepositSediment1             ! deposit sediment from water column
        procedure, public :: resuspend => ResuspendSediment1         ! resuspend sediment to water column
    end type
  contains
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
    ! function createBedSediment1(Me, x, y, s, b, riverReachGroup) result(r)
    function createBedSediment1(Me, riverReachGroup) result(r)
        class(BedSediment1) :: Me                                    !! self-reference
!        integer :: x, y, s, b                                        !! References to containing GridCell, SubRiver and RiverReaches
        type(NcGroup), intent(in) :: riverReachGroup                 !! NetCDF group reference to the RiverReach containing this object
        type(Result) :: r                                            !! returned Result object
        type(NcGroup) :: grp                                         ! LOCAL NetCDF group reference
        integer, allocatable :: bslType(:)                           ! LOCAL the type identification number of the BedSedimentLayer(s)
        type(BedSedimentLayer1), allocatable :: bsl1                 ! LOCAL object of type BedSedimentLayer1, for implementation of polymorphism
        integer :: L                                                 ! LOCAL loop counter
        integer :: allst                                             ! LOCAL array allocation status
        type(NcVariable) :: var                                      ! LOCAL variable to retrieve NetCDF data from
        character(len=256) :: tr                                     ! LOCAL error trace
        character(len=16), parameter :: ms = "Allocation error"      ! LOCAL allocation error message
        !
        ! Notes
        ! ----------------------------------------------------------------------------------
        ! no notes
        ! ----------------------------------------------------------------------------------
        Me%name = trim(ref(riverReachGroup%getName, "BedSediment")   ! object name: RiverReach_x_y_s_r_BedSediment
        Me%ncGroup = riverReachGroup%getGroup("BedSediment")         ! get the BedSediment group name
!        Me%name = trim(ref("BedSediment", x, y, s, b))               ! object name
!        Me%ncGroup = riverReachGroup%getGroup(trim(Me%name))         ! get the BedSediment_x_y_s_r group
        Me%nSizeClasses = C%nSizeClassesSpm                          ! set number of size classes from global value
        Me%nfComp = C%nFracCompsSpm                                  ! set number of compositional fractions from global value
        tr = Me%name // "%createBedSediment1"                        ! procedure name as trace
        var = Me%ncGroup%getVariable("nLayers")                      ! Get the number of BedSedimentLayers
        call var%getData(Me%nLayers)                                 ! retrieve into nLayers variable
        ! call r%addErrors(.errors. Me%setLayers(Me%nLayers))          ! set number of layers and allocate layer collection
        if (Me%nLayers <= 0) then                                    ! invalid number of layers
            call r%addError(ErrorInstance(code = 1, &
                               message = "Invalid number of &
                                          BedSedimentLayers", &
                                 trace = [tr] &
                                         ) &
                           )                                         ! add to Result
            return                                                   ! critical error, so return
        end if
        allocate(Me%colBedSedimentLayers(Me%nLayers), stat = allst)  ! create BedSedimentLayer collection
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                                          message = ms, &
                                          trace = [tr] &
                                         ) &
                           )                                         ! add to Result
            return                                                   ! critical error, so return
        end if
        allocate(bslType(Me%nLayers), stat = allst)                  ! create bslType collection
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                                          message = ms, &
                                          trace = [tr] &
                                         ) &
                           )                                         ! add to Result
            return                                                   ! critical error, so return
        end if
        var = Me%ncGroup%getVariable("layerType")                    ! Get the BedSedimentLayer type number
        call var%getData(bslType)                                    ! retrieve into bslType variable
        do L = 1, Me%nLayers                                         ! loop through each layer
            associate(O => Me%colBedSedimentLayers(L)%item)
                select case (bslType(L))                             ! loop through possible BedSedimentLayer types
                    case(1)                                          ! type number 1
                        allocate(bsl1, stat = allst)                 ! allocate empty object of this type
                        if (allst /= 0) then
                            call r%addError(ErrorInstance( &
                                               code = 1, &
                                            message = ms, &
                                              trace = [tr]))         ! add to Result
                            return                                   ! critical error, so return
                        end if
                        if (r%hasCriticalError()) then               ! if a critical error has been thrown
                            call r%addToTrace(tr)                    ! add trace to Result
                            return                                   ! exit, as a critical error has occurred
                        end if
                        call move_alloc(bsl1, &
                            Me%colBedSedimentLayers(L)%item)         ! move bsl1 object into layers collection
                    case default                                     ! invalid BedSedimentLayer type specified
                        call r%addError(ErrorInstance(code = 1, &
                                    message = "Invalid &
                                               BedSedimentLayer &
                                               object type &
                                               specified" &
                                                     ) &
                                       )                             ! add ErrorInstance
                        call r%addToTrace(tr)                        ! add trace to Result
                        return                                       ! critical error, so exit
                end select
                grp = Me%ncGroup%getGroup(trim(ref("Layer", L)))     ! Get the layer group
                call r%addErrors(.errors. O%create(Me%name, grp))    ! initialise the layer object
                if (r%hasCriticalError()) then                       ! if a critical error has been thrown
                    call r%addToTrace(tr)                            ! add trace to Result
                    return                                           ! exit, as a critical error has occurred
                end if
            end associate
        end do
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
    function destroyBedSediment1(Me) result(r)
        class(BedSediment1) :: Me                                    !! self-reference
        type(Result) :: r                                            !! returned Result object
        type(ErrorInstance) :: er                                    ! LOCAL ErrorInstance object for error handling.
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        integer :: L                                                 ! LOCAL Loop iterator
        integer :: allst                                             ! LOCAL array allocation status
        character(len=18), parameter :: ms = "Deallocation error"    ! LOCAL CONSTANT error message
        !
        ! Notes
        ! ----------------------------------------------------------------------------------
        !
        ! no notes
        ! ----------------------------------------------------------------------------------
        do L = 1, Me%nLayers
            call r%addErrors(.errors. &
                Me%colBedSedimentLayers(L)%item%destroy())           ! destroy enclosed BedSedimentLayers
        end do
        tr = Me%name // &
            "%destroyBedSedimentLayer1%colBedSedimentLayers"         ! trace message
        deallocate(Me%colBedSedimentLayers, stat = allst)            ! deallocate all allocatable variables
        if (allst /= 0) then
            er = ErrorInstance(code = 1, &
                               message = ms, &
                               trace = [tr] &
                              )                                      ! create warning if error thrown
            call r%addError(er)                                      ! add to Result
        end if
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
    !! r returns resuspended fine sediments as type ResultFineSediment2D
    !!
    !! ----------------------------------------------------------------------------------
    function resuspendSediment1(Me, M_resusp) result(r)
        class(BedSediment1) :: Me                                    !! self-reference
        real(dp), allocatable :: M_resusp(:)                         !! sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
        type(ResultFineSediment2D) :: r                              !! returned Result object. Type = FineSediment
        type(FineSediment1), allocatable :: FS(:,:)                  ! LOCAL resuspended fine sediment. Index 1 = size class, Index 2 = layer
!        type(FineSediment1), allocatable :: F                        ! LOCAL FineSediment object representing material that has been resuspended
        type(FineSediment1), allocatable :: G                        ! LOCAL FineSediment object representing material to be resuspended
        type(ErrorInstance) :: er                                    ! LOCAL error instance
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: allst                                             ! LOCAL array allocation status
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        type(ResultFineSediment1D1D) :: r1D                          ! LOCAL temporary variable for storing Result with 1D data in
!       class(*), allocatable :: data1D(:)                           ! LOCAL temporary variable to store polymorphic data in to use in select type
        !
        ! Notes
        ! -----------------------------------------------------------------------------------------
        ! do the references to Me%nSizeClasses and Me%nLayers preclude making this a pure function?
        ! -----------------------------------------------------------------------------------------
        tr = Me%name // "ResuspendSediment1"                         ! error trace for this procedure
        if (size(M_resusp) /= Me%nSizeClasses) then                  ! number of size classes must be consistent
            call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "Number of resuspended &
                                      sediment classes does not &
                                      match number of size classes &
                                      in sediment", &
                            trace = [tr] &
                                         ) &
                           )                                         ! create error instance
            return
        end if                                                       ! exit if a critical error has been thrown
!        allocate(F, stat = allst)                                    ! set up FineSediment1 variable F
!        if (allst /= 0) then
!            r%AddError = ErrorInstance(code = 1, &
!                               message = "Allocation error", &
!                               trace = [Me%name // &
!                                        "%resuspendSediment1%F"] &
!                              )                                      ! create  if error thrown
!        end if
        allocate(G, stat = allst)                                    ! set up FineSediment1 variable G
        if (allst /= 0) then
            r%AddError = ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name // &
                                        "%resuspendSediment1%G"] &
                              )                                      ! create  if error thrown
        end if
        allocate(FS(Me%nSizeClasses, Me%nLayers), stat = allst)      ! set up FineSediment1 variable FS
        if (allst /= 0) then
            r%AddError = ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name // &
                                        "%resuspendSediment1%FS"] &
                              )                                      ! create  if error thrown
        end if
        if (r%hasCriticalError()) return                             ! exit if allocation error thrown
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            call r%addErrors(.errors. G%set(Mf_in = M_resusp(S) &
                                           ) &
                            )                                        ! top layer: set up the temporary object G, with the resuspended mass
            if (r%hasCriticalError()) then
                call r%addToTrace(tr)
                return                                               ! exit if a critical error has been thrown
            end if
            L = 1                                                    ! start with top layer
            associate(O => Me%colBedSedimentLayers(L)%item)          ! association for brevity
                do while (M_resusp(S) > 0 .and. L <= Me%nLayers)     ! loop through layers until all sediment resuspended or all layers considered
                    call r%addErrors(.errors. &
                         G%set(Vw_in = M_resusp(S) / O%volSLR() &
                              ) &
                                    )                                ! add the water content to G
                                                                     ! and the volume of water to be resuspended along with the fine sediment
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        return                                       ! exit if a critical error has been thrown
                    end if
                    r1D = O%removeSediment(S, G)                     ! remove the resuspended sediment
                    call r%addErrors(.errors. r1D)                   ! add any errors
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        return                                       ! exit if a critical error has been thrown
                    end if
                    FS(L, S) = .finesediment. r1D                    ! retrieve FineSediment object from result, contains sediment that was resuspended
!                    allocate(data1D, source=r1D%getData())           ! Get the data from r0D to retrieve in select type
!                    select type (data => data1D(1))                  ! Put the resuspended sediment into F
!                        type is (FineSediment1)
!                            F = data
!                        class default
!                    end select
!                    call r%addErrors(.errors. &
!                         FS(L, S)%set(Mf_in = F%M_f(), &
!                                      Vw_in = F%V_w(), &
!                                      f_comp_in = F%f_comp &
!                                     ) &
!                                    )                                ! set resuspended fine sediment mass, water volume and fractional composition
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        return                                       ! exit if a critical error has been thrown
                    end if
                    M_resusp(S) = M_resusp(S) - FS(L, S)%M_f()       ! keep count of fine sediment that has been resuspended
                    L = L + 1                                        ! Repeat until all sediment has been
                end do                                               ! resuspended, or sediment has been removed from all layers
                if (M_resusp(S) > 0) then
                    call r%addError(ErrorInstance(1, &
                         "All sediment of size class " &
                         // trim(str(S)) // " resuspended", &
                         .false., &
                         [tr] &
                                                 ) &
                                   )                                 ! warning (noncritical error) if bed has been stripped of size class S
                end if
            end associate
        end do
        ! SL: these lines not needed as local variables automatically deallocated on exit?
!        deallocate(F, stat = allst)                                  ! deallocate FineSediment1 variable F
!        if ( allst /= 0) then
!            er = ErrorInstance(code = 1, &
!                               message = "Deallocation error", &
!                               trace = [Me%name // &
!                                        "%resuspendSediment1%F"] &
!                              )                                      ! create warning if error thrown
!            call r%addError(er)                                      ! add to Result
!        end if
!        deallocate(G, stat = allst)                                  ! deallocate FineSediment1 variable G
!        if ( allst /= 0) then
!            er = ErrorInstance(code = 1, &
!                               message = "Deallocation error", &
!                               trace = [Me%name // &
!                                        "%resuspendSediment1%G"] &
!                              )                                      ! create warning if error thrown
!            call r%addError(er)                                      ! add to Result
!        end if
        r = Result(data = FS)                                        ! copy output to Result
    end function
    !> compute deposition to bed sediment, including burial and downward shifting of fine sediment and water
    !
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
    function depositSediment1(Me, FS_dep) result (r)
        class(BedSediment1) :: Me                                    !! self-reference
        type(FineSediment1), allocatable :: FS_dep(:)                !! Depositing sediment by size class
        type(Result0D) :: r                                          !! Result object. Returns water requirement from the water column [m3 m-2], real(dp)
        real(dp) :: V_w_tot                                          ! LOCAL water requirement from the water column [m3 m-2]
        type(ResultFineSediment0D) :: r0D                            ! LOCAL Result0D object to return data from addSediment method
        type(ResultFineSediment1D) :: r1D                            ! LOCAL ResultFineSediment1D object to return data from removeSediment method
        type(ErrorInstance) :: er                                    ! LOCAL ErrorInstance object for error handling
        type(FineSediment1), allocatable :: DS(:)                    ! LOCAL FineSediment objects holding deposited material
        type(FineSediment1), allocatable :: T                        ! LOCAL object to receive sediment being buried
        type(FineSediment1), allocatable :: U                        ! LOCAL object to receive sediment that has been buried
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: A                                                 ! LOCAL second counter for layers
        integer :: allst                                             ! LOCAL array allocation status
        real(dp) :: A_f_sed                                          ! LOCAL available fine sediment capacity for size class
        real(dp) :: tempV                                            ! LOCAL volume variable
        real(dp) :: V_f_b                                            ! LOCAL available fine sediment capacity in the receiving layer [m3 m-2]
        real(dp) :: V_w_b                                            ! LOCAL available water capacity in the receiving layer [m3 m-2]
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
!        class(*), allocatable :: data1D(:)                           ! LOCAL temporary polymorphic data variable
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
        ! 2.    The FineSediment objects in FS_dep should not contain any water, but if they
        !       do it is not a problem as it will be overwritten.
        ! -------------------------------------------------------------------------------
        tr = trim(Me%name) // "%DepositSediment1"                    ! object and procedure binding name as trace
        if (size(FS_dep) /= Me%nSizeClasses) &
            call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "The number of fine &
                                       sediment masses does &
                                       not equal the number &
                                       of size classes", &
                              trace = [tr] &
                                         ) &
                           )                                         ! CRITICAL ERROR if size(FS_dep) <> nSizeClasses
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        allocate(DS(Me%nSizeClasses), stat = allst)                  ! allocate space for FineSediment1 objects
        if (allst /= 0) then
            er = ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%DS" &
                              )                                      ! create error
            call r%addError(er)                                      ! add to Result
        end if
        allocate(T, stat = allst)                                    ! set up T
        if (allst /= 0) then
            er = ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                              )                                      ! create error
            call r%addError(er)                                      ! add to Result
        end if
        allocate(U, stat = allst)                                    ! set up U
        if (allst /= 0) then
            er = ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                              )                                      ! create error
            call r%addError(er)                                      ! add to Result
        end if
        if (r%hasCriticalError()) return                             ! exit if allocation error thrown
!        do S = 1, Me%nSizeClasses                                    ! compose FineSediment1 objects from the inputs
!            call r%addErrors(.errors. &
!                 DS(S)%set(Mf_in = FS_dep(S)%M_f, &
!                       f_comp_in = FS_dep(S)%f_comp &
!                          ) &
!                            )                                        ! populate DS with depositing sediment and its fractional composition
!        end do
        do S = 1, Me%nSizeClasses
            call r%addError(.errors. FS_dep(S)%audit_comp())         ! audits fractional composition of deposition, returns error instance
            if (r%hasCriticalError()) then                           ! if fcomp_audit throws a critical error
                call r%addToTrace(tr)                                ! add trace to all errors
                return                                               ! and exit
            end if
        end do
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            if (int(FS_dep(S)%V_f() / Me%Cf_sediment(S)) > 0) then   ! check whether the depositing sediment in each size class exceeds the total
                do L = 1, Me%nLayers                                 ! capacity in the layer. If so, then remove all fine sediment, water and
                    call Me%colBedSedimentLayers(L)%item%clearAll()  ! fractional compositions from all layers for this size class
                    ! TODO: tally up the sediment being buried at this point
                end do
            end if
        end do
        do S = 1, Me%nSizeClasses
            A_f_sed = Me%Af_sediment(S)                              ! local copy of the capacity for this sediment size class in the whole bed
            if (FS_dep(S)%V_f() > A_f_sed) then                      ! do we need to bury sediment to create available capacity for deposition?
                call r%addErrors(.errors. &
                             T%set(Vf_in = FS_dep(S)%V_f() - A_f_sed, &
                                   Vw_in = 0.0_dp, &
                                   f_comp_in = FS_dep(S)%f_comp &
                                  ) &
                                )                                    ! set up temporary FineSediment object with volume of fine sediment requiring burial
                                                                     ! to compute the volume of water requiring burial, we must loop through layers
                                                                     ! from the top, computing for each layer the volume of fine sediment that must be
                                                                     ! removed to allow space for deposition, and the volume of water associated with the
                                                                     ! fine sediment
                L = 1                                                ! loop through layers, downwards from the top
                do while (T%V_f() > 0)                               ! use fine sediment volume in T as a counter
                    associate (O => Me%colBedSedimentLayers(L)%item) ! association to layer L
                        if (T%V_f() > O%C_f(S)) then                 ! does the depositing fine sediment fit into this layer,
                                                                     ! after accounting for the capacity in layers above?
                                                                     ! no, so increase water removal requirement by the water capacity of this layer
                                                                     ! and decrease the count of remaining depositing fine sediment by the capacity
                            call r%addErrors(.errors. &
                                 T%set(Vf_in = T%V_f() - O%C_f(S), &
                                       Vw_in = T%V_w() + O%C_w(S) &
                                      ) &
                                            )
                        else                                         ! yes,
                                                                     ! so increase the water burial requirement by the amount required to maintain the SLR in this layer
                                                                     ! and set the count of fine sediment to zero, to jump out of the loop
                            tempV = T%V_f() / O%volSLR()             ! temporary variable
                            call r%addErrors(.errors. &
                                 T%set(Vf_in = 0.0_dp, &
                                       Vw_in = T%V_w() + tempV &
                                      ) &
                                            )
                        end if
                    end associate
                    L = L + 1
                end do
                                                                     ! now to actually bury fine sediment and water
                call r%addErrors(.errors. &
                     T%set(Vf_in = FS_dep(S)%V_f() - A_f_sed &
                          ) &
                                )                                    ! reset the fine sediment burial requirement, still using temporary object T
                if (r%hasCriticalError()) return                     ! return if critical error thrown
                L = Me%nLayers                                       ! start with the bottom layer
                do while (L > 0 .and. T%V_f() + T%V_w() > 0)         ! loop through each layer, while there is still material to bury
                    if (T%V_f() > 0) Then
                        associate(O => &
                            Me%colBedSedimentLayers(L)%item)         ! association reference to layer L object
                            r1D = O%RemoveSediment(S, T)             ! remove the sediment, return amounts removed and not removed, and any errors thrown
                            call r%addErrors(.errors. r1D)           ! retrieve errors into main Result object
                            if (r%hasCriticalError()) then           ! if RemoveSediment throws a critical error
                                call r%addToTrace(tr)                ! add trace to all errors
                                return                               ! and exit
                            end if
                            T = .finesediment. r1D                   ! assign T to return value from removeSediment
!                            allocate(data1D, source=r1D%getData())   ! getData(array_index) doesn't work, so must store data in another var before using select type
!                            select type (data => data1D(2))          ! select type construct needed to get around casting constraints
!                                type is (FineSediment1)
!                                    T = data                         ! return sediment that could not be removed
!                                class default                        ! no need to check for default, as type can only be FineSediment1
!                            end select
                        end associate
                    end If
                    L = L - 1                                        ! move up to next layer
                end do                                               ! finished burial. temporary object T can be reused
                do L = Me%nLayers, 2, -1                             ! downward shift of fine sediment. Loop through the layers, starting at the bottom
                                                                     ! and working upwards
                    assoc1 : associate &
                        (O => Me%colBedSedimentLayers(L)%item)       ! association to "receiving" layer L
                        A = L - 1                                    ! counter for donating layers - initially the layer above
                        call r%addErrors(.errors. &
                            T%set(Vf_in = O%A_f(S), &
                                  Vw_in = O%A_w(S) &
                                 ) &
                                        )                            ! set FineSediment object T to hold sediment requiring removal. Note f_comp has been set previously, no need to set again
                        do while (A > 0 .or. &
                                    T%IsEmpty() .eqv. .false.)       ! loop through "donating" layers, moving upwards
                            assoc2 : associate (P => &
                            Me%colBedSedimentLayers(A)%item)         ! association to "donating" layer A
                            if (P%colFineSediment(S)%V_f() > 0) &
                                then                                 ! if there is sediment in the "donating" layer
                                r1D = O%RemoveSediment(S, T)         ! remove the sediment, return amounts removed and not removed, and any errors thrown
                                call r%addErrors(.errors. r1D)       ! retrieve errors into main Result object
                                if (r%hasCriticalError()) then       ! if RemoveSediment throws a critical error
                                    call r%addToTrace(tr)            ! add trace to all errors
                                    return                           ! and exit
                                end if
                                ! SL: trying to be concise with the next two commands,
                                !     but unsure whether they will work
                                U = .finesediment. r1D(1)            ! sediment removed - to be added to Layer L
                                T = .finesediment. r1D(2)            ! sediment not removed - *** should be none ***
                                ! SH: Couldn't get these select types working to be able to select
                                ! specific array elements in the `type is` section, so had to split to
                                ! do each array element separately.
!                                select type (data => data1D(1))  ! select type construct needed to get around casting constraints
!                                    type is (FineSediment1)
!                                        U = data                 ! sediment removed - to be added to Layer L
!                                    class default                ! no need to check for default as type can only be FineSediment1
!                                end select
!                                select type (data => data1D(2))  ! select type construct needed to get around casting constraints
!                                    type is (FineSediment1)
!                                        T = data                 ! sediment not removed
!                                    class default                ! no need to check for default as type can only be FineSediment1
!                                end select
                                r0D = O%addSediment(S, U)            ! add the sediment in U to the "receiving" layer
                                ! SL: r0D returns fine sediment that could not be added - *** should be none ***
                                call r%addErrors(.errors. r0D)       ! retrieve errors into main Result object
                                if (r%hasCriticalError()) then       ! if RemoveSediment throws a critical error
                                    call r%addToTrace(tr)            ! add trace to all errors
                                    return                           ! and exit
                                end if
                            end if
                            ! TODO: tally up removed sediment from U on each loop
                            A = A - 1                                ! shift up to next "donating" layer
                            end associate assoc2
                        end do
                    end associate assoc1
                end do
            end if
        end do
        do S = 1, Me%nSizeClasses                                    ! deposit sediment from the water column
            L = Me%nLayers                                           ! start with the bottom layer and work upwards
            do
                associate(O => Me%colBedSedimentLayers(L)%item)      ! size class S in Layer L
                    if (O%A_f(S) > 0 .or. O%A_w(S) > 0) then         ! if there is available capacity in this layer, add deposition here
                        V_w_b = FS_dep(S)%V_f() / O%volSLR()         ! the volume of water needed to maintain SLR in the "receiving" layer,
                        call r%addErrors(.errors. &
                            FS_dep(S)%set(Vw_in = V_w_b))            ! if all deposition fits into this layer
                        r0D = O%addSediment(S, FS_dep(S))            ! add the fine sediment in deposition. r0D returns volumes that could not be added
                        call r%addErrors(.errors. r0D)               ! retrieve errors into main Result object
                            if (r%hasCriticalError()) then           ! if addSediment throws a critical error
                                call r%addToTrace(tr)                ! add trace to all errors
                                return                               ! and exit
                            end if
                        select type (data => r0D%getData())          ! select type construct needed to get around casting constraints
                            type is (FineSediment1)
                                FS_dep(S) = data                     ! sediment not added
                            class default                            ! no need to check for default as type can only be FineSediment1
                        end select
                    end if
                    V_w_tot = V_w_tot + V_w_b - FS_dep(S)%V_w()      ! tally up V_w_b to compute water requirement to take from the water column
                    L = L - 1
                end associate
            end do
        end do
!        deallocate(T, stat = allst)                                  ! deallocate T
!        if (allst /= 0) then
!            er = ErrorInstance(code = 1, &
!                               message = "Deallocation error", &
!                               trace = [Me%name] // &
!                                        "%depositSediment1%T" &
!                              )                                      ! create warning if error thrown
!            call r%addError(er)                                      ! add to Result
!        end if
!        deallocate(U, stat = allst)                                  ! deallocate U
!        if (allst /= 0) then
!            er = ErrorInstance(code = 1, &
!                               message = "Deallocation error", &
!                               trace = [Me%name] // &
!                                        "%depositSediment1%U" &
!                              )                                      ! create warning if error thrown
!            call r%addError(er)                                      ! add to Result
!        end if
        r = Result(data = V_w_tot)                                   ! return Result object, with volume of water required from water column
    end function
    !> Calculate resuspension from bed sediment using
    ! [Bussi](http://www.sciencedirect.com/science/article/pii/S0022169416305625):
    ! $$
    !      m_{\text{ent}} = a m_{\text{bed}} \alpha \omega \frac{R_\text{h}}{R_{\text{h,max}}}
    ! $$
    !function calculateResuspensionBedSediment1(me, a, m_bed, alpha, omega, R_h, R_hmax) result(r)
    !    class(BedSediment1) :: me
    !    real(dp) :: a                                   ! Calibration factor [s2/kg]
    !    real(dp) :: m_bed                               ! Bed mass per unit area [kg/m2]
    !    real(dp) :: alpha                               ! Proportion of size class that can be resuspended [-]
    !    real(dp) :: omega                               ! Stream power per unit area of stream bed [J/s/m2]
    !    real(dp) :: R_h                                 ! Actual hydraulic radius [m]
    !    real(dp) :: R_hmax                              ! Maximum hydraulic radius [m]
    !    real(dp) :: f                                   ! Friction factor [-]
    !    type(Result0D) :: r
    !    f = R_h/R_hmax                                  ! Calculate the friction factor
    !    r = Result( &
    !        data = a * m_bed * alpha * omega * f &       ! Calculate the resuspension
    !    )
    !end function

    !> Calculate the stream power (per unit area of stream bed) using Bagnold's
    ! stream power equation:
    ! $$
    !      \omega = \frac{\rho g Q S}{W}
    ! $$
    ! Reference: [Bagnold, 1966](https://www.uvm.edu/~wbowden/Teaching/Stream_Geomorph_Assess/Resources/Private/Documents/1966_Bagnold_river_sediments.pdf)
    !function calculateStreamPowerBedSediment1(me, rho_water, g, Q, W, S) result(r)
    !    class(BedSediment1) :: me
    !    real(dp) :: rho_water                           ! Density of water [kg/m3]
    !    real(dp) :: g                                   ! Gravitational acceleration [m/s]
    !    real(dp) :: Q                                   ! Discharge [m3/s]
    !    real(dp) :: W                                   ! River width [m]
    !    real(dp) :: S                                   ! River slope [m/m]
    !    type(Result0D) :: r
    !    r = Result( &
    !        data = rho_water * g * Q * S / W &
    !    )
    !end function
end module

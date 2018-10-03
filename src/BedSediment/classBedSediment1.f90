!> Module containing definition of `BedSediment1`.
module classBedSediment1
    use Globals
    use ResultModule
    use spcBedSediment
    use classBedSedimentLayer1
    use classFineSediment1
    implicit none
    private

    !> Class representing a `BedSediment1` object, which is an extension of the
    !! abstract superclass `BedSediment`.
    type, public, extends(BedSediment) :: &
        BedSediment1
      contains
        procedure, public :: create => createBedSediment1            ! constructor method
        procedure, public :: destroy => destroyBedSediment1          ! finaliser method
        procedure, public :: deposit => DepositSediment1             ! deposit sediment from water column
        procedure, public :: resuspend => ResuspendSediment1         ! resuspend sediment to water column
        procedure, public :: repmass => ReportBedMassToConsole1      ! report mass of fine sediment in each layer to console [kg/m2]
        procedure, public :: initmatrix => initialiseMatrix1         ! initialise mass transfer coefficient matrix
    end type
  contains
    !> **Function purpose**                                         <br>
    !! Initialise a BedSediment object.
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Initialised `BedSediment` object, including all layers and included `FineSediment`
    !! objects
    function createBedSediment1(Me, riverReachGroup) result(r)
        class(BedSediment1) :: Me                                    !! Self-reference
        type(NcGroup), intent(in) :: riverReachGroup                 !! NetCDF group reference to the `RiverReach` containing this object
        type(Result) :: r                                            !! Returned `Result` object
        type(NcGroup) :: grp                                         ! LOCAL NetCDF group reference
        integer, allocatable :: bslType(:)                           ! LOCAL the type identification number of the BedSedimentLayer(s)
        type(BedSedimentLayer1), allocatable :: bsl1                 ! LOCAL object of type BedSedimentLayer1, for implementation of polymorphism
        integer :: L                                                 ! LOCAL loop counter
        integer :: allst                                             ! LOCAL array allocation status
        type(NcVariable) :: var                                      ! LOCAL variable to retrieve NetCDF data from
        character(len=256) :: tr                                     ! LOCAL error trace
        character(len=16), parameter :: ms = "Allocation error"      ! LOCAL allocation error message
        integer :: M                                                 ! LOCAL loop counter for printing
        !
        ! Notes
        ! ----------------------------------------------------------------------------------
        ! no notes
        ! ----------------------------------------------------------------------------------
        Me%name = trim(riverReachGroup%getName()) // "_BedSediment"  ! object name: RiverReach_x_y_s_r_BedSediment
        Me%ncGroup = riverReachGroup%getGroup("BedSediment")         ! get the BedSediment group name
        Me%nSizeClasses = C%nSizeClassesSpm                          ! set number of size classes from global value
        Me%nfComp = C%nFracCompsSpm                                  ! set number of compositional fractions from global value
        tr = trim(Me%name) // "%createBedSediment1"                  ! procedure name as trace
        var = Me%ncGroup%getVariable("n_layers")                     ! Get the number of BedSedimentLayers
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
        allocate(Me%delta_sed(Me%nLayers + 3, &
                              Me%nLayers + 3, &
                              Me%nSizeClasses), &
            stat = allst)                                            ! allocate space for sediment mass transfer matrix
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                                          message = ms, &
                                          trace = [tr] &
                                         ) &
                           )                                         ! add to Result
            return                                                   ! critical error, so return
        end if
        var = Me%ncGroup%getVariable("layer_type")                   ! Get the BedSedimentLayer type number
        call var%getData(bslType)                                    ! retrieve into bslType variable
        do L = 1, Me%nLayers                                         ! loop through each layer
            allocate(bsl1)                                           ! allocate the temporary local BedSedimentLayer variable
            select case (bslType(L))                                 ! loop through possible BedSedimentLayer types
                case(1)                                              ! type number 1
                    grp = Me%ncGroup%getGroup(trim(ref("Layer", L))) ! Get the layer group
                    call r%addErrors(.errors. &
                        bsl1%create(Me%name, grp))                   ! initialise the layer object
                    allocate(Me%colBedSedimentLayers(L)%item, &
                        source=bsl1, stat = allst)                   ! allocate empty object of this type
                    deallocate(bsl1)                                 ! deallocate local variable ready for the next iteration of the loop
                    if (allst /= 0) then
                        call r%addError(ErrorInstance( &
                                           code = 1, &
                                        message = ms, &
                                          trace = [tr]))             ! add to Result
                        return                                       ! critical error, so return
                    end if
                    if (r%hasCriticalError()) then                   ! if a critical error has been thrown
                        call r%addToTrace(tr)                        ! add trace to Result
                        return                                       ! exit, as a critical error has occurred
                    end if
                case default                                         ! invalid BedSedimentLayer type specified
                    call r%addError(ErrorInstance(code = 1, &
                                message = "Invalid &
                                           BedSedimentLayer &
                                           object type &
                                           specified" &
                                                 ) &
                                   )                                 ! add ErrorInstance
                    call r%addToTrace(tr)                            ! add trace to Result
                    return                                           ! critical error, so exit
            end select
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add trace to Result
                return                                               ! exit, as a critical error has occurred
            end if
            
            !print *, 'Bed sediment layer created'
            !print *, 'Coarse volume [m3/m2]: ', Me%colBedSedimentLayers(L)%item%V_c
            !print *, 'VolSLR: ', .dp. Me%colBedSedimentLayers(L)%item%volslr()
            !do M = 1, Me%nSizeClasses
            !    print *, 'For size class ',M
            !    print *, 'Fine sediment capacity [m3/m2]: ', .dp. Me%colBedSedimentLayers(L)%item%C_f(M)
            !    print *, 'Water capacity [m3/m2]: ', .dp. Me%colBedSedimentLayers(L)%item%C_w(M)
            !    print *, 'Available fine sediment capacity [m3/m2]: ', .dp. Me%colBedSedimentLayers(L)%item%A_f(M)
            !    print *, 'Available water capacity [m3/m2]: ', .dp. Me%colBedSedimentLayers(L)%item%A_w(M)
            !end do
            
        end do
    end function
    !> **Function purpose**                                         <br>
    !! Deallocate all allocatable variables and call destroy methods for all
    !! enclosed objects
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Returns a warning if any deallocation throws an error
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
        tr = trim(Me%name) // &
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
    !> **Function purpose**                                         <br>
    !! Resuspend specified masses of fine sediment in each size class, and their
    !! associated water
    !!                                                              <br>
    !! **Function inputs**
    !! `M_resusp (real, dp)`: 1D array of fine sediment masses to be resuspended
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Returns a warning if the resuspended mass in a size class exceeds the mass in the
    !! sediment bed. `r` returns resuspended fine sediments as type `ResultFineSediment2D`
    function resuspendSediment1(Me, M_resusp) result(r)
        class(BedSediment1) :: Me                                    !! Self-reference
        real(dp) :: M_resusp(:)                                      !! Sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
        type(ResultFineSediment2D) :: r                              !! Returned `Result` object. Type = `FineSediment`
        type(FineSediment1), allocatable :: FS(:,:)                  ! LOCAL resuspended fine sediment. Index 1 = size class, Index 2 = layer
        type(FineSediment1), allocatable :: F                        ! LOCAL FineSediment object representing material to be resuspended
        type(FineSediment1), allocatable :: G                        ! LOCAL FineSediment object representing material not (yet) resuspended
        real(dp), allocatable :: delta_l_r(:,:)                      ! LOCAL deltas for layers to resuspension [-]. L x S array.
        type(ErrorInstance) :: er                                    ! LOCAL error instance
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: allst                                             ! LOCAL array allocation status
        real(dp) :: d_temp                                           ! LOCAL to store the returned delta value from RemoveSediment
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        type(ResultFineSediment1D) :: r1D                            ! LOCAL temporary variable for storing Result returned from call BedSedimentLayer%remove
        class(*), allocatable :: data1D(:)                           ! LOCAL temporary variable to store polymorphic data in to use in select type
        character(len=250) :: tstring                                ! LOCAL temporary variable for constructing strings
        character(len=250) :: ostring                                ! LOCAL output message string
        !
        ! Notes
        ! -----------------------------------------------------------------------------------------
        ! do the references to Me%nSizeClasses and Me%nLayers preclude making this a pure function?
        ! -----------------------------------------------------------------------------------------
        tr = trim(Me%name) // "%resuspendSediment1"                  ! error trace for this procedure
        if (size(M_resusp) /= Me%nSizeClasses) then                  ! check for correct number of size classes
            call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "Number of resuspended &
                                      sediment classes does not &
                                      match number of size classes &
                                      in sediment", &
                            trace = [tr] &
                                         ) &
                           )                                         ! create error instance if number of size classes is incorrect
            return
        end if                                                       ! exit if a critical error has been thrown
        allocate(F, stat = allst)                                    ! set up FineSediment1 variable F
        if (allst /= 0) then
            call r%AddError(ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name // &
                                        "%resuspendSediment1%F"] &
                              ))                                     ! if error thrown on allocation, add to Result object
        end if
        call r%addErrors(.errors. F%create("FineSediment", &
                                           Me%nfComp))               ! create F
        if (r%hasCriticalError()) then
            call r%addToTrace(tr)
            return                                                   ! exit if a critical error has been thrown in creating F
        end if
        allocate(G, stat = allst)                                    ! set up FineSediment1 variable G
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name // &
                                        "%resuspendSediment1%G"] &
                            ))                                       ! if error thrown on allocation, add to Result object
        end if
        call r%addErrors(.errors. G%create("FineSediment", &
                                           Me%nfComp))               ! create G
        if (r%hasCriticalError()) then
            call r%addToTrace(tr)
            return                                                   ! exit if a critical error has been thrown in creating G
        end if
        allocate(FS(Me%nSizeClasses, Me%nLayers), stat = allst)      ! set up FineSediment1 array FS
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name // &
                                        "%resuspendSediment1%FS"] &
                              ))                                     ! if error thrown on allocation, add to Result object
        end if
        allocate(delta_l_r(Me%nLayers, Me%nSizeClasses), &
            stat = allst)                                            ! allocate delta_d-l
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        if (r%hasCriticalError()) return                             ! exit if a critical error has been thrown during allocation
        call r%addErrors(.errors. Me%initmatrix())                   ! initialise the overall matrix of mass transfer coefficients
        if (r%hasCriticalError()) return                             ! exit if a critical error has been thrown

        do S = 1, Me%nSizeClasses                                    ! initialise the delta_l_r values
            do L = 1, Me%nLayers
                delta_l_r(L, S) = 0.0_dp
            end do
        end do
                                                                     ! main loop
                                                                     ! for each size class (1 to S), remove the required amount of fine sediment
                                                                     ! from the bed, by looping through each layer from top to bottom
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            call r%addErrors( .errors. F%set(Mf_in = M_resusp(S)))   ! set up F with the mass of fine sediment in this size class to be resuspended [kg]
            if (r%hasCriticalError()) then
                call r%addToTrace(tr)
                return                                               ! exit if a critical error has been thrown
            end if
            L = 1                                                    ! start with top layer
            do while (M_resusp(S) > 0.000001 .and. L <= Me%nLayers)  ! loop through layers until all sediment resuspended or all layers considered
                associate(O => Me%colBedSedimentLayers(L)%item)      ! association for brevity
                    call r%addErrors(.errors. &
                        F%set(f_comp_in = &
                            O%colFineSediment(S)%f_comp))            ! set the fractional composition of F to that of the sediment being resuspended  
                    call r%AddErrors(.errors. &
                        O%removeSediment(S, F, G, d_temp))           ! remove the resuspended sediment from the layer in question 
                                                                     ! on entry, F contains the fine sediment to be resuspended
                                                                     ! on return, F contains the fine sediment that could not be removed because it exceeded
                                                                     ! the amount present in the layer
                                                                     ! on return, G contains the fine sediment that was removed 
                    delta_l_r(L, S) = d_temp                         ! assign the delta for layer to resuspension
                    if (r%hasCriticalError()) then                   ! if a critical error has been thrown
                        call r%addToTrace(tr)                        ! add the trace to the Result object
                        return                                       ! and exit
                    end if
                end associate
                M_resusp(S) = M_resusp(S) - G%M_f()                  ! modify the amount of sediment in the size class still to be resuspended
                call r%addErrors([ .errors. &
                    FS(S, L)%create("FS_" // trim(str(L)) // &
                                    "_" // trim(str(S)), &
                                    Me%nfComp), &             
                                    .errors. &
                    FS(S, L)%set(Mf_in = G%M_f(), &
                                    Vw_in = G%V_w(), &
                                    f_comp_in = G%f_comp &
                                ) &
                                ])                                   ! create and set up the element of the array FS for this size class and layer
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    return                                           ! exit if a critical error has been thrown
                end if
                L = L + 1                                            ! increment the layer count
            end do                                                   ! and loop to the next layer
            if (M_resusp(S) > 0) then
                call r%addError(ErrorInstance(1, &
                     "All sediment of size class " &
                     // trim(str(S)) // " resuspended", &
                     .false., &
                     [tr] &
                                             ) &
                               )                                     ! create a warning (noncritical error) if bed has been stripped of sediment of size class S
            end if
        end do                                                       ! and loop to the next size class
        call r%setData(FS)                                           ! copy output to Result
        do S = 1, Me%nSizeClasses                                    ! incorporate delta_l_r into the mass transfer coefficients matrix delta_sed
            do L = 1, Me%nLayers                            
                Me%delta_sed(2, L + 2, S) = &
                    Me%delta_sed(2, L + 2, S) + delta_l_r(L, S)      ! element (L, S) of delta_l_r is added to element (2, L+2, S) of delta_sed
                Me%delta_sed(L + 2, L + 2, S) = &
                    Me%delta_sed(L + 2, L + 2, S) - delta_l_r(L, S)  ! element (L, S) of delta_l_r is subtracted from element(L+2, L+2, S) of delta_sed
                                                                     ! this accounts for the loss of sediment from layers during resuspension
            end do
        end do
    end function
    !> Compute deposition to bed sediment, including burial and downward shifting of fine sediment and water <br>
    !> **Function purpose**                                         <br>
    !! Deposit specified masses of fine sediment in each size class, and their
    !! associated water. Function buries sediment and shifts remaining sediment down
    !! to make space for deposition, if required
    !!                                                              <br>
    !! **Function inputs**                                          <br>
    !! Function takes as inputs:
    !! `FS_dep (FineSediment1)`: 1D array of FineSediment1 objects containing the
    !! depositing fine sediment per size class
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (real(dp))`: returns water requirement from the water column [m3 m-2] real(dp)
    function depositSediment1(Me, FS_dep) result(r)
        class(BedSediment1) :: Me                                    !! Self-reference
        type(FineSediment1) :: FS_dep(:)                             !! Depositing sediment by size class
        type(Result0D) :: r                                          !! `Result` object. Returns water requirement from the water column [m3 m-2], real(dp)
        type(FineSediment1), allocatable :: T                        ! LOCAL object to receive sediment being buried
        type(FineSediment1), allocatable :: U                        ! LOCAL object to receive sediment that has been buried
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: LL                                                ! LOCAL second counter for layers
        integer :: A                                                 ! LOCAL second counter for layers
        integer :: allst                                             ! LOCAL array allocation status
        real(dp) :: A_f_sed = 0.0_dp                                 ! LOCAL available fine sediment capacity for size class [m3 m-2]
        real(dp) :: V_f_burial = 0.0_dp                              ! LOCAL excess of deposting fine sediment over capacity [m3 m-2]  
        real(dp) :: tempV = 0.0_dp                                   ! LOCAL volume variable
        real(dp) :: V_w_tot = 0.0_dp                                 ! LOCAL water requirement from the water column [m3 m-2]
        real(dp) :: V_f_b = 0.0_dp                                   ! LOCAL available fine sediment capacity in the receiving layer [m3 m-2]
        real(dp) :: V_w_b = 0.0_dp                                   ! LOCAL available water capacity in the receiving layer [m3 m-2]
        real(dp) :: dep_excess                                       ! LOCAL excess of deposition over available capacity [m3 m-2]
        real(dp), allocatable :: delta_d_b(:)                        ! LOCAL delta for deposition to burial [-]. S array.
        real(dp), allocatable :: delta_d_l(:,:)                      ! LOCAL deltas for deposition to layers [-]. L x S array.
        real(dp), allocatable :: delta_l_b(:,:)                      ! LOCAL deltas for layers to burial [-]. L x S array.
        real(dp), allocatable :: delta_l_l(:,:,:)                    ! LOCAL deltas for layers to layers [-]. L x L X S array.
        real(dp) :: M_f_la                                           ! LOCAL to store the mass of fine sediment in a layer, for computation of delta_l-b and delta_d-l
        real(dp) :: M_f_dep                                          ! LOCAL to store the mass of fine sediment in deposition, for computation of delta_d_l
        real(dp) :: d_temp                                           ! LOCAL to store the returned delta value from RemoveSediment
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        class(*), allocatable :: data0D                              ! LOCAL temporary polymorphic data variable
        type(ErrorInstance) :: tmpError

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
        do S = 1, Me%nSizeClasses
            print *, FS_dep(S)%M_f()
        end do
        tr = trim(Me%name) // "%DepositSediment1"                    ! object and procedure binding name as trace
        if (size(FS_dep) /= Me%nSizeClasses) &
            call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "The number of fine sediment masses does not equal the number of size classes", &
                            trace = [tr] &
                                         ) &
                           )                                         ! check that the array of depositing masses is the correct size
        if (r%hasCriticalError()) then                               ! if a critical error has been thrown
            call r%addToTrace(tr)                                    ! add trace to Result
            return                                                   ! and exit
        end if
        allocate(T, stat = allst)                                    ! allocate T
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                         ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(U, stat = allst)                                    ! allocate U
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(delta_d_l(Me%nLayers, Me%nSizeClasses), &
            stat = allst)                                            ! allocate delta_d-l
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(delta_d_b(Me%nSizeClasses),  stat = allst)          ! allocate delta_d-l
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(delta_l_b(Me%nLayers, Me%nSizeClasses), &
            stat = allst)                                            ! allocate delta_l-b
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(delta_l_l(Me%nLayers, Me%nLayers, Me%nSizeClasses), &
            stat = allst)                                            ! allocate delta_l-l
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        if (r%hasCriticalError()) return                             ! exit if critical allocation error thrown
        do S = 1, Me%nSizeClasses
            call r%addError(FS_dep(S)%audit_comp())                  ! audits fractional composition of deposition, returns an error instance object
            if (r%hasCriticalError()) then                           ! if fcomp_audit /=, throws a critical error
                call r%addToTrace(tr)                                ! add trace to error (note audit_comp method returns its own error message)
                return                                               ! and exit
            end if
        end do
        do S = 1, Me%nSizeClasses                                    ! initialise all delta values
            do L = 1, Me%nLayers                                     ! deposition and burial to zero
                delta_d_l(L, S) = 0.0_dp                             ! interlayer transfers to zero  
                delta_l_b(L, S) = 0.0_dp                             ! same layer transfers to unity
                do LL = 1, Me%nLayers
                    delta_l_l(L, LL, S) = 0.0_dp
                end do
            end do
        end do
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            dep_excess = FS_dep(S)%V_f() - .dp. Me%Cf_sediment(S)    ! compute the difference between the volume of depositing material and the bed capacity [m3 m-2]
                                                                     ! if this equals or exceeds zero, then there is complete replacement of the material in the bed and if
                                                                     ! it exceeds zero, there is direct burial of a portion of the depositing sediment
                                                                     ! in this case, delta[l,n-b] = 1 for all layers, and delta[d-b] > 0.
            if (dep_excess > 0) &
                then                                                 ! check whether the depositing sediment in each size class exceeds the total
                do L = 1, Me%nLayers                                 ! capacity for that size fraction in the bed. If so, then remove all fine sediment, water and
                    call Me%colBedSedimentLayers(L)%item%clearAll()  ! fractional compositions from all layers for this size class
                end do
                do L = 1, Me%nLayers
                    delta_l_b(L, S) = 1                              ! deltas for layers to burial
                end do
                delta_d_b(S) = dep_excess / FS_dep(S)%M_f()          ! delta for deposition to burial
            end if
        end do
        call r%addErrors(.errors. T%create("FineSediment_T", &
                                                   Me%nfComp))       ! create FineSediment object T
        call r%addErrors(.errors. U%create("FineSediment_U", & 
                                                   Me%nfComp))       ! create FineSediment object U
        if (r%hasCriticalError()) then                               ! if creation throws a critical error
            call r%addToTrace(tr)                                    ! add trace to error (note audit_comp method returns its own error message)
            return                                                   ! and exit
        end if
        do S = 1, Me%nSizeClasses                                    ! main loop for burial of sediment
                                                                     ! for each sediment size class, check whether the available capacity in the bed
                                                                     ! exceeds the amount of depositing sediment. If so, then bury sediment of this size class
                                                                     ! to provide the capacity for the depositing sediment
            A_f_sed = .dp. Me%Af_sediment(S)                         ! local copy of the capacity for this sediment size class in the whole bed [m3 m-2]
            V_f_burial = FS_dep(S)%V_f() 
            V_f_burial = FS_dep(S)%V_f() - A_f_sed                   ! difference between volume of depositing sediment and available capacity
                                                                     ! if > 0, then sediment needs to be buried to create capacity for deposition
            if (V_f_burial > 0.0_dp) then                            ! do we need to bury sediment to create available capacity for deposition?
                call r%addErrors(.errors. &
                    T%set(Vf_in = V_f_burial, &
                          Vw_in = 0.0_dp, &
                          f_comp_in = FS_dep(S)%f_comp &
                         ) &
                                )                                    ! yes, so
                                                                     ! set up temporary FineSediment object T with volume of fine sediment requiring burial
                                                                     ! to compute the volume of water requiring burial, we must loop through layers
                                                                     ! from the top, compute for each layer the volume of fine sediment that must be
                                                                     ! removed to allow space for deposition, and the volume of water associated with the
                                                                     ! fine sediment
                L = Me%nLayers                                       ! loop through layers, upwards from the bottom
                do while (T%V_f() > 0)                               ! use fine sediment volume in T as a counter. Through this loop, T holds the count of the  
                                                                     ! requirement for sediment burial that has not yet been accounted for by higher layers
                    associate (O => Me%colBedSedimentLayers(L)%item) ! association to layer L
                        if (T%V_f() > .dp. O%C_f(S)) then            ! does the depositing fine sediment fit into this layer,
                                                                     ! after accounting for the capacity in layers above?
                                                                     ! no, the depositing sediment will not fit into this layer
                                                                     ! so increase water removal requirement by the water capacity of this layer
                                                                     ! and decrease the count of remaining depositing fine sediment by the capacity
                            call r%addErrors(.errors. &
                                 T%set(Vf_in = T%V_f() - .dp.O%C_f(S), &
                                       Vw_in = T%V_w() + .dp. O%C_w(S) &
                                      ) &
                                            )
                        else                                         ! yes, depositing sediment fits into this layer
                                                                     ! so increase the water burial requirement by the amount required to maintain the SLR in this layer
                                                                     ! and set the count of fine sediment to zero, to jump out of the loop
                            tempV = T%V_f() / .dp. O%volSLR(S)       ! temporary variable
                            call r%addErrors(.errors. &
                                 T%set(Vf_in = 0.0_dp, &
                                       Vw_in = T%V_w() + tempV &
                                      ) &
                                            )
                        end if
                    end associate
                    L = L - 1                                        ! decrement the layer count
                end do                                               ! and loop
                                                                     ! now to actually bury fine sediment and water
                                                                     ! we still use the object T to hold the depositing material - firstly, its mass
                                                                     ! needs to be reset, as it was decremented to zero in the computation of the water requirement
                call r%addErrors(.errors. &
                     T%set(Vf_in = FS_dep(S)%V_f() - A_f_sed &
                          ) &
                                )                                    ! reset the fine sediment burial requirement, still using object T
                if (r%hasCriticalError()) return                     ! return if critical error thrown
                                                                     ! now we remove and bury material from the base of the sediment upwards, 
                                                                     ! to create sufficient space to accommodate deposited material
                L = Me%nLayers                                       ! start with the bottom layer
                do while (L > 0 .and. T%V_f() + T%V_w() > 0)         ! loop through each layer, while there is still material to bury
                    if (T%V_f() > 0) Then
                        associate(O => &
                            Me%colBedSedimentLayers(L)%item)         ! association reference to layer L object
                            call r%addErrors(.errors. &
                                O%RemoveSediment(S, T, U, d_temp) &
                                            )                        ! remove the sediment, return amount removed (U) and not removed (T), and any errors thrown
                            
                            if (r%hasCriticalError()) then           ! if RemoveSediment throws a critical error
                                call r%addToTrace(tr)                ! add trace to all errors
                                return                               ! and exit
                            end if
                        end associate
                    end if
                    delta_l_b(L, S) = d_temp                         ! computation of delta for layer L to burial
                    delta_l_l(L, L, S) = -1 * d_temp                 ! delta for loss of material to burial
                                                                     ! note that these are CHANGES in delta due to burial, not absolute values
                    L = L - 1                                        ! move up to next layer
                end do                                               ! finished burial. temporary object T can be reused
                                                                     ! now we shift sediment downwards from upper layers to fill the hole created by burial
                do L = Me%nLayers, 2, -1                             ! downward shift of fine sediment. Loop through the layers, starting at the bottom
                                                                     ! and working upwards
                    assoc1 : associate &
                        (O => Me%colBedSedimentLayers(L)%item)       ! association to "receiving" layer L
                        A = L - 1                                    ! counter for "donating" layer - initially the layer above
                        call r%addErrors(.errors. &
                            T%set(Vf_in = .dp. O%A_f(S), &
                                  Vw_in = .dp. O%A_w(S) &
                                 ) &
                                        )                            ! set FineSediment object T to hold the available capacity in the receiving layer 
                                                                     ! Note no need to set f_comp in T
                        do while (A > 0 .and. T%IsNotEmpty())        ! loop through "donating" layers, moving upwards
                            assoc2 : associate (P => &
                            Me%colBedSedimentLayers(A)%item)         ! association to "donating" layer A
                            if (P%colFineSediment(S)%V_f() > 0) &
                                then                                 ! if there is sediment in the "donating" layer
                                    call r%addErrors(.errors. &
                                        P%RemoveSediment(S, T, U, &
                                            d_temp))                 ! remove the sediment, return amounts removed (U) and not removed (T), the delta, and any errors thrown
                                    if (r%hasCriticalError()) then   ! if RemoveSediment throws a critical error
                                        call r%addToTrace(tr)        ! add trace to all errors
                                        return                       ! and exit
                                    end if
                                    call r%addErrors(.errors. &
                                        O%addSediment(S, U))         ! add the sediment in U to the "receiving" layer L
                                if (r%hasCriticalError()) then       ! if AddSediment throws a critical error
                                    call r%addToTrace(tr)            ! add trace to all errors
                                    return                           ! and exit
                                end if
                                    delta_l_l(A, L, S) = d_temp      ! delta for transfer of sediment from Layer A to Layer L
                                    delta_l_l(A, A, S) = -1 * d_temp ! delta for retention of sediment in Layer A
                                                                     ! note that these are CHANGES in delta due to transfer, not absolute values
                            end if
                            A = A - 1                                ! shift up to next "donating" layer
                            end associate assoc2
                        end do
                    end associate assoc1
                end do
            end if
        end do
        do S = 1, Me%nSizeClasses                                    ! now add in the depositing sediment, work by size class
            M_f_dep = FS_dep(S)%M_f()                                ! store the total amount of sediment in this size class being deposited, for computation of deltas                 
            do L = Me%nLayers, 1, -1                                 ! start with the bottom layer and work upwards
                associate(O => Me%colBedSedimentLayers(L)%item)      ! size class S in Layer L
                    if (.dp. O%A_f(S) > 0 .or. &
                        .dp. O%A_w(S) > 0) then                      ! if there is available capacity in this layer, add deposition here
                        V_w_b = FS_dep(S)%V_f() / .dp. O%volSLR(S)   ! the volume of water needed to maintain SLR in the "receiving" layer,
                        call r%addErrors(.errors. &
                            FS_dep(S)%set(Vw_in = V_w_b))            ! if all deposition were to fit into this layer
                        M_f_la = FS_dep(S)%M_f()                     ! store the amount of sediment still to be deposited, for computation of deltas
                        call r%addErrors(.errors. &
                            O%addSediment(S, FS_dep(S)))             ! add the fine sediment in deposition. FS_dep(S) returns volumes that could not be added
                        if (r%hasCriticalError()) then               ! if addSediment throws a critical error
                            call r%addToTrace(tr)                    ! add trace to all errors
                            return                                   ! and exit
                        end if
                        delta_d_l(L, S) = &
                            (M_f_la - FS_dep(S)%M_f()) / M_f_dep     ! delta_d_l for this layer: the mass of material deposited to this layer, divided by the total depostion
                    end if
                    V_w_tot = V_w_tot + V_w_b - FS_dep(S)%V_w()      ! tally up V_w_b to compute water requirement to take from the water column
                end associate
            end do
        end do
        r = Result(data = V_w_tot)                                   ! return Result object, with volume of water required from water column
        
        print *, "Final state of sediment..."
        call Me%repMass
        do S = 1, Me%nSizeClasses                                    ! incorporate delta_d_b, delta_d_l, delta_l_b, delta_l_l into the mass transfer coefficients matrix delta_sed 
            Me%delta_sed(Me%nLayers + 3, 1, S) = &
                Me%delta_sed(Me%nLayers + 3, 1, S) + delta_d_b(S)    ! element (S) of delta_d_b is added to element (Layers+3, 1, S) of Me%delta_sed
            do L = 1, Me%nLayers
                Me%delta_sed(L + 2, 1, S) = &
                    Me%delta_sed(L + 2, 1, S) + delta_d_l(L, S)      ! element (L, S) of delta_d_l is added to element (L+2, 1, S) of Me%delta_sed
                Me%delta_sed(Me%nLayers + 3, L + 2, S) = &
                    Me%delta_sed(Me%nLayers + 3, L + 2, S) + &
                    delta_l_b(L, S)                                  ! element (L, S) of delta_l_b is added to element (Layers+3, L+2, S) of Me%delta_sed
                do LL = 1, Me%nLayers
                    Me%delta_sed(L + 2, LL + 2, S) = &
                        Me%delta_sed(L + 2, LL + 2, S) + &
                        delta_l_l(LL, L, S)                          ! element (LL, L, S) of delta_l_l is added to element (L+2, LL+2, S) of Me%delta_sed
                end do
            end do
        end do
    end function
    !> **Function purpose**
    !! initialise the matrix of mass transfer coefficients for sediment deposition and resuspension
    !!                                                          
    !! **Function inputs**                                      
    !! none (uses class-level variable array delta_sed(:,:,:)
    !!                                                          
    !! **Function outputs/outcomes**                            
    !! delta_sed populated with initial values, all zero except for layer(x) ->layer(y) coefficients where x=y; these are set to unity
    function initialiseMatrix1(Me) result(r)
        class(BedSediment1) :: Me                                    !! The `BedSediment` instance
        type(Result) :: r                                            !! `Result` object. Returns water requirement from the water column [m3 m-2], real(dp)
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        integer :: L                                                 ! LOCAL loop counter for sediment layers
        integer :: LL                                                ! LOCAL second loop counter for sediment layers
        integer :: S                                                 ! LOCAL loop counter for size classes
        tr = trim(Me%name) // "%initialiseMatrix1"                   ! object and procedure binding name as trace
        if (size(Me%delta_sed, 1) /= Me%nLayers + 3 .or. &
            size(Me%delta_sed, 2) /= Me%nLayers + 3.or. &
            size(Me%delta_sed, 3) /= Me%nSizeClasses) then
            call r%addError(ErrorInstance(1, &
                     tr // "Array size error", .true., [tr] &
                                         ) &
                           )                                         ! create a critical error if there is an array size issue
            return
        end if
        do S = 1, Me%nSizeClasses
            do L = 1, Me%nLayers + 3
                do LL = 1, Me%nLayers + 3
                    Me%delta_sed(L, LL, S) = 0.0_dp                  ! initially set all values to a default of zero
                end do
            end do
        end do
        do S = 1, Me%nSizeClasses
            do L = 1, Me%nLayers
                Me%delta_sed(L + 2, L + 2, S) = 1.0_dp               ! set same layer transfer coefficients to a default of unity
            end do
        end do
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
    subroutine ReportBedMassToConsole1(Me)
        class(BedSediment1) :: Me                                    !! The `BedSediment` instance
        integer :: n                                                 !! LOCAL loop counter 
        type(result0D) :: r                                          !! LOCAL result object to hold return from Mf_bed_all derived property
        print *, trim(Me%name)                                       !! the name of this layer
        do n=1, Me%nLayers
            print *, "Layer ", n
            call Me%colBedSedimentLayers(n)%item%repMass()           !! print out mass of FS in each layer, by size class [kg/m2]
        end do
        r = Me%Mf_bed_all()
        print *, "Total: ", .real. r                                 ! print out mass of FS in bed [kg/m2]
    end subroutine

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

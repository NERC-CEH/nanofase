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
    type, public, extends(BedSediment) :: BedSediment1
      contains
        procedure, public :: create => createBedSediment1            ! constructor method
        procedure, public :: destroy => destroyBedSediment1          ! finaliser method
        procedure, public :: deposit => DepositSediment1             ! deposit sediment from water column
        procedure, public :: resuspend => ResuspendSediment1         ! resuspend sediment to water column
        procedure, public :: repmass => ReportBedMassToConsole1      ! report mass of fine sediment in each layer to console [kg/m2]
        ! procedure, public :: initmatrix => initialiseMatrix1         ! initialise mass transfer coefficient matrix
        procedure, public :: getmatrix => getMTCMatrix1              ! derives mass transfer coefficient matrix for sediment
        procedure, public :: transferNM => transferNMBedSediment1    ! Transfer NM masses between layers and to/from water body, using mass transfer coef matrix
    end type
    
  contains

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
    !! TODO: SH 27/09/19 this seems to modify the absolute mass transfer matrix to
    !! get the mass transfer coefficient matrix, which makes me a bit nervous. Need to
    !! check delta_sed is being properly re-set to absolute masses on each timestep, or
    !! just get this to create new matrix
    subroutine getMTCMatrix1(Me, djdep, djres)
        class(BedSediment1) :: Me                                    !! Self-reference
        real(dp) :: djdep(:)                                         !! deposition fluxes by size class [kg/m2]
        real(dp) :: djres(:)                                         !! resuspension fluxes by size class [kg/m2]
        real(dp) :: ml                                               ! LOCAL holds initial sediment layer masses [kg/m2]
        integer :: L                                                 ! LOCAL loop counter
        integer :: LL                                                ! LOCAL loop counter
        integer :: S                                                 ! LOCAL loop counter

        do S = 1, Me%nSizeClasses
            do L = 3, C%nSedimentLayers + 3 
                if (.not. isZero(djdep(S)) .and. .not. isZero(Me%delta_sed(L, 1, S))) then
                    Me%delta_sed(L, 1, S) = &
                        Me%delta_sed(L, 1, S) / djdep(S)             ! d -> l and d-> b
                else
                    Me%delta_sed(L, 1, S) = 0                        ! failsafe if no deposition
                end if 
            end do
            do LL = 3, C%nSedimentLayers + 2
                if (.not. isZero(djres(S)) .and. .not. isZero(Me%delta_sed(2, LL, S))) then
                    ml = Me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup() ! Phew!
                    Me%delta_sed(2, LL, S) = &
                        ! Me%delta_sed(2, LL, S) / djres(S)            ! l -> r
                        Me%delta_sed(2, LL, S) / ml                     ! l -> r
                else
                    Me%delta_sed(2, LL, S) = 0                       ! failsafe if no resuspension
                end if
            end do
            do L = 3, C%nSedimentLayers + 3
                do LL = 3, C%nSedimentLayers + 2
                    ml = Me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup() ! Phew!
                    ! print *, "per layer, S, from, to", S, LL - 2, L - 2, ml
                    if (.not. isZero(ml)) then
                        if (L == LL) then
                            if (.not. isZero(Me%delta_sed(L, LL, S))) then 

                                Me%delta_sed(L, LL, S) = &
                                (ml + Me%delta_sed(L, LL, S)) / ml   ! l -> l where l=l ('same-layer' transfers) and there is a mass transfer out of the layer
                            else
                                Me%delta_sed(L, LL, S) = 1.0_dp         ! If no transfer, must be 1 TODO check this
                            end if
                        else
                            Me%delta_sed(L, LL, S) = &
                                Me%delta_sed(L, LL, S) / ml          ! l -> l where l/=l (interlayer transfers), also l -> b
                        end if
                    else
                        Me%delta_sed(L, LL, S) = 0.0_dp              ! failsafe if no sediment initially in layer 
                    end if
                end do
            end do
        end do
    end subroutine
    !> **Function purpose**                                         <br>
    !! Initialise a BedSediment object.
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Initialised `BedSediment` object, including all layers and included `FineSediment`
    !! objects
    function createBedSediment1(Me, x, y, w) result(r)
        class(BedSediment1) :: Me                                    !! Self-reference
        integer :: x                                                !! x index of the containing water body
        integer :: y                                                !! y index of the containing water body
        integer :: w                                                !! w index of the containing water body
        type(Result) :: r                                            !! Returned `Result` object
        type(BedSedimentLayer1), allocatable :: bsl1                 ! LOCAL object of type BedSedimentLayer1, for implementation of polymorphism
        integer :: L                                                 ! LOCAL loop counter
        integer :: allst                                             ! LOCAL array allocation status
        character(len=256) :: tr                                     ! LOCAL error trace
        character(len=16), parameter :: ms = "Allocation error"      ! LOCAL allocation error message

        me%name = trim(ref('BedSediment', x, y, w))
        Me%nSizeClasses = C%nSizeClassesSpm                          ! set number of size classes from global value
        Me%nfComp = C%nFracCompsSpm                                  ! set number of compositional fractions from global value
        tr = trim(Me%name) // "%createBedSediment1"                  ! procedure name as trace

        ! Initialise NM mass pools matrix
        allocate(me%M_np(C%nSedimentLayers + 3, C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%C_np_byMass(C%nSedimentLayers, C%npDim(1), C%npDim(2), C%npDim(3)))
        me%M_np = 0.0_dp
        me%C_np_byMass = 0.0_dp

        allocate(Me%colBedSedimentLayers(C%nSedimentLayers))            ! create BedSedimentLayer collection
        allocate(Me%delta_sed(C%nSedimentLayers + 3, &
                              C%nSedimentLayers + 3, &
                              Me%nSizeClasses))                         ! allocate space for sediment mass transfer matrix
        me%delta_sed = 0.0_dp                                           ! initialise to zero

        do L = 1, C%nSedimentLayers                                         ! loop through each layer
            allocate(bsl1)                                           ! allocate the temporary local BedSedimentLayer variable
            call r%addErrors(.errors. &
                bsl1%create(Me%name, L))                   ! initialise the layer object
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
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add trace to Result
                return                                               ! exit, as a critical error has occurred
            end if
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
        do L = 1, C%nSedimentLayers
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

    !> Transfer NM between sediment layers, based on the mass transfer coefficient
    !! matrix delta_sed, which should already have been set prior to calling this procedure
    subroutine transferNMBedSediment1(me, j_np_dep)
        class(BedSediment1) :: me                               !! This BedSediment1 instance
        real(dp)            :: j_np_dep(:,:,:)                  !! Mass of NM deposited to bed sediment on this time step [kg/m2]
        integer             :: i, j, k, l                       ! Iterator
        real(dp)            :: M_f_byLayer(C%nSedimentLayers)   ! Mass of fine sediment by layer
        
        ! Assumes me%delta_sed has already been set
        ! Add new deposited NM to matrix, reset resus and buried to zero
        me%M_np(1,:,:,:) = j_np_dep                     ! Deposited     [kg/m2]
        me%M_np(2,:,:,:) = 0.0_dp                       ! Resuspended   [kg/m2]
        me%M_np(C%nSedimentLayers+3,:,:,:) = 0.0_dp     ! Buried        [kg/m2]

        ! Get the breakdown of fine sediment masses down the layers
        M_f_byLayer = me%Mf_bed_by_layer()

        ! Perform the transfer calculation to move NM between the layers
        do k = 1, C%nSizeClassesSpm
            do j = 1, C%npDim(2)
                do i = 1, C%npDim(1)
                    me%M_np(:,i,j,k+2) = matmul(me%delta_sed(:,:,k), me%M_np(:,i,j,k+2))
                    do l = 1, C%nSedimentLayers
                        me%C_np_byMass(l,i,j,k+2) = divideCheckZero(me%M_np(l+2,i,j,k+2), M_f_byLayer(l))
                    end do
                end do
            end do
        end do

        ! Mass balance check
        ! M_np_mass_balance = sum(me%M_np(3:6,:,:,:), dim=1) - &
        !     sum(old_M_np(3:6,:,:,:), dim=1) - old_M_np(1,:,:,:) + me%M_np(2,:,:,:) + me%M_np(7,:,:,:)
        ! if (.not. isZero(M_np_mass_balance)) then
        !     print *, "Woah!"
        !     call print_matrix(me%delta_sed)
        !     print *, "old mass in sediment", sum(old_M_np(3:6,1,1,3:7), dim=1)
        !     print *, "new mass in sediment", sum(me%M_np(3:6,1,1,3:7), dim=1)
        !     print *, "mass dep", old_M_np(1,1,1,3:7)
        !     print *, "mass res", me%M_np(2,1,1,3:7)
        !     print *, "mass buried", me%M_np(7,1,1,3:7)
        !     print *, M_np_mass_balance(1,1,3:7)
        !     error stop
        ! end if

        ! Reset delta_sed. It seems delta_sed is used interchangeably as absolute masses
        ! and mass coefficients, so resetting is playing it safe to avoid numerical errors
        ! in case not all elements are reset on each timestep. TODO need to figure this
        ! out properly
        me%delta_sed = 0.0_dp

    end subroutine

    !> **Function purpose**                                         <br>
    !! Resuspend specified masses of fine sediment in each size class, and their
    !! associated water
    !!                                                              <br>
    !! **Function inputs**
    !! `FS_resusp (real, dp)`: 1D array of fine sediment masses to be resuspended [kg m-2]
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Returns a warning if the resuspended mass in a size class exceeds the mass in the
    !! sediment bed. `r` returns resuspended fine sediments as type `ResultFineSediment2D`
    function resuspendSediment1(Me, FS_resusp) result(r)
        class(BedSediment1) :: Me                                    !! Self-reference
        real(dp) :: FS_resusp(:)                                      !! Sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
        type(ResultFineSediment2D) :: r                              !! Returned `Result` object. Type = `FineSediment`
        type(FineSediment1), allocatable :: FS(:,:)                  ! LOCAL resuspended fine sediment. Index 1 = size class, Index 2 = layer
        type(FineSediment1) :: F                        ! LOCAL FineSediment object representing material to be resuspended
        type(FineSediment1) :: G                        ! LOCAL FineSediment object representing material not (yet) resuspended
        real(dp), allocatable :: delta_l_r(:,:)                      ! LOCAL deltas for layers to resuspension [-]. L x S array.
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: allst                                             ! LOCAL anrray allocation status
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        
        tr = trim(Me%name) // "%resuspendSediment1"                  ! error trace for this procedure
        ! Create fine sediment objects F and G
        call F%create("FineSediment", Me%nfComp)
        call G%create("FineSediment", Me%nfComp)
        allocate(FS(Me%nSizeClasses, C%nSedimentLayers))            ! set up FineSediment1 array FS
        allocate(delta_l_r(C%nSedimentLayers, Me%nSizeClasses))     ! allocate delta_d-l
        me%delta_sed = 0.0_dp                                       ! Reset the matrix of mass transfer coefficients
        ! call r%addErrors(.errors. Me%initmatrix())                  ! initialise the overall matrix of mass transfer coefficients
        delta_l_r = 0.0_dp                                          ! initialise the delta_l_r values
        do S = 1, Me%nSizeClasses
            do L = 1, C%nSedimentLayers                          
                ! back up all the fine sediment masses, an essential part of the mass trasfer matrix computation
                call Me%colBedSedimentLayers(L)%item%colFineSediment(S)%backup_M_f()
            end do
        end do        
                                                                     ! main loop
                                                                     ! for each size class (1 to S), remove the required amount of fine sediment
                                                                     ! from the bed, by looping through each layer from top to bottom
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            call F%set(Mf_in = FS_resusp(S))                        ! set up F with the mass of fine sediment in this size class to be resuspended [kg]
            L = 1                                                    ! start with top layer
            do while (FS_resusp(S) > 0.000001 .and. L <= C%nSedimentLayers) ! loop through layers until all sediment resuspended or all layers considered
                associate(O => Me%colBedSedimentLayers(L)%item)      ! association for brevity
                    call F%set(f_comp_in = O%colFineSediment(S)%f_comp) ! set the fractional composition of F to that of the sediment being resuspended  
                    call r%addErrors(.errors. &
                        O%removeSediment(S, F, G))                   ! remove the resuspended sediment from the layer in question 
                                                                     ! on entry, F contains the fine sediment to be resuspended
                                                                     ! on return, F contains the fine sediment that could not be removed because it exceeded
                                                                     ! the amount present in the layer
                                                                     ! on return, G contains the fine sediment that was removed 
                    delta_l_r(L, S) = G%M_f()                        ! assign the delta for layer to resuspension
                    if (r%hasCriticalError()) then                   ! if a critical error has been thrown
                        call r%addToTrace(tr)                        ! add the trace to the Result object
                        return                                       ! and exit
                    end if
                end associate
                FS_resusp(S) = FS_resusp(S) - delta_l_r(L, S)          ! modify the amount of sediment in the size class still to be resuspended
                if (isZero(FS_resusp(s), 1.0e-10_dp)) then                      ! Just to be on the safe side
                    FS_resusp(S) = 0.0_dp
                end if
                call FS(s,l)%create("FS", me%nfComp)
                ! create and set up the element of the array FS for this size class and layer
                call FS(S, L)%set(Mf_in = G%M_f(), &
                                    Vw_in = G%V_w(), &
                                    f_comp_in = G%f_comp &
                                )
                L = L + 1                                            ! increment the layer count
            end do                                                   ! and loop to the next layer
            if (FS_resusp(S) > 0) then
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
            do L = 1, C%nSedimentLayers                            
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
        real(dp) :: A_f_sed = 0.0_dp                                 ! LOCAL available fine sediment capacity for size class [m3 m-2]
        real(dp) :: V_f_burial = 0.0_dp                              ! LOCAL excess of deposting fine sediment over capacity [m3 m-2]  
        real(dp) :: tempV = 0.0_dp                                   ! LOCAL volume variable
        real(dp) :: V_w_tot = 0.0_dp                                 ! LOCAL water requirement from the water column [m3 m-2]
        real(dp) :: V_w_b = 0.0_dp                                   ! LOCAL available water capacity in the receiving layer [m3 m-2]
        real(dp) :: dep_excess                                       ! LOCAL excess of deposition over available capacity [m3 m-2]
        real(dp), allocatable :: delta_d_b(:)                        ! LOCAL delta for deposition to burial [-]. S array.
        real(dp), allocatable :: delta_d_l(:,:)                      ! LOCAL deltas for deposition to layers [-]. L x S array.
        real(dp), allocatable :: delta_l_b(:,:)                      ! LOCAL deltas for layers to burial [-]. L x S array.
        real(dp), allocatable :: delta_l_l(:,:,:)                    ! LOCAL deltas for layers to layers [-]. L x L X S array.
        real(dp) :: M_f_la                                           ! LOCAL to store the mass of fine sediment in a layer, for computation of delta_l-b and delta_d-l
        logical, allocatable :: IsEmpty(:)                           ! LOCAL .true. bed has been emptied completely to make space for depositing sediment
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        integer :: allst

        ! -------------------------------------------------------------------------------
        !
        ! Notes
        ! -------------------------------------------------------------------------------
        ! 1.    Currently does not account fully for sediment burial, in the sense that
        !       it does not tally mass, volume and composition of buried material. This
        !       will need to be added before burial losses of a chemical vector can be
        !       computed.
        !       TODO: add code to mix FineSediments together and return a single
        !       TODO: FineSediment object. This code can be used to tally up the sediment that
        !       TODO: is lost through burial, and can also be called from AddSediment.
        ! 2.    The FineSediment objects in FS_dep should not contain any water, but if they
        !       do it is not a problem as it will be overwritten.
        ! -------------------------------------------------------------------------------
        
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
        allocate(IsEmpty(Me%nSizeClasses), stat = allst)             ! allocate IsEmpty
        if (allst /= 0) then
            call r%addError(ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%IsEmpty" &
                                         ) &
                           )                                         ! create error instance for allocation error, and add to Result
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
        allocate(delta_d_l(C%nSedimentLayers, Me%nSizeClasses), &
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
        allocate(delta_l_b(C%nSedimentLayers, Me%nSizeClasses), &
            stat = allst)                                            ! allocate delta_l-b
        if (allst /= 0) then
            call r%addError( ErrorInstance(code = 1, &
                               message = "Allocation error", &
                               trace = [Me%name] // &
                                        "%depositSediment1%T" &
                                          ) &
                           )                                         ! create error instance for allocation error, and add to Result
        end if
        allocate(delta_l_l(C%nSedimentLayers, C%nSedimentLayers, Me%nSizeClasses), &
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
            delta_d_b(S) = 0.0_dp                                    ! interlayer and same layer transfers to zero  
            do L = 1, C%nSedimentLayers                                     ! deposition and burial to zero
                delta_d_l(L, S) = 0.0_dp                             ! interlayer and same layer transfers to zero  
                delta_l_b(L, S) = 0.0_dp                             
                do LL = 1, C%nSedimentLayers
                    delta_l_l(L, LL, S) = 0.0_dp
                end do
            end do
            IsEmpty(S) = .true.                                      ! set flag to initial value
        end do
        do S = 1, Me%nSizeClasses                                    ! loop through all size classes
            dep_excess = FS_dep(S)%V_f() - Me%Cf_sediment(S)        ! compute the difference between the volume of depositing material and the bed capacity [m3 m-2]
                                                                     ! if this equals or exceeds zero, then there is complete replacement of the material in the bed and if
                                                                     ! it exceeds zero, there is direct burial of a portion of the depositing sediment
                                                                     ! in this case, delta[l,n-b] = 1 for all layers, and delta[d-b] > 0.
            if (dep_excess > 0) &
                then                                                 ! check whether the depositing sediment in each size class exceeds the total
                    associate(O => Me%colBedSedimentLayers)          ! association for brevity
                        do L = 1, C%nSedimentLayers                         ! capacity for that size fraction in the bed. If so, then remove all fine sediment, water and
                            delta_l_b(L, S) = O(L)%item%colFineSediment(S)%M_f()        ! delta l -> b
                            delta_l_l(L, L, S) = -O(L)%item%colFineSediment(S)%M_f()    ! delta l -> l
                            call O(L)%item%colFineSediment(S)%ClearAll()  ! fractional compositions from all layers for this size class
                        end do
                        delta_d_b(S) = dep_excess * &
                            FS_dep(S)%rho_part()                     ! delta for deposition to burial
                    end associate
            else
                IsEmpty(S) = .false.                                 ! set flag to indicate that there is sediment in this layer    
            end if
        end do
        call T%create("FineSediment_T", Me%nfComp)       ! create FineSediment object T
        call U%create("FineSediment_U", Me%nfComp)       ! create FineSediment object U
        do S = 1, Me%nSizeClasses                                    ! main loop for burial of sediment
                                                                     ! for each sediment size class, check whether the available capacity in the bed
                                                                     ! exceeds the amount of depositing sediment. If so, then bury sediment of this size class
                                                                     ! to provide the capacity for the depositing sediment
            if (.not. IsEmpty(S)) then                               ! only do if there is sediment of this size class in the bed
                A_f_sed = Me%Af_sediment(S)                         ! local copy of the capacity for this sediment size class in the whole bed [m3 m-2]
                V_f_burial = FS_dep(S)%V_f() 
                V_f_burial = FS_dep(S)%V_f() - A_f_sed               ! difference between volume of depositing sediment and available capacity
                                                                     ! if > 0, then sediment needs to be buried to create capacity for deposition
                if (V_f_burial > 0.0_dp) then                        ! do we need to bury sediment to create available capacity for deposition?
                    call T%set(Vf_in = V_f_burial, &
                              Vw_in = 0.0_dp, &
                              f_comp_in = FS_dep(S)%f_comp &
                                    )                                ! yes, so
                                                                     ! set up temporary FineSediment object T with volume of fine sediment requiring burial
                                                                     ! to compute the volume of water requiring burial, we must loop through layers
                                                                     ! from the top, compute for each layer the volume of fine sediment that must be
                                                                     ! removed to allow space for deposition, and the volume of water associated with the
                                                                     ! fine sediment
                    L = C%nSedimentLayers                            ! loop through layers, upwards from the bottom
                    do while (L > 0 .and. T%V_f() > 0)               ! use fine sediment volume in T as a counter. Through this loop, T holds the count of the  
                                                                     ! requirement for sediment burial that has not yet been accounted for by higher layers
                        associate (O => &
                            Me%colBedSedimentLayers(L)%item)         ! association to layer L
                            if (T%V_f() > O%C_f(S)) then        ! does the depositing fine sediment fit into this layer,
                                                                     ! after accounting for the capacity in layers above?
                                                                     ! no, the depositing sediment will not fit into this layer
                                                                     ! so increase water removal requirement by the water capacity of this layer
                                                                     ! and decrease the count of remaining depositing fine sediment by the capacity
                                call T%set( &
                                    Vf_in = T%V_f() - O%C_f(S), &
                                    Vw_in = T%V_w() + O%C_w(S) &
                                )
                            else                                     ! yes, depositing sediment fits into this layer
                                                                     ! so increase the water burial requirement by the amount required to maintain the SLR in this layer
                                                                     ! and set the count of fine sediment to zero, to jump out of the loop
                                tempV = T%V_f() / O%volSLR(S)       ! temporary variable
                                call T%set(Vf_in = 0.0_dp, &
                                           Vw_in = T%V_w() + tempV &
                                          )
                            end if
                        end associate
                        L = L - 1                                       ! decrement the layer count
                    end do                                              ! and loop
                                                                        ! now to actually bury fine sediment and water
                                                                        ! we still use the object T to hold the depositing material - firstly, its mass
                                                                        ! needs to be reset, as it was decremented to zero in the computation of the water requirement
                    call T%set(Vf_in = FS_dep(S)%V_f() - A_f_sed)       ! reset the fine sediment burial requirement, still using object T
                                                                        ! now we remove and bury material from the base of the sediment upwards, 
                                                                        ! to create sufficient space to accommodate deposited material
                    L = C%nSedimentLayers                               ! start with the bottom layer
                    do while (L > 0 .and. T%V_f() + T%V_w() > 0)        ! loop through each layer, while there is still material to bury
                        if (T%V_f() > 0) Then
                            associate(O => &
                                Me%colBedSedimentLayers(L)%item)        ! association reference to layer L object
                                call r%addErrors(.errors. &
                                    O%RemoveSediment(S, T, U) &
                                                )                       ! remove the sediment, return amount removed (U) and not removed (T), and any errors thrown
                            
                                if (r%hasCriticalError()) then          ! if RemoveSediment throws a critical error
                                    call r%addToTrace(tr)               ! add trace to all errors
                                    return                              ! and exit
                                end if
                                delta_l_b(L, S) = U%M_f()               ! delta for layer L to burial
                                delta_l_l(L, L, S) = &
                                    delta_l_l(L, L, S) - U%M_f()        ! delta for loss of sediment from L to burial
                            end associate
                        end if
                                                                        ! note that these are CHANGES in delta due to burial, not absolute values
                        L = L - 1                                       ! move up to next layer
                    end do                                              ! finished burial. temporary object T can be reused
                                                                        ! now we shift sediment downwards from upper layers to fill the hole created by burial
                    do L = C%nSedimentLayers, 2, -1                     ! downward shift of fine sediment. Loop through the layers, starting at the bottom
                                                                        ! and working upwards
                        assoc1 : associate &
                            (O => Me%colBedSedimentLayers(L)%item)      ! association to "receiving" layer L
                            A = L - 1                                   ! counter for "donating" layer - initially the layer above
                            call T%set(Vf_in = O%A_f(S), &
                                      Vw_in = O%A_w(S))            ! set FineSediment object T to hold the available capacity in the receiving layer i.e. the volumes that require shifting downwards
                                                                        ! Note no need to set f_comp in T
                            do while (A > 0 .and. T%IsNotEmpty())       ! loop through "donating" layers, moving upwards
                                assoc2 : associate (P => &
                                Me%colBedSedimentLayers(A)%item)        ! association to "donating" layer A
                                if (P%colFineSediment(S)%V_f() > 0) &
                                    then                                ! if there is sediment in the "donating" layer
                                        call r%addErrors(.errors. &
                                        P%RemoveSediment(S, T, U))      ! remove the sediment, return amounts removed (U) and not removed (T), the delta, and any errors thrown
                                        if (r%hasCriticalError()) then  ! if RemoveSediment throws a critical error
                                            call r%addToTrace(tr)       ! add trace to all errors
                                            return                      ! and exit
                                        end if
                                        delta_l_l(A, L, S) = &
                                            delta_l_l(A, L, S) + &
                                            U%M_f()                     ! delta for transfer of sediment from Layer A to Layer L, correcting for material previously buried
                                        delta_l_l(A, A, S) = &
                                            delta_l_l(A, A, S) - &
                                            U%M_f()                     ! delta for retention of sediment in Layer A
                                        call r%addErrors(.errors. &
                                            O%addSediment(S, U))        ! add the sediment in U to the "receiving" layer L
                                    if (r%hasCriticalError()) then      ! if AddSediment throws a critical error
                                        call r%addToTrace(tr)           ! add trace to all errors
                                        return                          ! and exit
                                    end if
                                end if
                                A = A - 1                               ! shift up to next "donating" layer
                                end associate assoc2
                            end do
                        end associate assoc1
                    end do
                end if
            end if
        end do
        V_w_tot = 0.0_dp                                                ! Initialise V_w_tot to zero
        do S = 1, Me%nSizeClasses                                       ! now add in the depositing sediment, work by size class
            do L = C%nSedimentLayers, 1, -1                             ! start with the bottom layer and work upwards
                if (FS_dep(S)%M_f() > 0.0_dp) then
                    associate(O => Me%colBedSedimentLayers(L)%item)     ! size class S in Layer L
                        if (O%A_f(S) > 0.0_dp .or. &
                            O%A_w(S) > 0.0_dp) then                ! if there is available capacity in this layer, add deposition here
                            V_w_b = FS_dep(S)%V_f() / O%volSLR(S)       ! the volume of water needed to maintain SLR in the "receiving" layer,
                            call FS_dep(S)%set(Vw_in = V_w_b)           ! if all deposition were to fit into this layer
                            M_f_la = FS_dep(S)%M_f()                    ! store the amount of sediment still to be deposited, for computation of deltas
                            call r%addErrors(.errors. &
                                O%addSediment(S, FS_dep(S)))            ! add the fine sediment in deposition. FS_dep(S) returns volumes that could not be added
                            if (r%hasCriticalError()) then              ! if addSediment throws a critical error
                                call r%addToTrace(tr)                   ! add trace to all errors
                                return                                  ! and exit
                            end if
                            delta_d_l(L, S) = &
                                M_f_la - FS_dep(S)%M_f()                ! delta_d_l: the mass of material deposited to this layer
                        end if
                        V_w_tot = V_w_tot + V_w_b - FS_dep(S)%V_w()     ! tally up V_w_b to compute water requirement to take from the water column
                    end associate
                end if
            end do
        end do
        r = Result(data = V_w_tot)                                      ! return Result object, with volume of water required from water column
        do S = 1, Me%nSizeClasses                                       ! incorporate delta_d_b, delta_d_l, delta_l_b, delta_l_l into the mass transfer coefficients matrix delta_sed 
            Me%delta_sed(C%nSedimentLayers + 3, 1, S) = &
                Me%delta_sed(C%nSedimentLayers + 3, 1, S) + delta_d_b(S)    ! element (S) of delta_d_b is added to element (Layers+3, 1, S) of Me%delta_sed
            do L = 1, C%nSedimentLayers
                Me%delta_sed(L + 2, 1, S) = &
                    Me%delta_sed(L + 2, 1, S) + delta_d_l(L, S)         ! element (L, S) of delta_d_l is added to element (L+2, 1, S) of Me%delta_sed
                Me%delta_sed(C%nSedimentLayers + 3, L + 2, S) = &
                    Me%delta_sed(C%nSedimentLayers + 3, L + 2, S) + &
                    delta_l_b(L, S)                                     ! element (L, S) of delta_l_b is added to element (Layers+3, L+2, S) of Me%delta_sed
                do LL = 1, C%nSedimentLayers
                    if (isZero(Me%delta_sed(L + 2, LL + 2, S))) then
                        Me%delta_sed(L + 2, LL + 2, S) = 0.0_dp
                    end if
                    Me%delta_sed(L + 2, LL + 2, S) = &
                        Me%delta_sed(L + 2, LL + 2, S) + &
                        delta_l_l(LL, L, S)                             ! element (LL, L, S) of delta_l_l is added to element (L+2, LL+2, S) of Me%delta_sed
                end do
            end do
        end do
    end function

    !> **Function purpose**
    !! initialise the matrix of mass transfer coefficients for sediment deposition and resuspension
    !!                                                          
    !! **Function inputs**                                      
    !! none (uses class-level variable array delta_sed(:,:,:))
    !!                                                          
    !! **Function outputs/outcomes**                            
    !! delta_sed populated with initial values, all zero except for layer(x) ->layer(y) coefficients where x=y; these are set to unity
    ! function initialiseMatrix1(Me) result(r)
    !     class(BedSediment1) :: Me                                    !! The `BedSediment` instance
    !     type(Result) :: r                                            !! `Result` object. Returns water requirement from the water column [m3 m-2], real(dp)
    !     character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
    !     integer :: L                                                 ! LOCAL loop counter for sediment layers
    !     integer :: LL                                                ! LOCAL second loop counter for sediment layers
    !     integer :: S                                                 ! LOCAL loop counter for size classes
    !     tr = trim(Me%name) // "%initialiseMatrix1"                   ! object and procedure binding name as trace
    !     if (size(Me%delta_sed, 1) /= C%nSedimentLayers + 3 .or. &
    !         size(Me%delta_sed, 2) /= C%nSedimentLayers + 3.or. &
    !         size(Me%delta_sed, 3) /= Me%nSizeClasses) then
    !         call r%addError(ErrorInstance(1, &
    !                  tr // "Array size error", .true., [tr] &
    !                                      ) &
    !                        )                                         ! create a critical error if there is an array size issue
    !         return
    !     end if
    !     me%delta_sed = 0.0_dp                                       ! Initialise to zero
    ! end function
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
        ! print *, trim(Me%name)                                       !! the name of this layer
        do n=1, C%nSedimentLayers
            ! print *, "Layer ", n
            call Me%colBedSedimentLayers(n)%item%repMass()           !! print out mass of FS in each layer, by size class [kg/m2]
        end do
        ! print *, "Total: ", .real. r                                 ! print out mass of FS in bed [kg/m2]
    end subroutine

end module

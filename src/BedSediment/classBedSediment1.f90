!> Module containing definition of `BedSediment1`.
module classBedSediment1
    use Globals
    use UtilModule
    use ResultModule
    use spcBedSediment
    use classBedSedimentLayer1
    use classFineSediment1
    use Spoof
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
    subroutine getMTCMatrix1(me, djdep, djres)
        class(BedSediment1) :: me                                    !! Self-reference
        real(dp) :: djdep(:)                                         !! deposition fluxes by size class [kg/m2]
        real(dp) :: djres(:)                                         !! resuspension fluxes by size class [kg/m2]
        real(dp) :: ml                                               ! LOCAL holds initial sediment layer masses [kg/m2]
        integer :: L                                                 ! LOCAL loop counter
        integer :: LL                                                ! LOCAL loop counter
        integer :: S                                                 ! LOCAL loop counter

        do S = 1, me%nSizeClasses
            do L = 3, C%nSedimentLayers + 3 
                if (.not. isZero(djdep(S)) .and. .not. isZero(me%delta_sed(L, 1, S))) then
                    me%delta_sed(L, 1, S) = &
                        me%delta_sed(L, 1, S) / djdep(S)             ! d -> l and d-> b
                else
                    me%delta_sed(L, 1, S) = 0                        ! failsafe if no deposition
                end if 
            end do
            do LL = 3, C%nSedimentLayers + 2
                if (.not. isZero(djres(S)) .and. .not. isZero(me%delta_sed(2, LL, S))) then
                    ml = me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup() ! Phew!
                    me%delta_sed(2, LL, S) = &
                        ! me%delta_sed(2, LL, S) / djres(S)            ! l -> r
                        me%delta_sed(2, LL, S) / ml                     ! l -> r
                else
                    me%delta_sed(2, LL, S) = 0                       ! failsafe if no resuspension
                end if
            end do
            do L = 3, C%nSedimentLayers + 3
                do LL = 3, C%nSedimentLayers + 2
                    ml = me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup() ! Phew!
                    ! print *, "per layer, S, from, to", S, LL - 2, L - 2, ml
                    if (.not. isZero(ml)) then
                        if (L == LL) then
                            if (.not. isZero(me%delta_sed(L, LL, S))) then 

                                me%delta_sed(L, LL, S) = &
                                (ml + me%delta_sed(L, LL, S)) / ml   ! l -> l where l=l ('same-layer' transfers) and there is a mass transfer out of the layer
                            else
                                me%delta_sed(L, LL, S) = 1.0_dp         ! If no transfer, must be 1 TODO check this
                            end if
                        else
                            me%delta_sed(L, LL, S) = &
                                me%delta_sed(L, LL, S) / ml          ! l -> l where l/=l (interlayer transfers), also l -> b
                        end if
                    else
                        me%delta_sed(L, LL, S) = 0.0_dp              ! failsafe if no sediment initially in layer 
                    end if
                end do
            end do
        end do
        ! Convert delta_sed to CSR storage, to speed up NM transfer during simulation
        do s = 1, C%nSizeClassesSpm
            me%delta_sed_csr(s) = CSRMatrix(me%delta_sed(:,:,s))
        end do
    end subroutine
    !> **Function purpose**                                         <br>
    !! Initialise a BedSediment object.
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Initialised `BedSediment` object, including all layers and included `FineSediment`
    !! objects
    function createBedSediment1(me, x, y, w) result(r)
        class(BedSediment1) :: me                                    !! Self-reference
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
        me%nSizeClasses = C%nSizeClassesSpm                          ! set number of size classes from global value
        me%nfComp = C%nFracCompsSpm                                  ! set number of compositional fractions from global value
        tr = trim(me%name) // "%createBedSediment1"                  ! procedure name as trace

        ! Initialise NM mass pools matrix
        allocate(me%M_np(C%nSedimentLayers + 3, C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%C_np_byMass(C%nSedimentLayers, C%npDim(1), C%npDim(2), C%npDim(3)))
        allocate(me%delta_sed_csr(C%nSizeClassesSpm))
        me%M_np = 0.0_dp
        me%C_np_byMass = 0.0_dp

        allocate(me%colBedSedimentLayers(C%nSedimentLayers))        ! Create BedSedimentLayer collection
        me%n_delta_sed = C%nSedimentLayers + 3                      ! The order of the delta_sed matrix
        allocate(me%delta_sed(C%nSedimentLayers + 3, &
                              C%nSedimentLayers + 3, &
                              me%nSizeClasses))                         ! allocate space for sediment mass transfer matrix
        me%delta_sed = 0.0_dp                                           ! initialise to zero

        do L = 1, C%nSedimentLayers                                         ! loop through each layer
            allocate(bsl1)                                           ! allocate the temporary local BedSedimentLayer variable
            call r%addErrors(.errors. bsl1%create(L))                   ! initialise the layer object
            allocate(me%colBedSedimentLayers(L)%item, &
                source=bsl1, stat = allst)                   ! allocate empty object of this type
            deallocate(bsl1)                                 ! deallocate local variable ready for the next iteration of the loop
            if (allst /= 0) then
                call r%addError(ErrorInstance( &
                                   code = 1, &
                                message = ms, &
                                  trace = [tr]))             ! add to Result
                return                                       ! critical error, so return
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
    function destroyBedSediment1(me) result(r)
        class(BedSediment1) :: me                                    !! self-reference
        type(Result) :: r                                            !! returned Result object
        type(ErrorInstance) :: er                                    ! LOCAL ErrorInstance object for error handling.
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        integer :: L                                                 ! LOCAL Loop iterator
        integer :: allst                                             ! LOCAL array allocation status
        character(len=18), parameter :: ms = "Deallocation error"    ! LOCAL CONSTANT error message

        do L = 1, C%nSedimentLayers
            call r%addErrors(.errors. &
                me%colBedSedimentLayers(L)%item%destroy())           ! destroy enclosed BedSedimentLayers
        end do
        tr = trim(me%name) // &
            "%destroyBedSedimentLayer1%colBedSedimentLayers"         ! trace message
        deallocate(me%colBedSedimentLayers, stat = allst)            ! deallocate all allocatable variables
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

        ! Perform the transfer calculation to move NM between the layers
        do k = 1, C%nSizeClassesSpm
            do j = 1, C%npDim(2)
                do i = 1, C%npDim(1)
                    ! Below are a number of different matrix multiplication methods. Generally, the fastest
                    ! is when delta_sed is stored in CSR format, for setups with ~5 sediment layers. You may
                    ! wish to play around with other methods if your setup typically uses fewer or more
                    ! sediment layers. This function is generally the most computationally expensive in the model.
                    ! If changing storage format, make sure delta_sed_dia or delta_sed_csr are initialised

                    ! CSR storage implementation
                    me%M_np(:,i,j,k+2) = me%delta_sed_csr(k)%multiply(me%M_np(:,i,j,k+2))
                    ! Set NM concentration for all layers 
                    me%C_np_byMass(:,i,j,k+2) = divideCheckZero(me%M_np(3:C%nSedimentLayers+2,i,j,k+2), me%Mf_bed_layer_array())

                    ! Matmul implementation. Might be faster for <5 sediment layers
                    ! me%M_np(:,i,j,k+2) = matmul(me%delta_sed(:,:,k), me%M_np(:,i,j,k+2))

                    ! Diagonal storage implementation. Might be faster for >5 sediment layers,
                    ! especially if transfers typically only between adjacent layers
                    ! me%M_np(:,i,j,k+2) = me%delta_sed_dia(k)%multiply(me%M_np(:,i,j,k+2))

                    ! OpenBLAS implementation. Might be faster for >5 sediment layers and if
                    ! transfers across multiple layers are possible. Make sure you have OpenBLAS/BLAS
                    ! installed and linked when compiling
                    ! call dgemv('n', me%n_delta_sed, me%n_delta_sed, 1.0_dp, me%delta_sed(:,:,k), &
                    !             me%n_delta_sed, me%M_np(:,i,j,k+2), 1, 0.0_dp, me%M_np(:,i,j,k+2), 1) 
                end do
            end do
        end do

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
    function resuspendSediment1(me, FS_resusp) result(r)
        class(BedSediment1) :: me                                    !! Self-reference
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
        
        tr = trim(me%name) // "%resuspendSediment1"                  ! error trace for this procedure
        ! Create fine sediment objects F and G
        call F%create("FineSediment", me%nfComp)
        call G%create("FineSediment", me%nfComp)
        allocate(FS(me%nSizeClasses, C%nSedimentLayers))            ! set up FineSediment1 array FS
        allocate(delta_l_r(C%nSedimentLayers, me%nSizeClasses))     ! allocate delta_d-l
        me%delta_sed = 0.0_dp                                       ! Reset the matrix of mass transfer coefficients
        delta_l_r = 0.0_dp                                          ! initialise the delta_l_r values
        do S = 1, me%nSizeClasses
            do L = 1, C%nSedimentLayers                          
                ! back up all the fine sediment masses, an essential part of the mass trasfer matrix computation
                call me%colBedSedimentLayers(L)%item%colFineSediment(S)%backup_M_f()
            end do
        end do        
                                                                     ! main loop
                                                                     ! for each size class (1 to S), remove the required amount of fine sediment
                                                                     ! from the bed, by looping through each layer from top to bottom
        do S = 1, me%nSizeClasses                                    ! loop through all size classes
            call F%set(Mf_in = FS_resusp(S))                        ! set up F with the mass of fine sediment in this size class to be resuspended [kg]
            L = 1                                                    ! start with top layer
            do while (FS_resusp(S) > 0.000001 .and. L <= C%nSedimentLayers) ! loop through layers until all sediment resuspended or all layers considered
                associate(O => me%colBedSedimentLayers(L)%item)      ! association for brevity
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
        do S = 1, me%nSizeClasses                                    ! incorporate delta_l_r into the mass transfer coefficients matrix delta_sed
            do L = 1, C%nSedimentLayers                            
                me%delta_sed(2, L + 2, S) = &
                    me%delta_sed(2, L + 2, S) + delta_l_r(L, S)      ! element (L, S) of delta_l_r is added to element (2, L+2, S) of delta_sed
                me%delta_sed(L + 2, L + 2, S) = &
                    me%delta_sed(L + 2, L + 2, S) - delta_l_r(L, S)  ! element (L, S) of delta_l_r is subtracted from element(L+2, L+2, S) of delta_sed
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
    function depositSediment1(me, FS_dep) result(r)
        class(BedSediment1) :: me                                    !! Self-reference
        type(FineSediment1) :: FS_dep(:)                             !! Depositing sediment by size class
        type(Result0D) :: r                                          !! `Result` object. Returns water requirement from the water column [m3 m-2], real(dp)
        type(FineSediment1) :: T                                    ! LOCAL object to receive sediment being buried
        type(FineSediment1) :: U                                    ! LOCAL object to receive sediment that has been buried
        integer :: s                                                 ! LOCAL loop counter for size classes
        integer :: l                                                 ! LOCAL counter for layers
        integer :: ll                                                ! LOCAL second counter for layers
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
        logical, allocatable :: isEmpty(:)                           ! LOCAL .true. bed has been emptied completely to make space for depositing sediment
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace

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
        
        ! Add this procedure to the trace, allocate space to arrays and initialise them
        tr = trim(me%name) // "%DepositSediment1"
        allocate(IsEmpty(me%nSizeClasses))
        allocate(delta_d_l(C%nSedimentLayers, me%nSizeClasses))
        allocate(delta_d_b(me%nSizeClasses))
        allocate(delta_l_b(C%nSedimentLayers, me%nSizeClasses))
        allocate(delta_l_l(C%nSedimentLayers, C%nSedimentLayers, me%nSizeClasses))
        delta_d_b = 0.0_dp
        delta_d_l = 0.0_dp
        delta_l_b = 0.0_dp
        delta_l_l = 0.0_dp
        isEmpty = .true.

        do s = 1, me%nSizeClasses                                   ! loop through all size classes
            dep_excess = FS_dep(S)%V_f() - me%Cf_sediment(S)        ! compute the difference between the volume of depositing material and the bed capacity [m3 m-2]
                                                                    ! if this equals or exceeds zero, then there is complete replacement of the material in the bed and if
                                                                    ! it exceeds zero, there is direct burial of a portion of the depositing sediment
                                                                    ! in this case, delta[l,n-b] = 1 for all layers, and delta[d-b] > 0.
            if (dep_excess > 0) then                                ! check whether the depositing sediment in each size class exceeds the total
                    associate(O => me%colBedSedimentLayers)         ! association for brevity
                        do l = 1, C%nSedimentLayers                 ! capacity for that size fraction in the bed. If so, then remove all fine sediment, water and
                            delta_l_b(l, s) = O(l)%item%colFineSediment(s)%M_f()        ! delta l -> b
                            delta_l_l(l, l, s) = -O(l)%item%colFineSediment(s)%M_f()    ! delta l -> l
                            call O(l)%item%colFineSediment(s)%ClearAll()  ! fractional compositions from all layers for this size class
                        end do
                        delta_d_b(S) = dep_excess * &
                            FS_dep(s)%rho_part()                     ! delta for deposition to burial
                    end associate
            else
                isEmpty(s) = .false.                                 ! set flag to indicate that there is sediment in this layer    
            end if
        end do
        call T%create("FineSediment_T", me%nfComp)       ! create FineSediment object T
        call U%create("FineSediment_U", me%nfComp)       ! create FineSediment object U
        do s = 1, me%nSizeClasses                                    ! main loop for burial of sediment
                                                                     ! for each sediment size class, check whether the available capacity in the bed
                                                                     ! exceeds the amount of depositing sediment. If so, then bury sediment of this size class
                                                                     ! to provide the capacity for the depositing sediment
            if (.not. isEmpty(S)) then                               ! only do if there is sediment of this size class in the bed
                A_f_sed = me%Af_sediment(S)                         ! local copy of the capacity for this sediment size class in the whole bed [m3 m-2]
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
                    l = C%nSedimentLayers                            ! loop through layers, upwards from the bottom
                    do while (l > 0 .and. T%V_f() > 0)               ! use fine sediment volume in T as a counter. Through this loop, T holds the count of the  
                                                                     ! requirement for sediment burial that has not yet been accounted for by higher layers
                        associate (O => me%colBedSedimentLayers(l)%item)         ! association to layer L
                            if (T%V_f() > O%C_f(s)) then        ! does the depositing fine sediment fit into this layer,
                                                                     ! after accounting for the capacity in layers above?
                                                                     ! no, the depositing sediment will not fit into this layer
                                                                     ! so increase water removal requirement by the water capacity of this layer
                                                                     ! and decrease the count of remaining depositing fine sediment by the capacity
                                call T%set( &
                                    Vf_in = T%V_f() - O%C_f(s), &
                                    Vw_in = T%V_w() + O%C_w(s))
                            else                                     ! yes, depositing sediment fits into this layer
                                                                     ! so increase the water burial requirement by the amount required to maintain the SLR in this layer
                                                                     ! and set the count of fine sediment to zero, to jump out of the loop
                                tempV = T%V_f() / O%volSLR(s)       ! temporary variable
                                call T%set(Vf_in = 0.0_dp, &
                                           Vw_in = T%V_w() + tempV)
                            end if
                        end associate
                        l = l - 1                                       ! decrement the layer count
                    end do                                              ! and loop
                                                                        ! now to actually bury fine sediment and water
                                                                        ! we still use the object T to hold the depositing material - firstly, its mass
                                                                        ! needs to be reset, as it was decremented to zero in the computation of the water requirement
                    call T%set(Vf_in = FS_dep(s)%V_f() - A_f_sed)       ! reset the fine sediment burial requirement, still using object T
                                                                        ! now we remove and bury material from the base of the sediment upwards, 
                                                                        ! to create sufficient space to accommodate deposited material
                    l = C%nSedimentLayers                               ! start with the bottom layer
                    do while (l > 0 .and. T%V_f() + T%V_w() > 0)        ! loop through each layer, while there is still material to bury
                        if (T%V_f() > 0) Then
                            associate(O => &
                                me%colBedSedimentLayers(l)%item)        ! association reference to layer L object
                                call r%addErrors(.errors. &
                                    O%RemoveSediment(S, T, U) &
                                                )                       ! remove the sediment, return amount removed (U) and not removed (T), and any errors thrown
                            
                                if (r%hasCriticalError()) then          ! if RemoveSediment throws a critical error
                                    call r%addToTrace(tr)               ! add trace to all errors
                                    return                              ! and exit
                                end if
                                delta_l_b(l, s) = U%M_f()               ! delta for layer L to burial
                                delta_l_l(l, l, s) = &
                                    delta_l_l(l, l, s) - U%M_f()        ! delta for loss of sediment from L to burial
                            end associate
                        end if
                                                                        ! note that these are CHANGES in delta due to burial, not absolute values
                        l = l - 1                                       ! move up to next layer
                    end do                                              ! finished burial. temporary object T can be reused
                                                                        ! now we shift sediment downwards from upper layers to fill the hole created by burial
                    do l = C%nSedimentLayers, 2, -1                     ! downward shift of fine sediment. Loop through the layers, starting at the bottom
                                                                        ! and working upwards
                        assoc1 : associate(O => me%colBedSedimentLayers(L)%item)      ! association to "receiving" layer L
                            A = l - 1                                   ! counter for "donating" layer - initially the layer above
                            call T%set(Vf_in = O%A_f(s), &
                                      Vw_in = O%A_w(s))            ! set FineSediment object T to hold the available capacity in the receiving layer i.e. the volumes that require shifting downwards
                                                                        ! Note no need to set f_comp in T
                            do while (A > 0 .and. T%IsNotEmpty())       ! loop through "donating" layers, moving upwards
                                assoc2 : associate (P => me%colBedSedimentLayers(A)%item)        ! association to "donating" layer A
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
        do s = 1, me%nSizeClasses                                       ! now add in the depositing sediment, work by size class
            do l = C%nSedimentLayers, 1, -1                             ! start with the bottom layer and work upwards
                if (FS_dep(s)%M_f() > 0.0_dp) then
                    associate(O => me%colBedSedimentLayers(l)%item)     ! size class S in Layer L
                        if (O%A_f(s) > 0.0_dp .or. &
                            O%A_w(s) > 0.0_dp) then                ! if there is available capacity in this layer, add deposition here
                            V_w_b = FS_dep(s)%V_f() / O%volSLR(s)       ! the volume of water needed to maintain SLR in the "receiving" layer,
                            call FS_dep(s)%set(Vw_in = V_w_b)           ! if all deposition were to fit into this layer
                            M_f_la = FS_dep(s)%M_f()                    ! store the amount of sediment still to be deposited, for computation of deltas
                            call r%addErrors(.errors. &
                                O%addSediment(s, FS_dep(s)))            ! add the fine sediment in deposition. FS_dep(S) returns volumes that could not be added
                            if (r%hasCriticalError()) then              ! if addSediment throws a critical error
                                call r%addToTrace(tr)                   ! add trace to all errors
                                return                                  ! and exit
                            end if
                            delta_d_l(l, s) = M_f_la - FS_dep(s)%M_f()                ! delta_d_l: the mass of material deposited to this layer
                        end if
                        V_w_tot = V_w_tot + V_w_b - FS_dep(s)%V_w()     ! tally up V_w_b to compute water requirement to take from the water column
                    end associate
                end if
            end do
        end do
        r = Result(data = V_w_tot)                                      ! return Result object, with volume of water required from water column
        do s = 1, me%nSizeClasses                                       ! incorporate delta_d_b, delta_d_l, delta_l_b, delta_l_l into the mass transfer coefficients matrix delta_sed 
            me%delta_sed(C%nSedimentLayers + 3, 1, S) = &
                me%delta_sed(C%nSedimentLayers + 3, 1, S) + delta_d_b(S)    ! element (S) of delta_d_b is added to element (Layers+3, 1, S) of me%delta_sed
            do L = 1, C%nSedimentLayers
                me%delta_sed(L + 2, 1, S) = &
                    me%delta_sed(L + 2, 1, S) + delta_d_l(L, S)         ! element (L, S) of delta_d_l is added to element (L+2, 1, S) of me%delta_sed
                me%delta_sed(C%nSedimentLayers + 3, L + 2, S) = &
                    me%delta_sed(C%nSedimentLayers + 3, L + 2, S) + &
                    delta_l_b(L, S)                                     ! element (L, S) of delta_l_b is added to element (Layers+3, L+2, S) of me%delta_sed
                do LL = 1, C%nSedimentLayers
                    if (isZero(me%delta_sed(L + 2, LL + 2, S))) then
                        me%delta_sed(L + 2, LL + 2, S) = 0.0_dp
                    end if
                    me%delta_sed(L + 2, LL + 2, S) = &
                        me%delta_sed(L + 2, LL + 2, S) + &
                        delta_l_l(LL, L, S)                             ! element (LL, L, S) of delta_l_l is added to element (L+2, LL+2, S) of me%delta_sed
                end do
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
    subroutine ReportBedMassToConsole1(me)
        class(BedSediment1) :: me                                    !! The `BedSediment` instance
        integer :: n                                                 !! LOCAL loop counter 
        do n=1, C%nSedimentLayers
            call me%colBedSedimentLayers(n)%item%repMass()           !! print out mass of FS in each layer, by size class [kg/m2]
        end do
    end subroutine

end module

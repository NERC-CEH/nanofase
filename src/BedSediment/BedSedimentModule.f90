!> Module containing definition of `BedSediment`.
module BedSedimentModule
    use GlobalsModule
    use UtilModule
    use ResultModule
    use AbstractBedSedimentModule
    use BedSedimentLayerModule
    use FineSedimentModule
    use ContaminantModule
    use Spoof
    use LoggerModule, only: LOGR
    implicit none
    private

    !> Class representing a `BedSediment` object, which is an extension of the
    !! abstract superclass `BedSediment`.
    type, public, extends(AbstractBedSediment) :: BedSediment
        contains
            procedure, public :: create                 => createBedSediment1
            procedure, public :: destroy                => destroyBedSediment1
            procedure, public :: deposit                => DepositSediment1
            procedure, public :: resuspend              => ResuspendSediment1
            procedure, public :: repmass                => ReportBedMassToConsole1
            procedure, public :: getmatrix              => getMTCMatrix1
            procedure, public :: transferContaminant    => transferContaminantBedSediment1
            procedure, public :: deposit_spm            => deposit_spm_BedSediment
            procedure, public :: resuspend_spm          => resuspend_spm_BedSediment
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
    
    !! Derive a mass transfer coefficient matrix
    subroutine getMTCMatrix1(me, djdep, djres)
        class(BedSediment) :: me            !! Self-reference
        real(dp) :: djdep(:)                !! deposition fluxes by size class [kg/m2]
        real(dp) :: djres(:)                !! resuspension fluxes by size class [kg/m2]
        real(dp) :: ml                      ! LOCAL holds initial sediment layer masses [kg/m2]
        integer :: L, LL, S                 ! Iterators

        do S = 1, me%nSizeClasses
            ! 1. Normalize Deposition Columns (L=Layer, 1=DepositionSource)
            do L = 3, C%nSedimentLayers + 3 
                if (.not. isZero(djdep(S)) .and. .not. isZero(me%delta_sed(L, 1, S))) then
                    me%delta_sed(L, 1, S) = me%delta_sed(L, 1, S) / djdep(S)
                else
                    me%delta_sed(L, 1, S) = 0.0_dp
                end if 
            end do

            ! 2. Normalize Resuspension Row (2=ResuspensionTarget, LL=LayerSource)
            do LL = 3, C%nSedimentLayers + 2
                ! Note: normalization base is the Layer Mass (ml), not the flux. 
                ! The flux calculation happened in resuspendSediment1.
                ml = me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup()
                
                if (.not. isZero(ml) .and. .not. isZero(me%delta_sed(2, LL, S))) then
                    me%delta_sed(2, LL, S) = me%delta_sed(2, LL, S) / ml
                else
                    me%delta_sed(2, LL, S) = 0.0_dp
                end if
            end do

            ! 3. Normalize Layer-to-Layer transfers (Burial/Mixing)
            do L = 3, C%nSedimentLayers + 3
                do LL = 3, C%nSedimentLayers + 2
                    ml = me%colBedSedimentLayers(LL - 2)%item%colFineSediment(S)%M_f_backup()
                    
                    if (.not. isZero(ml)) then
                        if (L == LL) then
                            ! Same-layer retention
                            if (.not. isZero(me%delta_sed(L, LL, S))) then 
                                ! Add remaining mass back to get retention coefficient
                                me%delta_sed(L, LL, S) = (ml + me%delta_sed(L, LL, S)) / ml
                            else
                                me%delta_sed(L, LL, S) = 1.0_dp
                            end if
                        else
                            ! Inter-layer transfer
                            me%delta_sed(L, LL, S) = me%delta_sed(L, LL, S) / ml
                        end if
                    else
                        me%delta_sed(L, LL, S) = 0.0_dp
                    end if
                end do
            end do
        end do
        
        ! Convert to CSR for fast multiplication in transferContaminant
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
        class(BedSediment) :: me
        integer            :: x, y, w
        type(Result)       :: r
        type(BedSedimentLayer), allocatable :: bsl1
        integer            :: L, allst
        character(len=256) :: tr
        character(len=16), parameter :: ms = "Allocation error"
        type(ErrorInstance) :: err(1)
        integer :: nx, ny
        logical :: inbounds
        integer :: nComp, ii, jj
        real(dp), allocatable :: I(:, :)

        me%name = trim(ref('BedSediment', x, y, w))
        ! >>> critical: make sure grid indices are set so initial conditions use [x,y]
        me%x = x
        me%y = y
        ! <<<

        me%nSizeClasses = C%nSizeClassesSpm
        me%nfComp       = C%nFracCompsSpm
        tr = trim(me%name) // "%createBedSediment1"

        ! Contaminant pools: [1]=interface (dep), [2]=ready to resuspend, [3..N+2]=layers, [N+3]=buried
        allocate(me%m_contaminant(C%nSedimentLayers + 3), stat=allst)
        if (allst /= 0) then
            err(1) = ErrorInstance(code=1, message=ms, trace=[tr])
            call r%addError(err(1))
            return
        end if

        do L = 1, C%nSedimentLayers + 3
            r = me%m_contaminant(L)%create_from_data( &
                compartment='sediment', &
                contaminantDensity=DATASET%contaminantDensity, &
                soilAttachmentEfficiency=DATASET%soilAttachmentEfficiencyConstant, &
                riverAttachmentEfficiency=DATASET%riverAttachmentEfficiency, &
                estuaryAttachmentEfficiency=DATASET%estuaryAttachmentEfficiency, &
                k_diss_pristine=DATASET%contaminant_k_diss_pristine, &
                k_diss_transformed=DATASET%contaminant_k_diss_transformed, &
                k_transform_pristine=DATASET%contaminant_k_transform_pristine, &
                waterTemperature=real(DATASET%waterTemperature(1), dp) )
            if (r%hasCriticalError()) return

            ! Seed initial concentrations by layer (if provided)
            if (L > 2 .and. allocated(DATASET%initialContaminantConcsSediment)) then
                nx = size(DATASET%initialContaminantConcsSediment,1)
                ny = size(DATASET%initialContaminantConcsSediment,2)
                inbounds = (me%x>=1 .and. me%y>=1 .and. me%x<=nx .and. me%y<=ny)
                if (inbounds) then
                    me%m_contaminant(L)%c = DATASET%initialContaminantConcsSediment(me%x, me%y, :, :, :)
                    if (allocated(DATASET%initialDissolvedConcsSediment)) &
                        me%m_contaminant(L)%m_dissolved = DATASET%initialDissolvedConcsSediment(me%x, me%y)
                else
                    me%m_contaminant(L)%c           = 0.0_dp
                    me%m_contaminant(L)%m_dissolved = 0.0_dp
                end if
            end if
        end do

        ! Build the sediment layers
        allocate(me%colBedSedimentLayers(C%nSedimentLayers), stat=allst); if (allst /= 0) then
            err(1) = ErrorInstance(code=1, message=ms, trace=[tr]); call r%addError(err(1)); return
        end if
        do L = 1, C%nSedimentLayers
            allocate(bsl1)
            call r%addErrors(.errors. bsl1%create(L))
            allocate(me%colBedSedimentLayers(L)%item, source=bsl1, stat=allst)
            deallocate(bsl1)
            if (allst /= 0) then
                err(1) = ErrorInstance(code=1, message=ms, trace=[tr]); call r%addError(err(1)); return
            end if
        end do

        ! Mass-transfer matrix (dense + CSR). Start as identity so early-step transfers are no-ops.
        me%n_delta_sed = C%nSedimentLayers + 3
        allocate(me%delta_sed(me%n_delta_sed, me%n_delta_sed, me%nSizeClasses)); me%delta_sed = 0.0_dp
        allocate(me%delta_sed_csr(C%nSizeClassesSpm))
        allocate(I(me%n_delta_sed, me%n_delta_sed)); I = 0.0_dp
        do ii=1, me%n_delta_sed; I(ii,ii) = 1.0_dp; end do
        do jj=1, C%nSizeClassesSpm
            me%delta_sed_csr(jj) = CSRMatrix(I)
        end do
        deallocate(I)

        call r%addToTrace(me%name // "%create: ok")
    end function

    !> **Function purpose**                                         <br>
    !! Deallocate all allocatable variables and call destroy methods for all
    !! enclosed objects
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! Returns a warning if any deallocation throws an error
    function destroyBedSediment1(me) result(r)
        class(BedSediment) :: me                                    !! self-reference
        type(Result) :: r                                            !! returned Result object
        type(ErrorInstance) :: er                                    ! LOCAL ErrorInstance object for error handling.
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        integer :: L, i                                              ! LOCAL Loop iterator
        integer :: allst                                             ! LOCAL array allocation status
        character(len=18), parameter :: ms = "Deallocation error"    ! LOCAL CONSTANT error message

        tr = trim(me%name) // "%destroyBedSedimentLayer%colBedSedimentLayers"
        do L = 1, C%nSedimentLayers
            call r%addErrors(.errors. me%colBedSedimentLayers(L)%item%destroy())
        end do
        if (allocated(me%m_contaminant)) then
            do i = 1, size(me%m_contaminant)
                call me%m_contaminant(i)%finalise()
            end do
            deallocate(me%m_contaminant, stat=allst)
            if (allst /= 0) then
                er = ErrorInstance(code=1, message=ms, trace=[tr])
                call r%addError(er)
                call LOGR%toFile(errors=[er])
            end if
        end if
        if (allocated(me%colBedSedimentLayers)) then
            deallocate(me%colBedSedimentLayers, stat=allst)
            if (allst /= 0) then
                er = ErrorInstance(code=1, message=ms, trace=[tr])
                call r%addError(er)
                call LOGR%toFile(errors=[er])
            end if
        end if
        if (allocated(me%delta_sed)) then
            deallocate(me%delta_sed, stat=allst)
            if (allst /= 0) then
                er = ErrorInstance(code=1, message=ms, trace=[tr])
                call r%addError(er)
                call LOGR%toFile(errors=[er])
            end if
        end if
        if (allocated(me%delta_sed_csr)) then
            deallocate(me%delta_sed_csr, stat=allst)
            if (allst /= 0) then
                er = ErrorInstance(code=1, message=ms, trace=[tr])
                call r%addError(er)
                call LOGR%toFile(errors=[er])
            end if
        end if
    end function

    !> Transfer Contaminant between sediment layers, based on the mass transfer coefficient
    !! matrix delta_sed, which should already have been set prior to calling this procedure
    !> P-FASE phase-aware PFAS transfer between water-column deposition,
    !! bed layers, resuspension target, and burial.
    !!
    !! Storage convention in m_contaminant(:):
    !!   1 = deposition inlet/interface scratch
    !!   2 = resuspension outlet scratch
    !!   3..C%nSedimentLayers+2 = bed sediment layers
    !!   C%nSedimentLayers+3 = buried mass
    !!
    !! Deposited PFAS enters as PFAS_SPM from the water column and becomes
    !! PFAS_SOL in bed layers/burial. Resuspended bed PFAS leaves as PFAS_SPM.
    function transferContaminantBedSediment1(me, j_contaminant_dep) result(r)
        class(BedSediment), intent(inout) :: me
        type(Contaminant),  intent(in)    :: j_contaminant_dep
        type(Result) :: r
        type(ErrorInstance) :: err(1)
        real(dp), allocatable :: state_in(:)
        real(dp), allocatable ::state_out(:)
        real(dp), allocatable ::A(:,:)
        real(dp), allocatable ::weights(:)
        real(dp) :: wsum
        integer :: nCompartments, s, n, f, i
        character(len=256) :: tr

        tr = trim(me%name) // "%transferContaminantBedSediment1"
        if (.not. allocated(me%m_contaminant)) then
            err(1) = ErrorInstance(code=105, message="Contaminant array not allocated", trace=[tr])
            call r%addError(err(1))
            return
        end if
        if (.not. allocated(j_contaminant_dep%c)) return

        nCompartments = C%nSedimentLayers + 3
        allocate(state_in(nCompartments), state_out(nCompartments), A(nCompartments,nCompartments), weights(me%nSizeClasses))

        ! Build one aggregate sediment-transfer matrix. PFAS is not size-resolved;
        ! sediment size classes only determine how much carrier sediment moves.
        weights = 0.0_dp
        do s = 1, me%nSizeClasses
            weights(s) = sum(abs(me%delta_sed(:,:,s)))
        end do
        wsum = sum(weights)
        A = 0.0_dp
        if (wsum > C%epsilon) then
            do s = 1, me%nSizeClasses
                A = A + me%delta_sed(:,:,s) * (weights(s) / wsum)
            end do
        else
            do i = 1, nCompartments
                A(i,i) = 1.0_dp
            end do
        end if

        do n = 1, size(j_contaminant_dep%c,1)
            do f = 1, size(j_contaminant_dep%c,2)
                state_in = 0.0_dp

                ! Incoming deposited SPM-associated PFAS from water column.
                state_in(1) = j_contaminant_dep%c(n,f,PFAS_SPM)

                ! Existing resuspension scratch, if any.
                state_in(2) = me%m_contaminant(2)%c(n,f,PFAS_SPM)

                ! Existing bed and buried PFAS are stored as solid-sorbed phase.
                do i = 3, nCompartments
                    state_in(i) = me%m_contaminant(i)%c(n,f,PFAS_SOL)
                end do

                state_out = matmul(A, state_in)
                state_out = max(0.0_dp, state_out)

                ! Row 1 is scratch only; clear it after transfer.
                me%m_contaminant(1)%c(n,f,:) = 0.0_dp

                ! Resuspended material is an export to water as SPM-associated PFAS.
                me%m_contaminant(2)%c(n,f,:) = 0.0_dp
                me%m_contaminant(2)%c(n,f,PFAS_SPM) = state_out(2)

                ! Bed layers and burial remain solid-sorbed.
                do i = 3, nCompartments
                    me%m_contaminant(i)%c(n,f,PFAS_SOL) = state_out(i)
                    me%m_contaminant(i)%c(n,f,PFAS_SPM) = 0.0_dp
                end do
            end do
        end do

        do i = 1, nCompartments
            me%m_contaminant(i)%m_dissolved = sum(me%m_contaminant(i)%c(:,:,PFAS_AQ))
        end do

        if (allocated(me%delta_sed)) me%delta_sed = 0.0_dp
        deallocate(state_in, state_out, A, weights)
    end function

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
        class(BedSediment) :: me                                    !! Self-reference
        real(dp) :: FS_resusp(:)                                    !! Sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
        type(ResultFineSediment2D) :: r                              !! Returned `Result` object. Type = `FineSediment`
        type(FineSediment), allocatable :: FS(:,:)                   ! LOCAL resuspended fine sediment. Index 1 = size class, Index 2 = layer
        type(FineSediment) :: F                                      ! LOCAL FineSediment object representing material to be resuspended
        type(FineSediment) :: G                                      ! LOCAL FineSediment object representing material not (yet) resuspended
        real(dp), allocatable :: delta_l_r(:,:)                      ! LOCAL deltas for layers to resuspension [-]. L x S array.
        integer :: S                                                 ! LOCAL loop counter for size classes
        integer :: L                                                 ! LOCAL counter for layers
        integer :: allst                                             ! LOCAL array allocation status
        character(len=256) :: tr                                     ! LOCAL name of this procedure, for trace
        
        tr = trim(me%name) // "%resuspendSediment1"                  ! error trace for this procedure
        ! Create fine sediment objects F and G
        call F%create("FineSediment", me%nfComp)
        call G%create("FineSediment", me%nfComp)
        allocate(FS(me%nSizeClasses, C%nSedimentLayers))             ! set up FineSediment array FS
        allocate(delta_l_r(C%nSedimentLayers, me%nSizeClasses))      ! allocate delta_d-l
        me%delta_sed = 0.0_dp                                        ! Reset the matrix of mass transfer coefficients
        delta_l_r = 0.0_dp                                           ! initialise the delta_l_r values
        do S = 1, me%nSizeClasses
            do L = 1, C%nSedimentLayers                          
                ! back up all the fine sediment masses, an essential part of the mass trasfer matrix computation
                call me%colBedSedimentLayers(L)%item%colFineSediment(S)%backup_M_f()
            end do
        end do        
        ! Main loop: for each size class (1..S), remove required fine sediment
        ! from the bed, by looping through each layer from top to bottom
        do S = 1, me%nSizeClasses
            call F%set(Mf_in = FS_resusp(S))                         ! mass to resuspend [kg]
            L = 1                                                    ! start with top layer
            do while (FS_resusp(S) > 0.000001 .and. L <= C%nSedimentLayers)
                associate(O => me%colBedSedimentLayers(L)%item)
                    call F%set(f_comp_in = O%colFineSediment(S)%f_comp) ! composition follows the donor
                    call r%addErrors(.errors. O%removeSediment(S, F, G))
                    delta_l_r(L, S) = G%M_f()                        ! layer->resusp mass
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        return
                    end if
                end associate
                FS_resusp(S) = FS_resusp(S) - delta_l_r(L, S)
                if (isZero(FS_resusp(s), 1.0e-10_dp)) FS_resusp(S) = 0.0_dp
                call FS(s,l)%create("FS", me%nfComp)
                call FS(S, L)%set(Mf_in = G%M_f(), Vw_in = G%V_w(), f_comp_in = G%f_comp)
                L = L + 1
            end do
            if (FS_resusp(S) > 0) then
                call r%addError(ErrorInstance(1, "All sediment of size class " // trim(str(S)) // " resuspended", .false., [tr]))
            end if
        end do
        call r%setData(FS)
        do S = 1, me%nSizeClasses
            do L = 1, C%nSedimentLayers
                me%delta_sed(2, L + 2, S) = me%delta_sed(2, L + 2, S) + delta_l_r(L, S)   ! l -> r
                me%delta_sed(L + 2, L + 2, S) = me%delta_sed(L + 2, L + 2, S) - delta_l_r(L, S) ! l -> l loss
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
    !! `FS_dep (FineSediment)`: 1D array of FineSediment objects containing the
    !! depositing fine sediment per size class
    !!                                                              <br>
    !! **Function outputs/outcomes**                                <br>
    !! `r (real(dp))`: returns water requirement from the water column [m3 m-2] real(dp)
    function depositSediment1(me, FS_dep) result(r)
        class(BedSediment) :: me
        type(FineSediment) :: FS_dep(:)
        type(Result0D)     :: r
        type(FineSediment) :: T, U
        integer :: s, l, ll, A
        real(dp) :: A_f_sed, V_f_burial, tempV, V_w_tot, V_w_b, dep_excess
        real(dp), allocatable :: delta_d_b(:), delta_d_l(:,:), delta_l_b(:,:), delta_l_l(:,:,:)
        real(dp) :: M_f_la
        logical, allocatable :: isEmpty(:)
        character(len=256) :: tr

        ! -------------------------------------------------------------------------------
        ! Notes
        ! -------------------------------------------------------------------------------
        ! 1. Currently does not tally mass/volume/composition of buried material as a single object.
        ! 2. FS_dep should not contain water (but if it does, it's overwritten).
        ! -------------------------------------------------------------------------------
        
        tr = trim(me%name) // "%DepositSediment1"
        allocate(IsEmpty(me%nSizeClasses))
        allocate(delta_d_l(C%nSedimentLayers, me%nSizeClasses))
        allocate(delta_d_b(me%nSizeClasses))
        allocate(delta_l_b(C%nSedimentLayers, me%nSizeClasses))
        allocate(delta_l_l(C%nSedimentLayers, C%nSedimentLayers, me%nSizeClasses))
        delta_d_b = 0.0_dp; delta_d_l = 0.0_dp; delta_l_b = 0.0_dp; delta_l_l = 0.0_dp; isEmpty = .true.

        do s = 1, me%nSizeClasses
            dep_excess = FS_dep(S)%V_f() - me%Cf_sediment(S)
            if (dep_excess > 0) then
                associate(O => me%colBedSedimentLayers)
                    do l = 1, C%nSedimentLayers
                        delta_l_b(l, s)   = O(l)%item%colFineSediment(s)%M_f()      ! l -> b
                        delta_l_l(l, l, s)= -O(l)%item%colFineSediment(s)%M_f()     ! loss from l
                        call O(l)%item%colFineSediment(s)%ClearAll()
                    end do
                    delta_d_b(S) = dep_excess * FS_dep(s)%rho_part()               ! d -> b
                end associate
            else
                isEmpty(s) = .false.
            end if
        end do

        call T%create("FineSediment_T", me%nfComp)
        call U%create("FineSediment_U", me%nfComp)

        do s = 1, me%nSizeClasses
            if (.not. isEmpty(S)) then
                A_f_sed   = me%Af_sediment(S)
                V_f_burial = FS_dep(S)%V_f() - A_f_sed
                if (V_f_burial > 0.0_dp) then
                    call T%set(Vf_in = V_f_burial, Vw_in = 0.0_dp, f_comp_in = FS_dep(S)%f_comp)

                    ! compute water to bury to preserve SLR
                    l = C%nSedimentLayers
                    do while (l > 0 .and. T%V_f() > 0)
                        associate (O => me%colBedSedimentLayers(l)%item)
                            if (T%V_f() > O%C_f(s)) then
                                call T%set(Vf_in = T%V_f() - O%C_f(s), Vw_in = T%V_w() + O%C_w(s))
                            else
                                if (O%volSLR(s) <= 1.0e-12_dp) then
                                    tempV = 0.0_dp
                                else
                                    tempV = T%V_f() / O%volSLR(s)
                                end if
                                call T%set(Vf_in = 0.0_dp, Vw_in = T%V_w() + tempV)
                            end if
                        end associate
                        l = l - 1
                    end do

                    ! actually bury fine sediment (and its water) from the bottom up
                    call T%set(Vf_in = FS_dep(s)%V_f() - A_f_sed)
                    l = C%nSedimentLayers
                    do while (l > 0 .and. T%V_f() + T%V_w() > 0)
                        if (T%V_f() > 0) then
                            associate(O => me%colBedSedimentLayers(l)%item)
                                call r%addErrors(.errors. O%RemoveSediment(S, T, U))
                                if (r%hasCriticalError()) then
                                    call r%addToTrace(tr)
                                    return
                                end if
                                delta_l_b(l, s)     = delta_l_b(l, s) + U%M_f()
                                delta_l_l(l, l, s)  = delta_l_l(l, l, s) - U%M_f()
                            end associate
                        end if
                        l = l - 1
                    end do

                    ! shift sediment downwards to fill the void created by burial
                    do l = C%nSedimentLayers, 2, -1
                        assoc1: associate(O => me%colBedSedimentLayers(L)%item)
                            A = l - 1
                            call T%set(Vf_in = O%A_f(s), Vw_in = O%A_w(s)) ! capacity to fill at receiving layer
                            do while (A > 0 .and. T%IsNotEmpty())
                                assoc2: associate(P => me%colBedSedimentLayers(A)%item)
                                    if (P%colFineSediment(S)%V_f() > 0) then
                                        call r%addErrors(.errors. P%RemoveSediment(S, T, U))
                                        if (r%hasCriticalError()) then
                                            call r%addToTrace(tr)
                                            return
                                        end if
                                        delta_l_l(A, L, S) = delta_l_l(A, L, S) + U%M_f()
                                        delta_l_l(A, A, S) = delta_l_l(A, A, S) - U%M_f()
                                        call r%addErrors(.errors. O%addSediment(S, U))
                                        if (r%hasCriticalError()) then
                                            call r%addToTrace(tr)
                                            return
                                        end if
                                    end if
                                    A = A - 1
                                end associate assoc2
                            end do
                        end associate assoc1
                    end do
                end if
            end if
        end do
        

        ! add the depositing sediment (bottom-up to respect capacities)
        V_w_tot = 0.0_dp
        do s = 1, me%nSizeClasses
            do l = C%nSedimentLayers, 1, -1
                if (FS_dep(s)%M_f() > 0.0_dp) then
                    associate(O => me%colBedSedimentLayers(l)%item)
                        if (O%A_f(s) > 0.0_dp .or. O%A_w(s) > 0.0_dp) then
                            if (O%volSLR(s) <= 1.0e-12_dp) then
                                V_w_b = 0.0_dp
                            else
                                V_w_b = FS_dep(s)%V_f() / O%volSLR(s)
                            end if
                            call FS_dep(s)%set(Vw_in = V_w_b)
                            M_f_la = FS_dep(s)%M_f()
                            call r%addErrors(.errors. O%addSediment(s, FS_dep(s)))
                            if (r%hasCriticalError()) then
                                call r%addToTrace(tr)
                                return
                            end if
                            delta_d_l(l, s) = M_f_la - FS_dep(s)%M_f()  ! d -> l
                        end if
                        V_w_tot = V_w_tot + V_w_b - FS_dep(s)%V_w()    ! water required from the water column
                    end associate
                end if
            end do
        end do
        r = Result(data = V_w_tot)

        ! Assemble delta_sed from deltas we collected
        do s = 1, me%nSizeClasses
            me%delta_sed(C%nSedimentLayers + 3, 1, S) = me%delta_sed(C%nSedimentLayers + 3, 1, S) + delta_d_b(S) ! d -> b
            do L = 1, C%nSedimentLayers
                me%delta_sed(L + 2, 1, S) = me%delta_sed(L + 2, 1, S) + delta_d_l(L, S)                           ! d -> l
                me%delta_sed(C%nSedimentLayers + 3, L + 2, S) = me%delta_sed(C%nSedimentLayers + 3, L + 2, S) + delta_l_b(L, S) ! l -> b
                do LL = 1, C%nSedimentLayers
                    if (isZero(me%delta_sed(L + 2, LL + 2, S))) me%delta_sed(L + 2, LL + 2, S) = 0.0_dp
                    me%delta_sed(L + 2, LL + 2, S) = me%delta_sed(L + 2, LL + 2, S) + delta_l_l(LL, L, S)         ! l -> l
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
        class(BedSediment) :: me
        integer :: n
        do n=1, C%nSedimentLayers
            call me%colBedSedimentLayers(n)%item%repMass()
        end do
    end subroutine

    !> Assemble contaminant package that co-deposits with SPM; scavenge FREE interface pool
    function deposit_spm_BedSediment(Me, dj_spm_deposit, bedArea, out_deposit, out_resus) result(r)
        class(BedSediment), intent(inout) :: Me
        real(dp), intent(in)              :: dj_spm_deposit(:)
        real(dp), intent(in)              :: bedArea
        type(Contaminant), intent(out)    :: out_deposit
        type(Contaminant), intent(out)    :: out_resus
        type(Result)                      :: r

        call r%addErrors(.errors. out_deposit%create())
        call r%addErrors(.errors. out_resus%create())
        if (.not. allocated(Me%m_contaminant)) return

        ! Deposition from water should be provided as PFAS_SPM to transferContaminant.
        ! This routine simply exposes any staged inlet/outlet packages.
        if (allocated(Me%m_contaminant(1)%c)) then
            out_deposit%c(:,:,PFAS_SPM) = Me%m_contaminant(1)%c(:,:,PFAS_SPM)
            Me%m_contaminant(1)%c(:,:,PFAS_SPM) = 0.0_dp
        end if

        if (allocated(Me%m_contaminant(2)%c)) then
            out_resus%c(:,:,PFAS_SPM) = Me%m_contaminant(2)%c(:,:,PFAS_SPM)
            Me%m_contaminant(2)%c(:,:,PFAS_SPM) = 0.0_dp
        end if

        out_deposit%m_dissolved = sum(out_deposit%c(:,:,PFAS_AQ))
        out_resus%m_dissolved   = sum(out_resus%c(:,:,PFAS_AQ))
    end function

    !> Assemble contaminant package that leaves with resuspended SPM; becomes FREE in water column
    function resuspend_spm_BedSediment(Me, dj_spm_resus, bedArea, out_resus) result(r)
        class(BedSediment), intent(inout) :: Me
        real(dp), intent(in)              :: dj_spm_resus(:)
        real(dp), intent(in)              :: bedArea
        type(Contaminant), intent(out)    :: out_resus
        type(Result)                      :: r

        integer :: n, f, l
        real(dp) :: m_bed, m_resus, frac_resus

        call r%addErrors(.errors. out_resus%create())
        if (.not. allocated(Me%m_contaminant)) return
        if (all(dj_spm_resus <= C%epsilon)) return

        m_bed   = max(C%epsilon, Me%Mf_bed_all())
        m_resus = max(0.0_dp, sum(dj_spm_resus))
        frac_resus = min(1.0_dp, m_resus / m_bed)
        if (frac_resus <= 0.0_dp) return

        ! Remove from bed layers and export as water-column SPM-associated PFAS.
        do l = 1, C%nSedimentLayers
            do n = 1, size(Me%m_contaminant(l+2)%c,1)
                do f = 1, size(Me%m_contaminant(l+2)%c,2)
                    out_resus%c(n,f,PFAS_SPM) = out_resus%c(n,f,PFAS_SPM) + &
                        Me%m_contaminant(l+2)%c(n,f,PFAS_SOL) * frac_resus
                    Me%m_contaminant(l+2)%c(n,f,PFAS_SOL) = &
                        Me%m_contaminant(l+2)%c(n,f,PFAS_SOL) * (1.0_dp - frac_resus)
                end do
            end do
        end do

        ! Include material staged by transferContaminant and then clear the scratch pool.
        if (allocated(Me%m_contaminant(2)%c)) then
            out_resus%c(:,:,PFAS_SPM) = out_resus%c(:,:,PFAS_SPM) + Me%m_contaminant(2)%c(:,:,PFAS_SPM)
            Me%m_contaminant(2)%c(:,:,PFAS_SPM) = 0.0_dp
        end if
        out_resus%m_dissolved = sum(out_resus%c(:,:,PFAS_AQ))
    end function

end module

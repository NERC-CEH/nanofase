module ReactorModule
    !! P-FASE-compatible Reactor implementation.
    !!
    !! Reactor owns no contaminant mass. It points to the live Contaminant object
    !! owned by water/soil/sediment compartments and orchestrates PFAS processes.
    !! Nanoparticle-only physics are not invoked here.

    use ContaminantModule, only: Contaminant
    use GlobalsModule, only: dp, C
    use ResultModule, only: Result
    use AbstractReactorModule
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    use ErrorInstanceModule
    use UtilModule, only: ref
    use PFASEConstantsModule, only: PFAS_AQ, PFAS_SOL, PFAS_SPM, PFAS_AWI, PFAS_FOAM, PFAS_AIR
    implicit none

    type, public, extends(AbstractReactor) :: Reactor
        real(dp)              :: T_water
        real(dp), allocatable :: C_spm(:)
        real(dp), allocatable :: W_settle_spm(:)
        real(dp)              :: G
        real(dp), allocatable :: k_att(:)
        real(dp)              :: alpha_att
        real(dp)              :: velocity
    contains
        procedure :: create => createReactor
        procedure :: update => updateReactor
        procedure :: finalise => finaliseReactor
        procedure :: parseInputData => parseInputDataReactor
        procedure :: setCarrierState => setCarrierStateReactor
    end type

contains

    function createReactor(me, x, y, compartment, contaminant_in, volume, T_water, &
                           C_spm, W_settle_spm, G, k_att, alpha_att, velocity) result(r)
        class(Reactor), intent(inout) :: me
        integer, intent(in) :: x, y
        character(len=*), intent(in) :: compartment
        type(Contaminant), target, intent(inout) :: contaminant_in
        real(dp), intent(in) :: volume
        real(dp), intent(in) :: T_water
        real(dp), intent(in), optional :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in), optional :: G
        real(dp), intent(in), optional :: k_att(:), alpha_att
        real(dp), intent(in), optional :: velocity

        type(Result) :: r
        type(ErrorInstance) :: err(1)

        me%x = x
        me%y = y
        me%ref = trim(ref('Reactor', x, y, 0))
        me%compartment = trim(compartment)

        ! These are real model inputs, not hard-coded defaults.
        me%volume = volume
        me%T_water = T_water

        if (associated(me%contaminant)) nullify(me%contaminant)

        if (.not. allocated(contaminant_in%c)) then
            err(1) = ErrorInstance(code=900, message='Reactor created with unallocated Contaminant')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if

        me%contaminant => contaminant_in

        ! Optional parameters are reset first, then overwritten if supplied.
        me%G = 0.0_dp
        me%alpha_att = 0.0_dp
        me%velocity = 0.0_dp

        if (present(G)) me%G = G
        if (present(alpha_att)) me%alpha_att = alpha_att
        if (present(velocity)) me%velocity = velocity

        call me%setCarrierState(C_spm, W_settle_spm, k_att)
    end function createReactor


    subroutine setCarrierStateReactor(me, C_spm, W_settle_spm, k_att)
        class(Reactor), intent(inout) :: me
        real(dp), intent(in), optional :: C_spm(:), W_settle_spm(:), k_att(:)

        integer :: nspm, nspecies

        if (allocated(me%C_spm)) deallocate(me%C_spm)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        if (allocated(me%k_att)) deallocate(me%k_att)

        if (present(C_spm)) then
            allocate(me%C_spm(size(C_spm)))
            me%C_spm = max(0.0_dp, C_spm)
        else
            nspm = max(1, C%nSizeClassesSpm)
            allocate(me%C_spm(nspm))
            me%C_spm = 0.0_dp
        end if

        if (present(W_settle_spm)) then
            allocate(me%W_settle_spm(size(W_settle_spm)))
            me%W_settle_spm = max(0.0_dp, W_settle_spm)
        else
            allocate(me%W_settle_spm(size(me%C_spm)))
            me%W_settle_spm = 0.0_dp
        end if

        if (present(k_att)) then
            allocate(me%k_att(size(k_att)))
            me%k_att = max(0.0_dp, k_att)
        else
            nspecies = max(1, C%contaminantDim(1))
            allocate(me%k_att(nspecies))
            me%k_att = 0.0_dp
        end if
    end subroutine setCarrierStateReactor


    function updateReactor(me, j_contaminant_in, dt) result(r)
        class(Reactor), intent(inout) :: me
        type(Contaminant), intent(in), optional :: j_contaminant_in
        real(dp), intent(in) :: dt

        type(Result) :: r
        type(ErrorInstance) :: err(1)
        character(len=32) :: comp

        if (.not. associated(me%contaminant)) then
            err(1) = ErrorInstance(code=905, message='Reactor contaminant pointer is not associated')
            call r%addError(err(1))
            return
        end if

        if (.not. allocated(me%contaminant%c)) then
            err(1) = ErrorInstance(code=906, message='Reactor contaminant state is not allocated')
            call r%addError(err(1))
            return
        end if

        if (dt <= 0.0_dp) return

        if (present(j_contaminant_in)) then
            if (allocated(j_contaminant_in%c)) call me%contaminant%add(j_contaminant_in)
        end if

        comp = trim(me%compartment)

        select case (comp)

        case ('water', 'estuary')
            call r%addErrors(.errors. me%contaminant%update( &
                dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume, comp))

            if (allocated(DATASET%pfasFoamCoeff)) then
                call apply_foam_from_dataset(me, dt)
            else if (DATASET%pfasFoamCoefficient > 0.0_dp) then
                call apply_foam_scalar(me, dt, DATASET%pfasFoamCoefficient)
            end if

            if (DATASET%pfasVolatilisationRateScalar > 0.0_dp .or. &
                DATASET%pfasSeaSprayAerosolRate > 0.0_dp) then
                call apply_atmosphere_scalar( &
                    me, dt, DATASET%pfasVolatilisationRateScalar, DATASET%pfasSeaSprayAerosolRate)
            end if

        case ('sediment')
            call r%addErrors(.errors. me%contaminant%update( &
                dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume, 'sediment'))

        case ('soil')
            call r%addErrors(.errors. me%contaminant%update( &
                dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume, 'soil'))

        case ('atmospheric', 'air')
            call r%addErrors(.errors. me%contaminant%transformation(dt))

        case default
            err(1) = ErrorInstance(code=900, message='Invalid P-FASE Reactor compartment: ' // trim(me%compartment))
            call r%addError(err(1))

        end select

        me%contaminant%m_dissolved = sum(me%contaminant%c(:,:,PFAS_AQ))
    end function updateReactor


    subroutine apply_foam_scalar(me, dt, foam_rate)
        class(Reactor), intent(inout) :: me
        real(dp), intent(in) :: dt, foam_rate

        type(Contaminant) :: j_foam
        real(dp) :: frac

        frac = max(0.0_dp, min(1.0_dp, foam_rate * dt))
        call me%contaminant%foam_exchange(frac, j_foam)
        call j_foam%finalise()
    end subroutine apply_foam_scalar


    subroutine apply_foam_from_dataset(me, dt)
        class(Reactor), intent(inout) :: me
        real(dp), intent(in) :: dt

        type(Contaminant) :: j_foam
        real(dp) :: frac

        if (.not. allocated(DATASET%pfasFoamCoeff)) return

        frac = max(0.0_dp, min(1.0_dp, maxval(DATASET%pfasFoamCoeff) * dt))
        call me%contaminant%foam_exchange(frac, j_foam)
        call j_foam%finalise()
    end subroutine apply_foam_from_dataset


    subroutine apply_atmosphere_scalar(me, dt, volatilisation_rate, aerosol_rate)
        class(Reactor), intent(inout) :: me
        real(dp), intent(in) :: dt, volatilisation_rate, aerosol_rate

        type(Contaminant) :: j_air
        real(dp) :: f_vol, f_aer

        f_vol = max(0.0_dp, min(1.0_dp, volatilisation_rate * dt))
        f_aer = max(0.0_dp, min(1.0_dp, aerosol_rate * dt))

        call me%contaminant%atmosphere_exchange(f_vol, f_aer, j_air)
        call j_air%finalise()
    end subroutine apply_atmosphere_scalar


    subroutine finaliseReactor(me)
        class(Reactor), intent(inout) :: me

        if (associated(me%contaminant)) nullify(me%contaminant)

        if (allocated(me%C_spm)) deallocate(me%C_spm)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        if (allocated(me%k_att)) deallocate(me%k_att)

    end subroutine finaliseReactor


    function parseInputDataReactor(me) result(r)
        class(Reactor), intent(inout) :: me
        type(Result) :: r
        !! Reserved for future per-cell/per-reach PFAS reactor parameter input.
    end function parseInputDataReactor

end module ReactorModule
module ContaminantModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT, SPM_CONTAMINANT_START
    use ResultModule, only: Result, Result0D
    use ErrorInstanceModule
    use mo_netcdf
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    use ConstantsDefaultsModule
    implicit none

    type, public :: Contaminant
        real(dp), allocatable :: c(:,:,:)
        real(dp) :: m_dissolved = 0.0_dp
        real(dp) :: rho_contaminant
        real(dp) :: k_diss_pristine
        real(dp) :: k_diss_transformed
        real(dp) :: k_transform_pristine
        real(dp) :: alpha_hetero
        real(dp) :: alpha_att
        real(dp), allocatable :: k_hetero(:,:)
        real(dp), allocatable :: W_settle_contaminant(:)
        real(dp), allocatable :: individualContaminantMass(:)
        real(dp), allocatable :: C_contaminant_free_particle(:)
        character(len=100) :: compartment
    contains
        procedure :: create => contaminant_create
        procedure :: create_from_data => contaminant_create_from_data
        procedure :: add => contaminant_add
        procedure :: add_scaled => contaminant_add_scaled
        procedure :: multiply_scalar => contaminant_multiply_scalar
        procedure :: finalise => contaminant_finalise
        procedure :: update => contaminant_update
        procedure :: update_water => contaminant_update_water
        procedure :: update_sediment => contaminant_update_sediment
        procedure :: update_soil => contaminant_update_soil
        procedure :: heteroaggregation => contaminant_heteroaggregation
        procedure :: dissolution => contaminant_dissolution
        procedure :: transformation => contaminant_transformation
        procedure :: getConcentration => contaminant_getConcentration
        procedure :: get_free => contaminant_get_free
        procedure :: get_attached => contaminant_get_attached
        procedure :: attachment => contaminant_attachment
        procedure :: calculateCollisionRate => contaminant_calculateCollisionRate
        procedure :: calculateParticleConcentration => contaminant_calculateParticleConcentration
        procedure :: calculateAttachmentRate => contaminant_calculateAttachmentRate
        procedure :: calculateSettlingVelocity => contaminant_calculateSettlingVelocity
        procedure :: divideCheckZero => contaminant_divideCheckZero  
        procedure :: empty => contaminant_empty
    end type

    interface operator(*)
        module procedure multiply_contaminant_scalar
    end interface

    interface operator(-)
        module procedure negate_contaminant
    end interface

    interface operator(+)
        module procedure add_contaminant
    end interface

contains

    !> Local function to replace str from UtilModule.
    !! This is included locally to avoid introducing a dependency on UtilModule,
    !! which could potentially create circular dependencies in the module graph.
    !! If UtilModule's str is needed elsewhere, consider importing it, but here
    !! it's isolated for simplicity.
    function int_to_string(i) result(s)
        integer, intent(in) :: i
        character(len=20) :: s
        write(s, '(I0)') i
        s = trim(adjustl(s))
    end function

    !> Negate a Contaminant object by multiplying its mass-related fields by -1.
    !! Properties (rho_contaminant, rates, etc.) are copied from the original.
    function negate_contaminant(this) result(negated)
        type(Contaminant), intent(in) :: this
        type(Contaminant) :: negated
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        r = negated%create()
        if (r%hasCriticalError()) then
            err(1) = ErrorInstance(code=901, message="Failed to create Contaminant in negate_contaminant")
            call LOGR%toFile(errors=r%errors)
            return
        end if
        negated%c = -this%c
        negated%m_dissolved = -this%m_dissolved
        negated%rho_contaminant = this%rho_contaminant
        negated%k_diss_pristine = this%k_diss_pristine
        negated%k_diss_transformed = this%k_diss_transformed
        negated%k_transform_pristine = this%k_transform_pristine
        negated%alpha_hetero = this%alpha_hetero
        negated%alpha_att = this%alpha_att
        negated%compartment = this%compartment
    end function

    !> Add two Contaminant objects, summing their mass fields (c and m_dissolved).
    !! Properties (rho_contaminant, rates, etc.) are taken from the first operand ('this').
    !! Note: This makes addition non-commutative for properties (A + B != B + A in terms of properties).
    !! Always use the left operand as the base for properties. This behavior is intentional
    !! to preserve the primary contaminant's characteristics; document usage accordingly.
    function add_contaminant(this, other) result(sum_result)
        type(Contaminant), intent(in) :: this
        type(Contaminant), intent(in) :: other
        type(Contaminant) :: sum_result
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        r = sum_result%create()
        if (r%hasCriticalError()) then
            err(1) = ErrorInstance(code=901, message="Failed to create Contaminant in add_contaminant")
            call LOGR%toFile(errors=r%errors)
            return
        end if
        call sum_result%add(this)
        call sum_result%add(other)
        sum_result%rho_contaminant = this%rho_contaminant
        sum_result%k_diss_pristine = this%k_diss_pristine
        sum_result%k_diss_transformed = this%k_diss_transformed
        sum_result%k_transform_pristine = this%k_transform_pristine
        sum_result%alpha_hetero = this%alpha_hetero
        sum_result%alpha_att = this%alpha_att
        sum_result%compartment = this%compartment
    end function

    !> Initialize a Contaminant object, allocating arrays and setting default values to zero.
    !! Adds defensive finalize, dimension checks, and verbose logging.
    function contaminant_create(this) result(r)
        class(Contaminant), intent(inout) :: this
        type(Result) :: r
        integer :: alloc_stat
        type(ErrorInstance) :: err(1)
        integer :: nx, nf, nz

        nx = C%contaminantDim(1)
        nf = C%contaminantDim(2)
        nz = C%contaminantDim(3)

        call LOGR%add("Contaminant%create: requested dims = (" // trim(int_to_string(nx)) // "," // &
                    trim(int_to_string(nf)) // "," // trim(int_to_string(nz)) // "); nSPM=" // &
                    trim(int_to_string(C%nSizeClassesSpm)))
        ! Defensive: clear any previous allocation
        if (allocated(this%c) .or. allocated(this%k_hetero) .or. allocated(this%W_settle_contaminant) .or. &
            allocated(this%individualContaminantMass) .or. allocated(this%C_contaminant_free_particle)) then
            call LOGR%add("Contaminant%create: finalising previous allocation")
            call this%finalise()
        end if

        ! Hard checks on dimensions
        if (min(nx, nf, nz) <= 0) then
            err(1) = ErrorInstance(code=900, message='Contaminant dims must all be > 0')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if
        if (nz < SPM_CONTAMINANT_START + C%nSizeClassesSpm - 1) then
            err(1) = ErrorInstance(code=900, message='Contaminant state dimension too small for SPM classes')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if

        allocate(this%c(nx, nf, nz), &
                this%k_hetero(nx, C%nSizeClassesSpm), &
                this%W_settle_contaminant(nx), &
                this%individualContaminantMass(nx), &
                this%C_contaminant_free_particle(nx), &
                stat=alloc_stat)

        if (alloc_stat /= 0) then
            err(1) = ErrorInstance(code=901, message='Contaminant allocation failed')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if

        this%c                          = 0.0_dp
        this%m_dissolved                = 0.0_dp
        this%k_hetero                   = 0.0_dp
        this%W_settle_contaminant       = 0.0_dp
        this%individualContaminantMass  = 0.0_dp
        this%C_contaminant_free_particle= 0.0_dp
        this%rho_contaminant            = 0.0_dp
        this%k_diss_pristine            = 0.0_dp
        this%k_diss_transformed         = 0.0_dp
        this%k_transform_pristine       = 0.0_dp
        this%alpha_hetero               = 0.0_dp
        this%alpha_att                  = 0.0_dp
        this%compartment                = ''

        call LOGR%add("Contaminant%create: allocation OK")
    end function


    !> Create a Contaminant from input data, setting properties and calculating settling velocities.
    !! Adds a warning if scalar/class counts disagree; keeps single create path and debug logs.
    function contaminant_create_from_data(this, compartment, contaminantDensity, &
                                        soilAttachmentEfficiency, riverAttachmentEfficiency, &
                                        estuaryAttachmentEfficiency, k_diss_pristine, &
                                        k_diss_transformed, k_transform_pristine, waterTemperature) result(r)
        class(Contaminant), intent(inout) :: this
        character(len=*), intent(in) :: compartment
        real(dp), intent(in) :: contaminantDensity
        real(dp), intent(in) :: soilAttachmentEfficiency
        real(dp), intent(in) :: riverAttachmentEfficiency
        real(dp), intent(in) :: estuaryAttachmentEfficiency
        real(dp), intent(in) :: k_diss_pristine
        real(dp), intent(in) :: k_diss_transformed
        real(dp), intent(in) :: k_transform_pristine
        real(dp), intent(in) :: waterTemperature
        type(Result) :: r
        type(ErrorInstance) :: err(1)
        integer :: n

        call LOGR%add("Contaminant%create_from_data: compartment=" // trim(compartment))
        r = this%create()
        if (r%hasCriticalError()) then
            call LOGR%toFile(errors=r%errors)
            return
        end if

        ! Sanity: warn if scalar count and allocated dimension differ
        if (C%nContaminantSizeClasses /= C%contaminantDim(1)) then
            err(1) = ErrorInstance(code=902, &
                message="Mismatch: nContaminantSizeClasses /= contaminantDim(1); proceeding with contaminantDim(1)", &
                isCritical=.false.)
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            call r%clear()
        end if

        this%compartment          = compartment
        this%rho_contaminant      = contaminantDensity
        this%k_diss_pristine      = k_diss_pristine
        this%k_diss_transformed   = k_diss_transformed
        this%k_transform_pristine = k_transform_pristine

        select case (compartment)
            case ('soil','atmospheric')
                this%alpha_hetero = soilAttachmentEfficiency
                this%alpha_att    = soilAttachmentEfficiency
            case ('water')
                this%alpha_hetero = riverAttachmentEfficiency
                this%alpha_att    = riverAttachmentEfficiency
            case ('estuary','sediment')
                this%alpha_hetero = estuaryAttachmentEfficiency
                this%alpha_att    = estuaryAttachmentEfficiency
            case default
                err(1) = ErrorInstance(code=900, message="Invalid compartment: " // trim(compartment))
                call r%addErrors(err)
                call LOGR%toFile(errors=r%errors)
                return
        end select

        do n = 1, C%contaminantDim(1)
            this%W_settle_contaminant(n) = this%calculateSettlingVelocity( &
                DATASET%contaminantSizeClasses(n), this%rho_contaminant, waterTemperature)
            this%individualContaminantMass(n) = this%rho_contaminant * (4.0_dp/3.0_dp) * &
                C%pi * (DATASET%contaminantSizeClasses(n)/2.0_dp)**3
        end do

        call LOGR%add("Contaminant%create_from_data: parameters set and settling velocities computed")
    end function

    !> Add the mass fields of another Contaminant to this one (in-place addition).
    subroutine contaminant_add(this, addition)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        this%c = this%c + addition%c
        this%m_dissolved = this%m_dissolved + addition%m_dissolved
    end subroutine

    !> Add a scaled version of another Contaminant's mass fields to this one.
    subroutine contaminant_add_scaled(this, addition, scale)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        real(dp), intent(in) :: scale
        this%c = this%c + addition%c * scale
        this%m_dissolved = this%m_dissolved + addition%m_dissolved * scale
    end subroutine

    !> Multiply a Contaminant by a scalar, returning a new Contaminant with scaled masses.
    function multiply_contaminant_scalar(this, scalar) result(product)
        type(Contaminant), intent(in) :: this
        real(dp), intent(in) :: scalar
        type(Contaminant) :: product
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        r = product%create()
        if (r%hasCriticalError()) then
            err(1) = ErrorInstance(code=901, message="Failed to create Contaminant in multiply_contaminant_scalar")
            call LOGR%toFile(errors=r%errors)
            return
        end if
        product%c = this%c * scalar
        product%m_dissolved = this%m_dissolved * scalar
        product%rho_contaminant = this%rho_contaminant
        product%k_diss_pristine = this%k_diss_pristine
        product%k_diss_transformed = this%k_diss_transformed
        product%k_transform_pristine = this%k_transform_pristine
        product%alpha_hetero = this%alpha_hetero
        product%alpha_att = this%alpha_att
        product%compartment = this%compartment
    end function

    !> Set this Contaminant's masses to a scaled copy of the source's masses.
    subroutine contaminant_multiply_scalar(this, source, scalar)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: source
        real(dp), intent(in) :: scalar
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        r = this%create()
        if (r%hasCriticalError()) then
            err(1) = ErrorInstance(code=901, message="Failed to create Contaminant in contaminant_multiply_scalar")
            call LOGR%toFile(errors=r%errors)
            return
        end if
        this%c = source%c * scalar
        this%m_dissolved = source%m_dissolved * scalar
        this%rho_contaminant = source%rho_contaminant
        this%k_diss_pristine = source%k_diss_pristine
        this%k_diss_transformed = source%k_diss_transformed
        this%k_transform_pristine = source%k_transform_pristine
        this%alpha_hetero = source%alpha_hetero
        this%alpha_att = source%alpha_att
        this%compartment = source%compartment
    end subroutine

    !> Divide the contaminant's masses by a denominator, returning a new object; sets to zero if denominator is near zero.
    function contaminant_divideCheckZero(this, denominator) result(divided)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: denominator
        type(Contaminant) :: divided
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        r = divided%create()
        if (r%hasCriticalError()) then
            err(1) = ErrorInstance(code=901, message="Failed to create Contaminant in divideCheckZero")
            call LOGR%toFile(errors=r%errors)
            return
        end if
        if (abs(denominator) < C%epsilon) then
            divided%c = 0.0_dp
            divided%m_dissolved = 0.0_dp
        else
            if (allocated(this%c)) then
                divided%c = this%c / denominator
            end if
            divided%m_dissolved = this%m_dissolved / denominator
        end if
        divided%rho_contaminant = this%rho_contaminant
        divided%k_diss_pristine = this%k_diss_pristine
        divided%k_diss_transformed = this%k_diss_transformed
        divided%k_transform_pristine = this%k_transform_pristine
        divided%alpha_hetero = this%alpha_hetero
        divided%alpha_att = this%alpha_att
        divided%compartment = this%compartment
    end function

    !> Reset the contaminant's mass fields to zero without deallocating arrays.
    subroutine contaminant_empty(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) this%c = 0.0_dp
        this%m_dissolved = 0.0_dp
    end subroutine

    !> Deallocate all allocated arrays in the Contaminant and reset compartment.
    subroutine contaminant_finalise(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) deallocate(this%c)
        if (allocated(this%k_hetero)) deallocate(this%k_hetero)
        if (allocated(this%W_settle_contaminant)) deallocate(this%W_settle_contaminant)
        if (allocated(this%individualContaminantMass)) deallocate(this%individualContaminantMass)
        if (allocated(this%C_contaminant_free_particle)) deallocate(this%C_contaminant_free_particle)
        this%m_dissolved = 0.0_dp
        this%compartment = ''
    end subroutine

    !> Update the contaminant based on compartment type, dispatching to specific update methods.
    function contaminant_update(this, dt, T_water, C_spm, W_settle_spm, G, volume, compartment, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in) :: G
        real(dp), intent(in) :: volume
        character(len=*), intent(in) :: compartment
        real(dp), intent(in), optional :: k_att(:), alpha_att
        type(Result) :: r
        type(ErrorInstance) :: err(1)

        select case (compartment)
            case ('water', 'estuary')
                if (present(k_att) .or. present(alpha_att)) then
                    err(1) = ErrorInstance(code=900, &
                        message="k_att and alpha_att not applicable for water or estuary compartment", &
                        isCritical=.false.)
                    call r%addErrors(err)
                    call LOGR%toFile(errors=err)
                end if
                call r%addErrors(.errors. this%update_water(dt, T_water, C_spm, W_settle_spm, G, volume))
            case ('sediment')
                if (present(k_att) .or. present(alpha_att)) then
                    err(1) = ErrorInstance(code=900, &
                        message="k_att and alpha_att not applicable for sediment compartment", &
                        isCritical=.false.)
                    call r%addErrors(err)
                    call LOGR%toFile(errors=err)
                end if
                call r%addErrors(.errors. this%update_sediment(dt, T_water, C_spm, W_settle_spm, G, volume))
            case ('soil')
                if (.not. (present(k_att) .and. present(alpha_att))) then
                    err(1) = ErrorInstance(code=900, message="k_att and alpha_att required for soil compartment")
                    call r%addErrors(err)
                    call LOGR%toFile(errors=err)
                    return
                end if
                call r%addErrors(.errors. this%update_soil(dt, T_water, C_spm, W_settle_spm, G, volume, k_att, alpha_att))
            case default
                err(1) = ErrorInstance(code=900, message="Invalid compartment: " // trim(compartment))
                call r%addErrors(err)
                call LOGR%toFile(errors=err)
        end select
    end function

    !> Update for water/estuary: Calculate particle concentrations, perform heteroaggregation, dissolution, and transformation.
    function contaminant_update_water(this, dt, T_water, C_spm, W_settle_spm, G, volume) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in) :: G
        real(dp), intent(in) :: volume
        type(Result) :: r
        integer :: s, n
        real(dp), allocatable :: C_spm_particle(:)
        allocate(C_spm_particle(C%nSizeClassesSpm))
        do s = 1, C%nSizeClassesSpm
            C_spm_particle(s) = this%calculateParticleConcentration(C_spm(s), &
                real(sum(C%sedimentParticleDensities)/C%nSizeClassesSpm,dp), real(C%d_spm(s),dp))
        end do
        do n = 1, C%contaminantDim(1)
            ! FIX: Use the initialized DATASET%contaminantSizeClasses instead of uninitialized C%d_contaminant
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, DATASET%contaminantSizeClasses(n))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

    !> Update for sediment: Similar to water update, focusing on heteroaggregation, dissolution, and transformation.
    function contaminant_update_sediment(this, dt, T_water, C_spm, W_settle_spm, G, volume) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in) :: G
        real(dp), intent(in) :: volume
        type(Result) :: r
        integer :: s, n
        real(dp), allocatable :: C_spm_particle(:)
        allocate(C_spm_particle(C%nSizeClassesSpm))
        do s = 1, C%nSizeClassesSpm
            C_spm_particle(s) = this%calculateParticleConcentration(C_spm(s), &
                real(sum(C%sedimentParticleDensities)/C%nSizeClassesSpm,dp), real(C%d_spm(s),dp))
        end do
        do n = 1, C%contaminantDim(1)
            ! FIX: Use the initialized DATASET%contaminantSizeClasses instead of uninitialized C%d_contaminant
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, DATASET%contaminantSizeClasses(n))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

    !> Update for soil: Includes heteroaggregation, attachment to soil matrix, dissolution, and transformation.
    function contaminant_update_soil(this, dt, T_water, C_spm, W_settle_spm, G, volume, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in) :: G
        real(dp), intent(in) :: volume
        real(dp), intent(in) :: k_att(:), alpha_att
        type(Result) :: r
        integer :: s, n
        real(dp), allocatable :: C_spm_particle(:)
        allocate(C_spm_particle(C%nSizeClassesSpm))
        do s = 1, C%nSizeClassesSpm
            C_spm_particle(s) = this%calculateParticleConcentration(C_spm(s), &
                real(sum(C%sedimentParticleDensities)/C%nSizeClassesSpm,dp), real(C%d_spm(s),dp))
        end do
        do n = 1, C%contaminantDim(1)
            ! FIX: Use the initialized DATASET%contaminantSizeClasses instead of uninitialized C%d_contaminant
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, DATASET%contaminantSizeClasses(n))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%attachment(dt, k_att, alpha_att))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

    !> Perform heteroaggregation: Calculate collision rates, update heteroaggregation rates, and transfer mass from free to attached states.
    function contaminant_heteroaggregation(this, dt, T_water, C_spm, W_settle_spm, C_spm_particle) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:)
        real(dp), intent(in) :: W_settle_spm(:), C_spm_particle(:)
        type(Result) :: r
        real(dp) :: k_coll(C%contaminantDim(1), C%nSizeClassesSpm)
        integer :: s, n, f
        real(dp) :: dm_hetero, G  
        G = 0.0_dp  
        k_coll = this%calculateCollisionRate(T_water, G, W_settle_spm)
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%contaminantDim(1)
                this%k_hetero(n,s) = k_coll(n,s) * this%alpha_hetero * C_spm_particle(s)
            end do
        end do
        do n = 1, C%contaminantDim(1)
            do f = 1, C%nContaminantForms
                dm_hetero = min(sum(this%k_hetero(n,:))*dt*this%c(n,f,FREE_CONTAMINANT), this%c(n,f,FREE_CONTAMINANT))
                this%c(n,f,FREE_CONTAMINANT) = this%c(n,f,FREE_CONTAMINANT) - dm_hetero
                do s = 1, C%nSizeClassesSpm
                    if (this%k_hetero(n,s) > C%epsilon) then
                        this%c(n,f,SPM_CONTAMINANT_START+s-1) = this%c(n,f,SPM_CONTAMINANT_START+s-1) + &
                            dm_hetero*(this%k_hetero(n,s)/sum(this%k_hetero(n,:)))
                    end if
                end do
            end do
        end do
    end function

    !> Perform attachment to matrix: Transfer mass from free to attached state based on attachment rates.
    function contaminant_attachment(this, dt, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        real(dp), intent(in) :: k_att(:), alpha_att
        type(Result) :: r
        integer :: n, f
        real(dp) :: dm_att
        do n = 1, C%contaminantDim(1)
            do f = 1, C%nContaminantForms
                dm_att = min(k_att(n) * alpha_att * dt * this%c(n,f,FREE_CONTAMINANT), this%c(n,f,FREE_CONTAMINANT))
                this%c(n,f,FREE_CONTAMINANT) = this%c(n,f,FREE_CONTAMINANT) - dm_att
                this%c(n,f,ATTACHED_CONTAMINANT) = this%c(n,f,ATTACHED_CONTAMINANT) + dm_att
            end do
        end do
    end function

    !> Perform dissolution: Transfer mass from particulate forms to dissolved based on dissolution rates.
    function contaminant_dissolution(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
        real(dp) :: dm_diss(C%contaminantDim(1), C%nContaminantForms, C%contaminantDim(3))
        integer :: f
        do f = 1, C%nContaminantForms
            if (f == 1) then
                dm_diss(:,f,:) = min(this%k_diss_pristine * dt * this%c(:,f,:), this%c(:,f,:))
            else
                dm_diss(:,f,:) = min(this%k_diss_transformed * dt * this%c(:,f,:), this%c(:,f,:))
            end if
            this%c(:,f,:) = this%c(:,f,:) - dm_diss(:,f,:)
            this%m_dissolved = this%m_dissolved + sum(dm_diss(:,f,:))
        end do
    end function

    !> Perform transformation: Transfer mass from pristine to transformed form if multiple forms exist.
    function contaminant_transformation(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
        real(dp) :: dm_transform(C%contaminantDim(1), C%nContaminantForms, C%contaminantDim(3))
        if (C%nContaminantForms > 1) then
            dm_transform = 0.0_dp
            dm_transform(:,1,:) = min(this%k_transform_pristine * dt * this%c(:,1,:), this%c(:,1,:))
            this%c(:,1,:) = this%c(:,1,:) - dm_transform(:,1,:)
            this%c(:,2,:) = this%c(:,2,:) + dm_transform(:,1,:)
        end if
    end function

    !> Calculate total concentration (sum of all masses divided by volume).
    function contaminant_getConcentration(this, volume) result(r)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: volume
        type(Result0D) :: r
        type(ErrorInstance) :: err(1)
        real(dp) :: C_total

        if (volume > C%epsilon) then
            C_total = sum(this%c) / volume
            allocate(r%data, source=C_total)
        else
            err(1) = ErrorInstance(code=900, message="Zero or negative volume in getConcentration")
            call r%addErrors(err)
            call LOGR%toFile(errors=err)
        end if
    end function

    !> Get concentrations of free (non-attached) contaminant.
    function contaminant_get_free(this) result(C_free)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_free(C%contaminantDim(1), C%contaminantDim(2))
        C_free = this%c(:,:,FREE_CONTAMINANT)
    end function

    !> Get concentrations of attached contaminant.
    function contaminant_get_attached(this) result(C_attached)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_attached(C%contaminantDim(1), C%contaminantDim(2))
        C_attached = this%c(:,:,ATTACHED_CONTAMINANT)
    end function

    !> Calculate collision rates between contaminant and SPM particles using Brownian, shear, and differential settling terms.
    function contaminant_calculateCollisionRate(this, T_water, G, W_settle_spm) result(k_coll)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water
        real(dp), intent(in) :: G
        real(dp), intent(in) :: W_settle_spm(:)
        real(dp) :: k_coll(C%contaminantDim(1), C%nSizeClassesSpm)
        integer :: n, s
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%contaminantDim(1)
                ! FIX: Use the initialized DATASET%contaminantSizeClasses instead of uninitialized C%d_contaminant
                k_coll(n,s) = (2.0_dp*C%k_B*(T_water+273.15_dp)/(3.0_dp*C%mu_w(T_water))) &
                            * (C%d_spm(s)/2.0_dp + DATASET%contaminantSizeClasses(n)/2.0_dp)**2 / &
                              ((C%d_spm(s)/2.0_dp)*(DATASET%contaminantSizeClasses(n)/2.0_dp)) &
                            + (4.0_dp/3.0_dp)*G*(DATASET%contaminantSizeClasses(n)/2.0_dp + C%d_spm(s)/2.0_dp)**3 &
                            + C%pi*(C%d_spm(s)/2.0_dp+DATASET%contaminantSizeClasses(n)/2.0_dp)**2 * &
                              abs(this%W_settle_contaminant(n) - W_settle_spm(s))
            end do
        end do
    end function

    !> Calculate number concentration of particles from mass concentration, density, and diameter.
    function contaminant_calculateParticleConcentration(this, C_mass, rho_particle, d) result(C_particle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: C_mass, rho_particle, d
        real(dp) :: C_particle
        C_particle = C_mass / (rho_particle*(4.0_dp/3.0_dp)*C%pi*(d/2.0_dp)**3)
    end function

    !> Calculate attachment rate to porous media using colloid filtration theory.
    function contaminant_calculateAttachmentRate(this, T_water, porosity, d_grain, velocity) result(k_att)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water
        real(dp), intent(in) :: porosity, d_grain
        real(dp), intent(in), optional :: velocity
        real(dp) :: k_att(C%contaminantDim(1))
        integer :: i
        real(dp) :: gamma, r_i, kBT, N_G, N_VDW, N_Pe, N_R, A_s, eta_grav, eta_intercept, eta_Brownian, eta_0, lambda_filter, D_i
        real(dp) :: v

        ! Use compartment-specific velocity
        if (present(velocity) .and. (this%compartment == 'water' .or. this%compartment == 'estuary')) then
            v = velocity
        else if (this%compartment == 'soil') then
            v = DATASET%soilDarcyVelocity
        else
            v = 0.0_dp  ! Default to zero if velocity not provided for water/estuary
        end if

        gamma = (1.0_dp - porosity) ** (1.0_dp/3.0_dp)
        kBT = C%k_B * (T_water + 273.15_dp)
        N_VDW = DATASET%soilHamakerConstant / kBT
        A_s = 2.0_dp * (1.0_dp - gamma**5) / (2.0_dp - 3.0_dp*gamma + 3.0_dp*gamma**5 - 2.0_dp*gamma**6)
        do i = 1, C%contaminantDim(1)
            ! FIX: Use the initialized DATASET%contaminantSizeClasses instead of uninitialized C%d_contaminant
            r_i = DATASET%contaminantSizeClasses(i) * 0.5_dp
            D_i = kBT / (6.0_dp * C%pi * C%mu_w(T_water) * r_i)
            N_Pe = v * d_grain / D_i
            N_G = 2.0_dp * r_i**2 * (DATASET%soilParticleDensity - C%rho_w(T_water)) * C%g &
                / (9.0_dp * C%mu_w(T_water) * v)
            N_R = r_i / (d_grain * 0.5_dp)
            eta_grav = 2.22_dp * N_R**(-0.024_dp) * N_G**1.11_dp * N_VDW**0.053_dp
            eta_intercept = 0.55_dp * N_R**1.55_dp * N_Pe**(-0.125_dp) * N_VDW**0.125_dp
            eta_Brownian = 2.4_dp * A_s**0.33_dp * N_R**(-0.081_dp) * N_Pe**(-0.715_dp) * N_VDW**0.053_dp
            eta_0 = eta_grav + eta_intercept + eta_Brownian
            lambda_filter = 1.5_dp * (1.0_dp - porosity) / (d_grain * porosity)
            k_att(i) = lambda_filter * eta_0 * v
        end do
    end function

    !> Calculate settling velocity using Stokes' law.
    function contaminant_calculateSettlingVelocity(this, d, rho_particle, T_water) result(W_settle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: d, rho_particle
        real(dp), intent(in) :: T_water
        real(dp) :: W_settle
        W_settle = (rho_particle - C%rho_w(T_water)) * C%g * d**2 / (18.0_dp * C%mu_w(T_water))
    end function
end module

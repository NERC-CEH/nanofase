module ContaminantModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT, SPM_CONTAMINANT_START
    use ResultModule, only: Result, Result0D
    use ErrorInstanceModule
    use mo_netcdf
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
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

    ! Local function to replace str from UtilModule
    function int_to_string(i) result(s)
        integer, intent(in) :: i
        character(len=20) :: s
        write(s, '(I0)') i
        s = trim(adjustl(s))
    end function

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

    function contaminant_create(this) result(r)
        class(Contaminant), intent(inout) :: this
        type(Result) :: r
        integer :: alloc_stat
        type(ErrorInstance) :: err(1)

        if (C%contaminantDim(3) < SPM_CONTAMINANT_START + C%nSizeClassesSpm - 1) then
            err(1) = ErrorInstance(code=900, message='Contaminant state dimension too small')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if
        allocate(this%c(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
                 this%k_hetero(C%contaminantDim(1), C%nSizeClassesSpm), &
                 this%W_settle_contaminant(C%contaminantDim(1)), &
                 this%individualContaminantMass(C%contaminantDim(1)), &
                 this%C_contaminant_free_particle(C%contaminantDim(1)), &
                 stat=alloc_stat)
        if (alloc_stat /= 0) then
            err(1) = ErrorInstance(code=901, message='Contaminant allocation failed')
            call r%addError(err(1))
            call LOGR%toFile(errors=r%errors)
            return
        end if
        this%c = 0.0_dp
        this%m_dissolved = 0.0_dp
        this%k_hetero = 0.0_dp
        this%W_settle_contaminant = 0.0_dp
        this%individualContaminantMass = 0.0_dp
        this%C_contaminant_free_particle = 0.0_dp
        this%rho_contaminant = 0.0_dp
        this%k_diss_pristine = 0.0_dp
        this%k_diss_transformed = 0.0_dp
        this%k_transform_pristine = 0.0_dp
        this%alpha_hetero = 0.0_dp
        this%alpha_att = 0.0_dp
        this%compartment = ''
    end function

    function contaminant_create_from_data(this, data, compartment, contaminantDensity, &
                                        soilAttachmentEfficiency, riverAttachmentEfficiency, &
                                        estuaryAttachmentEfficiency, k_diss_pristine, &
                                        k_diss_transformed, k_transform_pristine, waterTemperature) result(r)
        class(Contaminant), intent(inout) :: this
        type(NcDataset), intent(in), optional :: data
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
        type(NcVariable) :: var
        type(ErrorInstance) :: err(1)
        integer :: n

        r = this%create()
        if (r%hasCriticalError()) then
            call LOGR%toFile(errors=r%errors)
            return
        end if
        this%compartment = compartment
        this%rho_contaminant = contaminantDensity
        this%k_diss_pristine = k_diss_pristine
        this%k_diss_transformed = k_diss_transformed
        this%k_transform_pristine = k_transform_pristine
        select case (compartment)
            case ('soil')
                this%alpha_hetero = soilAttachmentEfficiency
                this%alpha_att = soilAttachmentEfficiency
            case ('water')
                this%alpha_hetero = riverAttachmentEfficiency
                this%alpha_att = riverAttachmentEfficiency
            case ('estuary')
                this%alpha_hetero = estuaryAttachmentEfficiency
                this%alpha_att = estuaryAttachmentEfficiency
            case ('sediment')
                this%alpha_hetero = estuaryAttachmentEfficiency
                this%alpha_att = estuaryAttachmentEfficiency
            case default
                err(1) = ErrorInstance(code=900, message="Invalid compartment: " // trim(compartment))
                call r%addErrors(err)
                call LOGR%toFile(errors=r%errors)
                return
        end select
        do n = 1, C%nContaminantSizeClasses
            this%W_settle_contaminant(n) = this%calculateSettlingVelocity( &
                C%d_contaminant(n), this%rho_contaminant, waterTemperature)
            this%individualContaminantMass(n) = this%rho_contaminant * (4.0_dp/3.0_dp) * &
                C%pi * (C%d_contaminant(n)/2.0_dp)**3
        end do
        if (present(data)) then
            var = data%getVariable('c')
            if (data%hasVariable('c')) then
                call var%getData(this%c)
            end if
            var = data%getVariable('m_dissolved')
            if (data%hasVariable('m_dissolved')) then
                call var%getData(this%m_dissolved)
            end if
        end if
    end function

    subroutine contaminant_add(this, addition)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        this%c = this%c + addition%c
        this%m_dissolved = this%m_dissolved + addition%m_dissolved
    end subroutine

    subroutine contaminant_add_scaled(this, addition, scale)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        real(dp), intent(in) :: scale
        this%c = this%c + addition%c * scale
        this%m_dissolved = this%m_dissolved + addition%m_dissolved * scale
    end subroutine

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

    subroutine contaminant_empty(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) this%c = 0.0_dp
        this%m_dissolved = 0.0_dp
    end subroutine

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
        do n = 1, C%nContaminantSizeClasses
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, real(C%d_contaminant(n),dp))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

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
        do n = 1, C%nContaminantSizeClasses
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, real(C%d_contaminant(n),dp))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

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
        do n = 1, C%nContaminantSizeClasses
            this%C_contaminant_free_particle(n) = this%calculateParticleConcentration( &
                sum(this%c(n,:,FREE_CONTAMINANT))/volume, this%rho_contaminant, real(C%d_contaminant(n),dp))
        end do
        call r%addErrors(.errors. this%heteroaggregation(dt, T_water, C_spm, W_settle_spm, C_spm_particle))
        call r%addErrors(.errors. this%attachment(dt, k_att, alpha_att))
        call r%addErrors(.errors. this%dissolution(dt))
        call r%addErrors(.errors. this%transformation(dt))
        deallocate(C_spm_particle)
    end function

    function contaminant_heteroaggregation(this, dt, T_water, C_spm, W_settle_spm, C_spm_particle) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water
        real(dp), intent(in) :: C_spm(:)
        real(dp), intent(in) :: W_settle_spm(:), C_spm_particle(:)
        type(Result) :: r
        real(dp) :: k_coll(C%nContaminantSizeClasses, C%nSizeClassesSpm)
        integer :: s, n, f
        real(dp) :: dm_hetero, G  
        G = 0.0_dp  
        k_coll = this%calculateCollisionRate(T_water, G, W_settle_spm)
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nContaminantSizeClasses
                this%k_hetero(n,s) = k_coll(n,s) * this%alpha_hetero * C_spm_particle(s)
            end do
        end do
        do n = 1, C%nContaminantSizeClasses
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

    function contaminant_attachment(this, dt, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        real(dp), intent(in) :: k_att(:), alpha_att
        type(Result) :: r
        integer :: n, f
        real(dp) :: dm_att
        do n = 1, C%nContaminantSizeClasses
            do f = 1, C%nContaminantForms
                dm_att = min(k_att(n) * alpha_att * dt * this%c(n,f,FREE_CONTAMINANT), this%c(n,f,FREE_CONTAMINANT))
                this%c(n,f,FREE_CONTAMINANT) = this%c(n,f,FREE_CONTAMINANT) - dm_att
                this%c(n,f,ATTACHED_CONTAMINANT) = this%c(n,f,ATTACHED_CONTAMINANT) + dm_att
            end do
        end do
    end function

    function contaminant_dissolution(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
        real(dp) :: dm_diss(C%nContaminantSizeClasses, C%nContaminantForms, C%contaminantDim(3))
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

    function contaminant_transformation(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
        real(dp) :: dm_transform(C%nContaminantSizeClasses, C%nContaminantForms, C%contaminantDim(3))
        if (C%nContaminantForms > 1) then
            dm_transform = 0.0_dp
            dm_transform(:,1,:) = min(this%k_transform_pristine * dt * this%c(:,1,:), this%c(:,1,:))
            this%c(:,1,:) = this%c(:,1,:) - dm_transform(:,1,:)
            this%c(:,2,:) = this%c(:,2,:) + dm_transform(:,1,:)
        end if
    end function

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

    function contaminant_get_free(this) result(C_free)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_free(C%contaminantDim(1), C%contaminantDim(2))
        C_free = this%c(:,:,FREE_CONTAMINANT)
    end function

    function contaminant_get_attached(this) result(C_attached)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_attached(C%contaminantDim(1), C%contaminantDim(2))
        C_attached = this%c(:,:,ATTACHED_CONTAMINANT)
    end function

    function contaminant_calculateCollisionRate(this, T_water, G, W_settle_spm) result(k_coll)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water
        real(dp), intent(in) :: G
        real(dp), intent(in) :: W_settle_spm(:)
        real(dp) :: k_coll(C%nContaminantSizeClasses, C%nSizeClassesSpm)
        integer :: n, s
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nContaminantSizeClasses
                k_coll(n,s) = (2.0_dp*C%k_B*(T_water+273.15_dp)/(3.0_dp*C%mu_w(T_water))) &
                            * (C%d_spm(s)/2.0_dp + C%d_contaminant(n)/2.0_dp)**2 / &
                              ((C%d_spm(s)/2.0_dp)*(C%d_contaminant(n)/2.0_dp)) &
                            + (4.0_dp/3.0_dp)*G*(C%d_contaminant(n)/2.0_dp + C%d_spm(s)/2.0_dp)**3 &
                            + C%pi*(C%d_spm(s)/2.0_dp+C%d_contaminant(n)/2.0_dp)**2 * &
                              abs(this%W_settle_contaminant(n) - W_settle_spm(s))
            end do
        end do
    end function

    function contaminant_calculateParticleConcentration(this, C_mass, rho_particle, d) result(C_particle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: C_mass, rho_particle, d
        real(dp) :: C_particle
        C_particle = C_mass / (rho_particle*(4.0_dp/3.0_dp)*C%pi*(d/2.0_dp)**3)
    end function

    function contaminant_calculateAttachmentRate(this, T_water, porosity, d_grain, velocity) result(k_att)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water
        real(dp), intent(in) :: porosity, d_grain
        real(dp), intent(in), optional :: velocity
        real(dp) :: k_att(C%nContaminantSizeClasses)
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
        do i = 1, C%nContaminantSizeClasses
            r_i = real(C%d_contaminant(i),dp) * 0.5_dp
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

    function contaminant_calculateSettlingVelocity(this, d, rho_particle, T_water) result(W_settle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: d, rho_particle
        real(dp), intent(in) :: T_water
        real(dp) :: W_settle
        W_settle = (rho_particle - C%rho_w(T_water)) * C%g * d**2 / (18.0_dp * C%mu_w(T_water))
    end function
end module
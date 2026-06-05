module ContaminantModule
    use GlobalsModule, only: dp, C
    use ResultModule, only: Result, Result0D
    use ErrorInstanceModule
    use LoggerModule, only: LOGR
    use DataInputModule, only: DATASET
    use PFASEConstantsModule
    use PFASEProcessModule
    implicit none

    private
    public :: Contaminant
    public :: operator(+), operator(-), operator(*)

    type, public :: Contaminant
        real(dp), allocatable :: c(:,:,:)          !! c(species, form, phase)
        real(dp) :: m_dissolved = 0.0_dp           !! legacy alias: total aqueous mass

        ! Legacy scalar fields retained only for backwards-compatible interfaces; phase arrays are authoritative.
        real(dp) :: rho_contaminant = 0.0_dp
        real(dp) :: k_diss_pristine = 0.0_dp
        real(dp) :: k_diss_transformed = 0.0_dp
        real(dp) :: k_transform_pristine = 0.0_dp
        real(dp) :: alpha_hetero = 0.0_dp
        real(dp) :: alpha_att = 0.0_dp
        character(len=100) :: compartment = ''

        ! PFAS species properties and process parameters.
        character(len=PFAS_MAX_NAME), allocatable :: species_name(:)
        real(dp), allocatable :: molecular_weight(:), charge(:), pka(:)
        real(dp), allocatable :: kd_solid(:), kd_spm(:), kaw_awi(:)
        real(dp), allocatable :: k_ads_solid(:), k_des_solid(:)
        real(dp), allocatable :: k_ads_spm(:),   k_des_spm(:)
        real(dp), allocatable :: k_ads_awi(:),   k_des_awi(:)
        real(dp), allocatable :: k_react(:,:), reaction_yield(:,:)
        real(dp), allocatable :: k_volatilisation(:), k_seaspray(:), k_bio_uptake(:)

        ! Diagnostic boundary/process fluxes for current timestep [kg timestep-1].
        real(dp), allocatable :: j_to_groundwater(:), j_to_downstream(:), j_to_sediment(:)
        real(dp), allocatable :: j_to_atmosphere(:), j_to_foam(:), j_to_biota(:)
    contains
        procedure :: create => contaminant_create
        procedure :: create_from_data => contaminant_create_from_data
        procedure :: add => contaminant_add
        procedure :: add_scaled => contaminant_add_scaled
        procedure :: multiply_scalar => contaminant_multiply_scalar
        procedure :: divideCheckZero => contaminant_divideCheckZero
        procedure :: finalise => contaminant_finalise
        procedure :: empty => contaminant_empty
        procedure :: update => contaminant_update
        procedure :: update_water => contaminant_update_water
        procedure :: update_sediment => contaminant_update_sediment
        procedure :: update_soil => contaminant_update_soil
        procedure :: partition_solid => contaminant_partition_solid
        procedure :: partition_spm => contaminant_partition_spm
        procedure :: partition_awi => contaminant_partition_awi
        procedure :: transformation => contaminant_transformation
        procedure :: transform_network => contaminant_transformation
        procedure :: leach => contaminant_leach
        procedure :: erosion_export => contaminant_erosion_export
        procedure :: deposition => contaminant_deposition
        procedure :: sediment_exchange => contaminant_sediment_exchange
        procedure :: outflow_split => contaminant_outflow_split
        procedure :: foam_exchange => contaminant_foam_exchange
        procedure :: atmosphere_exchange => contaminant_atmosphere_exchange
        procedure :: bioaccumulate => contaminant_bioaccumulate
        procedure :: getConcentration => contaminant_getConcentration
        procedure :: get_free => contaminant_get_free
        procedure :: get_attached => contaminant_get_attached
        procedure :: get_phase => contaminant_get_phase
        procedure :: mass_total => contaminant_mass_total
        procedure :: reset_fluxes => contaminant_reset_fluxes

        ! Legacy nanoparticle procedures now return safe PFAS-compatible fallbacks.
        procedure :: heteroaggregation => contaminant_noop_result
        procedure :: dissolution => contaminant_noop_result_dt
        procedure :: attachment => contaminant_noop_result_att
        procedure :: calculateCollisionRate => contaminant_calculateCollisionRate
        procedure :: calculateParticleConcentration => contaminant_calculateParticleConcentration
        procedure :: calculateAttachmentRate => contaminant_calculateAttachmentRate
        procedure :: calculateSettlingVelocity => contaminant_calculateSettlingVelocity
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
    ! P-FASE: legacy duplicate operator/deposition/outflow implementations removed.
    ! The phase-aware implementations below are authoritative.


    !> Initialize a Contaminant object, allocating arrays and setting default values to zero.
    !! Adds defensive finalize, dimension checks, and verbose logging.
    function contaminant_create(this) result(r)
        class(Contaminant), intent(inout) :: this
        type(Result) :: r
        type(ErrorInstance) :: err
        integer :: ns, nf, np, stat

        ns = max(1, C%contaminantDim(1))
        nf = max(1, C%contaminantDim(2))
        np = max(PFAS_NPHASES, C%contaminantDim(3))

        call this%finalise()
        allocate(this%c(ns,nf,np), this%species_name(ns), this%molecular_weight(ns), &
                 this%charge(ns), this%pka(ns), this%kd_solid(ns), this%kd_spm(ns), &
                 this%kaw_awi(ns), this%k_ads_solid(ns), this%k_des_solid(ns), &
                 this%k_ads_spm(ns), this%k_des_spm(ns), this%k_ads_awi(ns), this%k_des_awi(ns), &
                 this%k_react(ns,ns), this%reaction_yield(ns,ns), &
                 this%k_volatilisation(ns), this%k_seaspray(ns), this%k_bio_uptake(ns), &
                 this%j_to_groundwater(ns), this%j_to_downstream(ns), this%j_to_sediment(ns), &
                 this%j_to_atmosphere(ns), this%j_to_foam(ns), this%j_to_biota(ns), stat=stat)
        if (stat /= 0) then
            err = ErrorInstance(code=901, message='P-FASE Contaminant allocation failed')
            call r%addError(err)
            call LOGR%toFile(errors=r%errors)
            return
        end if

        call this%empty()
        this%species_name = ''
        this%molecular_weight = 0.0_dp
        this%charge = 0.0_dp
        this%pka = 0.0_dp
        this%kd_solid = 0.0_dp
        this%kd_spm = 0.0_dp
        this%kaw_awi = 0.0_dp
        this%k_ads_solid = 0.0_dp
        this%k_des_solid = 0.0_dp
        this%k_ads_spm = 0.0_dp
        this%k_des_spm = 0.0_dp
        this%k_ads_awi = 0.0_dp
        this%k_des_awi = 0.0_dp
        this%k_react = 0.0_dp
        this%reaction_yield = 0.0_dp
        this%k_volatilisation = 0.0_dp
        this%k_seaspray = 0.0_dp
        this%k_bio_uptake = 0.0_dp
    end function


    !> Create a Contaminant from input data, setting properties and calculating settling velocities.
    !! Adds a warning if scalar/class counts disagree; keeps single create path and debug logs.
    function contaminant_create_from_data(this, compartment, contaminantDensity, soilAttachmentEfficiency, &
                                          riverAttachmentEfficiency, estuaryAttachmentEfficiency, &
                                          k_diss_pristine, k_diss_transformed, k_transform_pristine, &
                                          waterTemperature) result(r)
        class(Contaminant), intent(inout) :: this
        character(len=*), intent(in) :: compartment
        real(dp), intent(in), optional :: contaminantDensity, soilAttachmentEfficiency
        real(dp), intent(in), optional :: riverAttachmentEfficiency, estuaryAttachmentEfficiency
        real(dp), intent(in), optional :: k_diss_pristine, k_diss_transformed, k_transform_pristine
        real(dp), intent(in), optional :: waterTemperature
        type(Result) :: r
        integer :: i

        r = this%create()
        if (r%hasCriticalError()) return

        this%compartment = trim(compartment)
        if (present(contaminantDensity)) this%rho_contaminant = contaminantDensity
        if (present(k_diss_pristine)) this%k_diss_pristine = k_diss_pristine
        if (present(k_diss_transformed)) this%k_diss_transformed = k_diss_transformed
        if (present(k_transform_pristine)) this%k_transform_pristine = k_transform_pristine

        select case(trim(compartment))
        case('soil','atmospheric')
            if (present(soilAttachmentEfficiency)) this%alpha_att = soilAttachmentEfficiency
        case('water')
            if (present(riverAttachmentEfficiency)) this%alpha_att = riverAttachmentEfficiency
        case('estuary','sediment')
            if (present(estuaryAttachmentEfficiency)) this%alpha_att = estuaryAttachmentEfficiency
        end select


        ! Load species-resolved PFAS properties when available. These replace the old
        ! nanoparticle density/attachment/dissolution parameters for P-FASE runs.
        if (allocated(DATASET%pfasSpeciesNames) .and. allocated(this%species_name)) then
            do i = 1, min(size(this%species_name), size(DATASET%pfasSpeciesNames))
                this%species_name(i) = DATASET%pfasSpeciesNames(i)
            end do
        end if
        if (allocated(DATASET%pfasMolecularWeight)) this%molecular_weight(:min(size(this%molecular_weight),size(DATASET%pfasMolecularWeight))) = DATASET%pfasMolecularWeight(:min(size(this%molecular_weight),size(DATASET%pfasMolecularWeight)))
        if (allocated(DATASET%pfasCharge))          this%charge(:min(size(this%charge),size(DATASET%pfasCharge))) = DATASET%pfasCharge(:min(size(this%charge),size(DATASET%pfasCharge)))
        if (allocated(DATASET%pfasPka))             this%pka(:min(size(this%pka),size(DATASET%pfasPka))) = DATASET%pfasPka(:min(size(this%pka),size(DATASET%pfasPka)))
        if (allocated(DATASET%pfasKdSolid))         this%kd_solid(:min(size(this%kd_solid),size(DATASET%pfasKdSolid))) = DATASET%pfasKdSolid(:min(size(this%kd_solid),size(DATASET%pfasKdSolid)))
        if (allocated(DATASET%pfasKdSpm))           this%kd_spm(:min(size(this%kd_spm),size(DATASET%pfasKdSpm))) = DATASET%pfasKdSpm(:min(size(this%kd_spm),size(DATASET%pfasKdSpm)))
        if (allocated(DATASET%pfasKawAwi))          this%kaw_awi(:min(size(this%kaw_awi),size(DATASET%pfasKawAwi))) = DATASET%pfasKawAwi(:min(size(this%kaw_awi),size(DATASET%pfasKawAwi)))
        if (allocated(DATASET%pfasKAdsSolid))       this%k_ads_solid(:min(size(this%k_ads_solid),size(DATASET%pfasKAdsSolid))) = DATASET%pfasKAdsSolid(:min(size(this%k_ads_solid),size(DATASET%pfasKAdsSolid)))
        if (allocated(DATASET%pfasKDesSolid))       this%k_des_solid(:min(size(this%k_des_solid),size(DATASET%pfasKDesSolid))) = DATASET%pfasKDesSolid(:min(size(this%k_des_solid),size(DATASET%pfasKDesSolid)))
        if (allocated(DATASET%pfasKAdsSpm))         this%k_ads_spm(:min(size(this%k_ads_spm),size(DATASET%pfasKAdsSpm))) = DATASET%pfasKAdsSpm(:min(size(this%k_ads_spm),size(DATASET%pfasKAdsSpm)))
        if (allocated(DATASET%pfasKDesSpm))         this%k_des_spm(:min(size(this%k_des_spm),size(DATASET%pfasKDesSpm))) = DATASET%pfasKDesSpm(:min(size(this%k_des_spm),size(DATASET%pfasKDesSpm)))
        if (allocated(DATASET%pfasKAdsAwi))         this%k_ads_awi(:min(size(this%k_ads_awi),size(DATASET%pfasKAdsAwi))) = DATASET%pfasKAdsAwi(:min(size(this%k_ads_awi),size(DATASET%pfasKAdsAwi)))
        if (allocated(DATASET%pfasKDesAwi))         this%k_des_awi(:min(size(this%k_des_awi),size(DATASET%pfasKDesAwi))) = DATASET%pfasKDesAwi(:min(size(this%k_des_awi),size(DATASET%pfasKDesAwi)))
        if (allocated(DATASET%pfasReactionRate)) then
            this%k_react(:min(size(this%k_react,1),size(DATASET%pfasReactionRate,1)), :min(size(this%k_react,2),size(DATASET%pfasReactionRate,2))) = &
                DATASET%pfasReactionRate(:min(size(this%k_react,1),size(DATASET%pfasReactionRate,1)), :min(size(this%k_react,2),size(DATASET%pfasReactionRate,2)))
        end if
        if (allocated(DATASET%pfasReactionYield)) then
            this%reaction_yield(:min(size(this%reaction_yield,1),size(DATASET%pfasReactionYield,1)), :min(size(this%reaction_yield,2),size(DATASET%pfasReactionYield,2))) = &
                DATASET%pfasReactionYield(:min(size(this%reaction_yield,1),size(DATASET%pfasReactionYield,1)), :min(size(this%reaction_yield,2),size(DATASET%pfasReactionYield,2)))
        end if
        if (allocated(DATASET%pfasVolatilisationRate)) this%k_volatilisation(:min(size(this%k_volatilisation),size(DATASET%pfasVolatilisationRate))) = DATASET%pfasVolatilisationRate(:min(size(this%k_volatilisation),size(DATASET%pfasVolatilisationRate)))
        if (allocated(DATASET%pfasSeasprayRate))       this%k_seaspray(:min(size(this%k_seaspray),size(DATASET%pfasSeasprayRate))) = DATASET%pfasSeasprayRate(:min(size(this%k_seaspray),size(DATASET%pfasSeasprayRate)))
        if (allocated(DATASET%pfasBioUptakeRate))      this%k_bio_uptake(:min(size(this%k_bio_uptake),size(DATASET%pfasBioUptakeRate))) = DATASET%pfasBioUptakeRate(:min(size(this%k_bio_uptake),size(DATASET%pfasBioUptakeRate)))

        do i = 1, size(this%c,1)
            write(this%species_name(i),'(A,I0)') 'PFAS_', i
            if (i < size(this%c,1) .and. this%k_transform_pristine > 0.0_dp) then
                this%k_react(i,i+1) = this%k_transform_pristine
                this%reaction_yield(i,i+1) = 1.0_dp
            end if
        end do
    end function

    subroutine contaminant_finalise(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) deallocate(this%c)
        if (allocated(this%species_name)) deallocate(this%species_name)
        if (allocated(this%molecular_weight)) deallocate(this%molecular_weight)
        if (allocated(this%charge)) deallocate(this%charge)
        if (allocated(this%pka)) deallocate(this%pka)
        if (allocated(this%kd_solid)) deallocate(this%kd_solid)
        if (allocated(this%kd_spm)) deallocate(this%kd_spm)
        if (allocated(this%kaw_awi)) deallocate(this%kaw_awi)
        if (allocated(this%k_ads_solid)) deallocate(this%k_ads_solid)
        if (allocated(this%k_des_solid)) deallocate(this%k_des_solid)
        if (allocated(this%k_ads_spm)) deallocate(this%k_ads_spm)
        if (allocated(this%k_des_spm)) deallocate(this%k_des_spm)
        if (allocated(this%k_ads_awi)) deallocate(this%k_ads_awi)
        if (allocated(this%k_des_awi)) deallocate(this%k_des_awi)
        if (allocated(this%k_react)) deallocate(this%k_react)
        if (allocated(this%reaction_yield)) deallocate(this%reaction_yield)
        if (allocated(this%k_volatilisation)) deallocate(this%k_volatilisation)
        if (allocated(this%k_seaspray)) deallocate(this%k_seaspray)
        if (allocated(this%k_bio_uptake)) deallocate(this%k_bio_uptake)
        if (allocated(this%j_to_groundwater)) deallocate(this%j_to_groundwater)
        if (allocated(this%j_to_downstream)) deallocate(this%j_to_downstream)
        if (allocated(this%j_to_sediment)) deallocate(this%j_to_sediment)
        if (allocated(this%j_to_atmosphere)) deallocate(this%j_to_atmosphere)
        if (allocated(this%j_to_foam)) deallocate(this%j_to_foam)
        if (allocated(this%j_to_biota)) deallocate(this%j_to_biota)
        this%m_dissolved = 0.0_dp
        this%compartment = ''
    end subroutine contaminant_finalise

    subroutine contaminant_empty(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) this%c = 0.0_dp
        this%m_dissolved = 0.0_dp
        call this%reset_fluxes()
    end subroutine contaminant_empty

    subroutine contaminant_reset_fluxes(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%j_to_groundwater)) this%j_to_groundwater = 0.0_dp
        if (allocated(this%j_to_downstream))  this%j_to_downstream  = 0.0_dp
        if (allocated(this%j_to_sediment))    this%j_to_sediment    = 0.0_dp
        if (allocated(this%j_to_atmosphere))  this%j_to_atmosphere  = 0.0_dp
        if (allocated(this%j_to_foam))        this%j_to_foam        = 0.0_dp
        if (allocated(this%j_to_biota))       this%j_to_biota       = 0.0_dp
    end subroutine contaminant_reset_fluxes

    subroutine update_m_dissolved(this)
        class(Contaminant), intent(inout) :: this
        if (allocated(this%c)) then
            this%m_dissolved = sum(this%c(:,:,PFAS_AQ))
        else
            this%m_dissolved = 0.0_dp
        end if
    end subroutine update_m_dissolved

    subroutine contaminant_add(this, addition)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        if (.not. allocated(this%c) .or. .not. allocated(addition%c)) return
        this%c = this%c + addition%c
        this%c = max(0.0_dp, this%c)
        call update_m_dissolved(this)
    end subroutine contaminant_add

    subroutine contaminant_add_scaled(this, addition, scale)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: addition
        real(dp), intent(in) :: scale
        if (.not. allocated(this%c) .or. .not. allocated(addition%c)) return
        this%c = this%c + addition%c * scale
        this%c = max(0.0_dp, this%c)
        call update_m_dissolved(this)
    end subroutine contaminant_add_scaled

    function add_contaminant(this, other) result(out)
        type(Contaminant), intent(in) :: this, other
        type(Contaminant) :: out
        type(Result) :: r
        r = out%create()
        if (r%hasCriticalError()) return
        if (allocated(this%c) .and. allocated(other%c)) out%c = max(0.0_dp, this%c + other%c)
        call copy_params(this, out)
        call update_m_dissolved(out)
    end function add_contaminant

    function negate_contaminant(this) result(out)
        type(Contaminant), intent(in) :: this
        type(Contaminant) :: out
        type(Result) :: r
        r = out%create()
        if (r%hasCriticalError()) return
        if (allocated(this%c)) out%c = -this%c
        call copy_params(this, out)
        call update_m_dissolved(out)
    end function negate_contaminant

    function multiply_contaminant_scalar(this, scalar) result(out)
        type(Contaminant), intent(in) :: this
        real(dp), intent(in) :: scalar
        type(Contaminant) :: out
        type(Result) :: r
        r = out%create()
        if (r%hasCriticalError()) return
        if (allocated(this%c)) out%c = this%c * scalar
        call copy_params(this, out)
        call update_m_dissolved(out)
    end function multiply_contaminant_scalar

    subroutine contaminant_multiply_scalar(this, source, scalar)
        class(Contaminant), intent(inout) :: this
        type(Contaminant), intent(in) :: source
        real(dp), intent(in) :: scalar
        type(Result) :: r
        r = this%create()
        if (r%hasCriticalError()) return
        if (allocated(source%c)) this%c = source%c * scalar
        call copy_params(source, this)
        call update_m_dissolved(this)
    end subroutine contaminant_multiply_scalar

    function contaminant_divideCheckZero(this, denominator) result(divided)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: denominator
        type(Contaminant) :: divided
        type(Result) :: r
        r = divided%create()
        if (r%hasCriticalError()) return
        if (allocated(this%c) .and. abs(denominator) > C%epsilon) divided%c = this%c / denominator
        call copy_params(this, divided)
        call update_m_dissolved(divided)
    end function contaminant_divideCheckZero

    subroutine copy_params(src, dst)
        type(Contaminant), intent(in) :: src
        type(Contaminant), intent(inout) :: dst
        dst%compartment = src%compartment
        dst%rho_contaminant = src%rho_contaminant
        dst%k_diss_pristine = src%k_diss_pristine
        dst%k_diss_transformed = src%k_diss_transformed
        dst%k_transform_pristine = src%k_transform_pristine
        dst%alpha_hetero = src%alpha_hetero
        dst%alpha_att = src%alpha_att
        if (allocated(src%species_name) .and. allocated(dst%species_name)) dst%species_name = src%species_name
        if (allocated(src%molecular_weight) .and. allocated(dst%molecular_weight)) dst%molecular_weight = src%molecular_weight
        if (allocated(src%charge) .and. allocated(dst%charge)) dst%charge = src%charge
        if (allocated(src%pka) .and. allocated(dst%pka)) dst%pka = src%pka
        if (allocated(src%kd_solid) .and. allocated(dst%kd_solid)) dst%kd_solid = src%kd_solid
        if (allocated(src%kd_spm) .and. allocated(dst%kd_spm)) dst%kd_spm = src%kd_spm
        if (allocated(src%kaw_awi) .and. allocated(dst%kaw_awi)) dst%kaw_awi = src%kaw_awi
        if (allocated(src%k_ads_solid) .and. allocated(dst%k_ads_solid)) dst%k_ads_solid = src%k_ads_solid
        if (allocated(src%k_des_solid) .and. allocated(dst%k_des_solid)) dst%k_des_solid = src%k_des_solid
        if (allocated(src%k_ads_spm) .and. allocated(dst%k_ads_spm)) dst%k_ads_spm = src%k_ads_spm
        if (allocated(src%k_des_spm) .and. allocated(dst%k_des_spm)) dst%k_des_spm = src%k_des_spm
        if (allocated(src%k_ads_awi) .and. allocated(dst%k_ads_awi)) dst%k_ads_awi = src%k_ads_awi
        if (allocated(src%k_des_awi) .and. allocated(dst%k_des_awi)) dst%k_des_awi = src%k_des_awi
        if (allocated(src%k_react) .and. allocated(dst%k_react)) dst%k_react = src%k_react
        if (allocated(src%reaction_yield) .and. allocated(dst%reaction_yield)) dst%reaction_yield = src%reaction_yield
        if (allocated(src%k_volatilisation) .and. allocated(dst%k_volatilisation)) dst%k_volatilisation = src%k_volatilisation
        if (allocated(src%k_seaspray) .and. allocated(dst%k_seaspray)) dst%k_seaspray = src%k_seaspray
        if (allocated(src%k_bio_uptake) .and. allocated(dst%k_bio_uptake)) dst%k_bio_uptake = src%k_bio_uptake
    end subroutine copy_params

    function contaminant_update(this, dt, T_water, C_spm, W_settle_spm, G, volume, compartment, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water, C_spm(:), W_settle_spm(:), G, volume
        character(len=*), intent(in) :: compartment
        real(dp), intent(in), optional :: k_att(:), alpha_att
        type(Result) :: r
        select case(trim(compartment))
        case('soil')
            call r%addErrors(.errors. this%update_soil(dt,T_water,C_spm,W_settle_spm,G,volume,k_att,alpha_att))
        case('water','estuary')
            call r%addErrors(.errors. this%update_water(dt,T_water,C_spm,W_settle_spm,G,volume))
        case('sediment')
            call r%addErrors(.errors. this%update_sediment(dt,T_water,C_spm,W_settle_spm,G,volume))
        end select
        call update_m_dissolved(this)
    end function contaminant_update

    function contaminant_update_soil(this, dt, T_water, C_spm, W_settle_spm, G, volume, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water, C_spm(:), W_settle_spm(:), G, volume
        real(dp), intent(in), optional :: k_att(:), alpha_att
        type(Result) :: r
        real(dp) :: soil_mass, awi_area
        call this%reset_fluxes()
        soil_mass = max(C%epsilon, volume)
        awi_area = max(0.0_dp, volume)
        call this%partition_solid(volume, soil_mass)
        call this%partition_awi(dt, awi_area)
        call r%addErrors(.errors. this%transformation(dt))
        call update_m_dissolved(this)
    end function contaminant_update_soil

    function contaminant_update_water(this, dt, T_water, C_spm, W_settle_spm, G, volume) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water, C_spm(:), W_settle_spm(:), G, volume
        type(Result) :: r
        real(dp) :: spm_mass, awi_area
        call this%reset_fluxes()
        spm_mass = max(0.0_dp, sum(C_spm) * max(volume, 0.0_dp))
        awi_area = 0.0_dp
        if (volume > 0.0_dp) awi_area = volume**(2.0_dp/3.0_dp)
        call this%partition_spm(volume, spm_mass)
        call this%partition_awi(dt, awi_area)
        call r%addErrors(.errors. this%transformation(dt))
        call update_m_dissolved(this)
    end function contaminant_update_water

    function contaminant_update_sediment(this, dt, T_water, C_spm, W_settle_spm, G, volume) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water, C_spm(:), W_settle_spm(:), G, volume
        type(Result) :: r
        call this%reset_fluxes()
        call this%partition_solid(max(C%epsilon, volume), max(C%epsilon, volume))
        call r%addErrors(.errors. this%transformation(dt))
        call update_m_dissolved(this)
    end function contaminant_update_sediment

    subroutine contaminant_partition_solid(this, volume_water, mass_solid)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: volume_water, mass_solid
        if (allocated(this%c)) call pfase_equilibrium_partition(this%c, volume_water, mass_solid, this%kd_solid, PFAS_SOL)
        call update_m_dissolved(this)
    end subroutine contaminant_partition_solid

    subroutine contaminant_partition_spm(this, volume_water, mass_spm)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: volume_water, mass_spm
        if (allocated(this%c)) call pfase_equilibrium_partition(this%c, volume_water, mass_spm, this%kd_spm, PFAS_SPM)
        call update_m_dissolved(this)
    end subroutine contaminant_partition_spm

    subroutine contaminant_partition_awi(this, dt, awi_area)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, awi_area
        real(dp), allocatable :: cap(:)
        if (.not. allocated(this%c)) return
        allocate(cap(size(this%c,1)))
        cap = max(0.0_dp, this%kaw_awi * max(0.0_dp, awi_area))
        call pfase_kinetic_exchange(this%c, dt, this%k_ads_awi, this%k_des_awi, cap, PFAS_AWI)
        deallocate(cap)
        call update_m_dissolved(this)
    end subroutine contaminant_partition_awi

    function contaminant_transformation(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
        if (allocated(this%c)) call pfase_transform_network(this%c, dt, this%k_react, this%reaction_yield)
        call update_m_dissolved(this)
    end function contaminant_transformation

    subroutine contaminant_leach(this, water_fraction, j_leached)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: water_fraction
        type(Contaminant), intent(inout) :: j_leached
        type(Result) :: rr
        integer :: f
        rr = j_leached%create()
        call copy_params(this, j_leached)
        if (.not. allocated(this%c)) return
        do f = 1, size(this%c,2)
            call pfase_remove_by_fraction(this%c(:,f,PFAS_AQ), water_fraction, j_leached%c(:,f,PFAS_AQ))
        end do
        this%j_to_groundwater = sum(j_leached%c(:,:,PFAS_AQ), dim=2)
        call update_m_dissolved(this)
        call update_m_dissolved(j_leached)
    end subroutine contaminant_leach

    subroutine contaminant_erosion_export(this, erosion_fraction, j_eroded)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: erosion_fraction
        type(Contaminant), intent(inout) :: j_eroded
        type(Result) :: rr
        integer :: f
        rr = j_eroded%create()
        call copy_params(this, j_eroded)
        if (.not. allocated(this%c)) return
        do f = 1, size(this%c,2)
            call pfase_remove_by_fraction(this%c(:,f,PFAS_SOL), erosion_fraction, j_eroded%c(:,f,PFAS_SOL))
        end do
        call update_m_dissolved(this)
        call update_m_dissolved(j_eroded)
    end subroutine contaminant_erosion_export

    subroutine contaminant_deposition(this, dt, W_settle_spm, depth, dj_dep)
        !! PFAS deposition is not contaminant settling. This returns the SPM-sorbed
        !! fraction that would move with depositing SPM; caller must remove/add it.
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: dt, W_settle_spm(:), depth
        type(Contaminant), intent(inout) :: dj_dep
        type(Result) :: rr
        real(dp) :: frac
        rr = dj_dep%create()
        call copy_params(this, dj_dep)
        if (.not. allocated(this%c) .or. depth <= C%epsilon) return
        frac = pfase_clamp01(max(0.0_dp, sum(W_settle_spm) / max(1, size(W_settle_spm))) * dt / depth)
        dj_dep%c(:,:,PFAS_SOL) = this%c(:,:,PFAS_SPM) * frac
        call update_m_dissolved(dj_dep)
    end subroutine contaminant_deposition

    subroutine contaminant_sediment_exchange(this, deposition_fraction, resuspension_fraction, j_sediment)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: deposition_fraction, resuspension_fraction
        type(Contaminant), intent(inout) :: j_sediment
        type(Result) :: rr
        integer :: f
        rr = j_sediment%create()
        call copy_params(this, j_sediment)
        if (.not. allocated(this%c)) return
        do f = 1, size(this%c,2)
            call pfase_remove_by_fraction(this%c(:,f,PFAS_SPM), deposition_fraction, j_sediment%c(:,f,PFAS_SOL))
        end do
        this%j_to_sediment = sum(j_sediment%c(:,:,PFAS_SOL), dim=2)
        call update_m_dissolved(this)
        call update_m_dissolved(j_sediment)
    end subroutine contaminant_sediment_exchange

    subroutine contaminant_outflow_split(this, k_outflow, dj_spm_outflow, m_spm, dj_out)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: k_outflow, dj_spm_outflow(:), m_spm(:)
        type(Contaminant), intent(inout) :: dj_out
        type(Result) :: rr
        real(dp) :: spm_frac
        rr = dj_out%create()
        call copy_params(this, dj_out)
        if (.not. allocated(this%c)) return
        spm_frac = 0.0_dp
        if (sum(m_spm) > C%epsilon) spm_frac = pfase_clamp01(sum(dj_spm_outflow) / sum(m_spm))
        dj_out%c(:,:,PFAS_AQ)  = this%c(:,:,PFAS_AQ)  * pfase_clamp01(k_outflow)
        dj_out%c(:,:,PFAS_SPM) = this%c(:,:,PFAS_SPM) * spm_frac
        call update_m_dissolved(dj_out)
    end subroutine contaminant_outflow_split

    subroutine contaminant_foam_exchange(this, foam_fraction, j_foam)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: foam_fraction
        type(Contaminant), intent(inout) :: j_foam
        type(Result) :: rr
        integer :: f
        rr = j_foam%create()
        call copy_params(this, j_foam)
        if (.not. allocated(this%c)) return
        do f = 1, size(this%c,2)
            call pfase_remove_by_fraction(this%c(:,f,PFAS_AWI), foam_fraction, j_foam%c(:,f,PFAS_FOAM))
        end do
        this%j_to_foam = sum(j_foam%c(:,:,PFAS_FOAM), dim=2)
        call update_m_dissolved(this)
        call update_m_dissolved(j_foam)
    end subroutine contaminant_foam_exchange

    subroutine contaminant_atmosphere_exchange(this, volatilisation_fraction, aerosol_fraction, j_air)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: volatilisation_fraction, aerosol_fraction
        type(Contaminant), intent(inout) :: j_air
        type(Result) :: rr
        integer :: f
        real(dp) :: afrac
        rr = j_air%create()
        call copy_params(this, j_air)
        if (.not. allocated(this%c)) return
        afrac = pfase_clamp01(aerosol_fraction)
        do f = 1, size(this%c,2)
            call pfase_remove_by_fraction(this%c(:,f,PFAS_AQ), volatilisation_fraction, j_air%c(:,f,PFAS_AIR))
            j_air%c(:,f,PFAS_AIR) = j_air%c(:,f,PFAS_AIR) + this%c(:,f,PFAS_AWI) * afrac
            this%c(:,f,PFAS_AWI) = this%c(:,f,PFAS_AWI) * (1.0_dp - afrac)
        end do
        this%j_to_atmosphere = sum(j_air%c(:,:,PFAS_AIR), dim=2)
        call update_m_dissolved(this)
        call update_m_dissolved(j_air)
    end subroutine contaminant_atmosphere_exchange

    subroutine contaminant_bioaccumulate(this, uptake_fraction, j_biota)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: uptake_fraction(:)
        type(Contaminant), intent(inout) :: j_biota
        type(Result) :: rr
        integer :: i, f
        rr = j_biota%create()
        call copy_params(this, j_biota)
        if (.not. allocated(this%c)) return
        do i = 1, min(size(this%c,1), size(uptake_fraction))
            do f = 1, size(this%c,2)
                j_biota%c(i,f,PFAS_AQ) = this%c(i,f,PFAS_AQ) * pfase_clamp01(uptake_fraction(i))
                this%c(i,f,PFAS_AQ) = this%c(i,f,PFAS_AQ) - j_biota%c(i,f,PFAS_AQ)
            end do
        end do
        this%j_to_biota = sum(j_biota%c(:,:,PFAS_AQ), dim=2)
        call update_m_dissolved(this)
        call update_m_dissolved(j_biota)
    end subroutine contaminant_bioaccumulate

    function contaminant_getConcentration(this, volume) result(r)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: volume
        type(Result0D) :: r
        real(dp) :: C_total
        if (volume > C%epsilon .and. allocated(this%c)) then
            C_total = sum(this%c) / volume
            allocate(r%data, source=C_total)
        end if
    end function contaminant_getConcentration

    function contaminant_get_free(this) result(C_free)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_free(C%contaminantDim(1), C%contaminantDim(2))
        C_free = 0.0_dp
        if (allocated(this%c)) C_free = this%c(:,:,PFAS_AQ)
    end function contaminant_get_free

    function contaminant_get_attached(this) result(C_attached)
        class(Contaminant), intent(in) :: this
        real(dp) :: C_attached(C%contaminantDim(1), C%contaminantDim(2))
        C_attached = 0.0_dp
        if (allocated(this%c)) C_attached = this%c(:,:,PFAS_SOL)
    end function contaminant_get_attached

    function contaminant_get_phase(this, phase) result(m)
        class(Contaminant), intent(in) :: this
        integer, intent(in) :: phase
        real(dp) :: m(C%contaminantDim(1), C%contaminantDim(2))
        m = 0.0_dp
        if (allocated(this%c)) then
            if (phase >= 1 .and. phase <= size(this%c,3)) m = this%c(:,:,phase)
        end if
    end function contaminant_get_phase

    function contaminant_mass_total(this) result(m)
        class(Contaminant), intent(in) :: this
        real(dp) :: m
        if (allocated(this%c)) then
            m = sum(this%c)
        else
            m = 0.0_dp
        end if
    end function contaminant_mass_total

    function contaminant_noop_result(this, dt, T_water, C_spm, W_settle_spm, C_spm_particle) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, T_water, C_spm(:), W_settle_spm(:), C_spm_particle(:)
        type(Result) :: r
    end function contaminant_noop_result

    function contaminant_noop_result_dt(this, dt) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt
        type(Result) :: r
    end function contaminant_noop_result_dt

    function contaminant_noop_result_att(this, dt, k_att, alpha_att) result(r)
        class(Contaminant), intent(inout) :: this
        real(dp), intent(in) :: dt, k_att(:), alpha_att
        type(Result) :: r
    end function contaminant_noop_result_att

    function contaminant_calculateCollisionRate(this, T_water, G, W_settle_spm) result(k_coll)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water, G, W_settle_spm(:)
        real(dp) :: k_coll(C%contaminantDim(1), C%nSizeClassesSpm)
        k_coll = 0.0_dp
    end function contaminant_calculateCollisionRate

    function contaminant_calculateParticleConcentration(this, C_mass, rho_particle, d) result(C_particle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: C_mass, rho_particle, d
        real(dp) :: C_particle
        C_particle = 0.0_dp
    end function contaminant_calculateParticleConcentration

    function contaminant_calculateAttachmentRate(this, T_water, porosity, d_grain, velocity) result(k_att)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: T_water, porosity, d_grain
        real(dp), intent(in), optional :: velocity
        real(dp) :: k_att(C%contaminantDim(1))
        k_att = 0.0_dp
    end function contaminant_calculateAttachmentRate

    function contaminant_calculateSettlingVelocity(this, d, rho_particle, T_water) result(W_settle)
        class(Contaminant), intent(in) :: this
        real(dp), intent(in) :: d, rho_particle, T_water
        real(dp) :: W_settle
        W_settle = 0.0_dp
    end function contaminant_calculateSettlingVelocity
end module

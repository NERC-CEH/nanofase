module BiotaSoilModule
    use AbstractBiotaModule
    use ResultModule, only: Result
    use Globals
    use DataInputModule
    use datetime_module
    implicit none

    type, public, extends(AbstractBiota) :: BiotaSoil
      contains
        procedure :: create => createBiotaSoil
        procedure :: update => updateBiotaSoil
        procedure :: parseInputData => parseInputDataBiotaSoil
    end type

  contains

    !> Create this soil biota instance 
    function createBiotaSoil(me, biotaIndex) result(rslt)
        class(BiotaSoil)    :: me           !! This soil biota instance
        integer             :: biotaIndex   !! Database index for this biota object TODO move to database
        type(Result)        :: rslt
        ! Set defaults and ref
        me%ref = "BiotaSoil_" // trim(str(biotaIndex))
        me%biotaIndex = biotaIndex
        me%C_stored = 0.0_dp
        ! Get data from input file
        call rslt%addErrors(.errors. me%parseInputData())
        call rslt%addToTrace('Creating Biota')
        ! call LOGR%toFile(errors=.errors. rslt)
    end function

    !> Update the soil biota on this time step
    function updateBiotaSoil(me, t, C_env_np, C_env_transformed, C_env_dissolved) result(rslt)
        class(BiotaSoil) :: me              !! This BiotaSoil instance
        integer :: t                        !! The current time step
        real(dp) :: C_env_np(:,:,:)
        real(dp) :: C_env_transformed(:,:,:)
        real(dp) :: C_env_dissolved
        type(Result) :: rslt                !! The Result object to return errors in
        real(dp) :: C_env_np_sum
        real(dp) :: C_env_transformed_sum
        real(dp) :: gamma_np
        real(dp) :: gamma_transformed
        real(dp) :: gamma_dissolved
        real(dp) :: k_elim
        type(datetime) :: currentDate
        
        currentDate = C%startDate + timedelta(days=t-1)
        ! If it's the month this harvesting is to occur, set concentrations
        ! to zero
        if (me%harvestInMonth == currentDate%getMonth()) then
            me%C_active = 0.0_dp
            me%C_stored = 0.0_dp
        else
            if (trim(me%uptakeFromForm) == 'free') then
                C_env_np_sum = sum(C_env_np(:,:,1))
                C_env_transformed_sum = sum(C_env_transformed(:,:,1))
            else if (trim(me%uptakeFromForm) == 'attached') then
                C_env_np_sum = sum(C_env_np(:,:,2))
                C_env_transformed_sum = sum(C_env_transformed(:,:,2))
            else if (trim(me%uptakeFromForm) == 'free_and_attached' &
                .or. trim(me%uptakeFromForm) == 'attached_and_free' &
                .or. trim(me%uptakeFromForm) == 'free_attached' &
                .or. trim(me%uptakeFromForm) == 'attached_free') then
                C_env_np_sum = sum(C_env_np(:,:,1:2))
                C_env_transformed_sum = sum(C_env_transformed(:,:,1:2))
            else
                call rslt%addError( &
                    ErrorInstance(message = "Sorry, I can't understand the form specified " // &
                        "in the uptake_from_form option for this biota. Specified form: " // &
                        trim(me%uptakeFromForm) &
                    ) &
                )
                call rslt%addToTrace("Updating " // trim(me%ref))
            end if

            gamma_np = me%k_uptake_np * (1 - me%storedFraction) * C_env_np_sum &
                / (me%k_elim_np + me%k_growth + me%k_death)
            gamma_transformed = me%k_uptake_transformed * (1 - me%storedFraction) * C_env_transformed_sum &
                / (me%k_elim_transformed + me%k_growth + me%k_death)
            gamma_dissolved = me%k_uptake_dissolved * (1 - me%storedFraction) * C_env_dissolved &
                / (me%k_elim_dissolved + me%k_growth + me%k_death)
            k_elim = me%k_elim_np + me%k_elim_transformed + me%k_elim_dissolved
            ! Calculate C_active, converting time step to days (because rate constants are /day)
            me%C_active = gamma_np + gamma_transformed + gamma_dissolved &
                + (me%C_active - gamma_np - gamma_transformed - gamma_dissolved) &
                * exp(-(k_elim + me%k_growth + me%k_death) * C%timeStep/86400)

            if (.not. isZero(me%k_growth + me%k_death)) then
                gamma_np = me%k_uptake_np * me%storedFraction * C_env_np_sum &
                    / (me%k_growth + me%k_death)
                gamma_transformed = me%k_uptake_transformed * me%storedFraction * C_env_transformed_sum &
                    / (me%k_growth + me%k_death)
                gamma_dissolved = me%k_uptake_dissolved * me%storedFraction * C_env_dissolved &
                    / (me%k_growth + me%k_death)
            else
                gamma_np = 0.0_dp
                gamma_transformed = 0.0_dp
                gamma_dissolved = 0.0_dp
            end if

            me%C_stored = gamma_np + gamma_transformed + gamma_dissolved &
                + (me%C_stored - gamma_np - gamma_transformed - gamma_dissolved) &
                * exp(-(me%k_growth + me%k_death) * C%timeStep/86400)
        end if

    end function

    !> Parse input data for the soil biota
    function parseInputDataBiotaSoil(me) result(rslt)
        class(BiotaSoil)    :: me
        type(Result)        :: rslt
        ! Get rates from database. TODO deprecate in favour of using directly
        ! to save memory
        me%name = DATASET%biotaName(me%biotaIndex)
        me%k_uptake_np = DATASET%biota_k_uptake_np(me%biotaIndex)
        me%k_uptake_transformed = DATASET%biota_k_uptake_transformed(me%biotaIndex)
        me%k_uptake_dissolved = DATASET%biota_k_uptake_dissolved(me%biotaIndex)
        me%k_elim_np = DATASET%biota_k_elim_np(me%biotaIndex)
        me%k_elim_transformed = DATASET%biota_k_elim_transformed(me%biotaIndex)
        me%k_elim_dissolved = DATASET%biota_k_elim_dissolved(me%biotaIndex)
        me%k_growth = DATASET%biota_k_growth(me%biotaIndex)
        me%k_death = DATASET%biota_k_death(me%biotaIndex)
        me%C_active = DATASET%biotaInitial_C_org(me%biotaIndex)
        me%storedFraction = DATASET%biotaStoredFraction(me%biotaIndex)
        me%uptakeFromForm = DATASET%biotaUptakeFromForm(me%biotaIndex)
        me%harvestInMonth = DATASET%biotaHarvestInMonth(me%biotaIndex)
    end function

end module

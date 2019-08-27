module classBiotaWater
    use spcBiota
    use ResultModule, only: Result
    use Globals
    use classDatabase
    use datetime_module
    implicit none
    type, public, extends(Biota) :: BiotaWater
        contains
            procedure :: create => createBiotaWater
            procedure :: update => updateBiotaWater
            procedure :: parseInputData => parseInputDataBiotaWater
    end type
    contains
        function createBiotaWater(me, biotaIndex) result(rslt)
            class(BiotaWater)    :: me           !! This Water biota instance
            integer             :: biotaIndex   !! Database index for this biota object TODO move to database
            type(Result)        :: rslt
            ! Set defaults and ref
            me%ref = "BiotaWater_" // trim(str(biotaIndex))
            me%biotaIndex = biotaIndex
            me%C_stored = 0.0_dp
            ! Get data from input file
            call rslt%addErrors(.errors. me%parseInputData())
            call rslt%addToTrace('Creating Biota')                 ! Add this procedure to the trace
            ! call LOGR%toFile(errors=.errors. rslt)
        end function

        function updateBiotaWater(me, t, C_env_np, C_env_transformed, C_env_dissolved) result(rslt)
            class(BiotaWater) :: me                 !! This BiotaWater instance
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
                    C_env_np_sum = sum(C_env_np(:,:,3:))
                    C_env_transformed_sum = sum(C_env_transformed(:,:,3:))
                else if (trim(me%uptakeFromForm) == 'free_and_attached' &
                    .or. trim(me%uptakeFromForm) == 'attached_and_free' &
                    .or. trim(me%uptakeFromForm) == 'free_attached' &
                    .or. trim(me%uptakeFromForm) == 'attached_free') then
                    C_env_np_sum = sum(C_env_np(:,:,:))
                    C_env_transformed_sum = sum(C_env_transformed(:,:,:))
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

        function parseInputDataBiotaWater(me) result(rslt)
            class(BiotaWater) :: me
            type(Result) :: rslt
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
            me%C_active = DATASET%biotaInitial_C_org(me%biotaIndex) ! TODO make possible to input C_init_stored too
            me%storedFraction = DATASET%biotaStoredFraction(me%biotaIndex)
            me%uptakeFromForm = DATASET%biotaUptakeFromForm(me%biotaIndex)
            me%harvestInMonth = DATASET%biotaHarvestInMonth(me%biotaIndex)
        end function
end module

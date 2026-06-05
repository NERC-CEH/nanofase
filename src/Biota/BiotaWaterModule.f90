module BiotaWaterModule
    use AbstractBiotaModule
    use ResultModule, only: Result
    use ErrorInstanceModule
    use GlobalsModule
    use DataInputModule
    use datetime_module
    use ContaminantModule
    use UtilModule, only: str
    use PFASEConstantsModule, only: PFAS_AQ, PFAS_SPM, PFAS_AWI
    implicit none

    type, public, extends(AbstractBiota) :: BiotaWater
      contains
        procedure :: create => createBiotaWater
        procedure :: update => updateBiotaWater
    end type

contains

    function createBiotaWater(me, biotaIndex) result(rslt)
        class(BiotaWater), intent(inout) :: me
        integer, intent(in) :: biotaIndex
        type(Result) :: rslt

        me%ref = "BiotaWater_" // trim(str(biotaIndex))
        call rslt%addErrors(.errors. createAbstractBiota(me, biotaIndex))

        if (len_trim(me%uptakeFromPhase) == 0 .or. trim(me%uptakeFromPhase) == "free") then
            me%uptakeFromPhase = "water_aq"
        else if (trim(me%uptakeFromPhase) == "free_and_attached") then
            me%uptakeFromPhase = "water_aq_spm"
        end if

        call rslt%addToTrace('Creating BiotaWater')
    end function createBiotaWater


    function updateBiotaWater(me, t, C_env_contaminant) result(rslt)
        class(BiotaWater), intent(inout) :: me
        integer, intent(in) :: t
        type(Contaminant), intent(in) :: C_env_contaminant
        type(Result) :: rslt
        type(ErrorInstance) :: err(1)

        real(dp), allocatable :: exposure(:), target_active(:), target_stored(:)
        real(dp) :: dt_days, lambda_active, lambda_stored
        type(datetime) :: currentDate
        integer :: i, nSpecies

        if (.not. allocated(C_env_contaminant%c)) then
            err(1) = ErrorInstance(code=105, message="Contaminant array not allocated in BiotaWater update")
            call rslt%addError(err(1))
            return
        end if

        nSpecies = size(C_env_contaminant%c, 1)

        if (.not. allocated(me%C_active)) then
            call rslt%addErrors(.errors. me%create(me%biotaIndex))
        else if (size(me%C_active) /= nSpecies) then
            call me%finalise()
            call rslt%addErrors(.errors. me%create(me%biotaIndex))
        end if
        if (rslt%hasCriticalError()) return

        allocate(exposure(nSpecies), target_active(nSpecies), target_stored(nSpecies))
        exposure = water_biota_exposure(C_env_contaminant, trim(me%uptakeFromPhase))
        target_active = 0.0_dp
        target_stored = 0.0_dp

        currentDate = C%startDate + timedelta(t-1)

        if (me%harvestInMonth > 0 .and. me%harvestInMonth == currentDate%getMonth()) then
            me%C_active = 0.0_dp
            me%C_stored = 0.0_dp
        else
            dt_days = real(C%timeStep, dp) / 86400.0_dp

            do i = 1, nSpecies
                lambda_active = max(0.0_dp, me%k_elim(i) + me%k_growth + me%k_death)
                if (lambda_active > C%epsilon) then
                    target_active(i) = me%k_uptake(i) * (1.0_dp - me%storedFraction) * exposure(i) / lambda_active
                    me%C_active(i) = target_active(i) + (me%C_active(i) - target_active(i)) * &
                                     exp(-lambda_active * dt_days)
                else
                    me%C_active(i) = me%C_active(i) + &
                                     me%k_uptake(i) * (1.0_dp - me%storedFraction) * exposure(i) * dt_days
                end if

                lambda_stored = max(0.0_dp, me%k_growth + me%k_death)
                if (lambda_stored > C%epsilon) then
                    target_stored(i) = me%k_uptake(i) * me%storedFraction * exposure(i) / lambda_stored
                    me%C_stored(i) = target_stored(i) + (me%C_stored(i) - target_stored(i)) * &
                                     exp(-lambda_stored * dt_days)
                else
                    me%C_stored(i) = me%C_stored(i) + &
                                     me%k_uptake(i) * me%storedFraction * exposure(i) * dt_days
                end if
            end do
        end if

        deallocate(exposure, target_active, target_stored)
    end function updateBiotaWater


    function water_biota_exposure(cont, source) result(exposure)
        type(Contaminant), intent(in) :: cont
        character(len=*), intent(in) :: source
        real(dp) :: exposure(size(cont%c,1))
        integer :: i

        exposure = 0.0_dp
        select case(trim(source))
        case("water_aq")
            do i = 1, size(cont%c,1)
                exposure(i) = sum(cont%c(i,:,PFAS_AQ))
            end do
        case("water_aq_spm")
            do i = 1, size(cont%c,1)
                exposure(i) = sum(cont%c(i,:,PFAS_AQ)) + sum(cont%c(i,:,PFAS_SPM))
            end do
        case("water_aq_spm_awi", "free_and_attached")
            do i = 1, size(cont%c,1)
                exposure(i) = sum(cont%c(i,:,PFAS_AQ)) + sum(cont%c(i,:,PFAS_SPM)) + &
                              sum(cont%c(i,:,PFAS_AWI))
            end do
        case default
            do i = 1, size(cont%c,1)
                exposure(i) = sum(cont%c(i,:,PFAS_AQ))
            end do
        end select
    end function water_biota_exposure

end module BiotaWaterModule

module BiotaWaterModule
    use AbstractBiotaModule
    use ResultModule, only: Result
    use GlobalsModule
    use DataInputModule
    use datetime_module
    use ContaminantModule
    implicit none

    type, public, extends(AbstractBiota) :: BiotaWater
      contains
        procedure :: create => createBiotaWater
        procedure :: update => updateBiotaWater
    end type

  contains

    !> Create this water biota instance
    function createBiotaWater(me, biotaIndex) result(rslt)
        class(BiotaWater)   :: me           !! This Water biota instance
        integer             :: biotaIndex   !! Database index for this biota object TODO move to database
        type(Result) :: rslt
        me%ref = "BiotaWater_" // trim(str(biotaIndex))
        call rslt%addErrors(.errors. createAbstractBiota(me, biotaIndex))  ! Call parent class's create method
        call rslt%addToTrace('Creating Biota')
    end function

    !> Update the water biota on this time step
    function updateBiotaWater(me, t, C_env_contaminant) result(rslt)
        class(BiotaWater) :: me                 !! This BiotaWater instance
        integer :: t                        !! The current time step
        type(Contaminant), intent(in) :: C_env_contaminant
        type(Result) :: rslt
        real(dp), allocatable :: C_env_sum(:)
        real(dp), allocatable :: gamma(:)
        real(dp) :: k_elim_total
        type(datetime) :: currentDate
        integer :: f, nForms
        if (.not. allocated(C_env_contaminant%c)) then
            call rslt%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
            return
        end if
        nForms = C%contaminantDim(2) + 1
        allocate(C_env_sum(nForms), gamma(nForms))
        currentDate = C%startDate + timedelta(days=t-1)
        if (me%harvestInMonth == currentDate%getMonth()) then
            me%C_active = 0.0_dp
            me%C_stored = 0.0_dp
        else
            C_env_sum = 0.0_dp
            select case (trim(me%uptakeFromForm))
            case ('free')
                do f = 1, C%contaminantDim(2)
                    C_env_sum(f) = sum(C_env_contaminant%c(:,f,FREE_CONTAMINANT))
                end do
                C_env_sum(nForms) = C_env_contaminant%m_dissolved
            case ('attached')
                do f = 1, C%contaminantDim(2)
                    C_env_sum(f) = sum(C_env_contaminant%c(:,f,3:))  ! Heteroaggregated states
                end do
            case ('free_and_attached', 'attached_and_free', 'free_attached', 'attached_free')
                do f = 1, C%contaminantDim(2)
                    C_env_sum(f) = sum(C_env_contaminant%c(:,f,:))
                end do
                C_env_sum(nForms) = C_env_contaminant%m_dissolved
            case default
                call rslt%addError(ErrorInstance(message="Invalid uptake_from_form: "//trim(me%uptakeFromForm)))
                return
            end select
            do f = 1, nForms
                gamma(f) = me%k_uptake(f) * (1 - me%storedFraction) * C_env_sum(f) / &
                           (me%k_elim(f) + me%k_growth + me%k_death)
            end do
            k_elim_total = sum(me%k_elim)
            me%C_active = sum(gamma) + (me%C_active - sum(gamma)) * &
                        exp(-(k_elim_total + me%k_growth + me%k_death) * C%timeStep/86400)
            if (.not. isZero(me%k_growth + me%k_death)) then
                do f = 1, nForms
                    gamma(f) = me%k_uptake(f) * me%storedFraction * C_env_sum(f) / &
                            (me%k_growth + me%k_death)
                end do
                me%C_stored = sum(gamma) + (me%C_stored - sum(gamma)) * &
                            exp(-(me%k_growth + me%k_death) * C%timeStep/86400)
            end if
        end if
    end function

end module
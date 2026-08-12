module BiotaSoilModule
    use AbstractBiotaModule
    use ResultModule, only: Result
    use GlobalsModule
    use DataInputModule
    use datetime_module
    use ContaminantModule, only: Contaminant, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
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
        integer             :: biotaIndex   !! Database index for this biota object
        type(Result)        :: rslt        !! Result object for error handling
        me%ref = "BiotaSoil_" // trim(str(biotaIndex))
        ! Call the parent class's create method
        call rslt%addErrors(.errors. createAbstractBiota(me, biotaIndex))
        call rslt%addToTrace('Creating BiotaSoil')
    end function

    !> Update the soil biota on this time step
    function updateBiotaSoil(me, t, C_env_contaminant) result(rslt)
        class(BiotaSoil) :: me              !! This BiotaSoil instance
        integer :: t                        !! The current time step
        type(Contaminant), intent(in) :: C_env_contaminant
        type(Result) :: rslt                !! The Result object to return errors in
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
                    C_env_sum(f) = sum(C_env_contaminant%c(:,f,ATTACHED_CONTAMINANT))
                end do
            case ('free_and_attached', 'attached_and_free', 'free_attached', 'attached_free')
                do f = 1, C%contaminantDim(2)
                    C_env_sum(f) = sum(C_env_contaminant%c(:,f,FREE_CONTAMINANT)) + &
                                   sum(C_env_contaminant%c(:,f,ATTACHED_CONTAMINANT))
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
            me%C_active = gamma + (me%C_active - gamma) * &
                          exp(-(k_elim_total + me%k_growth + me%k_death) * C%timeStep/86400)
            if (.not. isZero(me%k_growth + me%k_death)) then
                do f = 1, nForms
                    gamma(f) = me%k_uptake(f) * me%storedFraction * C_env_sum(f) / &
                               (me%k_growth + me%k_death)
                end do
                me%C_stored = gamma + (me%C_stored - gamma) * &
                              exp(-(me%k_growth + me%k_death) * C%timeStep/86400)
            end if
        end if
    end function

    !> Parse input data for the soil biota
    function parseInputDataBiotaSoil(me) result(rslt)
        class(BiotaSoil) :: me
        type(Result) :: rslt
        integer :: nForms, f
        nForms = C%contaminantDim(2) + 1
        me%name = DATASET%biotaName(me%biotaIndex)
        do f = 1, C%contaminantDim(2)
            me%k_uptake(f) = DATASET%biota_k_uptake_contaminant(me%biotaIndex, f)
            me%k_elim(f) = DATASET%biota_k_elim_contaminant(me%biotaIndex, f)
        end do
        me%k_uptake(nForms) = DATASET%biota_k_uptake_dissolved(me%biotaIndex)
        me%k_elim(nForms) = DATASET%biota_k_elim_dissolved(me%biotaIndex)
        me%k_growth = DATASET%biota_k_growth(me%biotaIndex)
        me%k_death = DATASET%biota_k_death(me%biotaIndex)
        me%C_active = DATASET%biotaInitial_C_org(me%biotaIndex)
        me%storedFraction = DATASET%biotaStoredFraction(me%biotaIndex)
        me%uptakeFromForm = DATASET%biotaUptakeFromForm(me%biotaIndex)
        me%harvestInMonth = DATASET%biotaHarvestInMonth(me%biotaIndex)
    end function

end module
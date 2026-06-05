module AbstractBiotaModule
    !! P-FASE biota abstraction.
    !! Body burdens and toxicokinetic parameters are PFAS species-resolved.
    !! Concentrations passed to update() are expected to be environmental concentrations,
    !! not total masses, using Contaminant%c(species, form, phase).
    use GlobalsModule, only: dp, C
    use ContaminantModule
    use ResultModule, only: Result
    use ErrorInstanceModule
    use DataInputModule, only: DATASET
    implicit none

    type, abstract, public :: AbstractBiota
        character(len=256) :: ref = ""
        character(len=100) :: name = ""
        integer :: biotaIndex = 0

        real(dp), allocatable :: C_active(:)      !! active PFAS burden per species [kg kg-1 dw]
        real(dp), allocatable :: C_stored(:)      !! stored PFAS burden per species [kg kg-1 dw]
        real(dp), allocatable :: k_uptake(:)      !! species-specific uptake [/day]
        real(dp), allocatable :: k_elim(:)        !! species-specific elimination [/day]

        real(dp) :: k_growth = 0.0_dp             !! growth dilution [/day]
        real(dp) :: k_death = 0.0_dp              !! loss/death dilution [/day]
        real(dp) :: storedFraction = 0.0_dp       !! fraction of uptake routed to stored burden [-]

        ! PFAS phase source selector.
        ! Accepted values:
        ! water_aq, water_aq_spm, water_aq_spm_awi,
        ! soil_aq, soil_aq_sol, soil_aq_sol_awi
        character(len=32) :: uptakeFromPhase = "water_aq"
        integer :: harvestInMonth = 0

    contains
        procedure, public :: create => createAbstractBiota
        procedure, public :: update => updateAbstractBiota
        procedure, public :: parseInputData => parseInputDataAbstractBiota
        procedure, public :: finalise => finaliseAbstractBiota
    end type

contains

    function createAbstractBiota(me, biotaIndex) result(rslt)
        class(AbstractBiota), intent(inout) :: me
        integer, intent(in) :: biotaIndex
        type(Result) :: rslt
        type(ErrorInstance) :: err(1)
        integer :: allocStat, nSpecies

        nSpecies = max(1, C%contaminantDim(1))
        me%biotaIndex = biotaIndex

        if (allocated(me%C_active) .or. allocated(me%C_stored) .or. &
            allocated(me%k_uptake) .or. allocated(me%k_elim)) then
            call me%finalise()
        end if

        allocate(me%C_active(nSpecies), me%C_stored(nSpecies), &
                 me%k_uptake(nSpecies), me%k_elim(nSpecies), stat=allocStat)

        if (allocStat /= 0) then
            err(1) = ErrorInstance(code=901, message="Allocation failed for P-FASE biota arrays")
            call rslt%addError(err(1))
            return
        end if

        me%C_active = 0.0_dp
        me%C_stored = 0.0_dp
        me%k_uptake = 0.0_dp
        me%k_elim = 0.0_dp

        call rslt%addErrors(.errors. me%parseInputData())
    end function createAbstractBiota


    function updateAbstractBiota(me, t, C_env_contaminant) result(rslt)
        class(AbstractBiota), intent(inout) :: me
        integer, intent(in) :: t
        type(Contaminant), intent(in) :: C_env_contaminant
        type(Result) :: rslt
        ! Abstract placeholder. Implemented in BiotaWaterModule/BiotaSoilModule.
    end function updateAbstractBiota


    function parseInputDataAbstractBiota(me) result(rslt)
        class(AbstractBiota), intent(inout) :: me
        type(Result) :: rslt
        type(ErrorInstance) :: err(1)
        integer :: i, nSpecies, nRateCols
        real(dp) :: initBurden

        nSpecies = max(1, C%contaminantDim(1))

        if (me%biotaIndex < 1 .or. me%biotaIndex > DATASET%nBiota) then
            err(1) = ErrorInstance(code=902, message="Invalid biota index")
            call rslt%addError(err(1))
            return
        end if

        me%name = DATASET%biotaName(me%biotaIndex)
        me%k_growth = DATASET%biota_k_growth(me%biotaIndex)
        me%k_death = DATASET%biota_k_death(me%biotaIndex)
        me%storedFraction = max(0.0_dp, min(1.0_dp, DATASET%biotaStoredFraction(me%biotaIndex)))

        ! Backward compatible: old DATASET field is reinterpreted as PFAS phase source.
        me%uptakeFromPhase = adjustl(DATASET%biotaUptakeFromForm(me%biotaIndex))
        if (len_trim(me%uptakeFromPhase) == 0) me%uptakeFromPhase = "water_aq"

        me%harvestInMonth = DATASET%biotaHarvestInMonth(me%biotaIndex)

        nRateCols = 0
        if (allocated(DATASET%biota_k_uptake_contaminant)) nRateCols = size(DATASET%biota_k_uptake_contaminant, 2)

        do i = 1, nSpecies
            if (nRateCols >= i) then
                me%k_uptake(i) = DATASET%biota_k_uptake_contaminant(me%biotaIndex, i)
            else if (nRateCols >= 1) then
                me%k_uptake(i) = DATASET%biota_k_uptake_contaminant(me%biotaIndex, min(i, nRateCols))
            else
                me%k_uptake(i) = 0.0_dp
            end if

            if (allocated(DATASET%biota_k_elim_contaminant)) then
                if (size(DATASET%biota_k_elim_contaminant, 2) >= i) then
                    me%k_elim(i) = DATASET%biota_k_elim_contaminant(me%biotaIndex, i)
                else if (size(DATASET%biota_k_elim_contaminant, 2) >= 1) then
                    me%k_elim(i) = DATASET%biota_k_elim_contaminant(me%biotaIndex, &
                                      min(i, size(DATASET%biota_k_elim_contaminant, 2)))
                else
                    me%k_elim(i) = 0.0_dp
                end if
            end if
        end do

        ! Backward compatibility for old dissolved-only fields.
        do i = 1, nSpecies
            if (me%k_uptake(i) <= 0.0_dp .and. allocated(DATASET%biota_k_uptake_dissolved)) &
                me%k_uptake(i) = DATASET%biota_k_uptake_dissolved(me%biotaIndex)
            if (me%k_elim(i) <= 0.0_dp .and. allocated(DATASET%biota_k_elim_dissolved)) &
                me%k_elim(i) = DATASET%biota_k_elim_dissolved(me%biotaIndex)
        end do

        ! Current DataInputModule defines biotaInitial_C_org as one value per biota.
        ! Apply that initial burden to all species unless a future species-resolved
        ! field is added to DataInputModule.
        initBurden = 0.0_dp
        if (allocated(DATASET%biotaInitial_C_org)) initBurden = DATASET%biotaInitial_C_org(me%biotaIndex)
        me%C_active = initBurden
        me%C_stored = 0.0_dp
    end function parseInputDataAbstractBiota


    subroutine finaliseAbstractBiota(me)
        class(AbstractBiota), intent(inout) :: me

        if (allocated(me%C_active)) deallocate(me%C_active)
        if (allocated(me%C_stored)) deallocate(me%C_stored)
        if (allocated(me%k_uptake)) deallocate(me%k_uptake)
        if (allocated(me%k_elim)) deallocate(me%k_elim)

        me%ref = ""
        me%name = ""
        me%biotaIndex = 0
        me%k_growth = 0.0_dp
        me%k_death = 0.0_dp
        me%storedFraction = 0.0_dp
        me%uptakeFromPhase = "water_aq"
        me%harvestInMonth = 0
    end subroutine finaliseAbstractBiota

end module AbstractBiotaModule

module AbstractBiotaModule
    use GlobalsModule, only: dp
    use ContaminantModule
    implicit none

   type, abstract, public :: AbstractBiota
        character(len=256)  :: ref                                  !! Reference for this instance
        character(len=100)  :: name                                 !! Name of this organism
        integer             :: biotaIndex                           !! Index of this biota index in database, TODO deprecate and deal with data better
        real(dp), allocatable :: C_active(:)                        !! Concentration of nanomaterial in biota [kg/kg dw]
        real(dp), allocatable :: C_stored(:)                       !! Concentration of nanomaterial in biota stored fraction [kg/kg dw]
        real(dp), allocatable :: k_uptake(:)                       !! Uptake constant [/day]
        real(dp), allocatable :: k_elim(:)                         !! Elimination constant [/day]
        real(dp)            :: k_growth                            !! Growth dilution rate [/day]
        real(dp)            :: k_death                             !! Death rate [/day]
        real(dp)            :: storedFraction                      !! Stored fraction of nanomaterial [-]
        character(len=17)   :: uptakeFromForm                      !! What form (free, attached) to uptake from. Options: free, attached, free_and_attached
        integer             :: harvestInMonth                      !! Month to harvest biota, i.e. set C_org to zero
    contains
        procedure, public :: create => createAbstractBiota
        procedure, public :: update => updateAbstractBiota
        procedure, public :: parseInputData => parseInputDataAbstractBiota
        procedure :: finalise => finaliseAbstractBiota
    end type

  contains

     function createAbstractBiota(me, biotaIndex) result(rslt)
        use ResultModule, only: Result
        class(AbstractBiota) :: me
        integer :: biotaIndex
        type(Result) :: rslt
        integer :: allocStat, nForms
        nForms = C%contaminantDim(2) + 1  ! Forms + dissolved
        me%biotaIndex = biotaIndex
        allocate(me%C_active(nForms), me%C_stored(nForms), &
                me%k_uptake(nForms), me%k_elim(nForms), stat=allocStat)
        if (allocStat /= 0) then
            call rslt%addError(ErrorInstance(code=1, message="Allocation failed for biota arrays"))
            return
        end if
        me%C_active = 0.0_dp
        me%C_stored = 0.0_dp
        me%k_uptake = 0.0_dp
        me%k_elim = 0.0_dp
        call rslt%addErrors(.errors. me%parseInputData())
    end function

    function updateAbstractBiota(me, t, C_env_contaminant) result(rslt)
        use ResultModule, only: Result
        class(AbstractBiota) :: me
        integer :: t
        type(Contaminant), intent(in) :: C_env_contaminant
        type(Result) :: rslt
    end function

    function parseInputDataAbstractBiota(me) result(rslt)
        use ResultModule, only: Result
        use DataInputModule, only: DATASET
        class(AbstractBiota) :: me
        type(Result) :: rslt
        integer :: i
        if (me%biotaIndex < 1 .or. me%biotaIndex > DATASET%nBiota) then
            call rslt%addError(ErrorInstance(code=902, message="Invalid biota index"))
            return
        end if
        me%name = DATASET%biotaName(me%biotaIndex)
        me%k_growth = DATASET%biota_k_growth(me%biotaIndex)
        me%k_death = DATASET%biota_k_death(me%biotaIndex)  ! Add k_death
        me%storedFraction = DATASET%biotaStoredFraction(me%biotaIndex)
        me%uptakeFromForm = DATASET%biotaUptakeFromForm(me%biotaIndex)
        me%harvestInMonth = DATASET%biotaHarvestInMonth(me%biotaIndex)
        do i = 1, C%contaminantDim(2)
            me%k_uptake(i) = DATASET%biota_k_uptake_contaminant(me%biotaIndex, i)
            me%k_elim(i) = DATASET%biota_k_elim_contaminant(me%biotaIndex, i)
        end do
        me%k_uptake(C%contaminantDim(2) + 1) = DATASET%biota_k_uptake_dissolved(me%biotaIndex)
        me%k_elim(C%contaminantDim(2) + 1) = DATASET%biota_k_elim_dissolved(me%biotaIndex)
        me%C_active = DATASET%biotaInitial_C_org(me%biotaIndex)
        me%C_stored = 0.0_dp
    end function

    subroutine finaliseAbstractBiota(me)
        class(AbstractBiota) :: me
        if (allocated(me%C_active)) deallocate(me%C_active)
        if (allocated(me%C_stored)) deallocate(me%C_stored)
        if (allocated(me%k_uptake)) deallocate(me%k_uptake)
        if (allocated(me%k_elim)) deallocate(me%k_elim)
    end subroutine
end module
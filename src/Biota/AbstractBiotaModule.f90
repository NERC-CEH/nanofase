module AbstractBiotaModule
    use Globals, only: dp
    implicit none

    type, abstract, public :: AbstractBiota
        character(len=256)  :: ref                                  !! Reference for this instance
        character(len=100)  :: name                                 !! Name of this organism
        integer             :: biotaIndex                           !! Index of this biota index in database, TODO deprecate and deal with data better
        real(dp)            :: C_active                             !! Concentration of nanomaterial in biota [kg/kg dw]
        real(dp)            :: C_stored                             !! Concentration of nanomaterial in biota stored fraction [kg/kg dw]
        real(dp)            :: k_uptake_np                          !! Uptake constant [/day]
        real(dp)            :: k_uptake_transformed                 !! Uptake constant [/day]
        real(dp)            :: k_uptake_dissolved                   !! Uptake constant [/day]
        real(dp)            :: k_elim_np                            !! Elimination constant [/day]
        real(dp)            :: k_elim_transformed
        real(dp)            :: k_elim_dissolved
        real(dp)            :: k_growth                             !! Growth dilution rate [/day]
        real(dp)            :: k_death
        real(dp)            :: storedFraction                       !! Stored fraction of namoaterial [-]
        character(len=17)   :: uptakeFromForm                       !! What form (free, attached) to uptake from. Options: free, attached, free_and_attached
        integer             :: harvestInMonth                       !! Month to harvest biota, i.e. set C_org to zero

      contains
        procedure, public :: create => createAbstractBiota
        procedure, public :: update => updateAbstractBiota
        procedure, public :: parseInputData => parseInputDataAbstractBiota
    end type

  contains

    function createAbstractBiota(me, biotaIndex) result(rslt)
        use ResultModule, only: Result
        class(AbstractBiota)    :: me
        integer                 :: biotaIndex
        type(Result)            :: rslt
    end function

    function updateAbstractBiota(me, t, C_env_np, C_env_transformed, C_env_dissolved) result(rslt)
        use ResultModule, only: Result
        class(AbstractBiota)    :: me
        integer                 :: t
        real(dp)                :: C_env_np(:,:,:)
        real(dp)                :: C_env_transformed(:,:,:)
        real(dp)                :: C_env_dissolved
        type(Result) :: rslt
    end function

    function parseInputDataAbstractBiota(me) result(rslt)
        use ResultModule, only: Result
        class(AbstractBiota)    :: me
        type(Result)            :: rslt
    end function

end module

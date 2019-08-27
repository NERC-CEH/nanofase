module spcBiota                                                     !! superclass definition for Biota
    use Globals
    implicit none                                                   ! force declaration of all variables
    type, abstract, public :: Biota                                 !! type declaration for class
                                                                    ! class properties
        character(len=256) :: ref                                   !! Reference for this instance
        character(len=100) :: name                                  !! Name of this organism
        integer     :: biotaIndex                                   !! Index of this biota index in database, TODO deprecate and deal with data better
        ! real(dp), allocatable :: m_np                               ! Mass of nanomaterial in biota [kg]          ! TODO is this needed?
        real(dp) :: C_active                                        !! Concentration of nanomaterial in biota [kg/kg dw]
        real(dp) :: C_stored                                        !! Concentration of nanomaterial in biota stored fraction [kg/kg dw]
        real(dp) :: k_uptake_np                               !! Uptake constant [/day]
        real(dp) :: k_uptake_transformed                            !! Uptake constant [/day]
        real(dp) :: k_uptake_dissolved                              !! Uptake constant [/day]
        real(dp) :: k_elim_np                                 !! Elimination constant [/day]
        real(dp) :: k_elim_transformed
        real(dp) :: k_elim_dissolved
        real(dp) :: k_growth                                        !! Growth dilution rate [/day]
        real(dp) :: k_death
        real(dp) :: storedFraction                                  !! Stored fraction of namoaterial [-]
        character(len=17) :: uptakeFromForm                         !! What form (free, attached) to uptake from. Options: free, attached, free_and_attached
        integer :: harvestInMonth                                   !! Month to harvest biota, i.e. set C_org to zero

      contains
        procedure, public :: create => createBiota                  ! constructor method
        procedure, public :: update => updateBiota                  ! Update method that is run every time step
        procedure, public :: parseInputData => parseInputDataBiota
    end type

  contains

    function createBiota(me, biotaIndex) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        integer     :: biotaIndex
        type(Result) :: rslt
    end function

    function updateBiota(me, t, C_env_np, C_env_transformed, C_env_dissolved) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        integer :: t
        real(dp) :: C_env_np(:,:,:)
        real(dp) :: C_env_transformed(:,:,:)
        real(dp) :: C_env_dissolved
        type(Result) :: rslt
    end function

    function parseInputDataBiota(me) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        type(Result) :: rslt
    end function

end module

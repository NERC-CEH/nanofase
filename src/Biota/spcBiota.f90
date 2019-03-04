module spcBiota                                                     !! superclass definition for Biota
    use Globals
    implicit none                                                   ! force declaration of all variables
    type, abstract, public :: Biota                                 !! type declaration for class
                                                                    ! class properties
        character(len=256) :: ref                                   !! Reference for this instance
        ! real(dp), allocatable :: m_np                               ! Mass of nanomaterial in biota [kg]          ! TODO is this needed?
        real(dp), allocatable :: C_np                               !! Concentration of nanomaterial in biota [kg/m3]
        real(dp) :: k_uptake(2)                                     !! Uptake constants, one per matrix uptaken from (e.g. soil/water, food) [/s]
        reaL(dp) :: k_elim                                          !! Elimination rate constant [/s]
        real(dp) :: k_growth                                        !! Growth dilution rate [/s]
        real(dp) :: storedFraction                                  !! Stored fraction of namoaterial [-]
        integer :: eliminationPhaseStart                            !! When the elimination phase starts [days]
        real(dp) :: C_np_init                                       !! Initial NM conc at start of model run [kg/m3]

      contains
        procedure, public :: create => createBiota                  ! constructor method
        procedure, public :: update => updateBiota                  ! Update method that is run every time step
        procedure, public :: parseInputData => parseInputDataBiota
    end type

  contains

    function createBiota(me) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        type(Result) :: rslt
    end function

    function updateBiota(me, t, C_np_matrices) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        integer :: t
        real(dp) :: C_np_matrices(2)            !! Concentration of nanomaterial in all matrix (soil/water) and diet
        type(Result) :: rslt
    end function

    function parseInputDataBiota(me) result(rslt)
        use ResultModule, only: Result
        class(Biota) :: me
        type(Result) :: rslt
    end function

end module

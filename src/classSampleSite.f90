module classSampleSite
    use Globals
    use spcReach
    implicit none

    type, public :: SampleSite
        character(len=6) :: ref
        integer :: x, y, r, easts, norths
        real(dp) :: C_spm                   !! [kg/m3]
        real(dp) :: Q                       !! [m3/timestep]
        character(len=256) :: description
        type(ReachPointer) :: reach
        logical :: isStartSite = .false.
        logical :: isEndSite = .false.
    end type

end module
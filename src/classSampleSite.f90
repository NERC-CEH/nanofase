module classSampleSite
    use Globals
    use spcReach
    implicit none

    !> The SampleSite user-defined type is use to describe a sample site,
    !! used for calibration purposes. See doc/calibration.md.
    type, public :: SampleSite
        character(len=6)    :: ref                          !! Reference for this sample site
        integer             :: x, y, r, easts, norths       !! x, y, reach indices and eastings/northings
        real(dp)            :: C_spm                        !! Concentration of SPM at this sample site [kg/m3]
        real(dp)            :: Q                            !! Flow rate at this sample site [m3/timestep]
        character(len=256)  :: description                  !! Sample site description
        type(ReachPointer)  :: reach                        !! Pointer to the reach the sample site is located on
        logical             :: isStartSite = .false.        !! Is this sample site the start site?
        logical             :: isEndSite = .false.          !! Is this sample site the end site?
    end type

end module
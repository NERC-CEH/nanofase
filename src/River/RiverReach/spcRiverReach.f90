!> Module containing definition of abstract base class `RiverReach`.
module spcRiverReach
    use Globals                                                     ! Global declarations
    use mo_netcdf                                                   ! Input/output handling
    use ResultModule, only: Result, Result0D                        ! Error handling classes, required for
    use ErrorInstanceModule
    use spcBedSediment
    use spcReactor
    implicit none

    !> Abstract base class for `RiverReach`. Defines properties and procedures
    !! required in any implementation of this class.
    type, abstract, public :: RiverReach
        character(len=100) :: ref                                   !! Reference for this object, of the form RiverReach_x_y_s_r
        integer :: x                                                !! `GridCell` x position
        integer :: y                                                !! `GridCell` y position
        integer :: s                                                !! `SubRiver` reference
        integer :: r                                                !! `RiverReach` reference
        real(dp) :: slope                                           !! Slope of reach [m/m]
        real(dp) :: Qin                                             !! Inflow from upstream reach [m3/timestep]
        real(dp) :: Qout                                            !! Outflow to the next reach [m3/timestep]
        real(dp) :: Qrunoff                                         !! Runoff from hydrological model [m3/s]
        real(dp), allocatable :: Q_runoff_timeSeries(:)             !! Time series runoff data from file [m3/s]
        real(dp), allocatable :: spmIn(:)                           !! Inflow SPM from upstream reach [kg/timestep]
        real(dp), allocatable :: spmOut(:)                          !! Outflow SPM to next reach [kg/timestep]
        real(dp), allocatable :: m_spm(:)                           !! Mass of the SPM currently in reach [kg]
        real(dp), allocatable :: spmDep(:)                          !! SPM deposited on current time step [kg/timestep]
        real(dp), allocatable :: j_spm_runoff(:)                    !! Eroded soil runoff for current time step [kg/timestep]
        real(dp) :: W                                               !! Width of reach [m]
        real(dp) :: D                                               !! Depth of water column [m]
        real(dp) :: v                                               !! Water velocity [m/s]
        real(dp) :: l                                               !! Length of the river, without meandering factor [m]
        real(dp) :: f_m = 1                                         !! Meandering factor used for calculating river volume. Default to 1 (no meandering).
            ! TODO: Move the default to config file
        real(dp) :: xsArea                                          !! The cross-sectional area of water in the reach [m2]
        real(dp) :: bedArea                                         !! The bed sediment area in the reach [m2]
        real(dp) :: volume                                          !! The volume of water in the reach [m3]
        real(dp), allocatable :: C_spm(:)                           !! Sediment concentration [kg/m3]
        real(dp), allocatable :: j_spm_res(:)                       !! Resuspension flux on a given timestep [kg/s]
        real(dp), allocatable :: k_settle(:)                        !! Sediment settling rate on a given timestep [s-1]
        real(dp) :: alpha_res                                       !! Maximum resuspendable particle size calibration param [-]
        real(dp) :: beta_res                                        !! Resuspension calibration factor [s2 kg-1]
        real(dp) :: n                                               !! Manning's roughness coefficient [-]
        class(BedSediment), allocatable :: bedSediment              !! Contained BedSediment object
        class(Reactor), allocatable :: reactor                      !! Contained Reactor object
        type(NcGroup) :: ncGroup                                    !! The NETCDF group for this RiverReach
      contains
       ! Create/destory
        procedure(createRiverReach), deferred :: create
        procedure(destroyRiverReach), deferred :: destroy
        ! Simulators
        procedure(updateRiverReach), deferred :: update
        procedure(resuspensionRiverReach), deferred :: resuspension
        procedure(settlingRiverReach), deferred :: settling
        procedure(depositToBedRiverReach), deferred :: depositToBed
        procedure(parseInputDataRiverReach), deferred :: parseInputData
        ! Calculators
        procedure(calculateDepth), deferred :: calculateDepth
        procedure(calculateWidth), deferred :: calculateWidth
        procedure(calculateVelocity), deferred :: calculateVelocity
        procedure(calculateSettlingVelocity), deferred :: calculateSettlingVelocity
        procedure(calculateResuspension), deferred :: calculateResuspension
        procedure(calculateVolume), deferred :: calculateVolume
        procedure(calculateArea), deferred :: calculateArea
        ! Getters
        procedure :: getVolume => getVolumeRiverReach
        procedure :: getQOut => getQOutRiverReach
        procedure :: getSpmOut => getSpmOutRiverReach
    end type

    abstract interface
        !> Create this `RiverReach`
        function createRiverReach(me, x, y, s, r, l, Q_runoff_timeSeries) result(res)
            use Globals
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! The `RiverReach` instance
            integer :: x, y, s, r                                       !! `GridCell`, `SubRiver` and `RiverReach` identifiers
            real(dp) :: l                                               !! The `RiverReach` length [m]
            real(dp), allocatable :: Q_runoff_timeSeries(:)             !! Any initial runoff [m3/s]
            type(Result) :: res                                         !! The Result object
        end function

        !> Destroy this `RiverReach`
        function destroyRiverReach(me) result(r)
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! The `RiverReach` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        !> Update this `RiverReach` on given time step
        function updateRiverReach(me, Qin, spmIn, t, j_spm_runoff) result(r)
            use Globals
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! This `RiverReach` instance
            real(dp) :: Qin                                             !! Inflow to this reach [m3/timestep]
            integer :: t                                                !! What time step are we on?
            real(dp) :: j_spm_runoff(:)		                            !! Eroded sediment runoff to this reach
            real(dp) :: spmIn(C%nSizeClassesSpm)                        !! Inflow SPM to this reach [kg/timestep]
            type(Result) :: r                                           !! The `Result` object
        end function

        !> Resuspend sediment based on current river flow
        function resuspensionRiverReach(me) result(r)
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! This `RiverReach` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        !> Set settling rate \( k_{\text{settle}} \) for this time step
        function settlingRiverReach(me) result(r)
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! This `RiverReach` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        function depositToBedRiverReach(me, spmDep) result(r)
            use Globals
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me                                     !! This `RiverReach` instance
            real(dp) :: spmDep(C%nSizeClassesSpm)                       !! The SPM to deposit
            type(Result) :: r                                           !! The `Result` object to return any errors in
        end function
        
        !> Obtain input data from the data file and store in object properties
        function parseInputDataRiverReach(me) result(r)
            use ResultModule, only: Result
            import RiverReach
            class(RiverReach) :: me             !! This `RiverReach1` instance
            type(Result) :: r                   !! The `Result` object to return, with any errors
        end function

        !> Calculate the depth of this `RiverReach`
        pure function calculateDepth(me, W, S, Q) result(r)
            use Globals
            use ResultModule, only: Result0D
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The `RiverReach` instance
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp), intent(in) :: S                                   !! River slope [-]
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            type(Result0D) :: r                                         !! The `Result` object
        end function

        !> Calculate the width of this `RiverReach`
        function calculateWidth(me, Q) result(W)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The `RiverReach` instance
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            real(dp) :: W                                               !! Calculated width [m]
        end function

        !> Calculate the volume of this `RiverReach`
        pure function calculateVolume(me, D, W, l, f_m) result(volume)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp), intent(in) :: l                                   !! River length, without meandering [m]
            real(dp), intent(in) :: f_m                                 !! Meandering factor [-]
            real(dp) :: volume                                          !! Calculated volume [m3]
        end function

        !> Calculate the cross-sectional area of this `RiverReach`
        pure function calculateArea(me, D, W) result(area)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The `RiverReach` instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp) :: area                                            !! Calculated area [m2]
        end function

        !> Calculate the velocity of water from the dimensions and flow
        pure function calculateVelocity(me, D, Q, W) result(v)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The `RiverReach` instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp) :: v                                               !! The calculated velocity [m/s]
        end function

        !> Calculate the settling velocity of sediment particles for an individual
        !! size class
        function calculateSettlingVelocity(Me, d, rho_spm, T) result(W_spm)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The `RiverReach` instance
            real(dp), intent(in) :: d                                   !! Sediment particle diameter [m]
            real(dp), intent(in) :: rho_spm                             !! Sediment particulate density [kg/m3]
            real(dp), intent(in) :: T                                   !! Temperature [C]
            real(dp) :: W_spm                                           !! Calculated settling velocity [m/s]
        end function

        !> Calculate the resuspension flux of sediment particles
        pure function calculateResuspension(me, beta, L, W, m_bed, M_prop, omega, f_fr) result(j_res)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me             !! This `RiverReach` instance
            real(dp), intent(in) :: beta                    !! Calibration parameter \( \beta \) [s2 kg-1]
            real(dp), intent(in) :: L                       !! Reach length \( L = lf_{\text{m}} \) [m]
            real(dp), intent(in) :: W                       !! Reach width \( W \) [m]
            real(dp), intent(in) :: m_bed                   !! `BedSediment` mass per unit area \( m_{\text{bed}} \) [kg m-2]
            real(dp), intent(in) :: M_prop(C%nTimeSteps)    !! Proportion of this size class that is resuspenable \( M_{\text{prop}} \) [-]
            real(dp), intent(in) :: omega                   !! Stream power per unit bed area \( \omega \) [kg m-2]
            real(dp), intent(in) :: f_fr                    !! Friction factor \( f \) [-]
            real(dp) :: j_res(C%nTimeSteps)                 !! Calculated resuspension flux \( j_{\text{res}} \) [kg/s]
        end function
    end interface

  contains

    !> Return the volume of the `RiverReach`.
    function getVolumeRiverReach(me) result(volume)
        class(RiverReach) :: me
        real(dp) :: volume
        volume = me%volume
    end function

    !> Return the outflow.
    function getQOutRiverReach(me) result(Qout)
        class(RiverReach) :: me
        real(dp) :: Qout
        Qout = me%Qout
    end function

    !> Return the SPM discahrge.
    function getSpmOutRiverReach(me) result(spmOut)
        class(RiverReach) :: me
        real(dp) :: spmOut(size(me%spmOut))
        spmOut = me%spmOut
    end function
end module
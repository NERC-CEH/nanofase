module spcRiverReach
                                                                    ! superclass for RiverReach subclasses
                                                                    ! defines properties and methods required in any implmentation
                                                                    ! of a RiverReach class
                                                                    ! a RiverReach class computes water velocity, depth and sediment dynamics for
                                                                    ! a defined length of (homogeneous) flowing water
                                                                    ! IMPORTED MODULES
                                                                    ! Description
                                                                    ! -----------
    use Globals                                                     ! global declarations
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    ! use spcBedSediment
    implicit none                                                   ! force declaration of all variables

    type, abstract, public :: RiverReach                            ! type declaration for superclass
        character(len=256) :: ref                                   ! Reference for this object, of the form RiverReach_x_y_s_r
                                                                    ! PROPERTIES
                                                                    ! Description
                                                                    ! -----------
        real(dp) :: S                                               !! Slope of reach [m/m]
        real(dp) :: Qin                                             !! Inflow from upstream reach [m3/timestep]
        real(dp) :: Qout                                            !! Outflow to the next reach [m3/timestep]
        real(dp) :: Qrunoff                                         !! Runoff from hydrological model [m3/s]
        real(dp), allocatable :: QrunoffTimeSeries(:)               !! Time series runoff data from file [m3/s]
        real(dp), allocatable :: spmIn(:)                           !! Inflow SPM from upstream reach [kg/timestep]
        real(dp), allocatable :: spmOut(:)                          !! Outflow SPM to next reach [kg/timestep]
        real(dp), allocatable :: m_spm(:)                           !! Mass of the SPM currently in reach [kg]
        real(dp), allocatable :: m_spmTimeSeries(:,:)               !! Time series of SPM inputs [kg/s]
        real(dp) :: W                                               !! Width of reach [m]
        real(dp) :: D                                               !! Depth of water column [m]
        real(dp) :: v                                               !! Water velocity [m/s]
        real(dp) :: l                                               !! Length of the river, without meandering factor [m]
        real(dp) :: f_m = 1                                         !! Meandering factor used for calculating river volume. Default to 1 (no meandering).
        real(dp) :: xsArea                                          !! The cross-sectional area of water in the reach [m2]
        real(dp) :: bsArea                                          !! The bed sediment area in the reach [m2]
        real(dp) :: volume                                          !! The volume of water in the reach [m3]
        real(dp), allocatable :: rho_spm(:)                         !! Sediment particle densities [kg/m3]
        real(dp), allocatable :: j_spm_res(:)                       !! Resuspension flux on a given timestep [kg/s]
        real(dp) :: alpha_res                                       !! Maximum resuspendable particle size calibration param [-]
        real(dp) :: beta_res                                        !! Resuspension calibration factor [s2 kg-1]
        real(dp) :: n                                               !! Manning's roughness coefficient [-]
        ! class(BedSediment), allocatable :: bedSediment              !! Contained BedSediment object
        type(NcGroup) :: ncGroup                                    !! The NETCDF group for this RiverReach
      contains
                                                                    ! METHODS
                                                                    ! Description
                                                                    ! -----------
        procedure(createRiverReach), deferred :: create             ! create the RiverReach object. Exposed name: create
        procedure(destroyRiverReach), deferred :: destroy           ! remove the RiverReach object and all contained objects. Exposed name: destroy
        procedure(updateRiverReach), deferred :: update             ! Run the RiverReach simulation for one timestep
        procedure(resuspensionRiverReach), deferred :: resuspension ! Run resuspension algorithm for one timestep
                                                                                ! PRIVATE routines
        procedure(calculateDepth), private, deferred :: calculateDepth           ! compute the depth of the water column
        procedure(calculateWidth), private, deferred :: calculateWidth           ! compute the width of the reach
        procedure(calculateVelocity), private, deferred :: calculateVelocity     ! compute the water velocity
        procedure(calculateSettlingVelocity), private, deferred :: calculateSettlingVelocity ! compute the sediment settling velocities
        procedure(calculateResuspension), private, deferred :: calculateResuspension
        procedure(calculateVolume), private, deferred :: calculateVolume         ! Calculate the volume of the reach
        procedure(calculateArea), private, deferred :: calculateArea             ! Calculate the area of the reach's cross-section
                                                                    ! GETTERS
        procedure :: getVolume => getVolumeRiverReach               ! Should getters all be non-abstract, seeing as all they're
        procedure :: getQOut => getQOutRiverReach                   ! doing is returning a type variable?
        procedure :: getSpmOut => getSpmOutRiverReach               ! Return the SPM discharge
    end type

    abstract interface
        function createRiverReach(me, x, y, s, r, l, QrunoffTimeSeries) result(res)
            use Globals
            import RiverReach, Result
            class(RiverReach) :: me                                     !! The RiverReach instance
            integer :: x, y, s, r                                       !! GridCell, SubRiver and RiverReach identifiers
            real(dp) :: l                                               !! The RiverReach length [m]
            real(dp), allocatable :: QrunoffTimeSeries(:)               !! Any initial runoff [m3/s]
            type(Result) :: res                                         !! The Result object
        end function
        function destroyRiverReach(me) result(r)
            import RiverReach, Result
            class(RiverReach) :: me                                     !! The RiverReach instance
            type(Result) :: r                                           !! The Result object to return
        end function
        function updateRiverReach(me, Qin, spmIn, t) result(r)
            use Globals
            import RiverReach, Result
            class(RiverReach) :: me                                     !! This RiverReach instance
            real(dp) :: Qin                                             !! Inflow to this reach [m3/timestep]
            integer :: t                                                !! What time step are we on?
            real(dp) :: spmIn(C%nSizeClassesSpm)                        !! Inflow SPM to this reach [kg/timestep]
            type(Result) :: r                                           !! The Result object
        end function
        function resuspensionRiverReach(me) result(r)
            import RiverReach, Result
            class(RiverReach) :: me                                     !! This RiverReach instance
            type(Result) :: r                                           !! The Result object to return
        end function
        pure function calculateDepth(me, W, S, Q) result(r)
            use Globals
            import RiverReach, Result0D
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp), intent(in) :: S                                   !! River slope [-]
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            type(Result0D) :: r                                         !! The result object
        end function
        function calculateWidth(me, Q) result(W)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            real(dp) :: W                                               !! Calculated width [m]
        end function
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
        pure function calculateArea(me, D, W) result(area)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp) :: area                                            !! Calculated area [m2]
        end function
        pure function calculateVelocity(me, D, Q, W) result(v)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
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
            class(RiverReach), intent(in) :: me                         !! The RiverReach instance
            real(dp), intent(in) :: d                                   !! Sediment particle diameter [m]
            real(dp), intent(in) :: rho_spm                             !! Sediment particle density [kg/m3]
            real(dp), intent(in) :: T                                   !! Temperature [C]
            real(dp) :: W_spm                                           !! Calculated settling velocity [m/s]
        end function
        !> Calculate the resuspension flux of sediment particles
        pure function calculateResuspension(me, beta, L, W, m_bed, M_prop, omega, f_fr) result(j_res)
            use Globals
            import RiverReach
            class(RiverReach), intent(in) :: me             !! This RiverReach instance
            real(dp), intent(in) :: beta                    !! Calibration parameter \( \beta \) [s2 kg-1]
            real(dp), intent(in) :: L                       !! Reach length \( L = lf_{\text{m}} \) [m]
            real(dp), intent(in) :: W                       !! Reach width \( W \) [m]
            real(dp), intent(in) :: m_bed                   !! BedSediment mass per unit area \( m_{\text{bed}} \) [kg m-2]
            real(dp), intent(in) :: M_prop(C%nTimeSteps)    !! Proportion of this size class that is resuspenable \( M_{\text{prop}} \) [-]
            real(dp), intent(in) :: omega                   !! Stream power per unit bed area \( \omega \) [kg m-2]
            real(dp), intent(in) :: f_fr                    !! Friction factor \( f \) [-]
            real(dp) :: j_res(C%nTimeSteps)                 !! Calculated resuspension flux \( j_{\text{res}} \) [kg/s]
        end function
    end interface

  contains

    !> Return the volume of the RiverReach.
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
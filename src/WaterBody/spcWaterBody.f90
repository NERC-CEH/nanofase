!> Module containing definition of base class `WaterBody`.
module spcWaterBody
    use Globals
    use ResultModule, only: Result, Result0D
    use ErrorInstanceModule
    use classPointSource
    use classDiffuseSource
    use spcBedSediment
    use spcReactor
    implicit none
    
    !> `WaterBodyPointer` used for `WaterBody` inflows array, so the elements within can
    !! point to other `GridCell`'s colWaterBody elements
    type WaterBodyPointer
        class(WaterBody), pointer :: item => null()                  !! Pointer to polymorphic `WaterBody` object
    end type
    
     !> An internal user-defined type, defining a reference to a `WaterBody` sending water to this
    !! `WaterBody`. Comprises row (x) and column (y) references to the `GridCell` containing the
    !! sending `RiveReach` and the in-cell `WaterBody` reference number
    type WaterBodyRef                                                 
        integer :: x                                                !! `GridCell` x reference
        integer :: y                                                !! `GridCell` y reference
        integer :: rr                                               !! `WaterBody` reference
    end type

    !> Abstract base class for `WaterBody`. Defines properties and procedures
    !! required in any implementation of this class.
    type, abstract, public :: WaterBody
        character(len=100) :: ref                                   !! Reference for this object, of the form WaterBody_x_y_s_r
        integer :: x                                                !! `GridCell` x position
        integer :: y                                                !! `GridCell` y position
        real(dp) :: gridCellArea                                    !! `GridCell` area
        integer :: rr                                               !! `WaterBody` reference
        type(WaterBodyRef), allocatable :: inflowRefs(:)           !! References to inflow `WaterBody`es
        type(WaterBodyPointer), allocatable :: inflows(:)          !! Array of pointers to inflow `WaterBody`es
        type(WaterBodyPointer) :: outflow                          !! Pointer to the outflow from this `WaterBody`
        integer, allocatable :: domainOutflow(:)                                 
            !! If this `WaterBody` flows out of the gridded domain, this array is used to specify where to,
            !! for reach length calculations
        integer :: nInflows                                         !! Integer to store the number of inflows to this reach in
        real(dp) :: slope                                           !! Slope of reach [m/m]
        real(dp) :: Q_in                                            !! Inflow from upstream reach [m3/timestep]
        real(dp) :: Q_out                                           !! Outflow to the next reach [m3/timestep]
        real(dp) :: tmp_Q_out                                       !! Temporary outflow storage until timestep loop complete [m3/timestep]
        real(dp) :: q_runoff                                        !! Runoff from hydrological model [m/timestep]
        real(dp), allocatable :: q_runoff_timeSeries(:)             !! Time series runoff data from file [m/timestep]
        real(dp), allocatable :: j_spm_in(:)                        !! Inflow SPM from upstream reach [kg/timestep]
        real(dp), allocatable :: j_np_in(:,:,:)                     !! Inflow SPM from upstream reach [kg/timestep]
        real(dp), allocatable :: j_spm_out(:)                       !! Outflow SPM to next reach [kg/timestep]
        real(dp), allocatable :: j_np_out(:,:,:)                    !! Outflow SPM to next reach [kg/timestep]
        real(dp), allocatable :: tmp_j_spm_out(:)                   !! Temporary outflow storage until timestep loop complete [kg/timestep]
        real(dp), allocatable :: tmp_j_np_out(:,:,:)                !! Temporary outflow storage until timestep loop complete [kg/timestep]
        real(dp), allocatable :: m_spm(:)                           !! Mass of the SPM currently in reach [kg]
        real(dp), allocatable :: m_np(:,:,:)                        !! Mass of NPs currently in reach [kg]
        real(dp), allocatable :: spmDep(:)                          !! SPM deposited on current time step [kg/timestep]
        real(dp), allocatable :: j_np_dep(:,:,:)                    !! NP deposited on current time step [kg/timestep]
        real(dp), allocatable :: j_spm_runoff(:)                    !! Eroded soil runoff for current time step [kg/timestep]
        real(dp), allocatable :: j_np_runoff(:,:,:)                 !! Eroded soil runoff for current time step [kg/timestep]
        real(dp) :: W                                               !! Width of reach [m]
        real(dp) :: D                                               !! Depth of water column [m]
        real(dp) :: v                                               !! Water velocity [m/s]
        real(dp) :: l                                               !! Length of the river, without meandering factor [m]
        real(dp) :: f_m                                             !! Meandering factor used for calculating river volume. Default to 1 (no meandering).
        real(dp) :: xsArea                                          !! The cross-sectional area of water in the reach [m2]
        real(dp) :: bedArea                                         !! The bed sediment area in the reach [m2]
        real(dp) :: volume                                          !! The volume of water in the reach [m3]
        real(dp), allocatable :: C_spm(:)                           !! Sediment concentration [kg/m3]
        real(dp), allocatable :: C_np(:,:,:)                        !! NP mass concentration [kg/m3]
        real(dp), allocatable :: j_spm_res(:)                       !! Resuspension flux for a given timestep [kg/s]
        real(dp), allocatable :: k_settle(:)                        !! Sediment settling rate on a given timestep [s-1]
        real(dp), allocatable :: W_settle_spm(:)                    !! SPM settling velocity [m/s]
        real(dp), allocatable :: W_settle_np(:)                     !! NP settling velocity [m/s]
        real(dp) :: alpha_res                                       !! Maximum resuspendable particle size calibration param [-]
        real(dp) :: beta_res                                        !! Resuspension calibration factor [s2 kg-1]
        real(dp) :: n                                               !! Manning's roughness coefficient [-]
        real(dp), allocatable :: T_water_timeSeries(:)              !! Water temperature time series [C]
        real(dp) :: T_water                                         !! Water temperature on a given time step [C]
        real(dp) :: alpha_hetero                                    !! Heteroaggregation attachment efficiency, 0-1 [-]
        logical :: isHeadwater = .false.                            !! Is this `WaterBody` a headwater (no inflows)?
        logical :: isGridCellInflow = .false.                       !! Is this `WaterBody` the inflow the `GridCell` its in
        logical :: isGridCellOutflow = .false.                      !! Does the `WaterBody` outflow to another cell?
        logical :: isDomainOutflow = .false.                        !! Does the `WaterBody` flow out of the gridded domain?
        integer :: branch = 0                                       !! Which branch is this `WaterBody` on in the GridCell? 0 = not processed yet
            !! TODO: Try and remove these; WaterBody shouldn't really know anything about
            !! the GridCell - not very encapsulated
        class(BedSediment), allocatable :: bedSediment              !! Contained `BedSediment` object
        class(Reactor), allocatable :: reactor                      !! Contained `Reactor` object
        type(PointSource), allocatable :: pointSources(:)           !! Contained `PointSource` objects (non-polymorphic)
        logical :: hasPointSource = .false.                         !! Does this `WaterBody` have a `PointSource`?
        type(DiffuseSource), allocatable :: diffuseSources(:)       !! Contained `DiffuseSource` objects (non-polymorphic)
        logical :: hasDiffuseSource = .false.                       !! Does this `WaterBody` have a `DiffuseSource`?
        type(NcGroup) :: ncGroup                                    !! The NetCDF group for this `WaterBody`
      contains
        ! Create/destory
        procedure(createWaterBody), deferred :: create
        procedure(destroyWaterBody), deferred :: destroy
        ! Simulators
        procedure(updateWaterBody), deferred :: update
        procedure(finaliseUpdateWaterBody), deferred :: finaliseUpdate
        procedure(resuspensionWaterBody), deferred :: resuspension
        procedure(settlingWaterBody), deferred :: settling
        procedure(depositToBedWaterBody), deferred :: depositToBed
        ! Data handlers
        procedure(setDefaultsWaterBody), deferred :: setDefaults
        procedure(parseInputDataWaterBody), deferred :: parseInputData
        ! Calculators
        procedure(calculateDepth), deferred :: calculateDepth
        procedure(calculateWidth), deferred :: calculateWidth
        procedure(calculateVelocity), deferred :: calculateVelocity
        procedure(calculateSettlingVelocity), deferred :: calculateSettlingVelocity
        procedure(calculateResuspension), deferred :: calculateResuspension
        procedure(calculateVolume), deferred :: calculateVolume
        procedure(calculateArea), deferred :: calculateArea
        ! Getters
        procedure :: getVolume => getVolumeWaterBody
        procedure :: getQOut => getQOutWaterBody
        procedure :: get_j_spm_out => get_j_spm_outWaterBody
    end type
      
    !> Container type for `class(WaterBody)`, the actual type of the `WaterBody` class.
    !! a variable of type `WaterBodyElement` can be of any object type inheriting from the
    !! `WaterBody` abstract base class.
    type WaterBodyElement                                          
        class(WaterBody), allocatable :: item                      !! Polymorphic `WaterBody` object
    end type

    abstract interface
        !> Create this `WaterBody`
        function createWaterBody(me, x, y, rr, q_runoff_timeSeries, T_water_timeSeries, gridCellArea) result(r)
            use Globals
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! The `WaterBody` instance
            integer :: x, y, rr                                         !! `GridCell` and `WaterBody` identifiers
            real(dp) :: q_runoff_timeSeries(:)                          !! The runoff = quickflow + slowflow [m/timestep]
            real(dp) :: T_water_timeSeries(:)                           !! Water temperature [C]
            real(dp) :: gridCellArea                                    !! Area of the containing `GridCell`
            type(Result) :: r                                           !! The Result object
        end function

        !> Destroy this `WaterBody`
        function destroyWaterBody(me) result(r)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! The `WaterBody` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        !> Update this `WaterBody` on given time step
        function updateWaterBody(me, t, j_spm_runoff, j_np_runoff) result(r)
            use Globals
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! This `WaterBody` instance
            integer :: t                                                !! What time step are we on?
            real(dp) :: j_spm_runoff(:)                                 !! Eroded sediment runoff to this reach [kg/timestep]
            real(dp), optional :: j_np_runoff(:,:,:)                              !! Eroded NP runoff to this reach [kg/timestep]
            type(Result) :: r                                           !! The `Result` object
        end function

        !> Set temporary outflow variable to real outflow variables
        function finaliseUpdateWaterBody(me) result(r)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me
            type(Result) :: r
        end function

        !> Resuspend sediment based on current river flow
        function resuspensionWaterBody(me) result(r)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! This `WaterBody` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        !> Set settling rate \( k_{\text{settle}} \) for this time step
        function settlingWaterBody(me) result(r)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! This `WaterBody` instance
            type(Result) :: r                                           !! The `Result` object to return
        end function

        function depositToBedWaterBody(me, spmDep) result(r)
            use Globals
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                     !! This `WaterBody` instance
            real(dp) :: spmDep(C%nSizeClassesSpm)                       !! The SPM to deposit
            type(Result) :: r                                           !! The `Result` object to return any errors in
        end function
        
        subroutine setDefaultsWaterBody(me)
            import WaterBody
            class(WaterBody) :: me                                     !! This `WaterBody` instance
        end subroutine
        
        !> Obtain input data from the data file and store in object properties
        function parseInputDataWaterBody(me) result(r)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me             !! This `WaterBody1` instance
            type(Result) :: r                   !! The `Result` object to return, with any errors
        end function

        !> Calculate the depth of this `WaterBody`
        function calculateDepth(me, W, S, Q) result(r)
            use Globals
            use ResultModule, only: Result0D
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The `WaterBody` instance
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp), intent(in) :: S                                   !! River slope [-]
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            type(Result0D) :: r                                         !! The `Result` object
        end function

        !> Calculate the width of this `WaterBody`
        function calculateWidth(me, Q) result(W)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The `WaterBody` instance
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            real(dp) :: W                                               !! Calculated width [m]
        end function

        !> Calculate the volume of this `WaterBody`
        function calculateVolume(me, D, W, l, f_m) result(volume)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The WaterBody instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp), intent(in) :: l                                   !! River length, without meandering [m]
            real(dp), intent(in) :: f_m                                 !! Meandering factor [-]
            real(dp) :: volume                                          !! Calculated volume [m3]
        end function

        !> Calculate the cross-sectional area of this `WaterBody`
        function calculateArea(me, D, W) result(area)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The `WaterBody` instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp) :: area                                            !! Calculated area [m2]
        end function

        !> Calculate the velocity of water from the dimensions and flow
        function calculateVelocity(me, D, Q, W) result(v)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The `WaterBody` instance
            real(dp), intent(in) :: D                                   !! River depth [m]
            real(dp), intent(in) :: Q                                   !! Flow rate [m3/s]
            real(dp), intent(in) :: W                                   !! River width [m]
            real(dp) :: v                                               !! The calculated velocity [m/s]
        end function

        !> Calculate the settling velocity of sediment particles for an individual
        !! size class
        function calculateSettlingVelocity(Me, d, rho_particle, T) result(W_spm)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me                         !! The `WaterBody` instance
            real(dp), intent(in) :: d                                   !! Sediment particle diameter [m]
            real(dp), intent(in) :: rho_particle                        !! Sediment particulate density [kg/m3]
            real(dp), intent(in) :: T                                   !! Temperature [C]
            real(dp) :: W_spm                                           !! Calculated settling velocity [m/s]
        end function

        !> Calculate the resuspension flux of sediment particles
        function calculateResuspension(me, beta, L, W, m_bed, M_prop, omega, f_fr) result(j_res)
            use Globals
            import WaterBody
            class(WaterBody), intent(in) :: me             !! This `WaterBody` instance
            real(dp), intent(in) :: beta                    !! Calibration parameter \( \beta \) [s2 kg-1]
            real(dp), intent(in) :: L                       !! Reach length \( L = lf_{\text{m}} \) [m]
            real(dp), intent(in) :: W                       !! Reach width \( W \) [m]
            real(dp), intent(in) :: m_bed                   !! `BedSediment` mass per unit area \( m_{\text{bed}} \) [kg m-2]
            real(dp), intent(in) :: M_prop(C%nTimeSteps)    !! Proportion of this size class that is resuspenable \( M_{\text{prop}} \) [-]
            real(dp), intent(in) :: omega                   !! Stream power per unit bed area \( \omega \) [kg m-2]
            real(dp), intent(in) :: f_fr                    !! Friction factor \( f \) [-]
            real(dp) :: j_res(C%nSizeClassesSpm)            !! Calculated resuspension flux \( j_{\text{res}} \) [kg/s]
        end function
    end interface

  contains

    !> Return the volume of the `WaterBody`.
    function getVolumeWaterBody(me) result(volume)
        class(WaterBody) :: me
        real(dp) :: volume
        volume = me%volume
    end function

    !> Return the outflow.
    function getQOutWaterBody(me) result(Q_out)
        class(WaterBody) :: me
        real(dp) :: Q_out
        Q_out = me%Q_out
    end function

    !> Return the SPM discahrge.
    function get_j_spm_outWaterBody(me) result(j_spm_out)
        class(WaterBody) :: me
        real(dp) :: j_spm_out(size(me%j_spm_out))
        j_spm_out = me%j_spm_out
    end function
end module
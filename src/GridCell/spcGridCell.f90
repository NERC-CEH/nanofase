!> Module container for abstract base class `GridCell`
module spcGridCell
    use Globals
    use mo_netcdf
    use ResultModule
    use ErrorInstanceModule
    use spcRiverReach
    use spcSoilProfile
    use spcEstuaryReach
    use classDiffuseSource
    use classPointSource
    implicit none

    ! TODO: Move these to Source classes
    type PointSourceElement                                             ! container type for class(PointSource), the actual type of the PointSource class
        class(PointSource), allocatable :: item                         ! a variable of type PointSource can be of any object type inheriting from the
    end type                                                            ! PointSource superclass
    type DiffuseSourceElement                                           ! container type for class(DiffuseSource), the actual type of the DiffuseSource class
        class(DiffuseSource), allocatable :: item                       ! a variable of type DiffuseSource can be of any object type inheriting from the
    end type                                                            ! DiffuseSource superclass

    !> Abstract base class `GridCell`. Extended classes are responsible
    !! for running creation and simulation procedures for `SoilProfile`
    !! and `RiverReach`es.
    type, abstract, public :: GridCell
        character(len=256) :: ref                                       !! A name for the object
        type(NcGroup) :: ncGroup                                        !! The NetCDF group for this dataset
        integer :: x                                                    !! `GridCell` x reference
        integer :: y                                                    !! `GridCell` y reference
        integer :: dx                                                   !! Size of `GridCell` in x direction [m]
        integer :: dy                                                   !! Size of `GridCell` in y direction [m]
        real(dp) :: area                                                !! Area of the `GridCell`
        type(RiverReachElement), allocatable :: colRiverReaches(:)      !! Array of `RiverReachElement` objects to hold the RiverReaches
        type(RiverReachPointer), allocatable :: routedRiverReaches(:,:) !! `RiverReach`es ordered by branch and flow direction
            !! Array of `RiverReachPointer` objects to order rivers in routing order and by branch.
            !! 1st dimension: Branches. 2nd dimension: RiverReaches in that branch
        real(dp), allocatable :: branchLengths(:)                       !! Calculated (or specified) lengths of river branches in this `GridCell`
        type(EstuaryReachElement), allocatable :: colEstuaryReaches(:)  !! Array of `EstuaryReachElement` objects
        type(SoilProfileElement), allocatable :: colSoilProfiles(:)     !! Array of `SoilProfileElement` objects to hold the soil profiles
            ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
        type(PointSourceElement), allocatable :: colPointSources(:)     !! Array of `PointSourceElement` objects to hold the point sources
        type(DiffuseSourceElement) :: objDiffuseSource                  !! `DiffuseSourceElement` object to hold the diffuse source
        integer :: nRiverReaches = 0                                    !! Number of contained `SubRiver`s
        integer :: nSoilProfiles = 0                                    !! Number of contained `SoilProfile`s
        integer :: nPointSources = 0                                    !! Number of contained `PointSource`s
        integer :: nBranches = 0                                        !! Number of river branches in the `GridCell`
        integer, allocatable :: nReachesInBranch(:)
            !! Number of reaches in each branch. Needed as different branches might have different numbers of reaches
            !! (and Fortran matrices must be rectangular)
        logical :: DiffS                                                !! Yes=diffuse source present; NO=no diffuse source
        real(dp), allocatable :: q_runoff_timeSeries(:)                 !! Runoff = slow flow + quick flow, from the hydrological model [m/timestep]
        real(dp), allocatable :: q_quickflow_timeSeries(:)              !! Quick flow from the hydrological model [m/timestep]
        real(dp), allocatable :: q_evap_timeSeries(:)                   !! Evaporation time series [m/s]
        real(dp), allocatable :: q_precip_timeSeries(:)                 !! Precipitation time series [m/s]
        real(dp) :: q_runoff                                            !! Runoff from the hydrological model for this time step [m/timestep]
        real(dp) :: Q_out                                               !! Discharge from this `GridCell` on a given timestep [m3/timestep]
        real(dp) :: tmp_Q_out                                           !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp) :: j_spm_out                                           !! SPM discharge from this GridCell on a given timestep [kg/timestep]
        real(dp) :: tmp_j_spm_out                                       !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp) :: slope                                               !! The slope of the `GridCell`
        real(dp) :: n_river                                             !! Manning's roughness coefficient for the river
        real(dp), allocatable :: erodedSediment(:)                      !! Sediment yield eroded on this timestep [kg/timestep], simulated by `SoilProfile`(s)
        logical :: isEmpty = .false.                                    !! Is there anything going on in the `GridCell` or should we skip over when simulating?
      
    contains
        ! Creation/destruction
        procedure(createGridCell), deferred :: create
        procedure(finaliseCreateGridCell), deferred :: finaliseCreate
        procedure(destroyGridCell), deferred :: destroy
        ! Simulation
        procedure(updateGridCell), deferred :: update
        procedure(finaliseUpdateGridCell), deferred :: finaliseUpdate
        ! Getters
        procedure(get_Q_outGridCell), deferred :: get_Q_out
        procedure(get_j_spm_outGridCell), deferred :: get_j_spm_out
        procedure(get_j_np_outGridCell), deferred :: get_j_np_out
        procedure(get_m_spmGridCell), deferred :: get_m_spm
    end type
      
    !> Container type for polymorphic `GridCell`s
    type GridCellElement                                               
        class(GridCell), allocatable :: item                           !! Polymorphic `GridCell` object
    end type

    abstract interface
        !> Create this `GridCell`
        function createGridCell(me, x, y, isEmpty) result(r)
            use ResultModule, only: Result
            import GridCell
            class(GridCell), target :: me                              !! The `GridCell` instance
            integer :: x, y                                            !! The (x,y) position of the `GridCell`
            logical, optional :: isEmpty                               !! Is anything to be simulated for this `GridCell`?
            type(Result) :: r                                          !! The `Result` object to return any errors in
        end function
        
        !> Finalise the creation of the `GridCell`, after river routing has been established
        function finaliseCreateGridCell(me) result(r)
            use ResultModule, only: Result    
            import GridCell
            class(GridCell) :: me                                   !! This `GridCell` instance
            type(Result) :: r                                       !! The `Result` object to return any errors in
        end function
        
        !> Destroy this `GridCell`
        function destroyGridCell(me) result(r)
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me                                      !! The `GridCell` instance
            type(Result) :: r                                          !! The `Result` object to return any errors in
        end function
        
        !> Run the `GridCell`'s simulation for this time step
        function updateGridCell(me, t) result(r)
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me                                      !! The `GridCell` instance
            integer :: t                                               !! The current time step
            type(Result) :: r                                          !! The `Result` object to return any errors in
        end function
        
        !> Finalise the `GridCell`'s state variables for this time step
        function finaliseUpdateGridCell(me) result(r)
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me                                      !! The `GridCell` instance
            type(Result) :: r                                          !! The `Result` object to return any errors in
        end function

        function get_Q_outGridCell(me, b) result(Q_out)
            use Globals, only: dp    
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me                                   !! The `GridCell` instance
            integer, optional :: b                                  !! The branch to get the outflow for
            real(dp) :: Q_out                                       !! Discharge to return
        end function

        function get_j_spm_outGridCell(me, b) result(j_spm_out)
            use Globals, only: dp, C
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me                                   !! The `GridCell` instance
            integer, optional :: b                                  !! The branch to get the outflow for
            real(dp) :: j_spm_out(C%nSizeClassesSpm)                !! SPM outflow to return
        end function

        function get_j_np_outGridCell(me, b) result(j_np_out)
            use Globals, only: dp, C
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me
            integer, optional :: b
            real(dp) :: j_np_out(C%nSizeClassesNP)
        end function

        function get_m_spmGridCell(me, b) result(m_spm)
            use Globals, only: dp, C
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me
            integer, optional :: b
            real(dp) :: m_spm(C%nSizeClassesSpm)
        end function

        function get_m_npGridCell(me, b) result(m_np)
            use Globals, only: dp, C
            use ResultModule, only: Result
            import GridCell
            class(GridCell) :: me
            integer, optional :: b
            real(dp) :: m_np(C%nSizeClassesNP)
        end function
    end interface
end module

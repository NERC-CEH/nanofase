!> Module container for abstract base class `GridCell`
module spcGridCell
    use Globals
    use mo_netcdf
    use ResultModule
    use ErrorInstanceModule
    ! use spcReach
    use ReachModule
    use spcSoilProfile
    use classDiffuseSource2
    use classCrop
    implicit none

    !> `GridCellPointer` used for to link `GridCell`s array, so the elements within can
    !! point to other `Reach`'s colReach elements
    type GridCellPointer
        class(GridCell), pointer :: item => null()                      !! Pointer to polymorphic `GridCell` object
    end type

    !> Abstract base class `GridCell`. Extended classes are responsible
    !! for running creation and simulation procedures for `SoilProfile`
    !! and `RiverReach`es.
    type, abstract, public :: GridCell
        character(len=256) :: ref                                       !! A name for the object
        type(NcGroup) :: ncGroup                                        !! The NetCDF group for this dataset
        integer :: x                                                    !! `GridCell` x reference
        integer :: y                                                    !! `GridCell` y reference
        real :: dx                                                      !! Size of `GridCell` in x direction [m]
        real :: dy                                                      !! Size of `GridCell` in y direction [m]
        real(dp) :: area                                                !! Area of the `GridCell`
        type(ReachElement), allocatable :: colRiverReaches(:)           !! Array of `RiverReachElement` objects to hold the RiverReaches
        character(len=3), allocatable :: reachTypes(:)                  !! Type of each indexed reach within this cell - 'riv' or 'est'
        type(ReachPointer), allocatable :: routedRiverReaches(:,:)      !! `RiverReach`es ordered by branch and flow direction
        type(GridCellPointer) :: outflow                                !! Where does the outflow reach from this GridCell go to?
            !! Array of `RiverReachPointer` objects to order rivers in routing order and by branch.
            !! 1st dimension: Branches. 2nd dimension: RiverReaches in that branch
        real(dp), allocatable :: branchLengths(:)                       !! Calculated (or specified) lengths of river branches in this `GridCell`
        type(SoilProfileElement), allocatable :: colSoilProfiles(:)     !! Array of `SoilProfileElement` objects to hold the soil profiles
            ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
        type(DiffuseSource2), allocatable :: diffuseSources(:)          !! Diffuse source object to provide, e.g., atmospheric deposition for this `GridCell`
        logical :: hasDiffuseSource = .false.                           !! Does this `GridCell` have a `DiffuseSource`?
        integer :: nReaches = 0                                         !! Number of `Reach`es in this cell
        integer :: nSoilProfiles = 0                                    !! Number of contained `SoilProfile`s
        integer :: nBranches = 0                                        !! Number of river branches in the `GridCell`
        integer, allocatable :: nReachesInBranch(:)
            !! Number of reaches in each branch. Needed as different branches might have different numbers of reaches
            !! (and Fortran matrices must be rectangular)
        real(dp), allocatable :: q_runoff_timeSeries(:)                 !! Runoff = slow flow + quick flow, from the hydrological model [m/timestep]
        real(dp), allocatable :: q_quickflow_timeSeries(:)              !! Quick flow from the hydrological model [m/timestep]
        real, allocatable :: q_evap_timeSeries(:)                       !! Evaporation time series [m/s]
        real, allocatable :: q_precip_timeSeries(:)                     !! Precipitation time series [m/s]
        real(dp) :: q_runoff                                            !! Runoff from the hydrological model for this time step [m/timestep]
        real(dp) :: Q_out                                               !! Discharge from this `GridCell` on a given timestep [m3/timestep]
        real(dp) :: tmp_Q_out                                           !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp) :: j_spm_out                                           !! SPM discharge from this GridCell on a given timestep [kg/timestep]
        real(dp) :: tmp_j_spm_out                                       !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp) :: n_river                                             !! Manning's roughness coefficient for the river
        real(dp), allocatable :: T_water_timeSeries(:)                  !! Water temperature [C]
        real(dp), allocatable :: erodedSediment(:)                      !! Sediment yield eroded on this timestep [kg/m2/day], simulated by `SoilProfile`(s)
        real(dp), allocatable :: distributionSediment(:)                !! Distribution used to split sediment yields across size classes
        real(dp), allocatable :: j_np_diffuseSource(:,:,:)              !! Input NPs from diffuse sources on this timestep [(kg/m2)/timestep]
        logical :: isEmpty = .false.                                    !! Is there anything going on in the `GridCell` or should we skip over when simulating?
        logical :: isHeadwater = .false.                                !! Is this `GridCell` a headwater?
        logical :: hasStreamJunctionInflow = .false.                    !! Is the inflow to this cell from more than one cell?
        logical :: isUpdated = .false.                                  !! Has this `GridCell` been updated for this timestep?
        ! Demands
        logical :: hasDemands = .false.                                 !! Does this `GridCell` have any water demand data?
        logical :: hasCrop = .false.                                    !! Does this `GridCell` have any crops?
        logical :: hasLargeCity = .false.                               !! Does this `GridCell` have a large city?
        real(dp) :: totalPopulation                                     !! Total population for the `GridCell` TODO what are the units?
        real(dp) :: urbanPopulation                                     !! Urban population for the `GridCell` TODO what are the units?
        real(dp) :: cattlePopulation = 0                                !! Cattle population for the `GridCell` TODO what are the units?
        real(dp) :: sheepGoatPopulation = 0                             !! Sheep/goat population for the `GridCell` TODO what are the units?
        real(dp) :: urbanDemandPerCapita = 0                            !! Urban demand per capita for the `GridCell` [l/day/capita]
        real(dp) :: ruralDemandPerCapita = 0                            !! Cattle population for the `GridCell` [1/day/capita]
        real(dp) :: industrialDemand = 0                                !! Industrial demand for `GridCell` [Mm3/day]
        real(dp) :: surfaceWaterToTotalWaterRatio                       !! Ratio of surface to total water demand (same for all demands) [-]
        type(Crop), allocatable :: crops(:)                             !! Crops present in this `GridCell`
      
    contains
        ! Creation/destruction
        procedure(createGridCell), deferred :: create
        procedure(finaliseCreateGridCell), deferred :: finaliseCreate
        procedure(snapPointSourcesToReachGridCell), deferred :: snapPointSourcesToReach
        ! Simulation
        procedure(updateGridCell), deferred :: update
        procedure(finaliseUpdateGridCell), deferred :: finaliseUpdate
        procedure(parseNewBatchDataGridCell), deferred :: parseNewBatchData
        ! Getters
        procedure(get_Q_outGridCell), deferred :: get_Q_out
        procedure(get_j_spm_outGridCell), deferred :: get_j_spm_out
        procedure(get_j_np_outGridCell), deferred :: get_j_np_out
        procedure(get_m_spmGridCell), deferred :: get_m_spm
        procedure(getTotalReachLengthGridCell), deferred :: getTotalReachLength
        procedure(get_C_np_soilGridCell), deferred :: get_C_np_soil
        procedure(get_C_np_waterGridCell), deferred :: get_C_np_water
        procedure(get_C_np_sedimentGridCell), deferred :: get_C_np_sediment
        procedure(getWaterVolumeGridCell), deferred :: getWaterVolume
        procedure(getBedSedimentAreaGridCell), deferred :: getBedSedimentArea
        procedure(getBedSedimentMassGridCell), deferred :: getBedSedimentMass
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
        subroutine finaliseCreateGridCell(me)
            import GridCell
            class(GridCell) :: me                                   !! This `GridCell` instance
        end subroutine
        
        !> Run the `GridCell`'s simulation for this time step
        subroutine updateGridCell(me, t)
            import GridCell
            class(GridCell) :: me                                      !! The `GridCell` instance
            integer :: t                                               !! The current time step
        end subroutine
        
        !> Finalise the `GridCell`'s state variables for this time step
        subroutine finaliseUpdateGridCell(me)
            import GridCell
            class(GridCell) :: me                                      !! The `GridCell` instance
        end subroutine

        subroutine snapPointSourcesToReachGridCell(me)
            import GridCell
            class(GridCell) :: me
        end subroutine

        subroutine parseNewBatchDataGridCell(me)
            import GridCell
            class(GridCell) :: me
        end subroutine

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
            real(dp) :: j_np_out(C%nSizeClassesNM)
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
            real(dp) :: m_np(C%nSizeClassesNM)
        end function

        function getTotalReachLengthGridCell(me) result(totalReachLength)
            use Globals, only: dp
            import GridCell
            class(GridCell) :: me
            real(dp) :: totalReachLength
        end function

        function get_C_np_soilGridCell(me) result(C_np_soil)
            use Globals, only: dp, C
            import GridCell
            class(GridCell) :: me
            real(dp), allocatable :: C_np_soil(:,:,:)
        end function

        function get_C_np_waterGridCell(me) result(C_np_water)
            use Globals, only: dp, C
            import GridCell
            class(GridCell) :: me
            real(dp), allocatable :: C_np_water(:,:,:)
        end function

        function get_C_np_sedimentGridCell(me) result(C_np_sediment)
            use Globals, only: dp, C
            import GridCell
            class(GridCell) :: me
            real(dp), allocatable :: C_np_sediment(:,:,:)
        end function

        function getWaterVolumeGridCell(me) result(waterVolume)
            use Globals, only: dp
            import GridCell
            class(GridCell) :: me
            real(dp) :: waterVolume
        end function

        function getBedSedimentAreaGridCell(me) result(bedArea)
            use Globals, only: dp
            import GridCell
            class(GridCell) :: me
            real(dp) :: bedArea
        end function

        function getBedSedimentMassGridCell(me) result(sedimentMass)
            use Globals, only: dp
            import GridCell
            class(GridCell) :: me
            real(dp) :: sedimentMass
        end function

    end interface
end module

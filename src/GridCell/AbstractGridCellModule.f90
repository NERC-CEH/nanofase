!> Module container for abstract base class AbstractGridCell
module AbstractGridCellModule
    use Globals
    use mo_netcdf
    use ResultModule
    use ReachModule
    use AbstractSoilProfileModule
    use DiffuseSourceModule
    use CropModule
    implicit none

    !> GridCellPointer used to link GridCells array, so the elements within can
    !! point to other Reach's colReach elements
    type GridCellPointer
        class(AbstractGridCell), pointer :: item => null()                      !! Pointer to polymorphic GridCell object
    end type

    !> Abstract base class AbstractGridCell define the interface for grid cells
    !! and their properties
    type, abstract, public :: AbstractGridCell
        character(len=256)              :: ref                                  !! A name for the object
        type(NcGroup)                   :: ncGroup                              !! The NetCDF group for this dataset
        integer                         :: x                                    !! GridCell x index
        integer                         :: y                                    !! GridCell y index
        real                            :: dx                                   !! Size of GridCell in x direction [m]
        real                            :: dy                                   !! Size of GridCell in y direction [m]
        real(dp)                        :: area                                 !! Area of the GridCell
        type(ReachElement), allocatable :: colRiverReaches(:)                   !! Array of `RiverReachElement` objects to hold the RiverReaches
        character(len=3), allocatable   :: reachTypes(:)                        !! Type of each indexed reach within this cell - 'riv' or 'est'
        character(len=3)                :: aggregatedReachType                  !! Type of reaches in this cell - 'riv', 'est' or 'mix'
        type(GridCellPointer)           :: outflow                              !! Where does the outflow reach from this GridCell go to?
            !! Array of `RiverReachPointer` objects to order rivers in routing order and by branch.
            !! 1st dimension: Branches. 2nd dimension: RiverReaches in that branch
        type(SoilProfileElement), allocatable :: colSoilProfiles(:)             !! Array of `SoilProfileElement` objects to hold the soil profiles
            ! NOTE current plan is to have single soil profile per Grid Cell. Declaring as an array for possible future flexibility.
        type(DiffuseSource), allocatable :: diffuseSources(:)                   !! Diffuse source object to provide, e.g., atmospheric deposition for this `GridCell`
        logical                         :: hasDiffuseSource = .false.           !! Does this `GridCell` have a `DiffuseSource`?
        integer                         :: nReaches = 0                         !! Number of `Reach`es in this cell
        integer                         :: nSoilProfiles = 0                    !! Number of contained `SoilProfile`s
            !! Number of reaches in each branch. Needed as different branches might have different numbers of reaches
            !! (and Fortran matrices must be rectangular)
        real(dp), allocatable           :: q_runoff_timeSeries(:)               !! Runoff = slow flow + quick flow, from the hydrological model [m/timestep]
        real(dp), allocatable           :: q_quickflow_timeSeries(:)            !! Quick flow from the hydrological model [m/timestep]
        real, allocatable               :: q_evap_timeSeries(:)                 !! Evaporation time series [m/s]
        real, allocatable               :: q_precip_timeSeries(:)               !! Precipitation time series [m/s]
        real(dp)                        :: q_runoff                             !! Runoff from the hydrological model for this time step [m/timestep]
        real(dp)                        :: Q_out                                !! Discharge from this `GridCell` on a given timestep [m3/timestep]
        real(dp)                        :: tmp_Q_out                            !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp)                        :: j_spm_out                            !! SPM discharge from this GridCell on a given timestep [kg/timestep]
        real(dp)                        :: tmp_j_spm_out                        !! Temporary storage for Q_out until all `GridCell`s have been processed
        real(dp)                        :: n_river                              !! Manning's roughness coefficient for the river
        real(dp), allocatable           :: T_water_timeSeries(:)                !! Water temperature [C]
        real(dp), allocatable           :: erodedSediment(:)                    !! Sediment yield eroded on this timestep [kg/m2/day], simulated by `SoilProfile`(s)
        real(dp), allocatable           :: distributionSediment(:)              !! Distribution used to split sediment yields across size classes
        real(dp), allocatable           :: j_np_diffuseSource(:,:,:)            !! Input NPs from diffuse sources on this timestep [(kg/m2)/timestep]
        logical                         :: isEmpty = .false.                    !! Is there anything going on in the `GridCell` or should we skip over when simulating?
        logical                         :: isHeadwater = .false.                !! Is this `GridCell` a headwater?
        logical                         :: hasStreamJunctionInflow = .false.    !! Is the inflow to this cell from more than one cell?
        logical                         :: isUpdated = .false.                  !! Has this `GridCell` been updated for this timestep?
        ! Demands
        logical                         :: hasDemands = .false.                 !! Does this `GridCell` have any water demand data?
        logical                         :: hasCrop = .false.                    !! Does this `GridCell` have any crops?
        logical                         :: hasLargeCity = .false.               !! Does this `GridCell` have a large city?
        real(dp)                        :: totalPopulation = 0.0_dp             !! Total population for the `GridCell` TODO what are the units?
        real(dp)                        :: urbanPopulation = 0.0_dp             !! Urban population for the `GridCell` TODO what are the units?
        real(dp)                        :: cattlePopulation = 0.0_dp            !! Cattle population for the `GridCell` TODO what are the units?
        real(dp)                        :: sheepGoatPopulation = 0.0_dp         !! Sheep/goat population for the `GridCell` TODO what are the units?
        real(dp)                        :: urbanDemandPerCapita = 0.0_dp        !! Urban demand per capita for the `GridCell` [l/day/capita]
        real(dp)                        :: ruralDemandPerCapita = 0.0_dp        !! Cattle population for the `GridCell` [1/day/capita]
        real(dp)                        :: industrialDemand = 0.0_dp            !! Industrial demand for `GridCell` [Mm3/day]
        real(dp)                        :: surfaceWaterToTotalWaterRatio        !! Ratio of surface to total water demand (same for all demands) [-]
        type(Crop), allocatable         :: crops(:)                             !! Crops present in this `GridCell`
      
    contains
        ! Creation/destruction
        procedure(createAbstractGridCell), deferred                         :: create
        procedure(finaliseCreateAbstractGridCell), deferred                 :: finaliseCreate
        procedure(snapPointSourcesToReachAbstractGridCell), deferred        :: snapPointSourcesToReach
        ! Simulation
        procedure(updateAbstractGridCell), deferred                         :: update
        procedure(finaliseUpdateAbstractGridCell), deferred                 :: finaliseUpdate
        procedure(parseNewBatchDataAbstractGridCell), deferred              :: parseNewBatchData
        ! Getters
        procedure(get_Q_outflowAbstractGridCell), deferred                  :: get_Q_outflow
        procedure(get_j_spm_outflowAbstractGridCell), deferred              :: get_j_spm_outflow
        procedure(get_m_spmAbstractGridCell), deferred                      :: get_m_spm
        procedure(get_j_spm_inflowAbstractGridCell), deferred               :: get_j_spm_inflow
        procedure(get_j_spm_soilErosionAbstractGridCell), deferred          :: get_j_spm_soilErosion
        procedure(get_j_spm_bankErosionAbstractGridCell), deferred          :: get_j_spm_bankErosion
        procedure(get_j_spm_depositionAbstractGridCell), deferred           :: get_j_spm_deposition
        procedure(get_j_spm_resuspensionAbstractGridCell), deferred         :: get_j_spm_resuspension
        procedure(get_m_np_waterAbstractGridCell), deferred                 :: get_m_np_water
        procedure(get_m_transformed_waterAbstractGridCell), deferred        :: get_m_transformed_water
        procedure(get_m_dissolved_waterAbstractGridCell), deferred          :: get_m_dissolved_water
        procedure(get_C_spmAbstractGridCell), deferred                      :: get_C_spm
        procedure(get_C_np_soilAbstractGridCell), deferred                  :: get_C_np_soil
        procedure(get_C_np_waterAbstractGridCell), deferred                 :: get_C_np_water
        procedure(get_C_np_sedimentAbstractGridCell), deferred              :: get_C_np_sediment
        procedure(get_C_np_sediment_byVolumeAbstractGridCell), deferred     :: get_C_np_sediment_byVolume
        procedure(get_C_np_sediment_lAbstractGridCell), deferred            :: get_C_np_sediment_l
        procedure(get_C_np_sediment_l_byVolumeAbstractGridCell), deferred   :: get_C_np_sediment_l_byVolume
        procedure(get_C_transformed_waterAbstractGridCell), deferred        :: get_C_transformed_water
        procedure(get_C_dissolved_waterAbstractGridCell), deferred          :: get_C_dissolved_water
        procedure(get_m_np_sedimentAbstractGridCell), deferred              :: get_m_np_sediment
        procedure(get_m_np_buried_sedimentAbstractGridCell), deferred       :: get_m_np_buried_sediment
        procedure(get_sediment_massAbstractGridCell), deferred              :: get_sediment_mass
        procedure(get_j_nm_depositionAbstractGridCell), deferred            :: get_j_nm_deposition
        procedure(get_j_transformed_depositionAbstractGridCell), deferred   :: get_j_transformed_deposition
        procedure(get_j_nm_resuspensionAbstractGridCell), deferred          :: get_j_nm_resuspension
        procedure(get_j_transformed_resuspensionAbstractGridCell), deferred :: get_j_transformed_resuspension
        procedure(get_j_nm_outflowAbstractGridCell), deferred               :: get_j_nm_outflow
        procedure(get_j_transformed_outflowAbstractGridCell), deferred      :: get_j_transformed_outflow
        procedure(get_j_dissolved_outflowAbstractGridCell), deferred        :: get_j_dissolved_outflow
        procedure(getTotalReachLengthAbstractGridCell), deferred            :: getTotalReachLength
        procedure(getWaterVolumeAbstractGridCell), deferred                 :: getWaterVolume
        procedure(getWaterDepthAbstractGridCell), deferred                  :: getWaterDepth
        procedure(getBedSedimentAreaAbstractGridCell), deferred             :: getBedSedimentArea
        procedure(getBedSedimentMassAbstractGridCell), deferred             :: getBedSedimentMass
    end type
      
    !> Container type for polymorphic AbstractGridCells
    type GridCellElement                                               
        class(AbstractGridCell), allocatable :: item                !! Polymorphic AbstractGridCell object
    end type

    abstract interface
        !> Create this grid cell
        function createAbstractGridCell(me, x, y, isEmpty) result(r)
            use ResultModule, only: Result
            import AbstractGridCell
            class(AbstractGridCell), target :: me               !! The AbstractGridCell instance
            integer                         :: x, y             !! The (x,y) position of the grid cell
            logical, optional               :: isEmpty          !! Is anything to be simulated for this grid cell?
            type(Result)                    :: r                !! The Result object to return any errors in
        end function
        
        !> Finalise the creation of the GridCell, after river routing has been established
        subroutine finaliseCreateAbstractGridCell(me)
            import AbstractGridCell
            class(AbstractGridCell) :: me               !! This GridCell instance
        end subroutine
        
        !> Run the AbstractGridCell's simulation for this time step
        subroutine updateAbstractGridCell(me, t, isWarmUp)
            import AbstractGridCell
            class(AbstractGridCell) :: me           !! The `GridCell` instance
            integer         :: t                    !! The current time step
            logical         :: isWarmUp             !! Are we in a warm up period?
        end subroutine
        
        !> Finalise the AbstractGridCell's state variables for this time step
        subroutine finaliseUpdateAbstractGridCell(me)
            import AbstractGridCell
            class(AbstractGridCell) :: me           !! The `AbstractGridCell` instance
        end subroutine

        subroutine snapPointSourcesToReachAbstractGridCell(me)
            import AbstractGridCell
            class(AbstractGridCell) :: me
        end subroutine

        subroutine parseNewBatchDataAbstractGridCell(me)
            import AbstractGridCell
            class(AbstractGridCell) :: me
        end subroutine

        function get_Q_outflowAbstractGridCell(me) result(Q_outflow)
            use Globals, only: dp    
            use ResultModule, only: Result
            import AbstractGridCell
            class(AbstractGridCell) :: me                   !! This grid cell
            real(dp) :: Q_outflow                   !! Discharge to return
        end function

        function get_j_spm_outflowAbstractGridCell(me) result(j_spm_outflow)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me                               !! This grid cell
            real(dp)                :: j_spm_outflow(C%nSizeClassesSpm) !! SPM outflow to return
        end function

        function get_m_spmAbstractGridCell(me) result(m_spm)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: m_spm(C%nSizeClassesSpm)
        end function

        function get_j_spm_inflowAbstractGridCell(me) result(j_spm_inflow)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_spm_inflow(C%nSizeClassesSpm)
        end function

        function get_j_spm_soilErosionAbstractGridCell(me) result(j_spm_soilErosion)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_spm_soilErosion(C%nSizeClassesSpm)
        end function

        function get_j_spm_bankErosionAbstractGridCell(me) result(j_spm_bankErosion)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_spm_bankErosion(C%nSizeClassesSpm)
        end function

        function get_j_spm_depositionAbstractGridCell(me) result(j_spm_deposition)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_spm_deposition(C%nSizeClassesSpm)
        end function

        function get_j_spm_resuspensionAbstractGridCell(me) result(j_spm_resuspension)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_spm_resuspension(C%nSizeClassesSpm)
        end function

        function get_m_np_waterAbstractGridCell(me) result(m_np)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: m_np(:,:,:)
        end function

        function get_m_np_sedimentAbstractGridCell(me) result(m_np)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: m_np(:,:,:)
        end function

        function get_m_transformed_waterAbstractGridCell(me) result(m_transformed)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: m_transformed(:,:,:)
        end function

        function get_m_dissolved_waterAbstractGridCell(me) result(m_dissolved)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: m_dissolved
        end function

        function get_C_spmAbstractGridCell(me) result(C_spm)
            use Globals, only: dp, C
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_spm(:)
        end function

        function get_C_np_soilAbstractGridCell(me) result(C_np_soil)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_np_soil(:,:,:)
        end function

        function get_C_np_waterAbstractGridCell(me) result(C_np_water)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_np_water(:,:,:)
        end function

        function get_C_np_sedimentAbstractGridCell(me) result(C_np_sediment)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_np_sediment(:,:,:)
        end function

        function get_C_np_sediment_byVolumeAbstractGridCell(me) result(C_np_sediment)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_np_sediment(:,:,:)
        end function

        function get_C_np_sediment_lAbstractGridCell(me, l) result(C_np_sediment)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            integer                 :: l
            real(dp), allocatable   :: C_np_sediment(:,:,:)
        end function

        function get_C_np_sediment_l_byVolumeAbstractGridCell(me, l) result(C_np_sediment)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            integer                 :: l
            real(dp), allocatable   :: C_np_sediment(:,:,:)
        end function

        function get_C_transformed_waterAbstractGridCell(me) result(C_transformed_water)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: C_transformed_water(:,:,:)
        end function

        function get_C_dissolved_waterAbstractGridCell(me) result(C_dissolved_water)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: C_dissolved_water
        end function

        function get_m_np_buried_sedimentAbstractGridCell(me) result(m_np_buried)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: m_np_buried(:,:,:)
        end function

        function get_sediment_massAbstractGridCell(me) result(sediment_mass) 
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: sediment_mass
        end function

        function get_j_nm_depositionAbstractGridCell(me) result(j_nm_deposition)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_nm_deposition(:,:,:)
        end function

        function get_j_transformed_depositionAbstractGridCell(me) result(j_transformed_deposition)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_transformed_deposition(:,:,:)
        end function

        function get_j_nm_resuspensionAbstractGridCell(me) result(j_nm_resuspension)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_nm_resuspension(:,:,:)
        end function

        function get_j_transformed_resuspensionAbstractGridCell(me) result(j_transformed_resuspension)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_transformed_resuspension(:,:,:)
        end function

        function get_j_nm_outflowAbstractGridCell(me) result(j_nm_outflow)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_nm_outflow(:,:,:)
        end function

        function get_j_transformed_outflowAbstractGridCell(me) result(j_transformed_outflow)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp), allocatable   :: j_transformed_outflow(:,:,:)
        end function

        function get_j_dissolved_outflowAbstractGridCell(me) result(j_dissolved_outflow)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: j_dissolved_outflow
        end function

        function getTotalReachLengthAbstractGridCell(me) result(totalReachLength)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: totalReachLength
        end function

        function getWaterVolumeAbstractGridCell(me) result(waterVolume)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: waterVolume
        end function

        function getWaterDepthAbstractGridCell(me) result(waterDepth)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: waterDepth
        end function

        function getBedSedimentAreaAbstractGridCell(me) result(bedArea)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: bedArea
        end function

        function getBedSedimentMassAbstractGridCell(me) result(sedimentMass)
            use Globals, only: dp
            import AbstractGridCell
            class(AbstractGridCell) :: me
            real(dp)                :: sedimentMass
        end function

    end interface
end module

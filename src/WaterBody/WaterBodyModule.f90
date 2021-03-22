
!> Module containing definition of base class `WaterBody1`, which provides the primitive
!! functionality to all environmental compartments that are water bodies.
module WaterBodyModule
    use Globals
    use classPointSource2
    use classDiffuseSource2
    use classDatabase, only: DATASET
    use spcBedSediment
    use spcReactor
    use classBiotaWater
    use FlowModule
    implicit none
    
    !> `WaterBodyPointer` used for `WaterBody1` inflows array, so the elements within can
    !! point to other `GridCell`'s colWaterBody elements
    type WaterBodyPointer1
        class(WaterBody1), pointer :: item => null()                  !! Pointer to polymorphic `WaterBody1` object
    end type
    
    !> An internal user-defined type, defining a reference to a `WaterBody1`.
    !! Comprises row (x) and column (y) references to the `GridCell` containing the
    !! `WaterBody1` and the in-cell `WaterBody1` reference number
    type WaterBodyRef
        integer :: x                                                !! `GridCell` x reference
        integer :: y                                                !! `GridCell` y reference
        integer :: w                                                !! `WaterBody1` reference
    end type

    !> Abstract base class for `WaterBody1`. Defines properties and procedures
    !! required in any implementation of this class.
    type, public :: WaterBody1
        ! Reference
        character(len=100) :: ref                                   !! Reference for this object, of the form WaterBody_x_y_w
        integer :: x                                                !! `GridCell` x position
        integer :: y                                                !! `GridCell` y position
        integer :: w                                                !! `WaterBody1` reference
        ! Physical properties
        real(dp) :: depth                                           !! Depth of the `WaterBody1` [m]
        real(dp) :: surfaceArea                                     !! Surface area of the `WaterBody1` [m2]
        real(dp) :: bedArea                                         !! Area of the contained `BedSediment` [m2]
        real(dp) :: volume                                          !! Volume of water in the body [m3]
        real :: T_water(366)                                        !! Water temperature [C]
        ! Concentrations
        real(dp), allocatable :: C_spm(:)                           !! Sediment concentration [kg/m3]
        real(dp), allocatable :: C_spm_final(:)                     !! Sediment concentration [kg/m3]
        real(dp), allocatable :: m_spm(:)                           !! Sediment mass [kg/m3]
        real(dp), allocatable :: C_np(:,:,:)                        !! NM mass concentration [kg/m3]
        real(dp), allocatable :: C_np_final(:,:,:)                  !! Final NM mass concentration [kg/m3]
        real(dp), allocatable :: m_np(:,:,:)                        !! NM mass mass [kg]
        real(dp), allocatable :: m_transformed(:,:,:)               !! Transformed NM mass [kg]
        real(dp), allocatable :: C_transformed(:,:,:)               !! Transformed NM concentration [kg/m3]
        real(dp), allocatable :: C_transformed_final(:,:,:)         !! Final transformed NM concentration [kg/m3]
        real(dp) :: m_dissolved                                     !! Dissolved NM mass [kg]
        real(dp) :: C_dissolved                                     !! Dissolved NM concentration [kg/m3]
        real(dp) :: C_dissolved_final                               !! Final dissolved NM concentration [kg/m3]
        ! Flows and fluxes
        integer, allocatable :: neighboursArray(:,:)                !! Neighbouring waterbodies, as array of indices
        type(WaterBodyPointer1), allocatable :: neighbours(:)        !! Neighbouring waterbodies
        real(dp) :: Q_in_total                                      !! Total inflow of water [m3/timestep]
        real(dp), allocatable :: k_resus(:)                         !! Resuspension rate for a given timestep [s-1]
        real(dp), allocatable :: k_settle(:)                        !! Sediment settling rate on a given timestep [s-1]
        real(dp), allocatable :: W_settle_spm(:)                    !! SPM settling velocity [m/s]
        real(dp), allocatable :: W_settle_np(:)                     !! NP settling velocity [m/s]
        real(dp) :: sedimentTransportCapacity                       !! Sediment transport capacity, to limit erosion [kg/m2/timestep]
        real(dp) :: a_stc                                           !! Sediment transport scaling factor[kg/m2/km2]
        real(dp) :: b_stc                                           !! Sediment transport direct runoff (overland flow) threshold [m2/s]
        real(dp) :: c_stc                                           !! Sediment transport non-linear coefficient [-]
        real(dp), allocatable :: distributionSediment(:)            !! Distribution to use to split sediment yields with
        real :: waterTemperature_t(366)                             !! Water temperature timeseries across a year [deg C]
        ! Contained objects
        class(BedSediment), allocatable :: bedSediment              !! Contained `BedSediment` object
        class(Reactor), allocatable :: reactor                      !! Contained `Reactor` object
        type(PointSource2), allocatable :: pointSources(:)          !! Contained `PointSource` objects
        logical :: hasPointSource = .false.                         !! Does this water body have any point sources?
        integer :: nPointSources = 0                                !! How many point sources this water body has
        type(DiffuseSource2), allocatable :: diffuseSources(:)      !! Contained `DiffuseSource` objects
        integer :: nDiffuseSources                                  !! How many diffuse sources this water body has
        logical :: hasDiffuseSource = .false.                       !! Does this water body have any diffuse sources?
        logical :: isTidalLimit = .false.                           !! Is this water body at the tidal limit?
        logical :: isUpdated = .false.                              !! Has the WaterBody1 been updated on this time step yet?
        ! Biota
        type(BiotaWater), allocatable :: biota(:)                  !! Contained `Biota` object
        integer :: nBiota = 0
        integer, allocatable :: biotaIndices(:)
        ! Flow objects
        type(WaterFlows) :: obj_Q
        type(SPMFlows) :: obj_j_spm
        type(NMFlows) :: obj_j_nm
        type(NMFlows) :: obj_j_nm_transformed
        type(DissolvedFlows) :: obj_j_dissolved
        type(WaterFlows) :: obj_Q_final
        type(SPMFlows) :: obj_j_spm_final
        type(NMFlows) :: obj_j_nm_final
        type(NMFlows) :: obj_j_nm_transformed_final
        type(DissolvedFlows) :: obj_j_dissolved_final
      contains
        ! Create
        procedure :: create => createWaterBody
        procedure :: finaliseCreate => finaliseCreateWaterBody
        procedure :: addPointSource => addPointSourceWaterBody
        ! Simulators
        procedure :: update => updateWaterBody
        procedure :: finaliseUpdate
        procedure :: emptyFlows
        ! Data handlers
        procedure :: allocateAndInitialise => allocateAndInitialiseWaterBody
        procedure :: parseInputData => parseInputDataWaterBody
        procedure :: parseNewBatchData => parseNewBatchDataWaterBody
    end type
      
    !> Container type for `class(WaterBody1)`, the actual type of the `WaterBody1` class.
    !! a variable of type `WaterBodyElement` can be of any object type inheriting from the
    !! `WaterBody1` abstract base class.
    type WaterBodyElement                                          
        class(WaterBody1), allocatable :: item                      !! Polymorphic `WaterBody1` object
    end type

  contains

    !> Create this `WaterBody1`
    function createWaterBody(me, x, y, w, distributionSediment) result(rslt)
        class(WaterBody1) :: me                                  !! The `WaterBody1` instance
        integer :: x, y, w                                      !! `GridCell` and `WaterBody1` identifiers
        real(dp) :: distributionSediment(C%nSizeClassesSPM)     !! Distribution to split sediment across size classes
        type(Result) :: rslt                                    !! The Result object
        ! Set reach indices and grid cell area
        me%x = x
        me%y = y
        me%w = w
        me%distributionSediment = distributionSediment
        ! Create the diffuse sources. One for water, one for atmospheric
        allocate(me%diffuseSources(2))
        call me%diffuseSources(1)%create(me%x, me%y, 1, 'water')
        call me%diffuseSources(2)%create(me%x, me%y, 2, 'atmospheric')
        me%nDiffuseSources = 2
        ! Make sure there are no point source to begin with (they're added one at a time)
        allocate(me%pointSources(0))

        ! Initialise the flow objects
        call me%obj_Q%init()
        call me%obj_j_SPM%init()
        call me%obj_j_nm%init()
        call me%obj_j_nm_transformed%init()
        call me%obj_j_dissolved%init()
        call me%obj_Q_final%init()
        call me%obj_j_spm_final%init()
        call me%obj_j_nm_final%init()
        call me%obj_j_nm_transformed_final%init()
        call me%obj_j_dissolved_final%init()
    end function

    !> Perform creation operations that required routing and point source snapping
    !! to reaches to be done.
    subroutine finaliseCreateWaterBody(me)
        class(WaterBody1) :: me
        ! We can't allocate j_nm until we know the number of point sources, which
        ! is calculated during GridCell%finaliseCreate. Hence this is done here
        call me%allocateAndInitialise()
    end subroutine

    !> Update this `WaterBody1` on given time step
    subroutine updateWaterBody(me, t, q_runoff, q_overland, j_spm_runoff, j_np_runoff, j_transformed_runoff, contributingArea)
        class(WaterBody1)   :: me                               !! This `WaterBody1` instance
        integer             :: t                                !! What time step are we on?
        real(dp)            :: q_runoff                         !! Runoff from the hydrological model [m/timestep]
        real(dp)            :: q_overland                       !! Overland flow [m3/m2/timestep]
        real(dp)            :: j_spm_runoff(:)                  !! Eroded sediment runoff to this water body [kg/timestep]
        real(dp)            :: j_np_runoff(:,:,:)               !! Eroded NP runoff to this water body [kg/timestep]
        real(dp)            :: j_transformed_runoff(:,:,:)      !! Eroded transformed NP runoff to this water body [kg/timestep]
        real(dp)            :: contributingArea                 !! Area contributing to this reach (e.g. the soil profile) [m2]
    end subroutine

    !> Set all flow object properties to zero. Useful for the start of 
    !! every timestep
    subroutine emptyFlows(me)
        class(WaterBody1) :: me
        call me%obj_Q%empty()
        call me%obj_j_SPM%empty()
        call me%obj_j_nm%empty()
        call me%obj_j_nm_transformed%empty()
        call me%obj_j_dissolved%empty() 
    end subroutine

    !> Allocate memory for arrays generic to any water body. Individual water bodies
    !! may extend this routine to allocate their own body specific variables
    subroutine allocateAndInitialiseWaterBody(me)
        class(WaterBody1) :: me
        allocate(me%C_spm(C%nSizeClassesSpm), &
            me%C_spm_final(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            me%C_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%C_np_final(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%k_resus(C%nSizeClassesSpm), &
            me%k_settle(C%nSizeClassesSpm), &
            me%W_settle_spm(C%nSizeClassesSpm), &
            me%W_settle_np(C%nSizeClassesNM), &
            me%C_transformed(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%C_transformed_final(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)) &
        )
        me%C_spm = 0.0_dp
        me%C_spm_final = 0.0_dp
        me%C_np_final = 0.0_dp
        me%m_spm = 0.0_dp
        me%C_np = 0.0_dp
        me%m_np = 0.0_dp
        me%C_transformed = 0.0_dp
        me%C_transformed_final = 0.0_dp
        me%m_transformed = 0.0_dp
        me%C_dissolved = 0.0_dp
        me%C_dissolved_final = 0.0_dp
        me%m_dissolved = 0.0_dp
        me%bedArea = 0.0_dp
        me%volume = 0.0_dp
    end subroutine

    subroutine addPointSourceWaterBody(me, index)
        class(WaterBody1)    :: me       !! This WaterBody1
        integer             :: index    !! Point source index
        type(PointSource2)  :: newSource
        type(PointSource2), allocatable :: oldPointSources(:)
        ! Create the new source
        call newSource%create(me%x, me%y, index, 'water')
        ! Store old point sources
        call move_alloc(from=me%pointSources, to=oldPointSources)
        me%pointSources = [oldPointSources, newSource]
        ! Update number of point sources
        me%nPointSources = size(me%pointSources)
    end subroutine

    !> Parse input data for this `WaterBody1`
    function parseInputDataWaterBody(me) result(rslt)
        class(WaterBody1) :: me
        type(Result) :: rslt
    end function

    subroutine parseNewBatchDataWaterBody(me)
        class(WaterBody1) :: me
    end subroutine

    !> Set the final flow arrays for this water body. These final arrays are used by other linked
    !! water bodies such that the avoid using the wrong timestep's values, in particular as inflows.
    subroutine finaliseUpdate(me)
        class(WaterBody1) :: me
        me%obj_Q_final = me%obj_Q
        me%obj_j_spm_final = me%obj_j_spm
        me%obj_j_nm_final = me%obj_j_nm
        me%obj_j_nm_transformed_final = me%obj_j_nm_transformed
        me%obj_j_dissolved = me%obj_j_dissolved
        me%C_spm_final = me%C_spm
        me%C_np_final = me%C_np
        me%C_transformed_final = me%C_transformed
        me%C_dissolved_final = me%C_dissolved
        me%isUpdated = .false.
    end subroutine

end module
module WaterBodyModule
    use GlobalsModule
    use PointSourceModule
    use DiffuseSourceModule
    use DataInputModule, only: DATASET
    use AbstractBedSedimentModule
    use AbstractReactorModule
    use ReactorModule
    use BiotaWaterModule
    use FlowModule
    use ContaminantModule
    implicit none
    
    !> WaterBodyPointer used for WaterBody inflows array, so the elements within can
    !! point to other GridCell's colWaterBody elements
    type WaterBodyPointer
        class(WaterBody), pointer :: item => null()                  !! Pointer to polymorphic WaterBody object
    end type
    
    !> An internal user-defined type, defining a reference to a WaterBody.
    !! Comprises row (x) and column (y) references to the GridCell containing the
    !! WaterBody and the in-cell WaterBody reference number
    type WaterBodyRef
        integer :: x                !! GridCell x reference
        integer :: y                !! GridCell y reference
        integer :: w                !! WaterBody reference
    end type

    !> Abstract base class for `WaterBody`. Defines properties and procedures
    !! required in any implementation of this class.
    type, public :: WaterBody
        ! Reference
        character(len=100)      :: ref                              !! Reference for this object, of the form WaterBody_x_y_w
        integer                 :: x                                !! `GridCell` x position
        integer                 :: y                                !! `GridCell` y position
        integer                 :: w                                !! `WaterBody` reference
        ! Physical properties
        real(dp)                :: depth                            !! Depth of the `WaterBody` [m]
        real(dp)                :: surfaceArea                      !! Surface area of the `WaterBody` [m2]
        real(dp)                :: bedArea                          !! Area of the contained `BedSediment` [m2]
        real(dp)                :: volume                           !! Volume of water in the body [m3]
        real(dp)                :: T_water(366)                     !! Water temperature [C]
        ! Concentrations
        real(dp), allocatable   :: C_spm(:)                         !! Sediment concentration [kg/m3]
        real(dp), allocatable   :: C_spm_final(:)                   !! Sediment concentration [kg/m3]
        real(dp), allocatable   :: m_spm(:)                         !! Sediment mass [kg/m3]
        type(Contaminant)       :: m_contaminant                    !! Mass in water body [kg]
        real(dp)                :: C_dissolved = 0.0_dp             !! Dissolved concentration [kg/m³]
        real(dp)                :: C_dissolved_final = 0.0_dp       !! Final dissolved concentration [kg/m³]
        ! Flows and fluxes
        integer, allocatable    :: neighboursArray(:,:)             !! Neighbouring waterbodies, as array of indices
        type(WaterBodyPointer), allocatable :: neighbours(:)     !! Neighbouring waterbodies
        real(dp)                :: Q_in_total                       !! Total inflow of water [m3/timestep]
        real(dp), allocatable   :: k_resus(:)                       !! Resuspension rate for a given timestep [s-1]
        real(dp), allocatable   :: k_settle(:)                      !! Sediment settling rate on a given timestep [s-1]
        real(dp), allocatable   :: W_settle_spm(:)                  !! SPM settling velocity [m/s]
        real(dp)                :: sedimentTransportCapacity        !! Sediment transport capacity, to limit erosion [kg/m2/timestep]
        real(dp)                :: a_stc                            !! Sediment transport scaling factor[kg/m2/km2]
        real(dp)                :: b_stc                            !! Sediment transport direct runoff (overland flow) threshold [m2/s]
        real(dp)                :: c_stc                            !! Sediment transport non-linear coefficient [-]
        real(dp), allocatable   :: distributionSediment(:)          !! Distribution to use to split sediment yields with
        real(dp)                :: waterTemperature_t(366)          !! Water temperature timeseries across a year [deg C]
        class(Reactor), allocatable :: reactor                      !! Concrete reactor instance
        ! Contained objects
        class(AbstractBedSediment), allocatable :: bedSediment      !! Contained BedSediment object
        type(PointSource), allocatable :: pointSources(:)           !! Contained PointSource objects
        logical                 :: hasPointSource = .false.         !! Does this water body have any point sources?
        integer                 :: nPointSources = 0                !! How many point sources this water body has
        type(DiffuseSource), allocatable :: diffuseSources(:)       !! Contained `DiffuseSource` objects
        integer                 :: nDiffuseSources                  !! How many diffuse sources this water body has
        logical                 :: hasDiffuseSource = .false.       !! Does this water body have any diffuse sources?
        logical                 :: isTidalLimit = .false.           !! Is this water body at the tidal limit?
        logical                 :: isUpdated = .false.              !! Has the WaterBody been updated on this time step yet?
        ! Biota
        type(BiotaWater), allocatable :: biota(:)                   !! Contained `Biota` object
        integer                 :: nBiota = 0
        integer, allocatable    :: biotaIndices(:)
        ! Flow objects
        type(WaterFlows)        :: Q
        type(SPMFlows)          :: j_spm
        type(WaterFlows)        :: Q_final
        type(SPMFlows)          :: j_spm_final
        type(Contaminant)       :: j_contaminant_inflow
        type(Contaminant)       :: j_contaminant_outflow
        type(Contaminant)       :: j_contaminant_runoff
        type(Contaminant)       :: j_contaminant_transfers
        type(Contaminant)       :: j_contaminant_pointSources
        type(Contaminant)       :: j_contaminant_diffuseSources
        type(Contaminant)       :: j_contaminant_soilErosion
        type(Contaminant)       :: j_contaminant_bankErosion
        type(Contaminant)       :: j_contaminant_deposition
        type(Contaminant)       :: j_contaminant_resuspension
        type(Contaminant)       :: j_contaminant_final

      contains
        ! Create
        procedure :: create => createWaterBody
        procedure :: finaliseCreate => finaliseCreateWaterBody
        procedure :: finalise => finaliseWaterBody
        procedure :: addPointSource => addPointSourceWaterBody
        ! Simulators
        procedure :: update => updateWaterBody
        procedure :: finaliseUpdate
        procedure :: emptyFlows
        ! Data handlers
        procedure :: allocateAndInitialise => allocateAndInitialiseWaterBody
        procedure :: parseInputData => parseInputDataWaterBody
        procedure :: parseNewBatchData => parseNewBatchDataWaterBody
        procedure :: get_m_contaminant => get_m_contaminant_WaterBody
        procedure :: get_C_contaminant => get_C_contaminant_WaterBody
    end type
      
    !> Container type for `class(WaterBody)`, the actual type of the `WaterBody` class.
    !! a variable of type `WaterBodyElement` can be of any object type inheriting from the
    !! `WaterBody` abstract base class.
    type WaterBodyElement                                          
        class(WaterBody), allocatable :: item                      !! Polymorphic `WaterBody` object
    end type

  contains

    !> Create this `WaterBody`
    function createWaterBody(me, x, y, w, distributionSediment) result(rslt)
        class(WaterBody), intent(inout) :: me                      !! The `WaterBody` instance
        integer, intent(in) :: x, y, w                             !! `GridCell` and `WaterBody` identifiers
        real(dp), intent(in) :: distributionSediment(C%nSizeClassesSPM) !! Distribution to split sediment across size classes
        type(Result) :: rslt                                       !! The Result object
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
        call me%Q%init()
        call me%j_spm%init()
        call rslt%addErrors(.errors. me%j_contaminant_inflow%create())
        call rslt%addErrors(.errors. me%j_contaminant_outflow%create())
        call rslt%addErrors(.errors. me%j_contaminant_runoff%create())
        call rslt%addErrors(.errors. me%j_contaminant_transfers%create())
        call rslt%addErrors(.errors. me%j_contaminant_pointSources%create())
        call rslt%addErrors(.errors. me%j_contaminant_diffuseSources%create())
        call rslt%addErrors(.errors. me%j_contaminant_soilErosion%create())
        call rslt%addErrors(.errors. me%j_contaminant_bankErosion%create())
        call rslt%addErrors(.errors. me%j_contaminant_deposition%create())
        call rslt%addErrors(.errors. me%j_contaminant_resuspension%create())
        call rslt%addErrors(.errors. me%j_contaminant_final%create())
    end function

    !> Perform creation operations that required routing and point source snapping
    !! to reaches to be done.
    subroutine finaliseCreateWaterBody(me)
        class(WaterBody), intent(inout) :: me
        type(Result) :: rslt
        ! We can't allocate contaminants until we know the number of point sources, which
        ! is calculated during GridCell%finaliseCreate. Hence this is done here
        call me%allocateAndInitialise()
        rslt = me%reactor%create( &
            me%x, me%y, 'water', me%m_contaminant, me%volume, me%T_water(1), &
            me%C_spm, me%W_settle_spm, 0.0_dp, velocity=0.0_dp &
        )
    end subroutine

    subroutine finaliseWaterBody(me)
        class(WaterBody), intent(inout) :: me
        integer :: i
        call me%m_contaminant%finalise()
        call me%j_contaminant_inflow%finalise()
        call me%j_contaminant_outflow%finalise()
        call me%j_contaminant_runoff%finalise()
        call me%j_contaminant_transfers%finalise()
        call me%j_contaminant_pointSources%finalise()
        call me%j_contaminant_diffuseSources%finalise()
        call me%j_contaminant_soilErosion%finalise()
        call me%j_contaminant_bankErosion%finalise()
        call me%j_contaminant_deposition%finalise()
        call me%j_contaminant_resuspension%finalise()
        call me%j_contaminant_final%finalise()
        if (allocated(me%reactor)) then
            call me%reactor%finalise()
            deallocate(me%reactor)
        end if
        if (allocated(me%bedSediment)) then
            call me%bedSediment%finalise()
            deallocate(me%bedSediment)
        end if
        if (allocated(me%pointSources)) deallocate(me%pointSources)
        if (allocated(me%diffuseSources)) deallocate(me%diffuseSources)
        if (allocated(me%biota)) then
            do i = 1, me%nBiota
                call me%biota(i)%finalise()
            end do
            deallocate(me%biota)
        end if
        if (allocated(me%C_spm)) deallocate(me%C_spm)
        if (allocated(me%C_spm_final)) deallocate(me%C_spm_final)
        if (allocated(me%m_spm)) deallocate(me%m_spm)
        if (allocated(me%k_resus)) deallocate(me%k_resus)
        if (allocated(me%k_settle)) deallocate(me%k_settle)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        if (allocated(me%neighboursArray)) deallocate(me%neighboursArray)
        if (allocated(me%neighbours)) deallocate(me%neighbours)
        if (allocated(me%biotaIndices)) deallocate(me%biotaIndices)
        if (allocated(me%distributionSediment)) deallocate(me%distributionSediment)
    end subroutine

    !> Update this `WaterBody` on given time step
    subroutine updateWaterBody(me, t, q_runoff, q_overland, j_spm_runoff, j_contaminant_runoff, &
                               contributingArea, isWarmUp)
        class(WaterBody), intent(inout) :: me                      !! This `WaterBody` instance
        integer, intent(in) :: t                                   !! What time step are we on?
        real(dp), intent(in) :: q_runoff                           !! Runoff from the hydrological model [m/timestep]
        real(dp), intent(in) :: q_overland                         !! Overland flow [m3/m2/timestep]
        real(dp), intent(in) :: j_spm_runoff(:)                    !! Eroded sediment runoff to this water body [kg/timestep]
        type(Contaminant), intent(in) :: j_contaminant_runoff      !! Contaminant runoff to this water body [kg/timestep]
        real(dp), intent(in) :: contributingArea                   !! Area contributing to this reach (e.g. the soil profile) [m2]
        logical, intent(in) :: isWarmUp                            !! Are we in a warm up period?
        type(Result) :: rslt

        call me%Q%addInflow(q_runoff)
        call me%j_spm%addInflow(j_spm_runoff)
        call me%j_contaminant_runoff%add(j_contaminant_runoff)
        rslt = me%reactor%update(j_contaminant_runoff, real(C%timeStep, dp))
    end subroutine

    !> Set all flow object properties to zero. Useful for the start of 
    !! every timestep
    subroutine emptyFlows(me)
        class(WaterBody), intent(inout) :: me
        call me%Q%empty()
        call me%j_spm%empty()
        call me%j_contaminant_inflow%empty()
        call me%j_contaminant_outflow%empty()
        call me%j_contaminant_runoff%empty()
        call me%j_contaminant_transfers%empty()
        call me%j_contaminant_pointSources%empty()
        call me%j_contaminant_diffuseSources%empty()
        call me%j_contaminant_soilErosion%empty()
        call me%j_contaminant_bankErosion%empty()
        call me%j_contaminant_deposition%empty()
        call me%j_contaminant_resuspension%empty()
        call me%j_contaminant_final%empty()
    end subroutine

    !> Allocate memory for arrays generic to any water body. Individual water bodies
    !! may extend this routine to allocate their own body specific variables
    subroutine allocateAndInitialiseWaterBody(me)
        class(WaterBody), intent(inout) :: me
        type(Result) :: rslt

        ! Be re-entry safe: deallocate before (re)allocating
        if (allocated(me%C_spm))        deallocate(me%C_spm)
        if (allocated(me%C_spm_final))  deallocate(me%C_spm_final)
        if (allocated(me%m_spm))        deallocate(me%m_spm)
        if (allocated(me%k_resus))      deallocate(me%k_resus)
        if (allocated(me%k_settle))     deallocate(me%k_settle)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)

        allocate(me%C_spm(        C%nSizeClassesSpm))
        allocate(me%C_spm_final(  C%nSizeClassesSpm))
        allocate(me%m_spm(        C%nSizeClassesSpm))
        allocate(me%k_resus(      C%nSizeClassesSpm))
        allocate(me%k_settle(     C%nSizeClassesSpm))
        allocate(me%W_settle_spm( C%nSizeClassesSpm))

        me%C_spm          = 0.0_dp
        me%C_spm_final    = 0.0_dp
        me%m_spm          = 0.0_dp
        me%k_resus        = 0.0_dp
        me%k_settle       = 0.0_dp
        me%W_settle_spm   = 0.0_dp
        me%C_dissolved        = 0.0_dp
        me%C_dissolved_final  = 0.0_dp
        me%bedArea            = 0.0_dp
        me%volume             = 0.0_dp

        ! Ensure contaminant internals are clean before re-create
        call me%m_contaminant%finalise()

        rslt = me%m_contaminant%create_from_data( &
            compartment='water', &
            contaminantDensity = DATASET%contaminantDensity, &
            soilAttachmentEfficiency = &
                DATASET%soilConstantAttachmentEfficiency, &
            riverAttachmentEfficiency = DATASET%riverAttachmentEfficiency, &
            estuaryAttachmentEfficiency = &
                DATASET%estuaryAttachmentEfficiency, &
            k_diss_pristine     = DATASET%contaminant_k_diss_pristine, &
            k_diss_transformed  = DATASET%contaminant_k_diss_transformed, &
            k_transform_pristine= &
                DATASET%contaminant_k_transform_pristine, &
            waterTemperature    = real(DATASET%waterTemperature(1), dp) )

        ! If this routine can be re-entered, (re)create flow objects too
        call rslt%addErrors(.errors. me%j_contaminant_inflow%create())
        call rslt%addErrors(.errors. me%j_contaminant_outflow%create())
        call rslt%addErrors(.errors. me%j_contaminant_runoff%create())
        call rslt%addErrors(.errors. me%j_contaminant_transfers%create())
        call rslt%addErrors(.errors. me%j_contaminant_pointSources%create())
        call rslt%addErrors(.errors. me%j_contaminant_diffuseSources%create())
        call rslt%addErrors(.errors. me%j_contaminant_soilErosion%create())
        call rslt%addErrors(.errors. me%j_contaminant_bankErosion%create())
        call rslt%addErrors(.errors. me%j_contaminant_deposition%create())
        call rslt%addErrors(.errors. me%j_contaminant_resuspension%create())
        call rslt%addErrors(.errors. me%j_contaminant_final%create())
    end subroutine


    !> Add a point source to this WaterBody 
    subroutine addPointSourceWaterBody(me, index)
        class(WaterBody), intent(inout) :: me                      !! This WaterBody
        integer, intent(in) :: index                               !! Point source index
        type(PointSource) :: newSource                             !! The new point source to add
        type(PointSource), allocatable :: oldPointSources(:)        !! The old point sources
        ! Create the new source
        call newSource%create(me%x, me%y, index, 'water')
        ! Store old point sources
        call move_alloc(from=me%pointSources, to=oldPointSources)
        me%pointSources = [oldPointSources, newSource]
        ! Update number of point sources
        me%nPointSources = size(me%pointSources)
    end subroutine

    !> Parse input data for this WaterBody
    function parseInputDataWaterBody(me) result(rslt)
        class(WaterBody), intent(inout) :: me                      !! This WaterBody
        type(Result) :: rslt                                       !! The Result object
    end function

    !> Parse new batch input data for this WaterBody
    subroutine parseNewBatchDataWaterBody(me)
        class(WaterBody), intent(inout) :: me                      !! This WaterBody
    end subroutine

    !> Set the final flow arrays for this water body. These final arrays are used by other linked
    !! water bodies such that the avoid using the wrong timestep's values, in particular as inflows.
    subroutine finaliseUpdate(me)
        class(WaterBody), intent(inout) :: me
        me%Q_final = me%Q
        me%j_spm_final = me%j_spm
        me%j_contaminant_final = me%j_contaminant_outflow
        me%C_spm_final = me%C_spm
        if (me%volume > 0.0_dp) then
            me%C_dissolved = me%m_contaminant%m_dissolved / me%volume
        else
            me%C_dissolved = 0.0_dp
        end if
        me%C_dissolved_final = me%C_dissolved
    end subroutine

    function get_m_contaminant_WaterBody(me) result(m_contaminant)
        class(WaterBody), intent(in) :: me
        type(Contaminant) :: m_contaminant
        m_contaminant = me%m_contaminant
    end function

    function get_C_contaminant_WaterBody(me) result(C_contaminant)
        class(WaterBody), intent(in) :: me
        real(dp), allocatable :: C_contaminant(:,:,:)
        allocate(C_contaminant(C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
        if (me%volume > 0.0_dp) then
            C_contaminant = me%m_contaminant%c / me%volume
        else
            C_contaminant = 0.0_dp
        end if
    end function
end module
module AbstractSoilProfileModule
    use GlobalsModule
    use AbstractSoilLayerModule
    use ResultModule, only: Result
    use ContaminantModule
    use DataInputModule, only: DATASET
    implicit none

    !> Abstract base class for soil profiles. Defines properties and methods required in any implementation
    !! of an AbstractSoilProfile class. This class acts as a container for a collection of SoilLayer objects,
    !! which collectively define the layout of the SoilProfile. The SoilLayer class routes water, eroded
    !! soil (and ultimately contaminants) through a layer of soil
    type, abstract, public :: AbstractSoilProfile
        ! Setup and dimensions
        character(len=256)      :: ref                                  !! A reference name for the object
        integer                 :: x                                    !! GridCell x index 
        integer                 :: y                                    !! GridCell y index
        integer                 :: p                                    !! SoilProfile index
        type(SoilLayerElement), allocatable :: colSoilLayers(:)         !! Array of `SoilLayerElement` objects to hold the soil layers
        real(dp)                :: area                                 !! The surface area of the `SoilProfile`
        ! Hydrology and met
        real(dp)                :: n_river                              !! Manning's roughness coefficient for the river
        real(dp)                :: V_pool                               !! Pooled water from top SoilLayer for this timestep [m3 m-2]
        real, allocatable       :: q_precip_timeSeries(:)               !! Time series of precipitation data [m3 m-2 s-1]
        real                    :: q_precip                             !! Precipitation for this time step [m3 m-2 s-1]
        real, allocatable       :: q_evap_timeSeries(:)                 !! Time series of evapotranspiration data [m3 m-2 s-1]
        real                    :: q_evap                               !! Evapotranspiration for this time step [m3 m-2 s-1]
        real(dp)                :: q_in                                 !! Infiltration for this time step: \( q_{\text{in}} = q_{\text{precip}} - q_{\text{evap}} \) [m3 m-2 s-1]
        real(dp)                :: WC_sat                               !! Water content at saturation [m3 m-3]
        real(dp)                :: WC_FC                                !! Water content at field capacity [m3 m-3]
        real(dp)                :: K_s                                  !! Saturated hydraulic conductivity [m s-1]
        real(dp)                :: V_buried                             !! Volume of buried water (from the bottom `SoilLayer`) [m3 m-2]
        ! Soil properties. Sand + silt + clay = 100 %
        real                    :: sandContent                          !! Sand content of the soil [%]
        real                    :: siltContent                          !! Silt content of the soil [%]
        real                    :: clayContent                          !! Clay content of the soil [%]
        real                    :: coarseFragContent                    !! Coarse fragment content of the soil [%]
        real(dp)                :: d_grain                              !! Average grain size [m]
        real(dp)                :: porosity                             !! Soil porosity [%]
        real(dp)                :: bulkDensity                          !! Soil bulk density [kg/m3]
        real(dp)                :: earthwormDensity                     !! Earthworm density [individuals/m2]
        character(len=23)       :: dominantLandUseName                  !! Name of the dominant land use category in this cell
        ! Soil erosion
        real(dp)                :: usle_C                               !! Cover and land management factor time series [-]
        real(dp)                :: usle_K                               !! Soil erodibility factor [t ha h ha-1 MJ-1 mm-1]
        real(dp)                :: usle_LS                              !! Topographic factor [-]
        real(dp)                :: usle_P                               !! Support practice factor [-]
        real(dp)                :: usle_CFRG                            !! Coarse fragment factor [-]
        real(dp)                :: erosivity_a1                         !! Rainfall erosivity seasonal variability parameter
        real(dp)                :: erosivity_a2                         !! Rainfall erosivity seasonal variability parameter
        real(dp)                :: erosivity_a3                         !! Rainfall erosivity seasonal variability parameter
        real(dp)                :: erosivity_I30                        !! Maximum half-hour rainfall [mm/hr]
        real(dp)                :: erosivity_b                          !! Rainfall erosivity parameter
        real(dp), allocatable   :: erodedSediment(:)                    !! Sediment yield eroded on this time step [kg/timestep]
        real(dp)                :: sedimentTransportCapacity            !! Maximum erodable sediment [kg/m2/timestep]
        real(dp), allocatable   :: distributionSediment(:)              !! Distribution to split sediment into
        logical                 :: isUrban = .false.                    !! Is this an urban soil?
        type(Contaminant)       :: m_contaminant                       !! Total contaminant mass in profile [kg]
        type(Contaminant)       :: m_contaminant_in                    !! Contaminant deposited to profile on a time step [kg]
        type(Contaminant)       :: m_contaminant_buried                !! Cumulative contaminant "lost" from the bottom `SoilLayer` [kg]
        type(Contaminant)       :: m_contaminant_eroded                !! Contaminant eroded on current timestep [kg]
      contains
        procedure(createAbstractSoilProfile), deferred :: create
        procedure(updateAbstractSoilProfile), deferred :: update
        procedure(percolateAbstractSoilProfile), deferred :: percolate
        procedure(erodeAbstractSoilProfile), deferred :: erode
        procedure(bioturbationAbstractSoilProfile), deferred :: bioturbation
        procedure(imposeSizeDistributionAbstractSoilProfile), deferred :: imposeSizeDistribution
        procedure(calculateSizeDistributionAbstractSoilProfile), deferred :: calculateSizeDistribution
        procedure(calculateAverageGrainSizeAbstractSoilProfile), deferred :: calculateAverageGrainSize
        procedure(parseInputDataAbstractSoilProfile), deferred :: parseInputData
        procedure(parseNewBatchDataAbstractSoilProfile), deferred :: parseNewBatchData
        procedure(get_m_contaminant_AbstractSoilProfile), deferred :: get_m_contaminant
        procedure(get_C_contaminant_AbstractSoilProfile), deferred :: get_C_contaminant
        procedure :: finalise => finaliseSoilProfile
    end type

    !> Container type for `class(AbstractSoilProfile)` such that a polymorphic
    !! `AbstractSoilProfile` class can be stored in `SoilProfileElement%item`.
    type SoilProfileElement
        class(AbstractSoilProfile), allocatable :: item     
            !! Polymorphic object that can contain any type that extends `AbstractSoilProfile`
    end type 

    abstract interface
        !> Creating the AbstractSoilProfile parses input data and fills the corresponding object
        !! properties, as well as setting up the contained SoilLayers.
        function createAbstractSoilProfile(me, &
                                   x, &
                                   y, &
                                   p, &
                                   n_river, &
                                   area, &
                                   q_precip_timeSeries, &
                                   q_evap_timeSeries) result(r)
            use GlobalsModule
            use ResultModule
            import AbstractSoilProfile
            class(AbstractSoilProfile) :: me                    !! The `AbstractSoilProfile` instance.
            integer             :: x                            !! Containing `GridCell` x position
            integer             :: y                            !! Containing `GridCell` y position
            integer             :: p                            !! `AbstractSoilProfile` reference
            real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
            real(dp)            :: area                         !! The area of the `AbstractSoilProfile`'s surface
            real, allocatable   :: q_precip_timeSeries(:)       !! Precipitation time series [m/timestep]
            real, allocatable   :: q_evap_timeSeries(:)         !! Evaporation time series [m/timestep]
            type(Result)        :: r                            !! `Result` object to return
        end function

        !> Perform the AbstractSoilProfile's simulation for one timestep
        function updateAbstractSoilProfile(me, t, j_contaminant_diffuseSource) result(r)
            use GlobalsModule, only: dp
            use ResultModule, only: Result
            use ContaminantModule
            import AbstractSoilProfile
            class(AbstractSoilProfile), intent(inout) :: me                    !! This AbstractSoilProfile instance
            integer, intent(in)           :: t                                 !! The current time step
            type(Contaminant), intent(in) :: j_contaminant_diffuseSource       !! Diffuse source of contaminant for this timestep
            type(Result)                  :: r                                 !! Result object to return
        end function

        !> Percolate water through the AbstractSoilProfile for the current time step
        function percolateAbstractSoilProfile(me, t, j_contaminant_diffuseSource) result(r)
            use GlobalsModule, only: dp
            use ResultModule, only: Result
            use ContaminantModule
            import AbstractSoilProfile
            class(AbstractSoilProfile)    :: me                                !! This AbstractSoilProfile instance
            integer                       :: t                                 !! The current time step
            type(Contaminant), intent(in) :: j_contaminant_diffuseSource       !! Diffuse source of contaminant for this timestep
            type(Result)                  :: r                                 !! The Result object to return
        end function

        !> Erode soil for the current time step
        function erodeAbstractSoilProfile(me, t) result(r)
            use ResultModule, only: Result
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me           !! This `AbstractSoilProfile` instance
            integer                     :: t            !! The current timestep
            type(Result)                :: r            !! `Result` object to return
        end function

        function bioturbationAbstractSoilProfile(me) result(rslt)
            use ResultModule, only: Result
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me
            type(Result)                :: rslt
        end function

        !> Impose a size class distribution on a total mass to split it up
        !! into separate size classes.
        function imposeSizeDistributionAbstractSoilProfile(me, mass) result(distribution)
            use GlobalsModule, only: dp, C
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me                               !! This `AbstractSoilProfile` instance
            real(dp)                    :: mass                             !! The mass to split into a distribution
            real(dp)                    :: distribution(C%nSizeClassesSpm)
        end function

        function calculateSizeDistributionAbstractSoilProfile(me, clay, silt, sand, enrichClay) result(ssd)
            use GlobalsModule, only: C
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me                               !! This AbstractSoilProfile instance
            real                        :: clay, silt, sand                 !! Percentage clay, silt and sand
            logical                     :: enrichClay                       !! Should we enrich the clay content?
            real                        :: ssd(C%nSizeClassesSpm)           !! Calculated sediment size distribution
        end function

        function calculateAverageGrainSizeAbstractSoilProfile(me, clay, silt, sand) result(d_grain)
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me                               !! This soil profile
            real                        :: clay, silt, sand                 !! Percentage clay, silt and sand
            real                        :: d_grain                          !! The average grain size
        end function

        !> Parses the input data for the `AbstractSoilProfile` from the data file
        function parseInputDataAbstractSoilProfile(me) result(r)
            use ResultModule, only: Result
            import AbstractSoilProfile
            class(AbstractSoilProfile)  :: me                               !! This `AbstractSoilProfile` instance
            type(Result)                :: r                                !! `Result` object to return
        end function

        subroutine parseNewBatchDataAbstractSoilProfile(me)
            import AbstractSoilProfile
            class(AbstractSoilProfile) :: me
        end subroutine

        function get_m_contaminant_AbstractSoilProfile(me) result(m_contaminant)
            use ContaminantModule
            import AbstractSoilProfile
            class(AbstractSoilProfile) :: me
            type(Contaminant) :: m_contaminant
        end function

        function get_C_contaminant_AbstractSoilProfile(me) result(C_contaminant)
            use GlobalsModule, only: dp
            import AbstractSoilProfile
            class(AbstractSoilProfile) :: me
            real(dp), allocatable :: C_contaminant(:,:,:)
        end function
    end interface

contains

    subroutine finaliseSoilProfile(me)
        class(AbstractSoilProfile) :: me
        integer :: i
        call me%m_contaminant%finalise()
        call me%m_contaminant_in%finalise()
        call me%m_contaminant_buried%finalise()
        call me%m_contaminant_eroded%finalise()
        if (allocated(me%colSoilLayers)) then
            do i = 1, size(me%colSoilLayers)
                if (allocated(me%colSoilLayers(i)%item)) then
                    call me%colSoilLayers(i)%item%finalise()
                end if
            end do
            deallocate(me%colSoilLayers)
        end if
        if (allocated(me%q_precip_timeSeries)) deallocate(me%q_precip_timeSeries)
        if (allocated(me%q_evap_timeSeries)) deallocate(me%q_evap_timeSeries)
        if (allocated(me%erodedSediment)) deallocate(me%erodedSediment)
        if (allocated(me%distributionSediment)) deallocate(me%distributionSediment)
    end subroutine
end module
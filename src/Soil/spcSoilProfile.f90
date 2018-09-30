!> Module containing definition of abstract base class `SoilProfile`.
module spcSoilProfile
    use Globals                                                     ! global declarations
    use spcSoilLayer                                                ! use containing object type
    implicit none                                                   ! force declaration of all variables

    !> Abstract base class for `SoilProfile`. Defines properties and methods
    !! required in any implmentation of a `SoilProfile` class. A `SoilProfile`
    !! class acts as a container for a collection of `SoilLayer` objects, which
    !! collectively define the layout of the `SoilProfile`. The `SoilLayer` class
    !! routes, water, eroded soil (and ultimately nanoparticles) through a
    !! layer of soil
    type, abstract, public :: SoilProfile
        ! Setup and dimensions
        character(len=256) :: ref                                   !! A reference name for the object
        integer :: x                                                !! `GridCell` x reference
        integer :: y                                                !! `GridCell` y reference
        integer :: p                                                !! `SoilProfile` reference (not needed currently as only one `SoilProfile` per `GridCell`)
        type(NcGroup) :: ncGroup                                    !! The NetCDF group for this object
        type(SoilLayerElement), allocatable :: colSoilLayers(:)     !! Array of `SoilLayerElement` objects to hold the soil layers
        integer :: nSoilLayers                                !! Number of contained `SoilLayer`s
        real(dp) :: slope                                           !! The slope of the containing `GridCell`
        real(dp) :: area                                            !! The surface area of the `SoilProfile`
        ! Nanomaterial
        real(dp), allocatable :: m_np(:,:,:)                        !! Mass of NM currently in profile [kg]
        ! Hydrology and met
        real(dp) :: n_river                                         !! Manning's roughness coefficient for the river
        real(dp), allocatable :: q_quickflow_timeSeries(:)          !! Time series of runoff (quickflow) data [m3 m-2 s-1]
        real(dp) :: q_quickflow                                     !! Runoff (quickflow) from the hydrological model for this timestep [m/timestep]
        real(dp) :: q_surf                                          !! Surface runoff (different to quickflow) for this time step [m3 m-2 s-1]
        real(dp) :: V_pool                                          !! Pooled water from top SoilLayer for this timestep (not used for anything current) [m3 m-2]
        real(dp), allocatable :: q_precip_timeSeries(:)             !! Time series of precipitation data [m3 m-2 s-1]
        real(dp) :: q_precip                                        !! Precipitation for this time step [m3 m-2 s-1]
        real(dp), allocatable :: q_evap_timeSeries(:)               !! Time series of evapotranspiration data [m3 m-2 s-1]
        real(dp) :: q_evap                                          !! Evapotranspiration for this time step [m3 m-2 s-1]
        real(dp) :: q_in
            !! Infiltration for this time step: \( q_{\text{in}} = q_{\text{precip}} - q_{\text{evap}} \) [m3 m-2 s-1]
        real(dp) :: WC_sat                                          !! Water content at saturation [m3 m-3]
        real(dp) :: WC_FC                                           !! Water content at field capacity [m3 m-3]
        real(dp) :: K_s                                             !! Saturated hydraulic conductivity [m s-1]
        real(dp) :: V_buried                                        !! Volume of buried water (from the bottom `SoilLayer`) [m3 m-2]
        ! Soil texture. Sand + silt + clay = 100 %
        integer :: sandContent                                      !! Sand content of the soil [%]
        integer :: siltContent                                      !! Silt content of the soil [%]
        integer :: clayContent                                      !! Clay content of the soil [%]
        integer :: coarseFragContent                                !! Coarse fragment content of the soil [%]
            !! Total volume of water lost from the bottom of the SoilProfile, over the complete model run [m3 m-2]
        ! Soil erosion
        real(dp), allocatable :: usle_C(:)                          !! Cover and land management factor time series [-]
        real(dp) :: usle_K                                          !! Soil erodibility factor [t ha h ha-1 MJ-1 mm-1]
        real(dp) :: usle_LS                                         !! Topographic factor [-]
        real(dp) :: usle_P                                          !! Support practice factor [-]
        real(dp) :: usle_CFRG                                       !! Coarse fragment factor [-]
        real(dp) :: erosivity_a1                                    !! Rainfall erosivity seasonal variability parameter
        real(dp) :: erosivity_a2                                    !! Rainfall erosivity seasonal variability parameter
        real(dp) :: erosivity_a3                                    !! Rainfall erosivity seasonal variability parameter
        real(dp) :: erosivity_I30                                   !! Maximum half-hour rainfall [mm/hr]
        real(dp) :: erosivity_b                                     !! Rainfall erosivity parameter

        ! ------------------------------------
        ! TODO Q_peak parameters - to be deprecated
        real(dp), allocatable :: usle_alpha_half(:)                 !! Fraction of rainfall falling during maximum half hour [-]
        real(dp) :: usle_area_hru                                   !! Area of the HRU corresponding to the containing `GridCell` [ha]
        real(dp) :: usle_area_sb                                    !! Area of the subbasin corresponding to the containing `GridCell` [km2]
        real(dp) :: usle_L_sb                                       !! Hillslope length for the subbasin [m]
        real(dp) :: usle_n_sb                                       !! Manning's roughness coefficient for the subbasin [-]
        real(dp) :: usle_slp_sb                                     !! Slope of the subbasin [m m-1]
        real(dp) :: usle_slp_ch                                     !! Slope of the channel [m m-1]
        real(dp) :: usle_L_ch                                       !! Hillslope length for the channel [km]
        !--------------------------------------

        real(dp), allocatable :: erodedSediment(:)                  !! Sediment yield eroded on this time step [kg/timestep]
        integer, allocatable :: distributionSediment(:)             !! Distribution to split sediment into

      contains
        procedure(createSoilProfile), deferred :: create                    ! Create the SoilProfile object
        procedure(destroySoilProfile), deferred :: destroy                  ! Remove the SoilProfile object and all contained objects
        procedure(updateSoilProfile), deferred :: update                    ! Perform simulation for given time step
        procedure(percolateSoilProfile), deferred :: percolate              ! Percolate water for given time step
        procedure(erodeSoilProfile), deferred :: erode                      ! Erode soil for given time step
        procedure(imposeSizeDistributionSoilProfile), deferred :: imposeSizeDistribution ! Impose size distribution on mass of sediment
        procedure(parseInputDataSoilProfile), deferred :: parseInputData    ! Parse the data from the input file and store in object properties
    end type

    !> Container type for `class(SoilProfile)` such that a polymorphic
    !! `SoilProfile` class can be stored in `SoilProfileElement%item`.
    type SoilProfileElement
        class(SoilProfile), allocatable :: item     
            !! Polymorphic `SoilProfile` object, which can contain any type that extends `SoilProfile`
    end type 

    abstract interface
        !> Creating the `SoilProfile` parses input data and fills
        !! the corresponding object properties, as well as setting
        !! up the contained `SoilLayer`s.
        function createSoilProfile(me, &
                                   x, &
                                   y, &
                                   p, &
                                   slope, &
                                   n_river, &
                                   area, &
                                   q_quickflow_timeSeries, &
                                   q_precip_timeSeries, &
                                   q_evap_timeSeries) result(r)
            use Globals
            import SoilProfile, Result
            class(SoilProfile)  :: me                           !! The `SoilProfile` instance.
            integer             :: x                            !! Containing `GridCell` x position
            integer             :: y                            !! Containing `GridCell` y position
            integer             :: p                            !! `SoilProfile` reference
            real(dp)            :: slope                        !! Slope of the containing `GridCell` [m m-1]
            real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
            real(dp)            :: area                         !! The area of the `SoilProfile`'s surface
            real(dp), allocatable :: q_quickflow_timeSeries(:)  !! Quickflow timeseries from the hydrological model [m/timestep]
            real(dp), allocatable :: q_precip_timeSeries(:)     !! Precipitation time series [m/timestep]
            real(dp), allocatable :: q_evap_timeSeries(:)       !! Evaporation time series [m/timestep]
            type(Result)        :: r                            !! `Result` object to return
        end function

        !> Destroy the `SoilProfile` object
        function destroySoilProfile(me) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! The `SoilProfile` instance
            type(Result) :: r                                   !! `Result` object to return
        end function

        !> Perform the `SoilProfile`'s simulation for one timestep
        function updateSoilProfile(me, t, j_np_diffuseSource) result(r)
            use Globals
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This `SoilProfile` instance
            integer :: t                                        !! The current time step
            real(dp) :: j_np_diffuseSource(:,:,:)               !! Difffuse source of NM for this timestep
            type(Result) :: r                                   !! `Result` object to return
        end function

        !> Percolate water through the `SoilProfile` for the current time step
        function percolateSoilProfile(me, t, j_np_diffuseSource) result(r)
            import SoilProfile, Result
            class(SoilProfile)  :: me                           !! This `SoilProfile` instance
            integer             :: t                            !! The current time step
            real(dp)            :: j_np_diffuseSource(:,:,:)    !! Difffuse source of NM for this timestep
            type(Result)        :: r                            !! The `Result` object to return
        end function

        !> Erode soil for the current time step
        function erodeSoilProfile(me, t) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This `SoilProfile` instance
            integer :: t                                        !! The current timestep
            type(Result) :: r                                   !! `Result` object to return
        end function

        !> Impose a size class distribution on a total mass to split it up
        !! into separate size classes.
        function imposeSizeDistributionSoilProfile(me, mass) result(distribution)
            use Globals
            import SoilProfile
            class(SoilProfile) :: me                            !! This `SoilProfile` instance
            real(dp) :: mass                                    !! The mass to split into a distribution
            real(dp) :: distribution(C%nSizeClassesSpm)
        end function

        !> Parses the input data for the `SoilProfile` from the data file
        function parseInputDataSoilProfile(me) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This `SoilProfile` instance
            type(Result) :: r                                   !! `Result` object to return
        end function
    end interface
end module

!> Module containing definition of abstract base class SoilProfile.
module spcSoilProfile
    use Globals                                                     ! global declarations
    use spcSoilLayer                                                ! use containing object type
    implicit none                                                   ! force declaration of all variables
    !> Abstract base class for SoilProfile. Defines properties and methods
    !! required in any implmentation of a SoilProfile class. A SoilProfile
    !! class acts as a container for a collection of SoilLayer objects, which
    !! collectively define the layout of the SoilProfile. The SoilLayer class
    !! routes, water, eroded soil (and ultimately nanoparticles) through a
    !! layer of soil
    type, abstract, public :: SoilProfile
        ! Setup and dimensions
        character(len=256) :: ref                                   !! A reference name for the object
        integer :: x                                                !! GridCell x reference
        integer :: y                                                !! GridCell y reference
        integer :: p                                                !! SoilProfile reference (not needed currently as only one SoilProfile per GridCell)
        type(NcGroup) :: ncGroup                                    !! The NetCDF group for this object
        type(SoilLayerElement), allocatable :: colSoilLayers(:)     !! array of SoilLayerElement objects to hold the soil layers
        type(integer) :: nSoilLayers                                !! Number of contained soil layers
        real(dp) :: slope                                           !! The slope of the containing GridCell
        real(dp) :: area                                            !! The surface area of the SoilProfile
        ! Hydrology and met
        real(dp) :: n_river                                         !! Manning's roughness coefficient for the river
        real(dp) :: Q_runoff                                        !! Runoff (quickflow) from the hydrological model for this timestep
        real(dp) :: Q_surf                                          !! Surface runoff (different to quickflow) for this timestep [m3 s-1]
        real(dp), allocatable :: Q_precip_timeSeries(:)             !! Time series of precipitation data [m3 m-2 s-1]
        real(dp) :: Q_precip                                        !! Precipitation for this time step [m3 m-2 s-1]
        real(dp), allocatable :: Q_evap_timeSeries(:)               !! Time series of evapotranspiration data [m3 m-2 s-1]
        real(dp) :: Q_evap                                          !! Evapotranspiration for this time step [m3 m-2 s-1]
        !> Infiltration for this time step: \( Q_{\text{in}} = Q_{\text{precip}} - Q_{\text{evap}} \) [m3 m-2 s-1]
        real(dp) :: Q_in
        real(dp) :: WC_sat                                          !! Water content at saturation [m3 m-3]
        real(dp) :: WC_FC                                           !! Water content at field capacity [m3 m-3]
        real(dp) :: K_s                                             !! Saturated hydraulic conductivity [m s-1]
        ! Soil erosion
        real(dp), allocatable :: usle_C(:)                          !! Cover and land management factor time series [-]
        real(dp) :: usle_K                                          !! Soil erodibility factor [t ha h ha-1 MJ-1 mm-1]
        real(dp) :: usle_LS                                         !! Topographic factor [-]
        real(dp) :: usle_P                                          !! Support practice factor [-]
        real(dp) :: usle_CFRG                                       !! Coarse fragment factor [-]
        real(dp), allocatable :: usle_alpha_half(:)                 !! Fraction of rainfall falling during maximum half hour [-]
        real(dp) :: usle_area_hru                                   !! Area of the HRU corresponding to this GridCell
        real(dp) :: usle_area_sb                                    !! Area of the subbasin corresponding to this GridCell
        real(dp) :: usle_L_sb                                       !! Hillslope length for the subbasin
        real(dp) :: usle_n_sb                                       !! Manning's roughness coefficient for the subbasin
        real(dp) :: usle_slp_sb                                     !! Slope of the subbasin
        real(dp) :: usle_slp_ch                                     !! Slope of the channel
        real(dp) :: usle_L_ch                                       !! Hillslope length for the channel
        real(dp), allocatable :: rusle2015_erodedSediment(:)        !! RUSLE2015 sediment yield for this GridCell (2010): https://esdac.jrc.ec.europa.eu/content/soil-erosion-water-rusle2015
        real(dp), allocatable :: erodedSediment(:)                  !! Sediment yield eroded on this timestep [kg/timestep]
        integer, allocatable :: distributionSediment(:)             !! Distribution to split sediment into

      contains
        procedure(createSoilProfile), deferred :: create            ! create the SoilProfile object. Exposed name: create
        procedure(destroySoilProfile), deferred :: destroy          ! remove the SoilProfile object and all contained objects. Exposed name: destroy
        procedure(updateSoilProfile), deferred :: update            ! Update on every timestep (e.g., perform routing of water through soil)
        procedure(erodeSoilProfile), deferred :: erode              ! Erode soil on a particular timestep
        procedure(imposeSizeDistributionSoilProfile), deferred :: imposeSizeDistribution ! Impose size distribution on mass of sediment
        procedure(parseInputDataSoilProfile), deferred :: parseInputData ! Parse the data from the input file and store in object properties
    end type

    !> Container type for class(SoilProfile) such a polymorphic
    !! SoilProfile class can be stored in SoilProfileElement%item.
    type SoilProfileElement
        class(SoilProfile), allocatable :: item                     !! Polymorphic SoilProfile object, which can contain any type that extends SoilProfile
    end type 

    abstract interface
        !> Creating the SoilProfile parses input data and fills
        !! the corresponding object properties, as well as setting
        !! up the contained SoilLayers.
        function createSoilProfile(me, x, y, p, slope, n_river, area) result(r)
            use Globals
            import SoilProfile, Result
            class(SoilProfile)  :: me                           !! The SoilProfile instance.
            integer             :: x                            !! Containing GridCell x position
            integer             :: y                            !! Containing GridCell y position
            integer             :: p                            !! SoilProfile reference
            real(dp)            :: slope                        !! Slope of the containing GridCell [m/m]
            real(dp)            :: n_river                      !! Manning's roughness coefficient for the GridCell's rivers [-]
            real(dp)            :: area                         !! The area of the SoilProfile's surface
            type(Result)        :: r                            !! Result object to return
        end function

        !> Destroy the SoilProfile object
        function destroySoilProfile(me) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! The SoilProfile instance
            type(Result) :: r                                   !! Result object to return
        end function

        !> Perform the SoilProfile's simulation for one timestep
        function updateSoilProfile(me, t, Q_runoff) result(r)
            use Globals
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This SoilProfile instance
            integer :: t                                        !! The current timestep
            real(dp) :: Q_runoff                                !! Runoff (quickflow) generated on this timestep
            type(Result) :: r                                   !! Result object to return
        end function

        !> Erode soil for the current timestep
        function erodeSoilProfile(me, t) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This SoilProfile instance
            integer :: t                                        !! The current timestep
            type(Result) :: r                                   !! Result object to return
        end function

        !> Impose a size class distribution on a total mass to split it up
        !! into separate size classes. If a size distribution has been specified
        !! for this SoilProfile, use that, otherwise, use the global size distribution.
        function imposeSizeDistributionSoilProfile(me, mass) result(distribution)
            use Globals
            import SoilProfile
            class(SoilProfile) :: me
            real(dp) :: mass
            real(dp) :: distribution(C%nSizeClassesSpm)
        end function

        !> Parses the input data for the SoilProfile from the data file
        function parseInputDataSoilProfile(me) result(r)
            import SoilProfile, Result
            class(SoilProfile) :: me                            !! This SoilProfile instance
            type(Result) :: r                                   !! Result object to return
        end function
    end interface
end module

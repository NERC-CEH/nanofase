!> Module containing definition of abstract base class SoilLayer.
module spcSoilLayer
    use Globals                                                     ! global declarations
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    implicit none                                                   ! force declaration of all variables

    !> Abstract base class for SoilLayer object. Defines properties and
    !! methods required in any implementation of a SoilLayer class. The
    !! SoilLayer class routes water and ultimately nanoparticles through
    !! a layer of soil.
    type, abstract, public :: SoilLayer
        ! Setup, dimensions and metadata
        character(len=256) :: ref                                   !! Reference name for this SoilLayer
        integer :: x                                                !! Containing GridCell x index
        integer :: y                                                !! Containing GridCell y index
        integer :: p                                                !! Containing SoilProfile index
        integer :: l                                                !! Layer index
        real(dp) :: depth                                           !! Layer depth [m]
        type(NcGroup) :: ncGroup                                    !! NetCDF group for this object
        ! Soil properties
        real(dp) :: bdens                                           !! Bulk density [kg/m3]
        real(dp) :: pH                                              !! Porewater pH
        real(dp) :: SOM                                             !! Soil organic matter content [% w/w]
        ! Hydrology
        real(dp) :: Q_in                                            !! Inflow to this SoilLayer [m3 m-2 s-1]
        real(dp) :: V_w                                             !! Volume of water currently in layer [m3 m-2]
        real(dp) :: V_pool                                          !! Volume of water above V_sat to be pooled into layer above [m3 m-2]
        real(dp) :: V_excess                                        !! Volume of water above V_FC that can percolate to the next layer [m3 m-2]
        real(dp) :: V_perc                                          !! Volume of water percolating to next layer on a given time step [m3 m-2]
        real(dp) :: V_sat                                           !! Water content at saturation [m3 m-2]
        real(dp) :: V_FC                                            !! Water content at field capacity [m3 m-2]
        real(dp) :: K_s                                             !! Saturated hydraulic conductivity [m s-1]
      contains
        procedure(createSoilLayer), deferred :: create              ! Create the SoilLayer object
        procedure(destroySoilLayer), deferred :: destroy            ! Remove the SoilLayer object and all contained objects
        procedure(updateSoilLayer), deferred :: update              ! Update on every timestep (e.g., perform soil percolation)
        procedure(parseInputDataSoilLayer), deferred :: parseInputData ! Parse data from the input file for this SoilLayer
    end type

    !> Container type for class(SoilLayer) such that a polymorphic
    !! SoilLayer class can be stored in SoilLayerElement%item.
    type SoilLayerElement
        class(SoilLayer), allocatable :: item                       !! Polymorphic SoilLayer object, which can contain any type that extends SoilLayer
    end type 

    abstract interface
        !> Create this SoilLayer
        function createSoilLayer(me, x, y, p, l, WC_sat, WC_FC, K_s) result(r)
            use Globals
            import SoilLayer, Result
            class(SoilLayer) :: me                          !! This SoilLayer instance
            integer, intent(in) :: x                        !! Containing GridCell x index
            integer, intent(in) :: y                        !! Containing GridCell y index
            integer, intent(in) :: p                        !! Containing SoilProfile index
            integer, intent(in) :: l                        !! Layer index
            real(dp), intent(in) :: WC_sat                  !! Water content at saturation [m3/m3]
            real(dp), intent(in) :: WC_FC                   !! Water content at field capacity [m3/m3]
            real(dp), intent(in) :: K_s                     !! Saturated hydraulic conductivity [m/s]
            type(Result) :: r                               !! The Result object to return
        end function

        !> Destroy this SoilLayer
        function destroySoilLayer(me) result(r)
            import SoilLayer, Result
            class(SoilLayer) :: me                          !! This SoilLayer1 instance
            type(Result) :: r                               !! The Result object to return
        end function

        !> Update the SoilLayer on a given timestep
        function updateSoilLayer(me, t, Q_in) result(r)
            use Globals
            import SoilLayer, Result
            class(SoilLayer) :: me                          !! This SoilLayer1 instance
            integer :: t                                    !! The current timestep
            real(dp) :: Q_in                                !! Water into the layer on this time step [m3/s]
            type(Result) :: r                               !! The Result object to return
        end function

        !> Parse the data input for this SoilLayer
        function parseInputDataSoilLayer(me) result(r)
            import SoilLayer, Result
            class(SoilLayer) :: me                          !! This SoilLayer1 instance
            type(Result) :: r
                !! The Result object to return any errors relating to the input data file
        end function
    end interface
end module
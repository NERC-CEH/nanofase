!> Module containing definition of abstract base class `SoilLayer`.
module spcSoilLayer
    use Globals                                                     ! Global definitions and constants
    use mo_netcdf                                                   ! NetCDF input/output
    use ResultModule, only: Result                                  ! Result object to pass errors
    use classBiotaSoil
    implicit none

    !> Abstract base class for `SoilLayer` object. Defines properties and
    !! methods required in any implementation of a `SoilLayer` class. The
    !! `SoilLayer` class routes water and ultimately nanoparticles through
    !! a layer of soil.
    type, abstract, public :: SoilLayer
        ! Setup, dimensions and metadata
        character(len=256) :: ref                                   !! Reference name for this `SoilLayer`
        integer :: x                                                !! Containing `GridCell` x index
        integer :: y                                                !! Containing `GridCell` y index
        integer :: p                                                !! Containing `SoilProfile` index
        integer :: l                                                !! Layer index
        real(dp) :: depth                                           !! Layer depth [m]
        real(dp) :: area                                            !! Area of the containing SoilProfile [m2]
        real(dp) :: volume                                          !! Volume of the soil layer [m3]
        type(NcGroup) :: ncGroup                                    !! NetCDF group for this object
        ! Soil properties
        real(dp) :: pH                                              !! Porewater pH
        real(dp) :: SOM                                             !! Soil organic matter content [% w/w]
        ! Nanomaterials
        real(dp), allocatable :: m_np(:,:,:)                        !! Mass of NM currently in layer [kg]
        real(dp), allocatable :: m_np_perc(:,:,:)                   !! Mass of NM percolating to layer below on given timestep [kg]
        real(dp), allocatable :: m_np_eroded(:,:,:)                 !! Mass of NM eroded on given timestep [kg]
        real(dp), allocatable :: C_np(:,:,:)                        !! Mass concentration of NM [kg/kg soil]
        real(dp), allocatable :: m_transformed(:,:,:)
        real(dp), allocatable :: m_transformed_perc(:,:,:)
        real(dp), allocatable :: m_transformed_eroded(:,:,:)
        real(dp), allocatable :: C_transformed(:,:,:)
        real(dp), allocatable :: m_dissolved
        real(dp), allocatable :: m_dissolved_perc
        real(dp), allocatable :: m_dissolved_eroded
        real(dp), allocatable :: C_dissolved
        ! Hydrology
        real(dp) :: Q_in                                            !! Inflow to this `SoilLayer` [m3 m-2 s-1]
        real(dp) :: V_w                                             !! Volume of water currently in layer [m3 m-2]
        real(dp) :: V_pool                                          !! Volume of water above `V_sat` to be pooled into layer above [m3 m-2]
        real(dp) :: V_excess                                        !! Volume of water above `V_FC` that can percolate to the next layer [m3 m-2]
        real(dp) :: V_perc                                          !! Volume of water percolating to next layer on a given time step [m3 m-2]
        real(dp) :: V_sat                                           !! Water content at saturation [m3 m-2]
        real(dp) :: V_FC                                            !! Water content at field capacity [m3 m-2]
        real(dp) :: K_s                                             !! Saturated hydraulic conductivity [m s-1]
        real(dp) :: bulkDensity                                     !! Bulk density [kg m-3]
        real(dp) :: d_grain                                         !! Average grain diameter [m]
        real(dp) :: porosity                                        !! Porosity [-]
        real(dp) :: earthwormDensity                                !! Earthworm density [individuals/layer]
        ! NM transformations
        real :: alpha_att                                           !! Attachment efficiency to soil matrix [-]
        real, allocatable :: k_att(:)                               !! Attachment rate to soil matrix [s-1]
        ! Biota
        integer :: nBiota = 0
        integer, allocatable :: biotaIndices(:)
        class(BiotaSoil), allocatable :: biota(:)
      contains
        procedure(createSoilLayer), deferred :: create              ! Create the SoilLayer object
        procedure(updateSoilLayer), deferred :: update              ! Update on every timestep (e.g., perform soil percolation)
        procedure(addPooledWaterSoilLayer), deferred :: addPooledWater  ! Add pooled water to this layer
        procedure(erodeSoilLayer), deferred :: erode                ! Erode NM from the top soil layer
        procedure(parseInputDataSoilLayer), deferred :: parseInputData ! Parse data from the input file for this SoilLayer
        procedure(calculateBioturbationRateSoilLayer), deferred :: calculateBioturbationRate
        ! Non-deferred procedures
        procedure :: setV_pool
    end type

    !> Container type for `class(SoilLayer)` such that a polymorphic
    !! `SoilLayer` class can be stored in `SoilLayerElement%item`.
    type SoilLayerElement
        class(SoilLayer), allocatable :: item
            !! Polymorphic `SoilLayer` object, which can contain any type that extends `SoilLayer`
    end type 

    abstract interface
        !> Create this `SoilLayer`
        function createSoilLayer(me, x, y, p, l, WC_sat, WC_FC, K_s, area, &
            bulkDensity, d_grain, porosity, earthwormDensity) result(r)
            use Globals, only: dp
            use ResultModule, only: Result
            import SoilLayer
            class(SoilLayer) :: me                          !! This `SoilLayer` instance
            integer, intent(in) :: x                        !! Containing `GridCell` x index
            integer, intent(in) :: y                        !! Containing `GridCell` y index
            integer, intent(in) :: p                        !! Containing `SoilProfile` index
            integer, intent(in) :: l                        !! Layer index
            real(dp), intent(in) :: WC_sat                  !! Water content at saturation [m3/m3]
            real(dp), intent(in) :: WC_FC                   !! Water content at field capacity [m3/m3]
            real(dp), intent(in) :: K_s                     !! Saturated hydraulic conductivity [m/s]
            real(dp), intent(in) :: area                    !! Area of the containing SoilProfile [m2]
            real(dp), intent(in) :: bulkDensity             !! Bulk density [kg/m3]
            real(dp), intent(in) :: d_grain                 !! Average grain diameter [m]
            real(dp), intent(in) :: porosity                !! Porosity [-]
            real(dp), intent(in) :: earthwormDensity        !! Earthworm density [individuals/layer]
            type(Result) :: r                               !! The `Result` object to return
        end function

        !> Update the `SoilLayer` on a given timestep
        function updateSoilLayer(me, t, q_in, m_np_in, m_transformed_in, m_dissolved_in) result(r)
            use ResultModule, only: Result
            use Globals, only: dp
            import SoilLayer
            class(SoilLayer) :: me                          !! This `SoilLayer` instance
            integer :: t                                    !! The current time step
            real(dp) :: q_in                                !! Water into the layer on this time step [m/timestep]
            real(dp) :: m_np_in(:,:,:)                      !! NM into the layer on this time step [kg/timestep]
            real(dp) :: m_transformed_in(:,:,:)             !! Transformed NM into the layer on this time step [kg/timestep]
            real(dp) :: m_dissolved_in                      !! Dissolved species into the layer on this time step [kg/timestep]
            type(Result) :: r                               !! The `Result` object to return, with no data
        end function

        !> Add a volume \( V_{\text{pool}} \) of pooled water to the layer.
        !! No percolation occurs as pooled water never really leaves the `SoilLayer`.
        function addPooledWaterSoilLayer(me, V_pool) result(r)
            use ResultModule, only: Result
            use Globals, only: dp
            import SoilLayer
            class(SoilLayer) :: me                          !! This `SoilLayer` instance
            real(dp) :: V_pool                              !! Volume of pooled water to add, \( V_{\text{pool}} \) [m3/m2]
            type(Result) :: r                               !! The `Result` object to return, with no data
        end function

        !> Erode NM from this soil layer
        function erodeSoilLayer(me, erodedSediment, bulkDensity, area) result(r)
            use ResultModule, only: Result
            use Globals, only: dp, C
            import SoilLayer
            class(SoilLayer) :: me
            real(dp) :: erodedSediment(:)
            real(dp)            :: bulkDensity
            real(dp)            :: area
            type(Result) :: r
        end function

        function calculateBioturbationRateSoilLayer(me) result(bioturbationRate)
            use Globals, only: dp
            import SoilLayer
            class(SoilLayer) :: me
            real(dp) :: bioturbationRate
        end function

        !> Parse the data input for this SoilLayer
        function parseInputDataSoilLayer(me) result(r)
            use ResultModule, only: Result
            use Globals, only: dp
            import SoilLayer
            class(SoilLayer) :: me                          !! This `SoilLayer` instance
            type(Result) :: r
                !! The Result object to return any errors relating to the input data file
        end function
    end interface

  contains
    !> Set the volume of pooled water, \( V_{\text{pool}} \)
    subroutine setV_pool(me, V_pool)
        class(SoilLayer) :: me                              !! This `SoilLayer` instance
        real(dp) :: V_pool                                  !! The pooled water \( V_{\text{pool}} \) [m3/m2]
        me%V_pool = V_pool
    end subroutine
end module
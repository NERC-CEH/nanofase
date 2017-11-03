module spcSoilLayer
                                                                    ! superclass for SoilLayer subclasses
                                                                    ! defines properties and methods required in any implmentation
                                                                    ! of a SoilLayer class
                                                                    ! the SoilLayer class routes, water, eroded soil (and ultimately nanoparticles) through a layer of soil
                                                                    ! IMPORTED MODULES
                                                                    ! Description
                                                                    ! -----------
    use Globals                                                     ! global declarations
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    implicit none                                                   ! force declaration of all variables

    type, abstract, public :: SoilLayer                             ! type declaration for superclass
        character(len=256) :: ref                                   ! a name for the object
                                                                    ! PROPERTIES
                                                                    ! Description
                                                                    ! -----------
        type(integer) :: GridX                                      ! enclosing grid cell x reference
        type(integer) :: GridY                                      ! enclosing grid cell y reference
        type(real(dp)) :: depth                                     ! the layer depth (m)
        type(real(dp)) :: bdens                                     ! the bulk density (kg/m3)
        type(real(dp)) :: pH                                        ! the porewater pH
        type(real(dp)) :: SOM                                       ! the soil organic matter content (% w/w)
                                                                    ! CONTAINED OBJECTS
                                                                    ! Description
                                                                    ! -----------
      contains
                                                                    ! METHODS
                                                                    ! Description
                                                                    ! -----------
        procedure(createSoilLayer), deferred :: create              ! create the SoilLayer object. Exposed name: create
        procedure(destroySoilLayer), deferred :: destroy            ! remove the SoilLayer object and all contained objects. Exposed name: destroy
        procedure(updateSoilLayer), deferred :: update              ! Update on every timestep (e.g., perform routing of water through soil)
    end type

    type SoilLayerElement                                           ! container type for class(SoilLayer), the actual type of the SoilLayer class
        class(SoilLayer), allocatable :: item                       ! a variable of type SoilLayer can be of any object type inheriting from the
    end type 

    abstract interface
        function createSoilLayer(me) result(r)
            import SoilLayer, Result
            class(SoilLayer) :: me                                  ! The SoilLayer instance.
            type(Result) :: r
        end function
        function destroySoilLayer(me) result(r)
            import SoilLayer, Result
            class(SoilLayer) :: me                                 ! The SoilLayer instance.
            type(Result) :: r
        end function
        function updateSoilLayer(me) result(r)
            import SoilLayer, Result
            class(SoilLayer) :: me                                 ! The SoilLayer instance.
            type(Result) :: r
        end function
    end interface
end module
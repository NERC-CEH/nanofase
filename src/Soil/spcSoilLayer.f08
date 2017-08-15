module spcSoilLayer
                                                                     ! superclass for SoilLayer subclasses
                                                                     ! defines properties and methods required in any implmentation
                                                                     ! of a SoilLayer class
                                                                     ! the SoilLayer class routes, water, eroded soil (and ultimately nanoparticles) through a layer of soil
                                                                     ! IMPORTED MODULES
                                                                     ! Description
                                                                     ! -----------
  use Globals                                                        ! global declarations
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
  implicit none                                                      ! force declaration of all variables
  type, abstract, public :: SoilLayer                                ! type declaration for superclass
    character(len=256) :: name                                       ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(integer) :: GridX                                           ! enclosing grid cell x reference
    type(integer) :: GridY                                           ! enclosing grid cell y reference
    integer :: nSPMSC                                                ! number of eroded soil size classes
    type(real(dp)) :: depth                                          ! the layer depth (m)
    type(real(dp)) :: bdens                                          ! the bulk density (kg/m3)
    type(real(dp)) :: pH                                             ! the porewater pH
    type(real(dp)) :: SOM                                            ! the soil organic matter content (% w/w)
    integer, private :: allst                                        ! array allocation status
    integer, private :: err                                          ! ?
    type(Result), private :: r                                       ! Result object for returning from functions, for error checking
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public, deferred :: create => createSoilLayer         ! create the SoilLayer object. Exposed name: create
    procedure, public, deferred :: destroy => destroySoilLayer       ! remove the SoilLayer object and all contained objects. Exposed name: destroy
    procedure, public, deferred :: routing => routingSoilLayer       ! route water and suspended solids. Exposed name: routing
  end type
  abstract interface
    function createSoilLayer(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilLayer instance.
      type(Result) :: r
    end function
    function destroySoilLayer(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilLayer instance.
      type(Result) :: r
    end function
    function routingSoilLayer(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilLayer instance.
      type(Result) :: r
    end function
  end abstract interface
end module

module spcSoilProfile
                                                                     ! superclass for SoilProfile subclasses
                                                                     ! defines properties and methods required in any implmentation
                                                                     ! of a SoilProfile class
                                                                     ! a SoilProfile class acts as a container for a collection of SoilLayer objects which collectively define the
                                                                     ! layout of the SoilProfile
                                                                     ! the SoilLayer class routes, water, eroded soil (and ultimately nanoparticles) through a layer of soil
                                                                     ! IMPORTED MODULES
                                                                     ! Description
                                                                     ! -----------
  use Globals                                                        ! global declarations
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  use spcSoilLayer                                                   ! use containing object type
  ! DO WE NEED TO USE ALL CONTAINING OBJECT TYPES?
  implicit none                                                      ! force declaration of all variables
  type SoilLayerElement                                              ! container type for class(SoilLayer), the actual type of the SoilLayer class
    class(SoilLayer), allocatable :: item                            ! a variable of type SoilLayer can be of any object type inheriting from the
  end type                                                           ! SoilLayer superclass
  type, abstract, public :: SoilProfile                              ! type declaration for superclass
    character(len=256) :: name                                       ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
    type(integer) :: GridX                                           ! grid cell x reference
    type(integer) :: GridY                                           ! grid cell y reference
    type(SoilLayerElement), allocatable :: colSoilLayers(:)          ! array of SoilLayerElement objects to hold the soil layers
    type(integer) :: nSoilLayers                                     ! Number of contained soil layers
    integer :: nSPMSC                                                ! number of eroded soil size classes
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
    procedure(createSoilProfile), deferred :: create                 ! create the SoilProfile object. Exposed name: create
    procedure(destroySoilProfile), deferred :: destroy               ! remove the SoilProfile object and all contained objects. Exposed name: destroy
    procedure(routingSoilProfile), deferred :: routing               ! route water and suspended solids through all SoilLayer objects. Exposed name: routing
  end type
  abstract interface
    function createSoilProfile(Me) result(r)
      import SoilProfile, Result
      class(SoilProfile) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
    function destroySoilProfile(Me) result(r)
      import SoilProfile, Result
      class(SoilProfile) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
    function routingSoilProfile(Me) result(r)
      import SoilProfile, Result
      class(SoilProfile) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
  end interface
end module

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
  type, public, extends(SoilProfile) :: SoilProfile1                 ! type declaration for class
                                                                     ! -----------
  contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public, deferred :: create => createSoilProfile1      ! create the SoilProfile object. Exposed name: create
    procedure, public, deferred :: destroy => destroySoilProfile1    ! remove the SoilProfile object and all contained objects. Exposed name: destroy
    procedure, public, deferred :: routing => routingSoilProfile1    ! route water and suspended solids through all SoilLayer objects. Exposed name: routing
  end type
  abstract interface
    function createSoilProfile1(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
    function destroySoilProfile1(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
    function routingSoilProfile1(Me) result(r)
      class(GridCell) :: Me                                          ! The SoilProfile instance.
      type(Result) :: r
    end function
  end abstract interface
end module
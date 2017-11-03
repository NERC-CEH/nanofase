!> A SoilProfile class acts as a container for a collection of
!! SoilLayer objects, which collectively define the layout of
!! the SoilProfile.
module classSoilProfile1
    use Globals                                                 ! global declarations
    use UtilModule                                              ! Useful functions
    use mo_netcdf                                               ! input/output handling
    use ResultModule                                            ! error handling classes, required for
    use spcSoilProfile                                          ! use containing object type
    implicit none                                               ! force declaration of all variables

    type, public, extends(SoilProfile) :: SoilProfile1

      contains
        procedure :: create => createSoilProfile1               ! Create the SoilProfile object. Exposed name: create
        procedure :: destroy => destroySoilProfile1             ! Remove the SoilProfile object and all contained objects. Exposed name: destroy
        procedure :: update => updateSoilProfile1               ! Update on every timestep (e.g., perform routing of water through soil)
    end type

  contains
    !> Creating the SoilProfile parses input data and fills
    !! the corresponding object properties, as well as setting
    !! up the contained SoilLayers.
    function createSoilProfile1(me, x, y, p, slope, n_river) result(r)
        class(SoilProfile1) :: me           !! The SoilProfile instance.
        integer :: x                        !! Containing GridCell x position
        integer :: y                        !! Containing GridCell y position
        integer :: p                        !! SoilProfile reference (redundant for now as only one SoilProfile per GridCell)
        real(dp) :: slope                   !! Slope of the containing GridCell [m/m]
        real(dp) :: n_river                 !! Manning's roughness coefficient for the GridCell's rivers [-]
        type(Result) :: r                   !! The Result object
        me%x = x
        me%y = y
        me%p = p
        me%ref = trim(ref("SoilProfile", x, y, p))
    end function

    function destroySoilProfile1(me) result(r)
        class(SoilProfile1) :: me                               ! The SoilProfile instance.
        type(Result) :: r
    end function

    function updateSoilProfile1(me) result(r)
        class(SoilProfile1) :: me                               ! The SoilProfile instance.
        type(Result) :: r
        ! NEEDS QRUNOFF
    end function
end module

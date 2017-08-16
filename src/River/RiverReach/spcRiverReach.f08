module spcRiverReach
                                                                     ! superclass for RiverReach subclasses
                                                                     ! defines properties and methods required in any implmentation
                                                                     ! of a RiverReach class
                                                                     ! a RiverReach class computes water velocity, depth and sediment dynamics for
                                                                     ! a defined length of (homogeneous) flowing water
                                                                     ! IMPORTED MODULES
                                                                     ! Description
                                                                     ! -----------
    use Globals                                                      ! global declarations
    use netcdf                                                       ! input/output handling
    use mo_netcdf                                                    ! input/output handling
    use ResultModule                                                 ! error handling classes, required for
    use ErrorInstanceModule                                          ! generation of trace error messages
    use spcBedSediment
    implicit none                                                    ! force declaration of all variables
    type BedSedimentElement                                          ! container type for class(BedSediment), the actual type of the bed sediment superclass
      class(BedSediment), allocatable :: item
    end type
    type, abstract, public :: RiverReach                             ! type declaration for superclass
      character(len=256) :: name                                     ! a name for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
      real(dp) :: S                                                  ! Slope of reach [m/m]. Specified in inputs.
      real(dp) :: Q                                                  ! Discharge [m3/s]. Specified in inputs.
      real(dp) :: W                                                  ! Width of reach [m]. Computed on each timestep.
      real(dp) :: D                                                  ! Depth of water column [m]. Computed on each timestep.
      real(dp) :: v                                                  ! Water velocity [m s-1]. Computed on each timestep.
      real(dp) :: volume                                             ! The volume of water in the reach [m3].
      ! Specified globally so don't need to be defined here. Access via C%nSizeClassesSPM and C%d_spm:
      ! integer :: n_s_classes                                         ! Number of sediment size classes. Specified globally.
      ! real(dp), allocatable :: d_spm(:)                              ! Sediment particle diameters [m]. Specified globally.
      real(dp), allocatable :: rho_spm(:)                            ! Sediment particle densities [kg m-3]. Specified globally.
      real(dp), allocatable :: k_settle(:)                           ! Sediment settling rates [s-1]. Computed on each timestep.
      real(dp), allocatable :: W_spm(:)                              ! Sediment settling velocities [m s-1]. Computed on each timestep.
      real(dp) :: n                                                  ! Manning's roughness coefficient, for natural streams and major rivers.
                                                                     ! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).
      integer, private :: allst                                      ! array allocation status
                                                                     ! TODO: Add allst handling to error handler
      ! type(Result), private :: r                                     ! Result object for returning from functions, for error checking
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
      type(BedSedimentElement), allocatable :: objBedSediment        ! contained BedSediment object
    contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
      procedure(createRiverReach), deferred :: create                ! create the RiverReach object. Exposed name: create
      procedure(destroyRiverReach), deferred :: destroy              ! remove the RiverReach object and all contained objects. Exposed name: destroy
                                                                     ! Interface names have to be globally unique as interfaces aren't aligned with modules
                                                                     ! PRIVATE ROUTINES
                                                                     ! Description
                                                                     ! -----------
      procedure(initDimensions), deferred :: initDimensions
      procedure(simulate), deferred :: simulate                      ! DUMMY FUNCTION for the time being
      procedure(calculateDepth), deferred :: calculateDepth          ! compute the depth of the water column
      procedure(calculateWidth), deferred :: calculateWidth          ! compute the width of the reach
      procedure(calculateVelocity), deferred :: calculateVelocity    ! compute the water velocity
      procedure(calculateSettlingVelocity), deferred :: calculateSettlingVelocity ! compute the sediment settling velocities
                                                                     ! GETTERS
      procedure :: getVolume                                         ! Should getters all be non-abstract, seeing as all they're
                                                                     ! doing is returning a type variable?
    end type
  abstract interface
    function createRiverReach(Me) result(r)
      use Globals                                                    ! Interface blocks don't have access to used modules in this module, thus we need to use/import again
      import RiverReach, Result, Result0D
      class(RiverReach) :: Me                                        ! The RiverReach instance.
      type(Result0D) :: D                                            ! Depth [m].
      type(Result0D) :: v                                            ! Water velocity [m/s].
      type(Result0D) :: W                                            ! River width [m].
      integer :: i                                                   ! Loop iterator.
      type(Result) :: r                                              ! The Result object.
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
      real(dp), allocatable :: spmDensities(:)                       ! Array of sediment particle densities for each size class
    end function
    function destroyRiverReach(Me) result(r)
      import RiverReach, Result
      class(RiverReach) :: Me                                        ! The RiverReach instance
      type(Result) :: r                                              ! The Result object to return
    end function
    function initDimensions(me, Q) result(r)
      use Globals
      import RiverReach, Result
      class(RiverReach) :: me
      real(dp) :: Q
      type(Result) :: r
    end function
    function simulate(me, dQ, dSPM) result(r)
      use Globals
      import RiverReach, Result
      class(RiverReach) :: me
      real(dp) :: dQ
      real(dp) :: dSPM(:)
      type(Result) :: r
    end function
    pure function calculateDepth(Me, W, S, Q) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: Me                          ! The RiverReach instance.
      real(dp), intent(in) :: W                                    ! River width \( W \) [m].
      real(dp), intent(in) :: S                                    ! River slope \( S \) [-].
      real(dp), intent(in) :: Q                                    ! Flow rate \( Q \) [m**3/s].
      real(dp) :: D_i                                              ! The iterative river depth \( D_i \) [m].
      real(dp) :: f                                                ! The function to find roots for \( f(D) \).
      real(dp) :: df                                               ! The derivative of \( f(D) \) with respect to \( D \).
      real(dp) :: alpha                                            ! Constant extracted from f and df
      integer :: i                                                 ! Loop iterator to make sure loop isn't endless.
      integer :: iMax                                              ! Maximum number of iterations before error.
      type(ErrorInstance) :: error                                 ! Variable to store error in.
      character(len=100) :: iChar                                  ! Loop iterator as character (for error message).
      character(len=100) :: fChar                                  ! f(D) value as character (for error message).
      type(Result0D) :: r                                          ! The result object.
    end function
    function calculateWidth(Me, Q) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: Me                            ! The RiverReach instance.
      real(dp), intent(in) :: Q                                      ! Flow rate \( Q \) [m**3/s].
      type(ErrorInstance) :: error                                   !! Variable to store error in.
      type(Result0D) :: r                                            !! Result object to return.
    end function
    pure function calculateVelocity(me, D, Q, W) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: me                            ! The RiverReach instance.
      real(dp), intent(in) :: D                                      ! River depth \( D \) [m].
      real(dp), intent(in) :: Q                                      ! Flow rate \( Q \) [m**3/s].
      real(dp), intent(in) :: W                                      ! River width \( W \) [m].
      type(Result0D) :: r                                            ! The result object.
    end function
    function calculateSettlingVelocity(Me, d, rho_spm, T) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: me                            ! The RiverReach instance.
      real(dp), intent(in) :: d                                      ! Sediment particle diameter.
      real(dp), intent(in) :: rho_spm                                ! Sediment particle density.
      real(dp), intent(in) :: T                                      ! Temperature [C]
      real(dp) :: dStar                                              ! Dimensionless particle diameter.
      real(dp) :: W_spm                                              ! Calculated settling velocity.
      type(Result0D) :: r                                            ! The Result object.
    end function
  end interface

  contains
    function getVolume(me) result(volume)
      class(RiverReach) :: me
      real(dp) :: volume
      volume = me%volume
    end function
end module

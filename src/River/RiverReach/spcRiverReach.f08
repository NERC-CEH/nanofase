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
      character(len=256) :: ref                                      ! a reference for the object
                                                                     ! PROPERTIES
                                                                     ! Description
                                                                     ! -----------
      real(dp) :: S                                                  ! Slope of reach [m/m]. Specified in inputs.
      real(dp) :: Qin                                                ! Inflow from upstream reach [m3]
      real(dp) :: Qout                                               ! Outflow to the next reach [m3]
      real(dp) :: Qrunoff                                            ! Runoff from hydrological model [m3]
      real(dp), allocatable :: spmIn(:)                              ! Inflow SPM from upstream reach [kg]
      real(dp), allocatable :: spmOut(:)                             ! Outflow SPM to next reach [kg]
      real(dp), allocatable :: m_spm(:)                              ! Mass of the SPM currently in reach [kg]
      real(dp) :: W                                                  ! Width of reach [m]. Computed on each timestep.
      real(dp) :: D                                                  ! Depth of water column [m]. Computed on each timestep.
      real(dp) :: v                                                  ! Water velocity [m s-1]. Computed on each timestep.
      real(dp) :: l                                                  ! Length of the river, without meandering factor [m].
      real(dp) :: f_m = 1                                            ! Meandering factor used for calculating river volume. Default to 1 (no meandering).
      real(dp) :: area                                               ! The cross-sectional area of water in the reach [m2].
      real(dp) :: volume                                             ! The volume of water in the reach [m3].
      real(dp), allocatable :: rho_spm(:)                            ! Sediment particle densities [kg m-3]. Specified globally.
      real(dp) :: n                                                  ! Manning's roughness coefficient, for natural streams and major rivers.
                                                                     ! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).
      integer :: allst                                               ! array allocation status, must be public so RiverReach1,2 etc can use it
                                                                     ! TODO: Add allst handling to error handler
                                                                     ! CONTAINED OBJECTS
                                                                     ! Description
                                                                     ! -----------
      type(BedSedimentElement), allocatable :: objBedSediment        ! contained BedSediment object
      real(dp) :: T                                                  ! Temperature [C]
      type(NcGroup) :: ncGroup                                       ! The NETCDF group for this RiverReach
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
      ! procedure(updateDimensions), deferred :: updateDimensions      ! Update the dimensions of the RiverReach on each timestep
      procedure(update), deferred :: update                          ! Run the RiverReach simulation for one timestep
      ! procedure(simulate), deferred :: simulate                      ! DUMMY FUNCTION for the time being
      procedure(calculateDepth), deferred :: calculateDepth          ! compute the depth of the water column
      procedure(calculateWidth), deferred :: calculateWidth          ! compute the width of the reach
      procedure(calculateVelocity), deferred :: calculateVelocity    ! compute the water velocity
      procedure(calculateSettlingVelocity), deferred :: calculateSettlingVelocity ! compute the sediment settling velocities
      procedure(calculateVolume), deferred :: calculateVolume        ! Calculate the volume of the reach
      procedure(calculateArea), deferred :: calculateArea            ! Calculate the area of the reach's cross-section
                                                                     ! GETTERS
      procedure :: getVolume => getVolumeRiverReach                  ! Should getters all be non-abstract, seeing as all they're
      procedure :: getQOut => getQOutRiverReach                      ! doing is returning a type variable?
      procedure :: getSpmOut => getSpmOutRiverReach                  ! Return the SPM discharge
    end type
  abstract interface
    function createRiverReach(me, x, y, s, r, l, Qrunoff) result(res)
      use Globals                                                    ! Interface blocks don't have access to used modules in this module, thus we need to use/import again
      import RiverReach, Result, Result0D
      class(RiverReach) :: me                                        ! The RiverReach instance.
      integer :: x, y, s, r                                          ! GridCell, SubRiver and RiverReach identifiers.
      real(dp) :: l                                                  ! The RiverReach length.
      real(dp) :: Qrunoff                                            ! Any initial runoff.
      type(Result) :: res                                            ! The Result object.
    end function
    function destroyRiverReach(Me) result(r)
      import RiverReach, Result
      class(RiverReach) :: Me                                        ! The RiverReach instance
      type(Result) :: r                                              ! The Result object to return
    end function
    function update(me, Qin, spmIn) result(r)
        use Globals
        import RiverReach, Result
        class(RiverReach) :: me                             !! This RiverReach instance
        real(dp) :: Qin                                     !! Inflow to this reach
        real(dp) :: spmIn(C%nSizeClassesSpm)                !! Inflow SPM to this reach
        type(Result) :: r                                   !! The Result object
    end function
    ! function updateDimensions(me, Qin) result(r)
    !   use Globals
    !   import RiverReach, Result
    !   class(RiverReach) :: me
    !   real(dp) :: Qin
    !   type(Result) :: r
    ! end function
    ! function simulate(me, Q, spm, nDisp) result(r)
    !   use Globals
    !   import RiverReach, Result1D
    !   class(RiverReach) :: me
    !   real(dp) :: Q                                                ! Inflow per timestep
    !   real(dp) :: spm(:)                                           ! SPM inflow per timestep
    !   integer, optional :: nDisp                                   ! Number of displacement inflow (Q, SPM) is split into
    !   type(Result1D) :: r
    ! end function
    pure function calculateDepth(Me, W, S, Q) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: Me                          ! The RiverReach instance.
      real(dp), intent(in) :: W                                    ! River width \( W \) [m].
      real(dp), intent(in) :: S                                    ! River slope \( S \) [-].
      real(dp), intent(in) :: Q                                    ! Flow rate \( Q \) [m3/s].
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
    pure function calculateVolume(me, D, W, l, f_m) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: me         !! The RiverReach instance
      real(dp), intent(in) :: D                   !! River depth [m]
      real(dp), intent(in) :: W                   !! River width [m]
      real(dp), intent(in) :: l                   !! River length, without meandering [m]
      real(dp), intent(in) :: f_m                 !! Meandering factor [-]
      type(Result0D) :: r                         !! The Result object
    end function
    pure function calculateArea(me, D, W) result(r)
      use Globals
      import RiverReach, Result0D
      class(RiverReach), intent(in) :: me         !! The RiverReach instance
      real(dp), intent(in) :: D                   !! River depth [m]
      real(dp), intent(in) :: W                   !! River width [m]
      type(Result0D) :: r                         !! The result object.
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
    !> Return the volume of the RiverReach.
    function getVolumeRiverReach(me) result(volume)
      class(RiverReach) :: me
      real(dp) :: volume
      volume = me%volume
    end function

    !> Return the outflow.
    function getQOutRiverReach(me) result(Qout)
      class(RiverReach) :: me
      real(dp) :: Qout
      Qout = me%Qout
    end function

    !> Return the SPM discahrge.
    function getSpmOutRiverReach(me) result(spmOut)
      class(RiverReach) :: me
      real(dp) :: spmOut(size(me%spmOut))
      spmOut = me%spmOut
    end function
end module

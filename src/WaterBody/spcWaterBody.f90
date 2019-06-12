!> Module containing definition of base class `WaterBody`, which provides
!! the primitive functionality to all environmental compartments that
!! are water bodies.
module spcWaterBody
    use Globals
    use classPointSource
    use classDiffuseSource
    use spcBedSediment
    use spcReactor
    use classBiota1
    implicit none
    
    !> `WaterBodyPointer` used for `WaterBody` inflows array, so the elements within can
    !! point to other `GridCell`'s colWaterBody elements
    type WaterBodyPointer
        class(WaterBody), pointer :: item => null()                  !! Pointer to polymorphic `WaterBody` object
    end type
    
    !> An internal user-defined type, defining a reference to a `WaterBody`.
    !! Comprises row (x) and column (y) references to the `GridCell` containing the
    !! `WaterBody` and the in-cell `WaterBody` reference number
    type WaterBodyRef                                                 
        integer :: x                                                !! `GridCell` x reference
        integer :: y                                                !! `GridCell` y reference
        integer :: w                                                !! `WaterBody` reference
    end type

    !> Abstract base class for `WaterBody`. Defines properties and procedures
    !! required in any implementation of this class.
    type, public :: WaterBody
        ! Reference
        character(len=100) :: ref                                   !! Reference for this object, of the form WaterBody_x_y_w
        integer :: x                                                !! `GridCell` x position
        integer :: y                                                !! `GridCell` y position
        integer :: w                                                !! `WaterBody` reference
        ! Grid cell
        real(dp) :: gridCellArea                                    !! `GridCell` area [m2]
        ! Physical properties
        real(dp) :: depth                                           !! Depth of the `WaterBody` [m]
        real(dp) :: surfaceArea                                     !! Surface area of the `WaterBody` [m2]
        real(dp) :: bedArea                                         !! Area of the contained `BedSediment` [m2]
        real(dp) :: volume                                          !! Volume of water in the body [m3]
        real(dp) :: T_water                                         !! Water temperature [C]
        ! Concentrations
        real(dp), allocatable :: C_spm(:)                           !! Sediment concentration [kg/m3]
        real(dp), allocatable :: m_spm(:)                           !! Sediment mass [kg/m3]
        real(dp), allocatable :: C_np(:,:,:)                        !! NM mass concentration [kg/m3]
        real(dp), allocatable :: m_np(:,:,:)                        !! NM mass mass [kg/m3]
        real(dp), allocatable :: C_ionic(:)                         !! Ionic metal concentration [kg/m3]
        real(dp), allocatable :: m_ionic(:)                         !! Ionic metal mass [kg/m3]
        real(dp), allocatable :: m_np_disp(:,:,:,:)                 !! Mass of nanomaterial on each displacement [kg]
        real(dp), allocatable :: C_np_disp(:,:,:,:)                 !! Concentration of nanomaterial on each displacement [kg]
        ! Flows and fluxes
        integer, allocatable :: neighboursArray(:,:,:,:)            !! Neighbouring waterbodies, as array of indices
        type(WaterBodyPointer), allocatable :: neighbours(:)        !! Neighbouring waterbodies
        real(dp), allocatable :: Q(:)
            !! Flow of water to neighbouring compartments. Size of array corresponds to the number
            !! of possible flow directions (including transfers). +ve denotes flow *from* the neighbouring
            !! compartment, -ve denotes flow *to*. [m3/timestep]
        real(dp), allocatable :: Q_final(:)
            !! Flow array updated at the end of the timestep. This is to enusre reaches don't use
            !! the wrong timestep's Q from another reach, in particular as their inflow. [m3/timestep]
        real(dp) :: Q_in_total                                      !! Total inflow of water [m3/timestep]
        real(dp), allocatable :: j_spm(:,:)
            !! Flow of SPM, same conventions as Q [kg/timestep]. 2nd dimension is the size class of the SPM
        real(dp), allocatable :: j_spm_final(:,:)                   !! Final SPM flux array - see Q_final [kg/timestep]
        real(dp), allocatable :: j_np(:,:,:,:)
            !! Flow of NM, same conventions as Q [kg/timestep]. 2nd-4th dimensions are the NM size class,
            !! NM form and NM state, respectively.
        real(dp), allocatable :: j_np_final(:,:,:,:)                !! Final NM flux array - see Q_final [kg/timestep]
        real(dp), allocatable :: j_ionic(:,:)
            !! Flow of ionic metal, same conventions as Q [kg/timestep]. 2nd dimension is the state of ionic metal
        real(dp), allocatable :: j_ionic_final(:,:)                 !! Final ionic flux array - see Q_final [kg/timestep]
        real(dp), allocatable :: k_resus(:)                         !! Resuspension rate for a given timestep [s-1]
        real(dp), allocatable :: k_settle(:)                        !! Sediment settling rate on a given timestep [s-1]
        real(dp), allocatable :: W_settle_spm(:)                    !! SPM settling velocity [m/s]
        real(dp), allocatable :: W_settle_np(:)                     !! NP settling velocity [m/s]
        ! Contained objects
        class(BedSediment), allocatable :: bedSediment              !! Contained `BedSediment` object
        class(Biota), allocatable :: biota                          !! Contained `Biota` object
        class(Reactor), allocatable :: reactor                      !! Contained `Reactor` object
        type(PointSource), allocatable :: pointSources(:)           !! Contained `PointSource` objects
        logical :: hasPointSource = .false.                         !! Does this water body have any point sources?
        integer :: nPointSources                                    !! How many point sources this water body has
        type(DiffuseSource), allocatable :: diffuseSources(:)       !! Contained `DiffuseSource` objects
        integer :: nDiffuseSources                                  !! How many diffuse sources this water body has
        logical :: hasDiffuseSource = .false.                       !! Does this water body have any diffuse sources?
        logical :: isTidalLimit = .false.

      contains
        ! Create
        procedure:: create => createWaterBody
        ! Simulators
        procedure:: update => updateWaterBody
        procedure :: finaliseUpdate
        ! Data handlers
        procedure :: allocateAndInitialise => allocateAndInitialiseWaterBody
        procedure:: parseInputData => parseInputDataWaterBody
        ! Getters
        procedure:: j_np_runoff => j_np_runoffWaterBody
        procedure:: j_np_transfer => j_np_transferWaterBody
        procedure:: j_np_deposit => j_np_depositWaterBody
        procedure:: j_np_diffusesource => j_np_diffusesourceWaterBody
        procedure:: j_np_pointsource => j_np_pointsourceWaterBody
    end type
      
    !> Container type for `class(WaterBody)`, the actual type of the `WaterBody` class.
    !! a variable of type `WaterBodyElement` can be of any object type inheriting from the
    !! `WaterBody` abstract base class.
    type WaterBodyElement                                          
        class(WaterBody), allocatable :: item                      !! Polymorphic `WaterBody` object
    end type

  contains

    !> Create this `WaterBody`
    function createWaterBody(me, x, y, w, gridCellArea, neighbours) result(rslt)
        class(WaterBody) :: me                                  !! The `WaterBody` instance
        integer :: x, y, w                                      !! `GridCell` and `WaterBody` identifiers
        real(dp) :: gridCellArea                                !! Area of the containing `GridCell`
        integer, optional :: neighbours(:,:,:,:)                          !! Neighbouring water bodies
        type(Result) :: rslt                                    !! The Result object
        ! Set reach indices and grid cell area
        me%x = x
        me%y = y
        me%w = w
        me%gridCellArea = gridCellArea
        if (present(neighbours)) then
            me%neighboursArray = neighbours
        else
            allocate(me%neighboursArray(0,0,0,0))
        end if
    end function

    !> Update this `WaterBody` on given time step
    function updateWaterBody(me, t, q_runoff, j_spm_runoff, j_np_runoff) result(rslt)
        class(WaterBody) :: me                                  !! This `WaterBody` instance
        integer :: t                                            !! What time step are we on?
        real(dp), optional :: q_runoff                          !! Runoff from the hydrological model [m/timestep]
        real(dp), optional :: j_spm_runoff(:)                   !! Eroded sediment runoff to this water body [kg/timestep]
        real(dp), optional :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this water body [kg/timestep]
        type(Result) :: rslt                                    !! The `Result` object

        ! Do stuff
    end function

    !> Allocate memory for arrays generic to any water body. Individual water bodies
    !! may extend this routine to allocate their own body specific variables
    subroutine allocateAndInitialiseWaterBody(me)
        class(WaterBody) :: me
        allocate(me%C_spm(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            me%C_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%C_ionic(C%ionicDim), &
            me%m_ionic(C%ionicDim), &
            me%k_resus(C%nSizeClassesSpm), &
            me%k_settle(C%nSizeClassesSpm), &
            me%W_settle_spm(C%nSizeClassesSpm), &
            me%W_settle_np(C%nSizeClassesNP) &
        )
        me%C_spm = 0
        me%m_spm = 0
        me%C_np = 0
        me%m_np = 0
        me%C_ionic = 0
        me%m_ionic = 0
        me%T_water = 10             ! TODO set this from data or empirical relationship
        me%bedArea = 0
    end subroutine

    !> Parse input data for this `WaterBody`
    function parseInputDataWaterBody(me) result(rslt)
        class(WaterBody) :: me
        type(Result) :: rslt

        ! Do stuff
    end function

    !> Set the final flow arrays for this water body. These final arrays are used by other linked
    !! water bodies such that the avoid using the wrong timestep's values, in particular as inflows.
    subroutine finaliseUpdate(me)
        class(WaterBody) :: me
        me%Q_final = me%Q
        me%j_spm_final = me%j_spm
        me%j_np_final = me%j_np
        me%j_ionic_final = me%j_ionic
    end subroutine

    function j_np_runoffWaterBody(me) result(j_np_runoff)
        class(WaterBody) :: me
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        ! Do stuff
    end function

    function j_np_transferWaterBody(me) result(j_np_transfer)
        class(WaterBody) :: me
        real(dp) :: j_np_transfer(C%npDim(1), C%npDim(2), C%npDim(3))
        ! Do stuff
    end function

    function j_np_depositWaterBody(me) result(j_np_deposit)
        class(WaterBody) :: me
        real(dp) :: j_np_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        ! Do stuff
    end function

    function j_np_diffusesourceWaterBody(me) result(j_np_diffusesource)
        class(WaterBody) :: me
        real(dp) :: j_np_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        ! Do stuff
    end function

    function j_np_pointsourceWaterBody(me) result(j_np_pointsource)
        class(WaterBody) :: me
        real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        ! Do stuff
    end function

end module
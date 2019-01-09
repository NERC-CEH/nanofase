!> Module containing definition of base class `WaterBody`, which provides
!! the primitive functionality to all environmental compartments that
!! are water bodies.
module spcWaterBody
    use Globals
    use classPointSource
    use classDiffuseSource
    use spcBedSediment
    use spcReactor
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
    type, abstract, public :: WaterBody
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
        ! Flows and fluxes
        type(WaterBodyPointer), allocatable :: neighbours(:)        !! Neighbouring waterbodies
        real(dp), allocatable :: Q(:)
            !! Flow of water to neighbouring compartments. Size of array corresponds to the number
            !! of possible flow directions (including transfers). +ve denotes flow *from* the neighbouring
            !! compartment, -ve denotes flow *to*. [m3/timestep]
        real(dp), allocatable :: tmp_Q(:)                           !! Temporary flow storage until timestep loop complete [kg/timestep]
        real(dp) :: Q_in_total                                      !! Total inflow of water [m3/timestep]
        real(dp), allocatable :: j_spm(:,:)
        real(dp), allocatable :: tmp_j_spm(:,:)                     !! Temporary flux storage until timestep loop complete [kg/timestep]
            !! Flow of SPM, same conventions as Q [kg/timestep]. 2nd dimension is the size class of the SPM
        real(dp), allocatable :: j_np(:,:,:,:)
        real(dp), allocatable :: tmp_j_np(:,:,:,:)                  !! Temporary flux storage until timestep loop complete [kg/timestep]
            !! Flow of NM, same conventions as Q [kg/timestep]. 2nd-4th dimensions are the NM size class,
            !! NM form and NM state, respectively.
        real(dp), allocatable :: j_ionic(:,:)
        real(dp), allocatable :: tmp_j_ionic(:,:)                   !! Temporary flux storage until timestep loop complete [kg/timestep]
            !! Flow of ionic metal, same conventions as Q [kg/timestep]. 2nd dimension is the state of ionic metal
        real(dp), allocatable :: k_resus(:)                         !! Resuspension rate for a given timestep [s-1]
        real(dp), allocatable :: k_settle(:)                        !! Sediment settling rate on a given timestep [s-1]
        real(dp), allocatable :: W_settle_spm(:)                    !! SPM settling velocity [m/s]
        real(dp), allocatable :: W_settle_np(:)                     !! NP settling velocity [m/s]
        ! Contained objects
        class(BedSediment), allocatable :: bedSediment              !! Contained `BedSediment` object
        class(Reactor), allocatable :: reactor                      !! Contained `Reactor` object
        type(PointSource), allocatable :: pointSources(:)           !! Contained `PointSource` objects
        logical :: hasPointSource = .false.                         !! Does this water body have any point sources?
        integer :: nPointSources                                    !! How many point sources this water body has
        type(DiffuseSource), allocatable :: diffuseSources(:)       !! Contained `DiffuseSource` objects
        integer :: nDiffuseSources                                  !! How many diffuse sources this water body has
        logical :: hasDiffuseSource = .false.                       !! Does this water body have any diffuse sources?

      contains
        ! Create/destory
        procedure(createWaterBody), deferred :: create
        procedure(destroyWaterBody), deferred :: destroy
        ! Simulators
        procedure(updateWaterBody), deferred :: update
        procedure(finaliseUpdateWaterBody), deferred :: finaliseUpdate
        ! Data handlers
        procedure(parseInputDataWaterBody), deferred :: parseInputData
        ! Getters
        procedure(j_np_runoffWaterBody), deferred :: j_np_runoff
        procedure(j_np_transferWaterBody), deferred :: j_np_transfer
        procedure(j_np_depositWaterBody), deferred :: j_np_deposit
        procedure(j_np_diffusesourceWaterBody), deferred :: j_np_diffusesource
        procedure(j_np_pointsourceWaterBody), deferred :: j_np_pointsource
    end type
      
    !> Container type for `class(WaterBody)`, the actual type of the `WaterBody` class.
    !! a variable of type `WaterBodyElement` can be of any object type inheriting from the
    !! `WaterBody` abstract base class.
    type WaterBodyElement                                          
        class(WaterBody), allocatable :: item                      !! Polymorphic `WaterBody` object
    end type

    abstract interface
        !> Create this `WaterBody`
        function createWaterBody(me, x, y, w, gridCellArea) result(rslt)
            use Globals
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                  !! The `WaterBody` instance
            integer :: x, y, w                                      !! `GridCell` and `WaterBody` identifiers
            real(dp) :: gridCellArea                                !! Area of the containing `GridCell`
            type(Result) :: rslt                                    !! The Result object
        end function

        !> Destroy this `WaterBody`
        function destroyWaterBody(me) result(rslt)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                  !! The `WaterBody` instance
            type(Result) :: rslt                                    !! The `Result` object to return
        end function

        !> Update this `WaterBody` on given time step
        function updateWaterBody(me, t, q_runoff, j_spm_runoff, j_np_runoff) result(rslt)
            use Globals
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me                                  !! This `WaterBody` instance
            integer :: t                                            !! What time step are we on?
            real(dp), optional :: q_runoff                          !! Runoff from the hydrological model [m/timestep]
            real(dp), optional :: j_spm_runoff(:)                   !! Eroded sediment runoff to this water body [kg/timestep]
            real(dp), optional :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this water body [kg/timestep]
            type(Result) :: rslt                                    !! The `Result` object
        end function

        !> Set temporary flow variables to real flow variables
        function finaliseUpdateWaterBody(me) result(rslt)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me
            type(Result) :: rslt
        end function

        !> Parse input data for this `WaterBody`
        function parseInputDataWaterBody(me) result(rslt)
            use ResultModule, only: Result
            import WaterBody
            class(WaterBody) :: me
            type(Result) :: rslt
        end function

        function j_np_runoffWaterBody(me) result(j_np_runoff)
            use Globals
            import WaterBody
            class(WaterBody) :: me
            real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        end function

        function j_np_transferWaterBody(me) result(j_np_transfer)
            use Globals
            import WaterBody
            class(WaterBody) :: me
            real(dp) :: j_np_transfer(C%npDim(1), C%npDim(2), C%npDim(3))
        end function

        function j_np_depositWaterBody(me) result(j_np_deposit)
            use Globals
            import WaterBody
            class(WaterBody) :: me
            real(dp) :: j_np_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        end function

        function j_np_diffusesourceWaterBody(me) result(j_np_diffusesource)
            use Globals
            import WaterBody
            class(WaterBody) :: me
            real(dp) :: j_np_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        end function

        function j_np_pointsourceWaterBody(me) result(j_np_pointsource)
            use Globals
            import WaterBody
            class(WaterBody) :: me
            real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        end function


    end interface

end module
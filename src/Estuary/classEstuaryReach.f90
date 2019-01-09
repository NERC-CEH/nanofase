module classEstuaryReach
    use Globals
    use ResultModule
    use spcReach
    use classBedSediment1
    use classReactor1
    implicit none
    
    type, public, extends(Reach) :: EstuaryReach
      contains
        ! Create/destroy
        procedure :: create => createEstuaryReach
        procedure :: destroy => destroyEstuaryReach
        ! Simulators
        procedure :: update => updateEstuaryReach
        procedure :: finaliseUpdate => finaliseUpdateEstuaryReach
        procedure :: resuspension => resuspensionEstuaryReach
        procedure :: settling => settlingEstuaryReach
        procedure :: depositToBed => depositToBedEstuaryReach
        ! Data handlers
        procedure :: setDefaults => setDefaultsEstuaryReach
        procedure :: parseInputData => parseInputDataEstuaryReach
        ! Getters
        procedure :: j_np_runoff => j_np_runoffEstuaryReach
        procedure :: j_np_transfer => j_np_transferEstuaryReach
        procedure :: j_np_deposit => j_np_depositEstuaryReach
        procedure :: j_np_diffusesource => j_np_diffusesourceEstuaryReach
        procedure :: j_np_pointsource => j_np_pointsourceEstuaryReach
    end type
    
  contains
    
    function createEstuaryReach(me, x, y, w, gridCellArea) result(rslt)
        class(EstuaryReach) :: me                 !! This `EstuaryReach` instance
        integer :: x                            !! Grid cell x-position index
        integer :: y                            !! Grid cell y-position index
        integer :: w                            !! Water body index within the cell
        real(dp) :: gridCellArea                !! Containing grid cell area [m2]
        type(Result) :: rslt                    !! Result object to return errors in
        integer :: i

        me%x = x
        me%y = y
        me%w = w
        me%ref = trim(ref("EstuaryReach", x, y, i))
        me%gridCellArea = gridCellArea

        ! Allocate arrays of size classes, form and state
        allocate(me%C_spm(C%nSizeClassesSpm), &
            me%C_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%C_ionic(C%ionicDim), &
            me%k_resus(C%nSizeClassesSpm), &
            me%k_settle(C%nSizeClassesSpm), &
            me%W_settle_spm(C%nSizeClassesSpm), &
            me%W_settle_np(C%nSizeClassesNP) &
        )
        me%C_spm = 0
        me%C_np = 0
        me%C_ionic = 0

        ! Parse the input data
        call rslt%addErrors(.errors. me%parseInputData())

        ! Create the BedSediment for this EstuaryReach
        ! TODO: Get the type of BedSediment from the data file, and check for allst
        allocate(BedSediment1::me%bedSediment)
        call rslt%addErrors(.errors. me%bedSediment%create(me%ncGroup))

        ! Create the Reactor object to deal with nanoparticle transformations
        allocate(Reactor1::me%reactor)
        call rslt%addErrors(.errors. me%reactor%create(me%x, me%y, me%alpha_hetero))
        
        ! Create the PointSource object(s), if this reach has any
        if (me%hasPointSource) then
            do i = 1, size(me%pointSources)
                call rslt%addErrors(.errors. me%pointSources(i)%create(me%x, me%y, i, [trim(me%ref)]))
            end do
        end if
        ! Create the DiffuseSource object(s), if this reach has any
        if (me%hasDiffuseSource) then
            do i = 1, size(me%diffuseSources)
                call rslt%addErrors(.errors. me%diffuseSources(i)%create(me%x, me%y, i, [trim(me%ref)]))
            end do
        end if
        call rslt%addToTrace('Creating ' // trim(me%ref))
        call LOG%toFile("Creating " // trim(me%ref) // ": success")
    end function

    !> Destroy this `EstuaryReach`
    function destroyEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me                             !! This `EstuaryReach` instance
        type(Result) :: rslt                                !! The `Result` object
        ! TODO: Write some destroy logic
    end function

    function updateEstuaryReach(me, t, q_runoff, j_spm_runoff, j_np_runoff) result(rslt)
        class(EstuaryReach) :: me
        integer :: t
        real(dp), optional :: q_runoff                          !! Runoff (slow + quick flow) from the hydrological model [m/timestep]
        real(dp), optional :: j_spm_runoff(:)                   !! Eroded sediment runoff to this reach [kg/timestep]
        real(dp), optional :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this reach [kg/timestep]
        type(Result) :: rslt
        ! Do stuff
    end function

    function finaliseUpdateEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
        ! Do stuff
    end function

    function resuspensionEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
    end function

    function settlingEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
    end function

    function depositToBedEstuaryReach(me, spmDep) result(rslt)
        class(EstuaryReach) :: me
        real(dp) :: spmDep(C%nSizeClassesSpm)                   !! The SPM to deposit
        type(Result) :: rslt
    end function

    subroutine setDefaultsEstuaryReach(me)
        class(EstuaryReach) :: me
    end subroutine

    function parseInputDataEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
    end function

    !> Get the total runoff from NM flux array
    function j_np_runoffEstuaryReach(me) result(j_np_runoff)
        class(EstuaryReach) :: me
        real(dp) :: j_np_runoff(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_runoff = me%j_np(2+me%nInflows,:,:,:)
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_np_transferEstuaryReach(me) result(j_np_transfer)
        class(EstuaryReach) :: me
        real(dp) :: j_np_transfer(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_transfer = me%j_np(3+me%nInflows,:,:,:)
    end function

    !> Get the total deposited NM (settling + resus) from NM flux array
    function j_np_depositEstuaryReach(me) result(j_np_deposit)
        class(EstuaryReach) :: me
        real(dp) :: j_np_deposit(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_deposit = me%j_np(4+me%nInflows,:,:,:)
    end function

    !> Get the total diffuse source fluxes from NM flux array
    function j_np_diffusesourceEstuaryReach(me) result(j_np_diffusesource)
        class(EstuaryReach) :: me
        real(dp) :: j_np_diffusesource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_diffusesource = sum(me%j_np(5+me%nInflows:4+me%nInflows+me%nDiffuseSources,:,:,:), dim=1)
    end function

    !> Get the total point source fluxes from NM flux array
    function j_np_pointsourceEstuaryReach(me) result(j_np_pointsource)
        class(EstuaryReach) :: me
        real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_pointsource &
            = sum(me%j_np(5+me%nInflows+me%nDiffuseSources:4+me%nInflows+me%nDiffuseSources+me%nPointSources,:,:,:), dim=1)
    end function
    
end module
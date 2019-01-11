module classEstuaryReach
    use Globals
    use spcReach
    use UtilModule
    use ResultModule
    use classBedSediment1
    use classLogger, only: LOG
    use classDataInterfacer, only: DATA
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
        ! Data handlers
        procedure :: parseInputData => parseInputDataEstuaryReach
        ! Calculators
        procedure :: calculateWidth => calculateWidth
        procedure :: calculateDepth => calculateDepth
        procedure :: calculateVelocity => calculateVelocity
        ! Getters
        procedure :: j_spm_inflows
        procedure :: j_np_runoff => j_np_runoffEstuaryReach
        procedure :: j_np_transfer => j_np_transferEstuaryReach
        procedure :: j_np_deposit => j_np_depositEstuaryReach
        procedure :: j_np_diffusesource => j_np_diffusesourceEstuaryReach
        procedure :: j_np_pointsource => j_np_pointsourceEstuaryReach
        ! Setters
        procedure :: set_j_spm_runoff
        procedure :: set_j_spm_transfer
        procedure :: set_j_spm_deposit
    end type

  contains

    function createEstuaryReach(me, x, y, w, gridCellArea) result(rslt)
        class(EstuaryReach) :: me                 !! This `EstuaryReach` instance
        integer :: x                            !! Grid cell x-position index
        integer :: y                            !! Grid cell y-position index
        integer :: w                            !! Water body index within the cell
        real(dp) :: gridCellArea                !! Containing grid cell area [m2]
        type(Result) :: rslt                    !! Result object to return errors in
        integer :: i, j                         ! Iterator

        me%x = x
        me%y = y
        me%w = w
        me%ref = trim(ref("EstuaryReach", x, y, w))
        me%gridCellArea = gridCellArea

        ! Allocate arrays of size classes, form and state
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
        me%n = C%n_river

        ! Parse the input data
        call rslt%addErrors(.errors. me%parseInputData())

        ! Now allocate the arrays that were based on the number of inflows (set by me%parseInputData()).
        ! These are the flux/flow arrays to/from this reach. The 1st dimension represents the compartment
        ! the flow is to/from, and for river reaches, this is indexed as so:
        !   1. outflow
        !   2 -> 1+nInflows: inflows
        !   2+nInflows: runoff
        !   3+nInflows: transfers
        !   (SPM & NM only) 4+nInflows: settling & resuspension
        !   (NM only)   5+nInflows -> 4+nInflows+nDiffuseSources: diffuse sources
        !   (NM only)   5+nInflows+nDiffuseSources -> 4+nInflows+nDiffuseSources+nPointSources: point sources
        allocate(me%Q(me%nInflows + 3), &
            me%tmp_Q(me%nInflows + 3), &
            me%j_spm(me%nInflows + 4, C%nSizeClassesSpm), &
            me%tmp_j_spm(me%nInflows + 4, C%nSizeClassesSpm), &
            me%j_np(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%tmp_j_np(me%nInflows + me%nPointSources + me%nDiffuseSources + 4, C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_ionic(me%nInflows + 3, C%ionicDim), &
            me%tmp_j_ionic(me%nInflows + 3, C%ionicDim) &
        )
        me%Q = 0
        me%tmp_Q = 0
        me%j_spm = 0
        me%tmp_j_spm = 0
        me%j_np = 0
        me%tmp_j_np = 0
        me%j_ionic = 0
        me%tmp_j_ionic = 0

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
        !---
        real(dp) :: j_spm_in_total(C%nSizeClassesSpm)           ! Total inflow of SPM [kg/timestep]
        real(dp) :: j_np_in_total(C%npDim(1), C%npDim(2), C%npDim(3))   ! Total inflow of NP [kg/timestep]
        real(dp) :: fractionSpmDep(C%nSizeClassesSpm)           ! Fraction of SPM deposited on each time step [-]
        type(Result0D) :: depthRslt                             ! Result object for depth
        integer :: i, j                                         ! Iterator
        integer :: nDisp                                        ! Number of displacements to split this time step into
        real(dp) :: dt                                          ! Length of each displacement [s]
        real(dp) :: dQ_in                                       ! Q_in for each displacement
        real(dp) :: dj_spm_in(C%nSizeClassesSpm)                ! j_spm_in for each displacement
        real(dp) :: dj_np_in(C%npDim(1), C%npDim(2), C%npDim(3)) ! j_np_in for each displacement
        real(dp) :: dj_spm_deposit(C%nSizeClassesSpm)           ! Deposited SPM for each displacement
        real(dp) :: dj_spm_out(C%nSizeClassesSpm)               ! SPM outflow for the displacement
        real(dp) :: j_spm_deposit(C%nSizeClassesSpm)            ! To keep track of SPM deposited
        type(Result1D) :: rslt1D                                ! Result 1D object
        real(dp) :: m_bed(C%nSizeClassesSpm)                    ! Mass of fine material in the sediment [kg]
        real(dp) :: dj_spm_res(C%nSizeClassesSpm)               ! Mass of each sediment size class resuspended on each displacement [kg]

        ! Initialise flows to zero
        me%Q_in_total = 0
        j_spm_in_total = 0
        j_np_in_total = 0
        fractionSpmDep = 0
        j_spm_deposit = 0
        me%tmp_Q = 0
        me%Q(2:) = 0            ! Don't reset the outflow (element 1), other cells might need this as inflow
        me%tmp_j_spm = 0
        me%j_spm(2:,:) = 0
        me%tmp_j_np = 0
        me%j_np(2:,:,:,:) = 0
        me%tmp_j_ionic = 0
        me%j_ionic(2:,:) = 0
        ! TODO get this from data (or empirical based on seasonal variation)
        me%T_water = 10

        ! TODO change the below to use tmp_Q, tmp_j_spm and tmp_j_np only, until finalise update
        ! to keep things in place from the previous timestep for inflow cells to use

        ! Inflows from runoff
        if (present(q_runoff)) then
            me%Q(3+me%nInflows) = q_runoff*me%gridCellArea
        end if
        if (present(j_spm_runoff)) then
            me%j_spm(3+me%nInflows,:) = j_spm_runoff
        end if
        if (present(j_np_runoff)) then
            me%j_np(3+me%nInflows,:,:,:) = j_np_runoff
        end if

        ! Inflows from water bodies
        do i = 1, me%nInflows
            me%Q(1+i) = -me%inflows(i)%item%Q(1)
            me%j_spm(1+i,:) = -me%inflows(i)%item%j_spm(1,:)
            me%j_np(1+i,:,:,:) = -me%inflows(i)%item%j_np(1,:,:,:)
        end do

        ! Inflows from transfers
        ! TODO

        ! Loop through point and diffuse sources and get their inputs (if there are any):
        ! Run the update method, which sets PointSource's j_np_pointsource variable
        ! for this time step. j_np_pointsource = 0 if there isn't a point source
        ! Same for diffuse sources, convert j_np_diffuseSource from kg/m2/timestep to kg/reach/timestep
        if (me%hasDiffuseSource) then
            do i = 1, size(me%diffuseSources)
                call rslt%addErrors(.errors. me%diffuseSources(i)%update(t))
                me%j_np(4+me%nInflows+i,:,:,:) = me%diffuseSources(i)%j_np_diffuseSource*me%bedArea
            end do
        end if
        if (me%hasPointSource) then
            do i = 1, size(me%pointSources)
                call rslt%addErrors(.errors. me%pointSources(i)%update(t))
                me%j_np(4+me%nInflows+me%nDiffuseSources+i,:,:,:) = me%pointSources(i)%j_np_pointSource
            end do
        end if

        ! Total inflows = inflow water bodies + runoff + transfers (+ sources for NM)
        me%Q_in_total = sum(me%Q(2:))
        j_spm_in_total = sum(me%j_spm(2:,:), dim=1)
        j_np_in_total = sum(me%j_np(2:,:,:,:), dim=1)

        ! Calculate the depth, velocity, area and volume
        me%width = me%calculateWidth(me%Q_in_total/C%timeStep)
        depthRslt = me%calculateDepth(me%width, me%slope, me%Q_in_total/C%timeStep)
        me%depth = .dp. depthRslt                           ! Get real(dp) data from Result object
        call rslt%addError(.error. depthRslt)                  ! Add any error that occurred
        me%velocity = me%calculateVelocity(me%depth, me%Q_in_total/C%timeStep, me%width)
        me%xsArea = me%depth*me%width                       ! Calculate the cross-sectional area of the reach [m2]
        me%bedArea = me%width*me%length*me%f_m              ! Calculate the BedSediment area [m2]
        me%surfaceArea = me%bedArea                         ! For river reaches, set surface area equal to bed area
        me%volume = me%depth*me%width*me%length*me%f_m      ! Reach volume

        ! HACK
        if (me%volume < 10.0_dp) then
            me%volume = 0.0_dp
        end if

        ! Set the resuspension rate me%k_resus and settling rate me%k_settle
        ! (but don't acutally settle until we're looping through
        ! displacements). This can be done now as settling/resuspension rates
        ! don't depend on anything that changes on each displacement
        call rslt%addErrors([ &
            .errors. me%resuspension(), &                   ! me%resuspension computes resuspension rate [s-1] over complete timestep
            .errors. me%settling() &                        ! me%settling computes settling rate [s-1] over complete timestep
        ])

        ! If Q_in for this timestep is bigger than the reach volume, then we need to
        ! split into a number of displacements. If Q_in is zero, just have 1 displacement.
        if (isZero(me%Q_in_total) .or. isZero(me%volume)) then
            nDisp = 1
        else
            nDisp = ceiling(me%Q_in_total/me%volume)        ! Number of displacements
        end if
        dt = C%timeStep/nDisp                               ! Length of each displacement [s]
        dQ_in = me%Q_in_total/nDisp                         ! Inflow to the first displacement [m3]
        dj_spm_in = j_spm_in_total/nDisp                    ! SPM inflow to the first displacment [kg]
        dj_np_in = j_np_in_total/nDisp                      ! NM inflow to the first displacement [kg]

        do i = 1, nDisp
            ! Add the inflow NP to the current mass for this displacement
            me%m_np = me%m_np + dj_np_in
            
            ! Update SPM according to inflow for this displacement, then calculate
            ! new SPM concentration based on this and the dimensions
            if (.not. isZero(me%volume)) then
                dj_spm_out = min(me%m_spm * dQ_in / me%volume, me%m_spm) ! SPM loss as a fraction of reach volume moving downstream on this displacement
            else
                dj_spm_out = 0
            end if
            ! Update SPM mass after advection and add inflow SPM
            me%m_spm = me%m_spm - dj_spm_out + dj_spm_in

            ! Remove settled SPM from the displacement. TODO: This will go to BedSediment eventually
            ! If we've removed all of the SPM, set to 0
            dj_spm_deposit = min(me%k_settle*dt*me%m_spm, me%m_spm)     ! Up to a maximum of the mass of SPM currently in reach
            ! print *, "dj_spm_deposit disp: ", dj_spm_deposit, i
            ! Check if dj_spm_deposit (and thus me%m_spm) is zero to avoid numerical errors
            ! SPM deposited doesn't include resuspension
            do j = 1, C%nSizeClassesSpm 
                if (isZero(dj_spm_deposit(j))) then
                    fractionSpmDep(j) = fractionSpmDep(j)
                else
                    fractionSpmDep(j) = fractionSpmDep(j) + dj_spm_deposit(j)/me%m_spm(j)
                end if
            end do
            me%m_spm = me%m_spm - dj_spm_deposit
            j_spm_deposit = j_spm_deposit - dj_spm_deposit          ! Keep track of deposited SPM for this time step. -ve means loss

            ! Resuspended SPM must be taken from BedSediment
            ! TODO: [DONE REQUIRES CHECKING] Get masses of bed sediment by size fraction
            rslt1D = me%bedSediment%Mf_bed_by_size()                ! retrieve bed masses [kg m-2] by size class
            call rslt%addErrors(.errors. rslt1D)                    ! add any errors to trace
            if (rslt%hasCriticalError()) return                     ! If getting bed throws error
            m_bed = .dp. rslt1D                                     ! Extract bed sediment mass [kg] by size fraction
            dj_spm_res = me%k_resus * m_bed * dt                    ! the mass of sediment resuspending on each displacement [kg]
            me%m_spm = me%m_spm + dj_spm_res                        ! SPM resuspended is resuspension flux * displacement length
            j_spm_deposit = j_spm_deposit + dj_spm_res              ! Remove the resuspended mass from the deposit tally. +ve means gain

            ! Update the concentration. isZero check used to avoid numerical errors
            ! from very small m_spm numbers.
            do j = 1, C%nSizeClassesSpm
                if (isZero(me%m_spm(j)) .or. isZero(me%volume)) then
                    me%m_spm(j) = 0.0_dp
                    me%C_spm(j) = 0.0_dp
                else
                    me%C_spm(j) = me%m_spm(j) / me%volume
                end if
            end do

            if (C%includeBedSediment) then

                ! print *, "Bed sediment before resuspension"
                ! call Me%bedSediment%repmass                              ! report bed sediment masses before resuspension  
                
                call rslt%addErrors(.errors. &
                    Me%bedSediment%resuspend(dj_spm_res / me%bedArea))   ! remove resuspended SPM from BedSediment
                if (rslt%hasCriticalError()) return                         ! exit if a critical error has been thrown
                
                ! print *, "Bed sediment after resuspension"
                ! call Me%bedSediment%repmass                              ! report bed sediment masses after resuspension  
                ! call print_matrix(Me%bedSediment%delta_sed)

                call rslt%addErrors(.errors. Me%depositToBed(dj_spm_deposit)) ! add deposited SPM to BedSediment 
                if (rslt%hasCriticalError()) return                         ! exit if a critical error has been thrown
            
                ! print *, "Bed sediment after deposition"
                ! call Me%bedSediment%repmass                              ! report bed sediment masses after deposition  
                ! call print_matrix(Me%bedSediment%delta_sed)
                
                call rslt%addErrors(.errors. &
                    Me%bedSediment%getmatrix(dj_spm_deposit/Me%bedArea, &
                                             dj_spm_res/Me%bedArea))
                ! call print_matrix(Me%bedSediment%delta_sed)

                ! if (t == 1) then
                !     m_np_sediment_layers = 0
                ! end if

                ! HACK to get NM mass in sediment layers. Need to implement properly.
                ! do s = 1, C%nSizeClassesSpm
                !     if (isZero(dj_spm_dep(s)) .or. isZero(me%m_spm(s))) then
                !         dj_np_dep(:,:,s) = 0.0_dp
                !     else
                !         dj_np_dep(:,:,s) = min(me%m_np(:,:,s+2)*(dj_spm_dep(s)/me%m_spm(s)), me%m_np(:,:,s+2))
                !     end if
                !     do n = 1, C%nSizeClassesNP
                !         m_np_sediment_layers(1,s,n) = sum(dj_np_dep(n,:,s))
                !     end do
                ! end do
                
                ! do n = 1, C%nSizeClassesNP
                !     do s = 1, C%nSizeClassesSpm
                !         m_np_sediment_layers(:,s,n) = matmul(Me%bedSediment%delta_sed(:,:,s), m_np_sediment_layers(:,s,n))
                !     end do
                ! end do
            end if

            ! Sum the displacement outflows and mass for the final outflow
            ! Currently, Q_out = Q_in. Abstraction etc will change this
            me%tmp_Q(1) = me%tmp_Q(1) - dQ_in
            me%tmp_j_spm(1,:) = me%tmp_j_spm(1,:) - dj_spm_out
        end do
        ! Set the depositing flux in the SPM flux array
        call me%set_j_spm_deposit(j_spm_deposit)   

        ! Use amount of settled SPM to get rid of heteroaggregated NPs, assuming
        ! uniformly distributed amongst SPM. fractionSpmDep doesn't include resuspension???
        ! TODO inclue resuspension which will set sign of -min correctly
        do i = 1, C%nSizeClassesSpm
            me%j_np(4+me%nInflows,:,:,i+2) = -min(me%m_np(:,:,i+2)*fractionSpmDep(i), me%m_np(:,:,i+2))
        end do
        me%m_np = me%m_np + me%j_np(4+me%nInflows,:,:,:)    ! Remove/add deposited NPs.

        ! Transform the NPs. TODO: Should this be done before or after settling/resuspension?
        ! TODO for the moment, ignoring heteroaggregation if no volume, need to figure out
        ! what to really do if there are no flows
        if (.not. isZero(me%volume)) then
            call rslt%addErrors([ &
                .errors. me%reactor%update( &
                    t, &
                    me%m_np, &
                    me%C_spm, &
                    me%T_water, &
                    me%W_settle_np, &
                    me%W_settle_spm, &
                    10.0_dp, &                      ! HACK: Where is the shear rate from?
                    me%volume, &
                    -me%tmp_Q(1) &                   ! TODO: Is this used?
                ) &
            ])
            ! Get the resultant transformed mass from the Reactor
            me%m_np = me%reactor%m_np
        end if

        ! Reactor only deals with transformations, not flows, so we must now
        ! set an outflow based on the transformed mass
        if (.not. isZero(me%volume)) then
            me%tmp_j_np(1,:,:,:) = -min(-me%tmp_Q(1)*(me%m_np/me%volume), me%m_np)
        else
            me%tmp_j_np(1,:,:,:) = 0.0_dp
        end if
        me%m_np = me%m_np + me%tmp_j_np(1,:,:,:)    ! Outflow will be negative, hence the + sign here

        ! TODO Update all of the above so that me%m_np can be updated by j_np (and similar for SPM)
        ! in one go, instead of doing it individually for each process

        ! Set the final concentrations, checking that the river has a volume
        if (.not. isZero(me%volume)) then
            me%C_spm = me%m_spm/me%volume
            me%C_np = me%m_np/me%volume
        else
            me%C_spm = 0.0_dp
            me%C_np = 0.0_dp
        end if

        ! If there's no SPM left, add the "all SPM advected" warning
        ! TODO Maybe the same for NPs
        do i = 1, C%nSizeClassesSpm
            if (isZero(me%m_spm(i)) .and. j_spm_in_total(i) /= 0) then
                call rslt%addError(ErrorInstance( &
                    code = 500, &
                    message = "All SPM in size class " // trim(str(i)) // " (" // trim(str(C%d_spm(i)*1e6)) // &
                            " um) advected from EstuaryReach.", &
                    isCritical = .false.) &
                )
            end if 
        end do

        ! Add what we're doing here to the error trace
        call rslt%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function


    !> TODO change to subroutine
    function finaliseUpdateEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
        me%Q(1) = me%tmp_Q(1)
        me%j_spm(1,:) = me%tmp_j_spm(1,:)
        me%j_np(1,:,:,:) = me%tmp_j_np(1,:,:,:)
    end function


    function parseInputDataEstuaryReach(me) result(rslt)
        class(EstuaryReach) :: me
        type(Result) :: rslt
        integer :: i                                ! Loop iterator
        integer, allocatable :: inflowArray(:,:)    ! Temporary array for storing inflows from data file in

        ! Set the data interfacer's group to the group for this reach
        call rslt%addErrors(.errors. DATA%setGroup([character(len=100) :: &
            'Environment', &
            ref('GridCell', me%x, me%y), &
            me%ref &
        ]))
        me%ncGroup = DATA%grp

        ! Check if this reach has/   any diffuse sources. me%hasDiffuseSource defauls to .false.
        ! Allocate me%diffuseSources accordingly. The DiffuseSource class actually gets the data.
        if (DATA%grp%hasGroup("PointSource") .or. DATA%grp%hasGroup("PointSource_1")) then
            me%hasPointSource = .true.
            allocate(me%pointSources(1))
            i = 2               ! Any extra point sources?
            do while (DATA%grp%hasGroup("PointSource_" // trim(str(i))))
                deallocate(me%pointSources)
                allocate(me%pointSources(i))
                i = i+1
            end do
            me%nPointSources = size(me%pointSources)
        else
            me%nPointSources = 0
        end if

        ! Check if this reach has any diffuse sources. me%hasDiffuseSource defauls to .false.
        ! Allocate me%diffuseSources accordingly. The DiffuseSource class actually gets the data.
        if (DATA%grp%hasGroup("DiffuseSource") .or. DATA%grp%hasGroup("DiffuseSource_1")) then
            me%hasDiffuseSource = .true.
            allocate(me%diffuseSources(1))
            i = 2               ! Any extra diffuse sources?
            do while (DATA%grp%hasGroup("DiffuseSource_" // trim(str(i))))
                deallocate(me%diffuseSources)
                allocate(me%diffuseSources(i))
                i = i+1
            end do
            me%nDiffuseSources = size(me%diffuseSources)
        else
            me%nDiffuseSources = 0
        end if

        ! Get the length of the reach, if present. Otherwise, set to 0 and GridCell will deal with calculating
        ! length. Note that errors might be thrown from GridCell if the reaches lengths within the GridCell are
        ! not physically possible within the reach (e.g., too short).
        call rslt%addErrors([ &
            .errors. DATA%get('length', me%length, 0.0_dp), &   ! Length is calculated by GridCell if it defaults here
                ! Note that errors might be thrown from GridCell if the reaches' lengths within GridCell are
                ! not physicaly possible within the reach (e.g. too short)       
            .errors. DATA%get('slope', me%slope), &             ! TODO: Slope should default to GridCell slope
            .errors. DATA%get('f_m', me%f_m, C%defaultMeanderingFactor), &         ! Meandering factor
            .errors. DATA%get('alpha_res', me%alpha_resus), &   ! Resuspension alpha parameter
            .errors. DATA%get('beta_res', me%beta_resus), &     ! Resuspension beta parameter
            .errors. DATA%get('alpha_hetero', me%alpha_hetero, C%default_alpha_hetero, warnIfDefaulting=.true.), &
                ! alpha_hetero defaults to that specified in config.nml
            .errors. DATA%get('domain_outflow', me%domainOutflow, silentlyFail=.true.) &
        ])
        if (allocated(me%domainOutflow)) me%isDomainOutflow = .true.    ! If we managed to set domainOutflow, then this reach is one
        
        ! ROUTING: Get the references to the inflow(s) EstuaryReaches and
        ! store in inflowRefs(). Do some auditing as well.
        if (DATA%grp%hasVariable("inflows")) then
            call rslt%addErrors(.errors. DATA%get('inflows', inflowArray))
            ! There mustn't be more than 7 inflows to a reach (one from
            ! each side/corner of the inflow GridCell)
            if (size(inflowArray, 2) > 7) then
                call rslt%addError(ErrorInstance(code=403))
            ! If there is an inflow group but nothing in it, this reach
            ! must be a headwater
            else if (size(inflowArray, 2) == 0) then
                allocate(me%inflowRefs(0))                          
                me%nInflows = 0
                me%isHeadwater = .true.
            else
                ! Set the number of inflows from the input inflowArray
                allocate(me%inflowRefs(size(inflowArray, 2)))
                me%nInflows = size(me%inflowRefs)
                ! Loop through the inflow from data and store them at the object level
                do i = 1, me%nInflows                               ! Loop through the inflows
                    me%inflowRefs(i)%x = inflowArray(1,i)           ! Inflow x reference
                    me%inflowRefs(i)%y = inflowArray(2,i)           ! Inflow y reference
                    me%inflowRefs(i)%w = inflowArray(3,i)           ! Inflow EstuaryReach reference
                    ! Check the inflow is from a neighbouring EstuaryReach
                    if (abs(me%inflowRefs(i)%x - me%x) > 1 .or. abs(me%inflowRefs(i)%y - me%y) > 1) then
                        call rslt%addError(ErrorInstance(code=401))
                    end if
                    ! Is this reach an inflow to the GridCell (i.e., are the inflows to this reach
                    ! from another GridCell)? We only need to check for the first inflow (i=1),
                    ! as the next bit checks that all inflows are from the same GridCell
                    if (i == 1 .and. (me%inflowRefs(i)%x /= me%x .or. me%inflowRefs(i)%y /= me%y)) then
                        me%isGridCellInflow = .true.
                    end if
                    ! If there is more than one inflow to the reach, it must be
                    ! an inflow to the cell. Therefore, we need to check all
                    ! inflows are from the same cell
                    if (i > 1 .and. me%inflowRefs(i)%x /= me%inflowRefs(1)%x .and. me%inflowRefs(i)%y /= me%inflowRefs(1)%y) then
                        call rslt%addError(ErrorInstance(code=402))
                    end if
                end do
            end if
        else
        ! Else there mustn't be any inflows (i.e. it's a headwater)
            allocate(me%inflowRefs(0))                          
            me%nInflows = 0
            me%isHeadwater = .true.
        end if
        ! Allocate inflows() array (the array of pointers) to the correct size
        allocate(me%inflows(me%nInflows))
        
        ! If the data has an outflow to the model domain specified, set that
        if (me%ncGroup%hasVariable("domain_outflow")) then
            me%isDomainOutflow = .true.
        end if

        call rslt%addToTrace('Parsing input data')             ! Add this procedure to the trace
    end function

    !> Calculate the width \( W \) of the river based on the discharge:
    !! $$
    !!      W = 1.22Q^{0.557}
    !! $$
    !! References:
    !! <ul>
    !!  <li>[Dumont et al., 2012](https://doi.org/10.1080/02626667.2012.715747)</li>
    !!  <li>[Allen et al., 1994](https://doi.org/10.1111/j.1752-1688.1994.tb03321.x)</li>
    !! </ul>
    function calculateWidth(me, Q) result(width)
        class(EstuaryReach), intent(in) :: me     !! The `EstuaryReach` instance
        real(dp), intent(in) :: Q               !! `GridCell` discharge \( Q \) [m3/s]
        real(dp) :: width                       !! The calculated width \( W \) [m]
        width = 1.22*Q**0.557
    end function

    !> Calculate water depth from Manning's roughness coefficient,
    !! using Newton's method:
    !! $$
    !!      D_i = D_{i-1} - \frac{f(D_{i-1})}{f'(D_{i-1})}
    !! $$
    !! where
    !! $$
    !!      f(D) = WD \left( \frac{WD}{W+2D} \right)^{2/3} \frac{\sqrt{S}}{n} - Q = 0
    !! $$
    !! and
    !! $$
    !!      f'(D) = \frac{\sqrt{S}}{n} \frac{(DW)^{5/3}(6D + 5W)}{3D(2D + W)^{5/3}}
    !! $$
    function calculateDepth(me, W, S, Q) result(rslt)
        class(EstuaryReach), intent(in) :: me     !! The `EstuaryReach` instance.
        real(dp), intent(in) :: W               !! River width \( W \) [m].
        real(dp), intent(in) :: S               !! River slope \( S \) [-].
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m3/s].
        type(Result0D) :: rslt
            !! The Result object, containing the calculated depth [m] and any numerical error.
        real(dp) :: D_i                         ! The iterative river depth \( D_i \) [m].
        real(dp) :: f                           ! The function to find roots for \( f(D) \).
        real(dp) :: df                          ! The derivative of \( f(D) \) with respect to \( D \).
        real(dp) :: alpha                       ! Constant extracted from f and df
        integer :: i                            ! Loop iterator to make sure loop isn't endless.
        integer :: iMax                         ! Maximum number of iterations before error.
        real(dp) :: epsilon                     ! Proximity to zero allowed.
        type(ErrorInstance) :: error            ! Variable to store error in.
        character(len=100) :: iChar             ! Loop iterator as character (for error message).
        character(len=100) :: fChar             ! f(D) value as character (for error message).
        character(len=100) :: epsilonChar       ! Proximity of f(D) to zero as character (for error message).

        ! TODO: Allow user (e.g., data file) to specify max iterations and precision?
        D_i = 1.0_dp                                                            ! Take a guess at D being 1m to begin
        i = 1                                                                   ! Iterator for Newton solver
        iMax = 100000                                                           ! Allow 10000 iterations
        epsilon = 1.0e-9_dp                                                     ! Proximity to zero allowed
        alpha = W**(5.0_dp/3.0_dp) * sqrt(S)/me%n                               ! Extract constant to simplify f and df.
        f = alpha*D_i*((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q                    ! First value for f, based guessed D_i

        ! Loop through and solve until f(D) is within e-9 of zero, or max iterations reached
        do while (abs(f) > epsilon .and. i <= iMax)
            f = alpha * D_i * ((D_i/(W+2*D_i))**(2.0_dp/3.0_dp)) - Q            ! f(D) based on D_{m-1}
            df = alpha * ((D_i)**(5.0_dp/3.0_dp) * (6*D_i + 5*W))/(3*D_i * (2*D_i + W)**(5.0_dp/3.0_dp))
            D_i = D_i - f/df                                                    ! Calculate D_i based on D_{m-1}
            i = i + 1
        end do

        if (isnan(D_i)) then                                                    ! If method diverges (results in NaN)
            write(iChar,*) i
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method diverged to NaN after " // trim(adjustl(iChar)) // " iterations." &
            )
        else if (i > iMax) then                                                 ! If max number of iterations reached
            write(iChar,*) iMax
            write(fChar,*) f
            write(epsilonChar,*) epsilon
            error = ErrorInstance( &
                code = 300, &
                message = "Newton's method failed to converge - maximum number of iterations (" &
                    // trim(adjustl(iChar)) // ") exceeded. " &
                    // "Precision (proximity to zero) required: " // trim(adjustl(epsilonChar)) &
                    // ". Final value: " // trim(adjustl(fChar)) // "." &
            )
        else
            error = ERROR_HANDLER%getNoError()                                  ! Otherwise, no error occurred
        end if
        call rslt%setData(D_i)
        call rslt%addError(error)
        call rslt%addToTrace("Calculating river depth")
    end function

    !> Calculate the velocity of the river:
    !! $$
    !!      v = \frac{Q}{WD}
    !! $$
    function calculateVelocity(me, D, Q, W) result(v)
        class(EstuaryReach), intent(in) :: me    !! This `EstuaryReach` instance
        real(dp), intent(in) :: D               !! River depth \( D \) [m]
        real(dp), intent(in) :: Q               !! Flow rate \( Q \) [m**3/s]
        real(dp), intent(in) :: W               !! River width \( W \) [m]
        real(dp) :: v                           !! The calculated velocity \( v \) [m/s]
        if (isZero(Q) .or. isZero(W) .or. isZero(D)) then
            v = 0.0_dp
        else
            v = Q/(W*D)
        end if
    end function

    function j_spm_inflows(me)
        class(EstuaryReach) :: me
        real(dp) :: j_spm_inflows(C%nSizeClassesSpm)
        if (me%nInflows > 0) then
            j_spm_inflows = sum(me%j_spm(2:1+me%nInflows,:), dim=1)
        else
            j_spm_inflows = 0
        end if
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
        j_np_diffusesource = sum(me%j_np(5+me%nInflows:5+me%nInflows+me%nDiffuseSources,:,:,:), dim=1)
    end function

    !> Get the total point source fluxes from NM flux array
    function j_np_pointsourceEstuaryReach(me) result(j_np_pointsource)
        class(EstuaryReach) :: me
        real(dp) :: j_np_pointsource(C%npDim(1), C%npDim(2), C%npDim(3))
        j_np_pointsource &
            = sum(me%j_np(5+me%nInflows+me%nDiffuseSources:4+me%nInflows+me%nDiffuseSources+me%nPointSources,:,:,:), dim=1)
    end function

    !> Set the runoff flux of the SPM flux array
    subroutine set_j_spm_runoff(me, j_spm_runoff)
        class(EstuaryReach) :: me
        real(dp) :: j_spm_runoff(C%nSizeClassesSpm)
        me%j_spm(2+me%nInflows,:) = j_spm_runoff
    end subroutine

    !> Set the transfer flux of the SPM flux array
    subroutine set_j_spm_transfer(me, j_spm_transfer)
        class(EstuaryReach) :: me
        real(dp) :: j_spm_transfer(C%nSizeClassesSpm)
        me%j_spm(3+me%nInflows,:) = j_spm_transfer
    end subroutine

    !> Set the settling/resuspension flux of the SPM flux array
    subroutine set_j_spm_deposit(me, j_spm_deposit)
        class(EstuaryReach) :: me
        real(dp) :: j_spm_deposit(C%nSizeClassesSpm)
        me%j_spm(4+me%nInflows,:) = j_spm_deposit
    end subroutine


end module
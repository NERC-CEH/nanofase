module classGridCell2
    use Globals
    use UtilModule
    use classDatabase, only: DATASET
    use classLogger
    use ResultModule
    use spcGridCell
    use classSoilProfile1
    use classRiverReach
    use classEstuaryReach
    use classCrop
    implicit none

    !> Responsible for the creation of simulation of grid cells
    !! and contained compartments (e.g. rivers, soils).
    type, public, extends(GridCell) :: GridCell2
      contains
        ! Create/destroy
        procedure :: create => createGridCell2
        procedure :: finaliseCreate => finaliseCreateGridCell2
        procedure :: destroy => destroyGridCell2
        ! procedure, private :: setBranchRouting
        ! procedure, private :: setRiverReachLengths
        procedure, private :: createReaches
        ! Simulators
        procedure :: update => updateGridCell2
        procedure :: finaliseUpdate => finaliseUpdateGridCell2
        procedure :: demands => demandsGridCell2
        procedure :: transfers => transfersGridCell2
        ! Data handlers
        procedure :: parseInputData => parseInputDataGridCell2
        ! Getters
        procedure :: get_Q_out => get_Q_outGridCell2
        procedure :: get_j_spm_out => get_j_spm_outGridCell2
        procedure :: get_j_np_out => get_j_np_outGridCell2
        procedure :: get_m_spm => get_m_spmGridCell2
        procedure :: get_m_np => get_m_npGridCell2
    end type

  contains

    !> Create a GridCell with coordinates x and y.
    function createGridCell2(me, x, y, isEmpty) result(rslt)
        class(GridCell2), target :: me              !! The `GridCell2` instance.
        type(Result)          :: rslt               !! The `Result` object to return.
        integer               :: x, y               !! Spatial index of the grid cell
        logical, optional     :: isEmpty            !! Is anything to be simulated in this `GridCell`?
        integer               :: s                  ! Iterator for `DiffuseSource`s
        type(SoilProfile1)    :: soilProfile        ! The soil profile contained in this GridCell

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%colSoilProfiles(1))
        allocate(me%routedRiverReaches(0,0))            ! No routed RiverReach pointers to begin with
        allocate(me%j_np_diffuseSource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2))
        me%q_runoff = 0   

        ! Set the GridCell's position, whether it's empty and its name
        me%x = x
        me%y = y
        if (present(isEmpty)) me%isEmpty = isEmpty      ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module
        
        ! Only carry on if there's stuff to be simulated for this GridCell
        if (.not. me%isEmpty) then
            
            ! Parse the input data for this cell
            rslt = me%parseInputData()

            ! Create the DiffuseSource object(s), if this cell has any
            if (me%hasDiffuseSource) then
                do s = 1, size(me%diffuseSources, 1)
                    call rslt%addErrors(.errors. me%diffuseSources(s)%create(me%x, me%y, s))
                end do
            end if

            ! Create a soil profile and add to this GridCell
            call rslt%addErrors(.errors. &
                soilProfile%create( &
                    me%x, &
                    me%y, &
                    1, &
                    me%slope, &
                    me%n_river, &
                    me%area, &
                    me%q_precip_timeSeries, &
                    me%q_evap_timeSeries &
                ) &
            )
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)

            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. rslt%hasCriticalError()) then
                ! Add RiverReaches to the GridCell (if any are present in the data file)
                call rslt%addErrors(.errors. me%createReaches())
            end if
        end if

        call rslt%addToTrace("Creating " // trim(me%ref))
        call LOG%toFile(errors = .errors. rslt)
        call ERROR_HANDLER%trigger(errors = .errors. rslt)
        call rslt%clear()                  ! Clear errors from the Result object so they're not reported twice
        if (.not. me%isEmpty) then
            call LOG%toConsole("\tCreating " // trim(me%ref) // ": \x1B[32msuccess\x1B[0m")
            call LOG%toFile("Creating " // trim(me%ref) // ": success")
        else
            call LOG%toConsole("\tCreating " // trim(me%ref) // ": \x1B[32mempty\x1B[0m")
            call LOG%toFile("Creating " // trim(me%ref) // ": empty")
        end if
    end function

    !> Set the routedRiverReaches array, presuming that the first reach in each
    !! branch has already been set by the GridCell%create method. Then set
    !! river reach lengths.
    function finaliseCreateGridCell2(me) result(r)
        class(GridCell2) :: me              !! This `GridCell2` instance
        type(Result) :: r                   !! The `Result` object to return
        integer :: b                        ! River branch iterator
        integer :: rr                       ! RiverReach iterator
        ! Loop through the branch and call the setBranchRouting
        ! function, which follows a particular branch downstream and fills
        ! routedRiverReaches accordingly
        ! if (.not. me%isEmpty) then
        !     do b = 1, me%nBranches
        !         call r%addErrors(.errors. me%setBranchRouting(b,1))
        !         call r%addErrors(.errors. me%setRiverReachLengths(b))
        !     end do
        ! end if

        ! Trigger any errors
        call r%addToTrace("Finalising creation of " // trim(me%ref))
        call LOG%toFile(errors=.errors.r)
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear errors from the Result object so they're not reported twice
    end function

    function createReaches(me) result(rslt)
        class(GridCell2), target :: me          !! This `GridCell2` instance
        type(Result) :: rslt                    !! The `Result` object to return any errors in
        integer :: w

        ! Loop through waterbodies and create them
        do w = 1, me%nReaches
            ! What type of waterbody is this?
            if (me%reachTypes(w) == 'riv') then
                allocate(RiverReach::me%colRiverReaches(rr)%item)
            else if (me%reachTypes(w) == 'est') then
                allocate(EstuaryReach::me%colRiverReaches(rr)%item)
            else
                call rslt%addError(ErrorInstance( &
                    message="Trying to create waterbody of unknown type " // trim(me%reachTypes(w)) // "." &
                ))
            end if
            ! Call creation method, passing the outflow and inflows from the input data.
            ! This is presuming that each branch in the cell has one reach, and thus the
            ! grid cell outflow is the outflow for all reaches in the cell, and the inflows
            ! are the inflows to each reach
            if (.not. DATASET%isHeadwater(me%x, me%y)) then
                call rslt%addErrors(.errors. me%colRiverReaches(w)%item%create( &
                    me%x, &
                    me%y, &
                    w, &
                    me%area, &
                    [DATASET%outflow(:, me%x, me%y), DATASET%inflows(:, w, me%x, me%y)] &
                ))
            else
                print *, DATASET%inflows(:, w, me%x, me%y)
                call rslt%addErrors(.errors. me%colRiverReaches(w)%item%create( &
                    me%x, &
                    me%y, &
                    w, &
                    me%area, &
                    [DATASET%outflow(:, me%x, me%y)] &
                ))
            end if 
        end do


    end function

    ! FINALISE CREATE?

    ! SET BRANCH ROUTING?

    ! SET REACH LENGTHS?

    !> Destroy this `GridCell`
    function destroyGridCell2(Me) result(r)
        class(GridCell2) :: Me                              !! The `GridCell` instance.
        type(Result) :: r                                   !! The `Result` object
        integer :: x                                        ! Loop iterator
        ! TODO:  Get rid
    end function

    !> Perform the simulations required for an individual time step
    function updateGridCell2(me, t) result(r)
        class(GridCell2) :: me                                  !! The `GridCell` instance
        integer :: t                                            !! The timestep we're on
        type(Result) :: r                                       !! `Result` object to return errors in
        integer :: i, b                                            ! Iterator
        real(dp) :: lengthRatio                                 ! Reach length as a proportion of total river length
        real(dp) :: j_np_runoff(C%nSizeClassesNP, 4, 2 + C%nSizeClassesSpm) ! NP runoff for this time step

        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            ! Reset variables
            me%j_np_diffuseSource = 0.0_dp
            
            ! Get any inputs from diffuse source
            if (me%hasDiffuseSource) then
                do i = 1, size(me%diffuseSources)
                    call r%addErrors(.errors. me%diffuseSources(i)%update(t))
                    me%j_np_diffuseSource = me%j_np_diffuseSource + me%diffuseSources(i)%j_np_diffuseSource     ! [kg/m2/timestep]
                end do
            end if
            
            ! Demands and transfers
            call r%addErrors([ &
                .errors. me%demands(), &
                .errors. me%transfers() &
            ])

            ! Loop through all SoilProfiles (only one for the moment), run their
            ! simulations and store the eroded sediment in this object
            ! TODO Add DiffuseSource to soil profile
            call r%addErrors( &
                .errors. me%colSoilProfiles(1)%item%update(t, me%j_np_diffuseSource) &
            )
            if (r%hasCriticalError()) return
            me%erodedSediment = me%colSoilProfiles(1)%item%erodedSediment

            ! Reaches will be updated separately in reach routing order, by the `Environment` object

            ! Loop through the reaches in their routed order, and call their update methods for this
            ! timestep, providing them the eroded sediment split according to their length
            ! TODO Add diffuse source to reaches
            ! do b = 1, me%nBranches
            !     do rr = 1, me%nReachesInBranch(b)
            !         ! Determine the proportion of this reach's length to the the total
            !         ! river length in this GridCell
            !         lengthRatio = me%routedRiverReaches(b,rr)%item%length/sum(me%branchLengths)
            !         ! HACK to set NP runoff to 1e-9 SPM runoff
            !         ! j_np_runoff = lengthRatio*sum(me%erodedSediment)*0
            !         j_np_runoff = lengthRatio*me%colSoilProfiles(1)%item%m_np_eroded    ! [kg/timestep]
            !         ! Update the reach for this timestep
            !         call r%addErrors(.errors. &
            !             me%routedRiverReaches(b,rr)%item%update( &
            !                 t = t, &
            !                 q_runoff = me%q_runoff_timeSeries(t), &
            !                 j_spm_runoff = me%erodedSediment*lengthRatio, &
            !                 j_np_runoff = j_np_runoff &
            !             ) &
            !         )
            !     end do
            ! end do
        end if

        ! Set flag to see we've run the update for this timestep
        me%isUpdated = .true.

        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call LOG%toFile(errors = .errors. r)            ! Log any errors to the output file
        call ERROR_HANDLER%trigger(errors = .errors. r)
        call r%clear()                  ! Clear the errors so we're not reporting twice
        call LOG%toFile("Performing simulation for " // trim(me%ref) // " on time step #" // trim(str(t)) // ": success")
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another `RiverReach` whilst the reaches
    !! are looped through.
    subroutine finaliseUpdateGridCell2(me)
        class(GridCell2) :: me                                      !! This `GridCell2` instace
        integer :: rr                                               ! Iterator for reaches
        if (.not. me%isEmpty) then
            do rr = 1, me%nReaches
                call me%colRiverReaches(rr)%item%finaliseUpdate()
            end do
            me%isUpdated = .false.      ! Reset updated flag for the next timestep
        end if
    end subroutine

    !> Process the water demands for this `GridCell`
    function demandsGridCell2(me) result(r)
        class(GridCell2) :: me
        type(Result) :: r
        integer :: pcLossUrban = 0                      ! TODO where should this come from?
        integer :: pcLossRural = 0                      ! TODO where should this come from?
        integer :: pcLossLivestockConsumption = 10      ! TODO where should this come from?
        real(dp) :: cattleDemandPerCapita = 140         ! TODO where should this come from?
        real(dp) :: sheepGoatDemandPerCapita = 70       ! TODO where should this come from?
        real(dp) :: totalUrbanDemand
        real(dp) :: totalLivestockDemand
        real(dp) :: totalRuralDemand
        
        ! TODO Population increase factor is excluded here - check this is okay?
        ! I'm thinking that population increase can be factored into population
        ! numbers in dataset instead
        totalUrbanDemand = (me%urbanPopulation * me%urbanDemandPerCapita * 1.0e-9)/(1.0_dp - 0.01_dp * pcLossUrban)   ! [Mm3/day]
        totalLivestockDemand = ((me%cattlePopulation * cattleDemandPerCapita + me%sheepGoatPopulation * sheepGoatDemandPerCapita) &
                                * 0.01_dp * pcLossLivestockConsumption * 1.0e-9) / (1.0_dp - 0.01_dp * pcLossRural)
        totalRuralDemand = ((me%totalPopulation - me%urbanPopulation) * me%ruralDemandPerCapita * 1.0e-9) &
                            / (1.0_dp - 0.01_dp * pcLossRural)
        ! TODO See Virginie's email 29/08/2018
        
    end function
    
    !> Process the water abstractions and transferss for this `GridCell`
    function transfersGridCell2(me) result(r)
        class(GridCell2) :: me
        type(Result) :: r
        ! Transfer some water!
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell2(me) result(r)
        class(GridCell2)        :: me                   !! This `GridCell2` object
        type(Result)            :: r                    !! The `Result` object
        integer, allocatable    :: xySize(:)            ! The size of the GridCell
        integer                 :: i                    ! Iterator
        type(Result)            :: rslt                 ! Variable to store Result in
        integer                 :: hasLargeCityInt      ! Integer representation of hasLargeCity logical
        integer                 :: cropType             ! Temporary var to store crop type int in
        real(dp)                :: cropArea             ! Temporary var to store crop area in
        integer                 :: cropPlantingMonth    ! Temporary var to store crop planting month in

        ! Allocate arrays to store flows in
        allocate(me%q_runoff_timeSeries(C%nTimeSteps))
        allocate(me%q_evap_timeSeries(C%nTimeSteps))
        allocate(me%q_precip_timeSeries(C%nTimeSteps))
        allocate(me%T_water_timeSeries(C%nTimeSteps))

        ! Get grid cell size from grid resolution
        me%dx = DATASET%gridRes(1)
        me%dy = DATASET%gridRes(2)
        me%area = me%dx * me%dy
        
        ! Set the data interfacer's group to the group for this GridCell
        call r%addErrors(.errors. DATA%setGroup([character(len=100)::'Environment', me%ref]))

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
        end if

        ! Get the number of waterbodies
        me%nReaches = DATASET%nWaterbodies(me%x, me%y)
        allocate(me%reachTypes(me%nReaches))
        ! What are the types of those waterbodies?
        if (DATASET%isEstuary(me%x, me%y)) then
            me%reachTypes = 'est'
        else
            me%reachTypes = 'riv'
        end if

        ! Get hydrology, topography and water data
        call r%addErrors([ &
            ! .errors. DATA%get('runoff', me%q_runoff_timeSeries, 0.0_dp), &
            ! .errors. DATA%get('quickflow', me%q_quickflow_timeSeries, 0.0_dp), &
            ! .errors. DATA%get('precip', me%q_precip_timeSeries, 0.0_dp), &          ! [m/s]
            ! .errors. DATA%get('evap', me%q_evap_timeSeries, 0.0_dp), &
            .errors. DATA%get('slope', me%slope), &
            .errors. DATA%get('n_river', me%n_river, 0.035_dp), &
            .errors. DATA%get('T_water', me%T_water_timeSeries, C%defaultWaterTemperature, warnIfDefaulting=.false.) &
        ])
        ! FLAT DATA
        me%q_runoff_timeSeries = DATASET%runoff(me%x, me%y, :)
        me%q_precip_timeSeries = DATASET%precip(me%x, me%y, :)
        me%q_evap_timeSeries = DATASET%evap(me%x, me%y, :)
        
        ! Try and set the group to the demands group. It will produce an error if group
        ! doesn't exist - use this to set me%hasDemands to .false.
        rslt = DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands'])
        if (.not. rslt%hasError()) then
            me%hasDemands = .true.
            ! Now get the data from the group. These should all default to zero.
            ! TODO What should the default surface water to total water ratio be?
            call r%addErrors([ &
                .errors. DATA%get('total_population', me%totalPopulation, 0.0_dp), &
                .errors. DATA%get('urban_population', me%urbanPopulation, 0.0_dp), &
                .errors. DATA%get('cattle_population', me%cattlePopulation, 0.0_dp), &
                .errors. DATA%get('sheep_goat_population', me%sheepGoatPopulation, 0.0_dp), &
                .errors. DATA%get('urban_demand', me%urbanDemandPerCapita, 0.0_dp), &
                .errors. DATA%get('rural_demand', me%ruralDemandPerCapita, 0.0_dp), &
                .errors. DATA%get('industrial_demand', me%industrialDemand, 0.0_dp), &
                .errors. DATA%get('sw_to_tw_ratio', me%surfaceWaterToTotalWaterRatio, 0.42_dp, warnIfDefaulting=.true.), &
                .errors. DATA%get('has_large_city', hasLargeCityInt, 0) &
            ])
            me%hasLargeCity = lgcl(hasLargeCityInt)     ! Convert int to bool
            
            ! Check if there are any crops to get. These will be retrieved iteratively
            ! (i.e. crop_1, crop_2, crop_3). Then get the data for those crops and create
            ! array of Crop objects in me%crops
            i = 1
            do while (DATA%grp%hasGroup("crop_" // trim(str(i))))
                allocate(me%crops(i))
                call r%addErrors(.errors. &
                    DATA%setGroup([character(len=100)::'Environment', me%ref, 'demands', 'crop_' // trim(str(i))]))
                call r%addErrors([ &
                    .errors. DATA%get('crop_area', cropArea), &
                    .errors. DATA%get('crop_type', cropType), &
                    .errors. DATA%get('planting_month', cropPlantingMonth) &
                ])
                me%crops(i) = Crop(cropType, cropArea, cropPlantingMonth)
                i = i+1
            end do
        end if  

    end function

!---------------!
!--- GETTERS ---!
!---------------!

    function get_Q_outGridCell2(me, b) result(Q_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: Q_out
        integer :: r
        Q_out = 0
        ! If branch is present, just get the outflow for that branch,
        ! else, sum the outflows from each branch
        if (present(b)) then
            Q_out = me%routedRiverReaches(b,me%nReachesInBranch(b))%item%Q(1)
        else
            do b = 1, me%nBranches
                Q_out = Q_out + me%routedRiverReaches(b,me%nReachesInBranch(b))%item%Q(1)
            end do
        end if
    end function

    function get_j_spm_outGridCell2(me, b) result(j_spm_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: j_spm_out(C%nSizeClassesSpm)
        j_spm_out = 0
        ! If branch is present, just get the outflow for that branch,
        ! else, sum the outflows from each branch
        if (present(b)) then
            j_spm_out = me%routedRiverReaches(b,me%nReachesInBranch(b))%item%j_spm(1,:)
        else
            do b = 1, me%nBranches
                j_spm_out = j_spm_out + me%routedRiverReaches(b,me%nReachesInBranch(b))%item%j_spm(1,:)
            end do
        end if
    end function

    function get_j_np_outGridCell2(me, b) result(j_np_out)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: j_np_out(C%nSizeClassesNP)
        j_np_out = 0
        ! TODO: Sum NP outflows here
    end function

    !> Get the total mass of SPM currently in the GridCell
    function get_m_spmGridCell2(me, b) result(m_spm)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: m_spm(C%nSizeClassesSpm)
        integer :: rr
        m_spm = 0
        if (present(b)) then
            ! Loop through the reaches in the branch and sum the SPM masses
            do rr = 1, me%nReachesInBranch(b)
                m_spm = m_spm + me%routedRiverReaches(b,rr)%item%m_spm
            end do
        else
            ! Loop through the reaches and sum the SPM masses
            do rr = 1, me%nReaches
                m_spm = m_spm + me%colRiverReaches(rr)%item%m_spm
            end do
        end if
    end function

    !> Get the total mass of NPs currently in the GridCell
    function get_m_npGridCell2(me, b) result(m_np)
        class(GridCell2) :: me                      !! This `GridCell2` instance
        integer, optional :: b                      !! Branch
        real(dp) :: m_np(C%nSizeClassesNP)
        m_np = 0
        ! TODO: Sum NP masses here
    end function
    
end module
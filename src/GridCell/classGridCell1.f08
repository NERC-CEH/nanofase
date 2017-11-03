module classGridCell1
                                                                         ! superclass for SubRiver subclasses
                                                                         ! defines properties and methods required in any implmentation
                                                                         ! of a River class
                                                                         ! a River class acts as a container for a collection of RiverReach objects which collectively define the
                                                                         ! layout of the flowing waters within each grid cell
                                                                         ! the RiverReach class routes, water, suspended sediments (and ultimately nanoparticles) through the flowing waters within
                                                                         ! the grid cell
                                                                         ! IMPORTED MODULES
                                                                         ! Description
                                                                         ! -----------
    use Globals                                                        ! global declarations
    use UtilModule                                                     ! useful functions, e.g. str()
    use mo_netcdf                                                      ! input/output handling
    use ResultModule                                                   ! error handling classes, required for
    use ErrorInstanceModule                                            ! generation of trace error messages
    use spcGridCell
    use classSoilProfile1
    use classSubRiver1
    implicit none                                                      ! force declaration of all variables
    type, public, extends(GridCell) :: GridCell1                       ! type declaration for subclass
                                                                     ! -----------
      contains
                                                                    ! METHODS
                                                                    ! Description
                                                                    ! -----------
        procedure :: create => createGridCell1                      ! create the GridCell object. Exposed name: create
        procedure :: destroy => destroyGridCell1                    ! remove the GridCell object and all contained objects. Exposed name: destroy
        procedure :: update => updateGridCell1                      ! route water and suspended solids through all SubRiver objects. Exposed name: update
        procedure :: finaliseUpdate => finaliseUpdateGridCell1
        procedure :: parseInputData => parseInputDataGridCell1
        procedure :: erodeSoil => erodeSoilGridCell1
        procedure :: imposeSizeDistribution => imposeSizeDistributionGridCell1
    end type

    !> Interface so that we can create new GridCells by `gc = GridCell1()`
    interface GridCell1
        module procedure newGridCell1
    end interface

  contains
    !> Return a newly-created GridCell1 object.
    !! TODO: Do something with result object
    function newGridCell1(x, y, isEmpty) result(me)
        type(GridCell1) :: me                         !! The new GridCell to return
        integer :: x, y                               !! Location of the GridCell
        logical, optional :: isEmpty                  !! Is anything to be simulated in this GridCell?
        type(Result) :: r                             !! Result object
        ! Create the new GridCell, specifying isEmpty if it's present (it default to false if not)
        if (present(isEmpty)) r = me%create(x, y, isEmpty)
        if (.not. present(isEmpty)) r = me%create(x, y)
    end function

    !> Create a GridCell with coordinates x and y.
    function createGridCell1(me, x, y, isEmpty) result(r)
        class(GridCell1)      :: me                   !! The GridCell instance.
        type(Result)          :: r                    !! The Result object to return.
        integer               :: x, y                 !! Location of the GridCell
        logical, optional     :: isEmpty              !! Is anything to be simulated in this GridCell?
        type(SoilProfile1)    :: soilProfile          !! The soil profile contained in this GridCell
        integer               :: s                    !! Iterator for SubRivers
        integer               :: t                    !! Iterator for time series
        character(len=100)    :: subRiverPrefix       !! Prefix for SubRivers ref, e.g. SubRiver_1_1
        real(dp)              :: subRiverLength       !! Length of the SubRivers
        real(dp), allocatable :: subRiverRunoffTimeSeries(:) !! Runoff to each SubRiver
        type(Result)          :: srR                  !! Result object for individual SubRivers
        type(SubRiver1), allocatable :: sr1           !! SubRiver1 object for storing created SubRiver1s in

        ! TODO: Move to SoilProfile
        allocate(me%usle_C(C%nTimeSteps))
        allocate(me%usle_alpha_half(C%nTimeSteps))
        allocate(me%rusle2015_erodedSediment(C%nTimeSteps))
        allocate(me%erodedSediment(C%nTimeSteps))

        ! Allocate the object properties that need to be and set up defaults
        allocate(me%QrunoffTimeSeries(C%nTimeSteps))
        allocate(subRiverRunoffTimeSeries(C%nTimeSteps))
        allocate(me%colSoilProfiles(1))
        me%Qrunoff = 0                                  ! Default to no runoff

        ! Set the GridCell's position, area, whether it's empty and its name
        me%gridX = x
        me%gridY = y
        me%area = C%gridCellSize**2                   ! TODO: This will be changed to take into account lat-lon
        if (present(isEmpty)) me%isEmpty = isEmpty    ! isEmpty defaults to false if not present
        me%ref = trim(ref("GridCell", x, y))            ! ref() interface is from the Util module

        ! Only carry on if there's stuff to be simulated for this GridCell
        if (me%isEmpty .eqv. .false.) then
            r = me%parseInputData()                         ! Parse and store input data

            ! Create a soil profile and add to this GridCell
            r = soilProfile%create(me%gridX, me%gridY, 1, me%slope, me%n_river)
            allocate(me%colSoilProfiles(1)%item, source=soilProfile)

            ! Add SubRivers to the GridCell (if any are present in the data file)
            ! Only proceed if there are no critical errors (which might be caused by parseInputData())
            if (.not. r%hasCriticalError()) then
                subRiverPrefix = "SubRiver_" // trim(str(me%gridX)) // &
                                    "_" // trim(str(me%gridY)) // "_"
                ! Set SubRiver size to half of the grid cell size if there's more than one SubRiver,
                ! otherwise the full size of the grid cell. TODO: Constrain number of SubRivers somewhere
                ! so this makes sense.
                if (me%nSubRivers > 1) then
                    subRiverLength = C%gridCellSize / 2.0_dp
                else
                    subRiverLength = C%gridCellSize
                end if
                ! Loop through SubRivers, incrementing s (from SubRiver_x_y_s), until none found
                do s = 1, me%nSubRivers
                    ! Split the runoff between SubRivers
                    do t = 1, size(me%QrunoffTimeSeries)
                        if (me%QrunoffTimeSeries(t) > 0) then
                            subRiverRunoffTimeSeries(t) = me%QrunoffTimeSeries(t)/me%nSubRivers
                        else
                            subRiverRunoffTimeSeries(t) = 0
                        end if
                    end do
                    ! Check that group actually exists
                    ! TODO: Maybe perform this check somewhere else
                    if (me%ncGroup%hasGroup(trim(subRiverPrefix) // trim(str(s)))) then
                        allocate(sr1)                                   ! Create the new SubRiver
                        srR = sr1%create(me%gridX, me%gridY, s, subRiverLength, subRiverRunoffTimeSeries)
                        call r%addErrors(errors = .errors. srR)         ! Add any errors to final Result object
                        call move_alloc(sr1, me%colSubRivers(s)%item)   ! Allocate a new SubRiver to the colSubRivers array
                    else
                        call r%addError(ErrorInstance( &
                            code = 501, &
                            message = "No input data provided for " // trim(subRiverPrefix) // trim(str(s)) // &
                                        " - check nSubRivers is set correctly." &
                        ))
                    end if
                end do
            end if
        end if

        call r%addToTrace("Creating " // trim(me%ref))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function

    function destroyGridCell1(Me) result(r)
        class(GridCell1) :: Me                              !! The GridCell instance.
        type(Result) :: r                                   !! The Result object
        type(integer) :: x                                  !! loop counter
        do x = 1, me%nSubRivers
            r = Me%colSubRivers(x)%item%destroy()           ! remove all SubRiver objects and any contained objects
        end do
        do x = 1, Me%nSoilProfiles
            r = Me%colSoilProfiles(x)%item%destroy()        ! remove all SoilProfile objects and any contained objects
        end do
        do x = 1, Me%nPointSources
            r = Me%colPointSources(x)%item%destroy()        ! remove all PointSource objects and any contained objects
        end do
        r = Me%objDiffuseSource%item%destroy()              ! remove the DiffuseSource object and any contained objects
    end function

    !> Perform the simulations required for an individual timestep t.
    function updateGridCell1(Me, t) result(r)
        class(GridCell1) :: Me                                         ! The GridCell instance
        integer :: t                                                   ! The timestep we're on
        type(Result) :: srR                                            ! Result object for SubRivers
        type(Result) :: r                                              ! Result object
        type(integer) :: s                                             ! Loop counter
        ! Check that the GridCell is not empty before simulating anything
        if (.not. me%isEmpty) then
            ! First, calculate the soil erosion for this GridCell
            r = me%erodeSoil(t)
            ! Loop through each SubRiver and run its update procedure
            do s = 1, me%nSubRivers
                srR = me%colSubRivers(s)%item%update(t)
                call r%addErrors(errors = .errors. srR)
            end do
        end if
        ! Add this procedure to the error trace and trigger any errors that occurred
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
        call ERROR_HANDLER%trigger(errors = .errors. r)
    end function

    !> Set the outflow from the temporary outflow variables that were setting by the
    !! update procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another SubRiver whilst the SubRivers
    !! are looped through.
    function finaliseUpdateGridCell1(me) result(r)
        class(GridCell1) :: me                                      !! This SubRiver1 instace
        integer :: s                                                !! Iterator of SubRivers
        type(Result) :: r                                           !! The Result object
        if (.not. me%isEmpty) then
            do s = 1, me%nSubRivers
                r = me%colSubRivers(s)%item%finaliseUpdate()
            end do
        end if
    end function

    !> Calculates soil erosion for this timstep t. Updates this GridCell's
    !! state variable erodedSediment accordingly.
    function erodeSoilGridCell1(me, t) result(r)
        class(GridCell1)    :: me               !! This GridCell instance
        integer             :: t                !! The timestep we're on
        type(NcDataset)     :: nc               !! NetCDF dataset
        type(NcVariable)    :: var              !! NetCDF variable
        type(NcGroup)       :: grp              !! NetCDF group
        real(dp)            :: Q_surf           !! Surface runoff \( Q_{\text{surf}} \)
        real(dp)            :: t_conc           !! Time of concentration \( t_{\text{tc}} \)
        real(dp)            :: q_peak           !! Peak rainfall \( q_{\text{peak}} \)
        real(dp)            :: S_tot            !! Total eroded sediment
        type(Result)        :: r                !! The Result object
        ! Change units of Q_surf from HMF from m3/s for the GridCell, to m3/day for the HRU,
        ! and only use 10% of it to drive soil erosion        
        Q_surf = ((me%QrunoffTimeSeries(t)*me%usle_area_hru*8640/me%area))      ! [m3/day]
        ! Estimate the time of concentration
        t_conc = (me%usle_L_sb**0.6 * me%usle_n_sb**0.6)/(18 * me%usle_slp_sb) &
                + (0.62 * me%usle_L_ch * me%n_river**0.75)/(me%usle_area_sb**0.125 * me%usle_slp_ch**0.375)
        ! Estimate the peak flow
        q_peak = me%usle_alpha_half(t)*Q_surf*me%usle_area_sb/(3.6*t_conc)
        ! Bring this all together to calculate eroded sediment, converted to kg/timestep (from metric ton/day)
        S_tot = (118*C%timeStep/864) * (Q_surf * q_peak * me%usle_area_hru)**0.56 &
                * me%usle_K * me%usle_C(t) * me%usle_P * me%usle_LS * me%usle_CFRG
        me%erodedSediment = me%imposeSizeDistribution(S_tot)
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data.
    function parseInputDataGridCell1(me) result(r)
        class(GridCell1)        :: me                   !! This GridCell1 object
        type(NcDataset)         :: nc                   !! NetCDF dataset
        type(NcVariable)        :: var                  !! NetCDF variable
        type(NcGroup)           :: grp                  !! NetCDF group
        real(dp), allocatable   :: usle_C_min(:)        !! Minimum cover factor for USLE
        real(dp), allocatable   :: usle_C_av(:)         !! Average cover factor for USLE
        integer                 :: usle_rock            !! % rock in top of soil profile, to calculate usle_CFRG param
        real(dp), allocatable   :: usle_rsd(:)          !! Residue on soil surface [kg/ha]
        type(Result)            :: r                    !! The result object
        ! Allocate the data which are time series. These must be allocatable (as opposed to
        ! being declared that length) to work with the mo_netcdf getData() procedure.
        allocate(usle_C_min(C%nTimeSteps))
        allocate(usle_C_av(C%nTimeSteps))
        allocate(usle_rsd(C%nTimeSteps))
        ! Open the dataset
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        me%ncGroup = grp%getGroup(me%ref)                              ! Get this GridCell's group
        ! Get the number of SubRivers for looping over
        if (me%ncGroup%hasVariable("nSubRivers")) then
            var = me%ncGroup%getVariable("nSubRivers")                     
            call var%getData(me%nSubRivers)
        else
            me%nSubRivers = 0   ! If nSubRivers isn't present, default to having no SubRivers
        end if
        allocate(me%colSubRivers(me%nSubRivers))        ! Allocate the colSubRivers array to the number of SubRivers in the GridCell
        ! Get the time-dependent runoff data from the file and put in array ready for use
        ! TODO: Runoff data currently m3/s, but maybe this should be m/s instead?
        if (me%ncGroup%hasVariable("runoff")) then
            var = me%ncGroup%getVariable("runoff")
            call var%getData(me%QrunoffTimeSeries)                 
            me%QrunoffTimeSeries = me%QrunoffTimeSeries*C%timeStep ! Convert to m3/timestep
        else
            me%QrunoffTimeSeries = 0
        end if
        ! Slope of the GridCell [m/m]
        if (me%ncGroup%hasVariable('slope')) then
            var = me%ncGroup%getVariable('slope')
            call var%getData(me%slope)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for slope not found in input file." &
            ))
        end if
        ! Manning's roughness coefficient [-]
        if (me%ncGroup%hasVariable('n_river')) then
            var = me%ncGroup%getVariable('n_river')
            call var%getData(me%n_river)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for n_river not found in input file. " // &
                            "Defaulting to 0.035 (natural streams and major rivers).", &
                isCritical = .false. &
            ))
            me%n_river = 0.035
        end if

        ! -- SOIL EROSION -------------------------------------------------------------!
        ! C     Cover and land management factor, defined as the ratio of soil loss
        !       from land cropped under specified conditions to the corresponding loss
        !       from clean-tilled, continuous flow. Should be time-dependent (as crop
        !       cover changes). [-]
        if (me%ncGroup%hasVariable('usle_C')) then             ! First, check if time series of C-factors is available
            var = me%ncGroup%getVariable('usle_C')
            call var%getData(me%usle_C)
        else                                            ! Else, we can estimate C
            if (me%ncGroup%hasVariable('usle_C_min')) then     ! Use minimum C to estimate C
                var = me%ncGroup%getVariable('usle_C_min')
                call var%getData(usle_C_min)
            else if (me%ncGroup%hasVariable('usle_C_av')) then ! Average annual C can be used to estimate minimum C
                var = me%ncGroup%getVariable('usle_C_av')
                call var%getData(usle_C_av)
                usle_C_min = 1.463*log(usle_C_av) + 0.1034
            else                                        ! If neither exist, default C_min to 1 (fallow soil)
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Values for usle_C_min or usle_C_av not found in input file. " // &
                                "usle_C_min defaulting to 1 (fallow soil).", &
                    isCritical = .false. &
                ))
                usle_C_min = 1
            end if
            if (me%ncGroup%hasVariable('usle_rsd')) then       ! Residue on surface also needed to esimate C [kg/ha]
                var = me%ncGroup%getVariable('usle_rsd')
                call var%getData(usle_rsd)
            else                                        ! Default to zero (no residue = no crops)
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Value for usle_rsd not found in input file. " // &
                                "Defaulting to 0 (no crops).", &
                    isCritical = .false. &
                ))
                usle_rsd = 0
            end if 
            ! Defaults to 0.8, based on C_min = 1 and rsd = 0.
            me%usle_C = exp((log(0.8) - log(usle_C_min))*exp(-0.00115*usle_rsd) + log(usle_C_min))
        end if
        ! K     Soil erodibility factor, which depends on soil structure and is treated as
        !       time-independent. [t ha h ha-1 MJ-1 mm-1]
        if (me%ncGroup%hasVariable('usle_K')) then
            var = me%ncGroup%getVariable('usle_K')
            call var%getData(me%usle_K)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_K not found in input file." &
            ))
        end if
        ! P     Support practice factor, the ratio of soil loss with a specific support
        !       practice to the corresponding loss with up-and-down slope culture. Support
        !       practices: Contour tillage, strip-cropping, terrace systems. [-]
        if (me%ncGroup%hasVariable('usle_P')) then
            var = me%ncGroup%getVariable('usle_P')
            call var%getData(me%usle_P)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_P not found in input file. " // &
                            "Defaulting to 1 (no support practice).", &
                isCritical = .false. &
            ))
            me%usle_P = 1
        end if
        ! LS    Topographic factor, a function of the terrain's topography. [-]
        if (me%ncGroup%hasVariable('usle_LS')) then
            var = me%ncGroup%getVariable('usle_LS')
            call var%getData(me%usle_LS)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_LS not found in input file." &
            ))
        end if
        ! CFRG  Coase fragment factor, CFRG = exp(0.035 * % rock in first soil layer). [-]
        if (me%ncGroup%hasVariable('usle_rock')) then
            var = me%ncGroup%getVariable('usle_rock')          ! % rock in top of soil profile [-]
            call var%getData(usle_rock)
            me%usle_CFRG = exp(-0.052*usle_rock)        ! Coarse fragment factor [-]
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_rock not found in input file. " // &
                            "Defaulting to 0 (no rock in top soil layer).", &
                isCritical = .false. &
            ))
            me%usle_CFRG = 1                            ! Default to 1 (rock_usle = 0)
        end if
        ! Params affecting q_peak
        ! alpha_half        Fraction of rainfall happening in maximum half hour. [-]
        if (me%ncGroup%hasVariable('usle_alpha_half')) then
            var = me%ncGroup%getVariable('usle_alpha_half')
            call var%getData(me%usle_alpha_half)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_alpha_half not found in input file. " // &
                            "Defaulting to 0.33.", &
                isCritical = .false. &
            ))
            me%usle_alpha_half = 0.33                   ! Defaults to 0.33
        end if
        ! Area of the HRU [ha]
        if (me%ncGroup%hasVariable('usle_area_hru')) then
            var = me%ncGroup%getVariable('usle_area_hru')
            call var%getData(me%usle_area_hru)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_area_hru not found in input file." &
            ))
        end if
        ! Subbassin area [km2]
        if (me%ncGroup%hasVariable('usle_area_sb')) then
            var = me%ncGroup%getVariable('usle_area_sb')
            call var%getData(me%usle_area_sb)
        else if (me%ncGroup%hasVariable('usle_area_hru')) then         ! Default to area_hru, if that is present
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_area_sb not found in input file. " // &
                            "Defaulting to usle_area_hru (" // trim(str(me%usle_area_hru)) // " ha).", &
                isCritical = .false. &
            ))
            me%usle_area_sb = me%usle_area_hru*0.01             ! Convert from ha to km2
        else                                                    ! Otherwise, throw a critical error
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_area_sb not found in input file. " &
            ))
        end if
        ! Subbasin slope length [m]
        if (me%ncGroup%hasVariable('usle_L_sb')) then
            var = me%ncGroup%getVariable('usle_L_sb')
            call var%getData(me%usle_L_sb)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_L_sb not found in input file. " // &
                            "Defaulting to 50 m.", &
                isCritical = .false. &
            ))
            me%usle_L_sb = 50
        end if
        ! Manning's coefficient for the subbasin. [-]
        if (me%ncGroup%hasVariable('usle_n_sb')) then
            var = me%ncGroup%getVariable('usle_n_sb')
            call var%getData(me%usle_n_sb)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_n_sb not found in input file. " // &
                            "Defaulting to 0.01 (fallow, no residue).", &
                isCritical = .false. &
            ))
            me%usle_n_sb = 0.01
        end if
        ! Slope of the subbasin [m/m]. Defaults to GridCell slope.
        if (me%ncGroup%hasVariable('usle_slp_sb')) then
            var = me%ncGroup%getVariable('usle_slp_sb')
            call var%getData(me%usle_slp_sb)
        else if (me%ncGroup%hasVariable('slope')) then             ! Default to GridCell slope, if present
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_sb not found in input file. " // &
                            "Defaulting to GridCell slope (" // trim(str(me%slope)) // ").", &
                isCritical = .false. &
            ))
            me%usle_slp_sb = me%slope
        else                                                ! Otherwise, thrown a critical error
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_sb not found in input file." &
            ))
        end if
        !> Slope of the channel [m/m]. Defaults to GridCell slope.
        if (me%ncGroup%hasVariable('usle_slp_ch')) then
            var = me%ncGroup%getVariable('usle_slp_ch')
            call var%getData(me%usle_slp_ch)
        else if (me%ncGroup%hasVariable('slope')) then             ! Default to GridCell slope, if present
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_ch not found in input file. " // &
                            "Defaulting to GridCell slope (" // trim(str(me%slope)) // ").", &
                isCritical = .false. &
            ))
            me%usle_slp_ch = me%slope
        else                                                ! Otherwise, thrown a critical error
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_ch not found in input file." &
            ))
        end if
        !> Hillslope length of the channel [km]
        if (me%ncGroup%hasVariable('usle_L_ch')) then
            var = me%ncGroup%getVariable('usle_L_ch')
            call var%getData(me%usle_L_ch)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_L_ch not found in input file. " &
            ))
        end if
        ! Check if RUSLE2015's eroded sediment data has been provided, for comparison's sake
        if (me%ncGroup%hasVariable('rusle2015_erodedSediment')) then
            var = me%ncGroup%getVariable('rusle2015_erodedSediment')
            call var%getData(me%rusle2015_erodedSediment)
        end if
        call r%addToTrace('Parsing input data')             ! Add this procedure to the trace
    end function

    !> Impose a size class distribution on a total mass to split it up
    !! into separate size classes
    function imposeSizeDistributionGridCell1(me, mass) result(distribution)
        class(GridCell1) :: me
        real(dp) :: mass
        integer :: i
        real(dp) :: distribution(C%nSizeClassesSpm)
        do i = 1, C%nSizeClassesSpm
            distribution(i) = mass*C%defaultDistributionSpm(i)*0.01
        end do
        ! TODO: Check mass = sum(distribution(i)) - actually, just check sum(distributionSpm(i)) = 100
    end function
end module
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
        procedure :: erode => erodeSoilProfile1                 ! Erode soil for a given timestep
        procedure :: imposeSizeDistribution => imposeSizeDistributionSoilProfile1 ! Impose size distribution on mass of sediment
        procedure :: parseInputData => parseInputDataSoilProfile1 ! Parse the data from the input file and store in object properties
    end type

  contains
    !> Creating the SoilProfile parses input data and fills
    !! the corresponding object properties, as well as setting
    !! up the contained SoilLayers.
    function createSoilProfile1(me, x, y, p, slope, n_river, area) result(r)
        class(SoilProfile1) :: me                           !! The SoilProfile instance.
        integer             :: x                            !! Containing GridCell x position
        integer             :: y                            !! Containing GridCell y position
        integer             :: p                            !! SoilProfile reference (redundant for now as only one SoilProfile per GridCell)
        real(dp)            :: slope                        !! Slope of the containing GridCell [m/m]
        real(dp)            :: n_river                      !! Manning's roughness coefficient for the GridCell's rivers [-]
        real(dp)            :: area                         !! The surface area of the SoilProfile [m3]
        type(Result)        :: r                            !! The Result object

        ! Allocate the object properties that need to be
        allocate(me%usle_C(C%nTimeSteps))
        allocate(me%usle_alpha_half(C%nTimeSteps))
        allocate(me%rusle2015_erodedSediment(C%nTimeSteps))
        allocate(me%erodedSediment(C%nTimeSteps))
        allocate(me%distributionSediment(C%nSizeClassesSpm))

        me%x = x                                            ! GridCell x index
        me%y = y                                            ! GridCell y index
        me%p = p                                            ! SoilProfile index within the GridCell
        me%ref = trim(ref("SoilProfile", x, y, p))          ! Generate the reference name for the SoilProfile
        me%slope = slope
        me%n_river = n_river
        me%area = area
        
        ! Parse and store input data in this object
        r = me%parseInputData()                             
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    function destroySoilProfile1(me) result(r)
        class(SoilProfile1) :: me                               !! This SoilProfile instance
        type(Result) :: r                                       !! Result object to return
    end function

    function updateSoilProfile1(me, t, Qrunoff) result(r)
        class(SoilProfile1) :: me                               !! This SoilProfile instance
        integer :: t                                            !! The current timestep
        real(dp) :: Qrunoff                                     !! Runoff generated on this timestep
        type(Result) :: r                                       !! Result object to return
        me%Qrunoff = Qrunoff                                    ! Set the runoff for this timestep
        r = me%erode(t)                                         ! Erode soil on this timestep
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function

    !> Calculates soil erosion for this timstep t. Updates this GridCell's
    !! state variable erodedSediment accordingly.
    function erodeSoilProfile1(me, t) result(r)
        class(SoilProfile1) :: me               !! This SoilProfile instance
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
        Q_surf = ((me%Qrunoff*me%usle_area_hru*8640/me%area))      ! [m3/day]
        ! Estimate the time of concentration
        t_conc = (me%usle_L_sb**0.6 * me%usle_n_sb**0.6)/(18 * me%usle_slp_sb) &
                + (0.62 * me%usle_L_ch * me%n_river**0.75)/(me%usle_area_sb**0.125 * me%usle_slp_ch**0.375)
        ! Estimate the peak flow
        q_peak = me%usle_alpha_half(t)*Q_surf*me%usle_area_sb/(3.6*t_conc)
        ! Bring this all together to calculate eroded sediment, converted to kg/timestep (from metric ton/day)
        S_tot = (118*C%timeStep/864) * (Q_surf * q_peak * me%usle_area_hru)**0.56 &
                * me%usle_K * me%usle_C(t) * me%usle_P * me%usle_LS * me%usle_CFRG
        ! TODO: Need to convert sediment yield for the HRU to sediment yield for the grid cell.
        ! Simply scaling linearly from HRU area to grid cell area like below isn't realistic
        ! (not everywhere in the grid cell is going to be contributing as much as whatever
        ! HRU we're doing the calculation for).
        me%erodedSediment = me%imposeSizeDistribution(S_tot*me%area/me%usle_area_hru)
        call r%addToTrace("Eroding soil on timestep #" // trim(str(t)))
    end function

    !> Impose a size class distribution on a total mass to split it up
    !! into separate size classes. If a size distribution has been specified
    !! for this SoilProfile, use that, otherwise, use the global size distribution.
    function imposeSizeDistributionSoilProfile1(me, mass) result(distribution)
        class(SoilProfile1) :: me                               !! This SoilProfile instance
        real(dp)            :: mass                             !! The mass to split into size classes
        integer             :: i                                !! Loop iterator for size classes
        real(dp)            :: distribution(C%nSizeClassesSpm)  !! The resulting distribution
        do i = 1, C%nSizeClassesSpm
            distribution(i) = mass*me%distributionSediment(i)*0.01
        end do
        ! TODO: Check mass = sum(distribution(i)) - actually, just check sum(distributionSpm(i)) = 100
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data
    function parseInputDataSoilProfile1(me) result(r)
        class(SoilProfile1)     :: me                       !! This SoilProfile instance
        type(NcDataset)         :: nc                       !! NetCDF dataset
        type(NcVariable)        :: var                      !! NetCDF variable
        type(NcGroup)           :: grp                      !! NetCDF group
        real(dp), allocatable   :: usle_C_min(:)            !! Minimum cover factor for USLE
        real(dp), allocatable   :: usle_C_av(:)             !! Average cover factor for USLE
        integer                 :: usle_rock                !! % rock in top of soil profile, to calculate usle_CFRG param
        real(dp), allocatable   :: usle_rsd(:)              !! Residue on soil surface [kg/ha]
        type(Result)            :: r                        !! Result object to return

        ! Allocate the data which are time series. These must be allocatable (as opposed to
        ! being declared that length) to work with the mo_netcdf getData() procedure.
        allocate(usle_C_min(C%nTimeSteps))
        allocate(usle_C_av(C%nTimeSteps))
        allocate(usle_rsd(C%nTimeSteps))

        ! Open the dataset
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")                        ! Get the Environment group
        grp = grp%getGroup(ref("GridCell",me%x,me%y))           ! Get the containing GridCell's group
        me%ncGroup = grp%getGroup(me%ref)                       ! Get this SoilProfile's group

        ! Distribution used to split sediment mass into size classes
        if (me%ncGroup%hasVariable('distributionSediment')) then
            var = me%ncGroup%getVariable('distributionSediment')
            call var%getData(me%distributionSediment)
            ! Check the distribution adds up to 100%
            call r%addError( &
                ERROR_HANDLER%equal( &
                    value = sum(me%distributionSediment), &
                    criterion = 100, &
                    message = "Specified size class distribution for sediments " &
                                // "does not sum to 100%." &
                ) &
            )
        else                                                    ! Default to value specified in globals
            me%distributionSediment = C%defaultDistributionSediment ! Should have been checked that it sums to 100%
        end if

        ! -- SOIL EROSION DATA ---------------------------------------------------------!
        ! C     Cover and land management factor, defined as the ratio of soil loss
        !       from land cropped under specified conditions to the corresponding loss
        !       from clean-tilled, continuous flow. Should be time-dependent (as crop
        !       cover changes). [-]
        if (me%ncGroup%hasVariable('usle_C')) then              ! First, check if time series of C-factors is available
            var = me%ncGroup%getVariable('usle_C')
            call var%getData(me%usle_C)
        else                                                    ! Else, we can estimate C
            if (me%ncGroup%hasVariable('usle_C_min')) then      ! Use minimum C to estimate C
                var = me%ncGroup%getVariable('usle_C_min')
                call var%getData(usle_C_min)
            else if (me%ncGroup%hasVariable('usle_C_av')) then  ! Average annual C can be used to estimate minimum C
                var = me%ncGroup%getVariable('usle_C_av')
                call var%getData(usle_C_av)
                usle_C_min = 1.463*log(usle_C_av) + 0.1034
            else                                                ! If neither exist, default C_min to 1 (fallow soil)
                call r%addError(ErrorInstance( &
                    code = 201, &
                    message = "Values for usle_C_min or usle_C_av not found in input file. " // &
                                "usle_C_min defaulting to 1 (fallow soil).", &
                    isCritical = .false. &
                ))
                usle_C_min = 1
            end if
            if (me%ncGroup%hasVariable('usle_rsd')) then        ! Residue on surface also needed to esimate C [kg/ha]
                var = me%ncGroup%getVariable('usle_rsd')
                call var%getData(usle_rsd)
            else                                                ! Default to zero (no residue = no crops)
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
            var = me%ncGroup%getVariable('usle_rock')           ! % rock in top of soil profile [-]
            call var%getData(usle_rock)
            me%usle_CFRG = exp(-0.052*usle_rock)                ! Coarse fragment factor [-]
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_rock not found in input file. " // &
                            "Defaulting to 0 (no rock in top soil layer).", &
                isCritical = .false. &
            ))
            me%usle_CFRG = 1                                    ! Default to 1 (rock_usle = 0)
        end if
        ! Params affecting q_peak
        ! alpha_half        Fraction of daily rainfall happening in maximum half hour. [-]
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
            me%usle_alpha_half = 0.33                           ! Defaults to 0.33
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
        else if (me%ncGroup%hasVariable('usle_area_hru')) then  ! Default to area_hru, if that is present
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
        else                                                    ! Default to 0.01 (fallow, no residue)
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
        else                                                    ! Default to GridCell slope, if present
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_sb not found in input file. " // &
                            "Defaulting to GridCell slope (" // trim(str(me%slope)) // ").", &
                isCritical = .false. &
            ))
            me%usle_slp_sb = me%slope
        end if
        !> Slope of the channel [m/m]. Defaults to GridCell slope.
        if (me%ncGroup%hasVariable('usle_slp_ch')) then
            var = me%ncGroup%getVariable('usle_slp_ch')
            call var%getData(me%usle_slp_ch)
        else                                                ! Default to GridCell slope, if present
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_slp_ch not found in input file. " // &
                            "Defaulting to GridCell slope (" // trim(str(me%slope)) // ").", &
                isCritical = .false. &
            ))
            me%usle_slp_ch = me%slope
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
end module

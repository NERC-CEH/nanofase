!> Module containing the `SoilProfile1` class definition.
module classSoilProfile1
    use Globals                                                 ! Global definitions and constants
    use UtilModule                                              ! Useful functions
    use mo_netcdf                                               ! Input/output handling
    use datetime_module
    use ResultModule, only: Result                              ! Error handling classes
    use spcSoilProfile                                          ! Parent class
    use classSoilLayer1                                         ! SoilLayers will be contained in the SoilProfile
    use classDataInterfacer, only: DATA
    implicit none

    !> A `SoilProfile` class acts as a container for a collection of
    !! `SoilLayer` objects, which collectively define the layout of
    !! the `SoilProfile`.
    type, public, extends(SoilProfile) :: SoilProfile1
      contains
        procedure :: create => createSoilProfile1                   ! Create the SoilProfile object
        procedure :: destroy => destroySoilProfile1                 ! Remove the SoilProfile object and all contained objects
        procedure :: update => updateSoilProfile1                   ! Update on every timestep (e.g., perform routing of water through soil)
        procedure :: percolate => percolateSoilProfile1             ! Percolate soil on a given time step
        procedure :: erode => erodeSoilProfile1                     ! Erode soil on a given time step
        procedure :: erodeMUSLE => erodeMUSLESoilProfile1           ! Erode soil using MUSLE on a given time step
        procedure :: imposeSizeDistribution => imposeSizeDistributionSoilProfile1 ! Impose size distribution on mass of sediment
        procedure :: parseInputData => parseInputDataSoilProfile1   ! Parse the data from the input file and store in object properties
    end type

  contains
    !> Creating the `SoilProfil`e parses input data and fills
    !! the corresponding object properties, as well as setting
    !! up the contained `SoilLayers`.
    function createSoilProfile1(me, &
                                x, &
                                y, &
                                p, &
                                slope, &
                                n_river, &
                                area, &
                                q_quickflow_timeSeries, &
                                q_precip_timeSeries, &
                                q_evap_timeSeries) result(r)
        class(SoilProfile1) :: me                           !! The `SoilProfile` instance.
        integer             :: x                            !! Containing `GridCell` x index
        integer             :: y                            !! Containing `GridCell` y index
        integer             :: p                            !! `SoilProfile` reference (redundant for now as only one `SoilProfile` per `GridCell`)
        real(dp)            :: slope                        !! Slope of the containing `GridCell` [m/m]
        real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
        real(dp)            :: area                         !! The surface area of the `SoilProfile` [m3]
        real(dp), allocatable :: q_quickflow_timeSeries(:)  !! The quickflow runoff from HMF [m/timestep]
        real(dp), allocatable :: q_precip_timeSeries(:)     !! Precipitation time series [m/timestep]
        real(dp), allocatable :: q_evap_timeSeries(:)       !! Evaporation time series [m/timestep]
        type(Result)        :: r                            !! The `Result` object
        integer             :: l                            ! Soil layer iterator
        type(SoilLayer1), allocatable :: sl                 ! Temporary SoilLayer1 variable

        me%ref = ref("SoilProfile", x, y, p)                ! Generate the reference name for the SoilProfile

        ! Allocate the object properties that need to be
        allocate(me%usle_C(C%nTimeSteps))
        allocate(me%usle_alpha_half(C%nTimeSteps))
        allocate(me%erodedSediment(C%nTimeSteps))
        allocate(me%distributionSediment(C%nSizeClassesSpm))

        me%x = x                                            ! GridCell x index
        me%y = y                                            ! GridCell y index
        me%p = p                                            ! SoilProfile index within the GridCell
        me%slope = slope
        me%n_river = n_river
        me%area = area                                      ! Surface area
        allocate(me%q_quickflow_timeSeries, source=q_quickflow_timeSeries)
        allocate(me%q_precip_timeSeries, source=q_precip_timeSeries)
        allocate(me%q_evap_timeSeries, source=q_evap_timeSeries)
        me%V_buried = 0                                     ! Volume of water "lost" from the bottom of SoilProfile
        
        ! Parse and store input data in this object's properties
        call r%addErrors(.errors. me%parseInputData())
        if (r%hasCriticalError()) return                    ! Return early if there are critical errors

        ! Set up the SoilLayers
        ! TODO: Different types of SoilLayer
        do l = 1, me%nSoilLayers
            allocate(sl)        ! Must be allocated on every time step
            ! Create the SoilLayer and add any errors to Result object
            call r%addErrors(.errors. &
                sl%create(me%x, me%y, me%p, l, me%WC_sat, me%WC_FC, me%K_s) &
            )
            call move_alloc(sl, me%colSoilLayers(l)%item)
        end do
        call r%addToTrace("Creating " // trim(me%ref))      ! Add this procedure to the error trace
    end function

    !> Destroy this SoilProfile
    function destroySoilProfile1(me) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        type(Result) :: r                                       !! `Result` object to return
    end function

    !> Perform the simulation of the `SoilProfile` for the current time step:
    !! <ul>
    !!  <li>Percolate soil through all SoilLayers</li>
    !!  <li>Erode sediment</li>
    !! </ul>
    function updateSoilProfile1(me, t) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        integer :: t                                            !! The current timestep
        type(Result) :: r                                       !! Result object to return
        
        ! Set the timestep-specific object properties
        me%q_quickflow = me%q_quickflow_timeSeries(t)           ! Set the runoff (quickflow) for this timestep [m/s]
        ! TODO: Q_runoff still in m3/s, change to m/s
        me%q_surf = 0.1*me%q_quickflow                          ! Surface runoff = 10% of quickflow [m/timestep]
        me%q_precip = me%q_precip_timeSeries(t)                 ! Get the relevant time step's precipitation [m/timestep]
        me%q_evap = me%q_evap_timeSeries(t)                     ! and evaporation [m/timestep]
        me%q_in = max(me%q_precip - me%q_evap, 0.0_dp)          ! Infiltration = precip - evap. This is supplied to SoilLayer_1 [m/timestep]. Minimum = 0.
            ! TODO: Should the minimum q_in be 0, or should evaporation be allowed to remove water from top soil layer?
        
        ! Perform percolation and erosion simluation, at the same
        ! time adding any errors to the Result object
        call r%addErrors([ &
            .errors. me%percolate(t), &
            .errors. me%erode(t) &
        ])

        ! Add this procedure to the Result object's trace                               
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function

    !> Percolate water through the `SoilProfile`, by looping through `SoilLayer`s
    !! and running their individual percolation procedures, and then passing
    !! percolated and pooled flows between `SoilLayer`s. Pooled water from top
    !! `SoilLayer` forms surface runoff, and "lost" water from bottom `SoilLayer`
    !! is kept track of in `me%V_buried`
    function percolateSoilProfile1(me, t) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile1` instance
        integer             :: t                                !! The current time step
        type(Result)        :: r                                !! The `Result` object to return
        integer             :: l, i                             ! Loop iterator for SoilLayers
        real(dp)            :: q_l_in                           ! Temporary inflow for a particular SoilLayer

        ! Loop through SoilLayers and remove 
        do l = 1, me%nSoilLayers
            if (l == 1) then                        ! If it's the first SoilLayer, Q_in will be from precip - ET
                q_l_in = me%q_in
            else                                    ! Else, get Q_in from previous layer's Q_perc
                q_l_in = me%colSoilLayers(l-1)%item%V_perc
            end if

            ! Run the percolation simulation for individual layer, setting V_perc, V_pool etc.
            call r%addErrors(.errors. &
                me%colSoilLayers(l)%item%update(t, q_l_in) &
            )
            ! If there is pooled water, we must push up to the previous layer, recursively
            ! for each SoilLayer above this
            do i = 1, l
                ! Check if the layer beneath has pooled any water
                if (me%colSoilLayers(l-i+1)%item%V_pool > 0) then
                    if (l-i == 0) then                          ! If it's the top soil layer, track how much pooled above soil
                        me%V_pool = me%colSoilLayers(l-i+1)%item%V_pool
                        ! TODO: Think about how this relates to quickflow
                    else                                        ! Else, add pooled volume to layer above
                        call r%addErrors(.errors. &
                            me%colSoilLayers(l-i)%item%addPooledWater( &
                                me%colSoilLayers(l-i+1)%item%V_pool &
                            ) &
                        )
                    end if
                end if
            end do
        end do

        ! Add the amount of water percolating out of the final layer to V_buried
        ! to keep track of "lost" water
        me%V_buried = me%V_buried + me%colSoilLayers(me%nSoilLayers)%item%V_perc

        ! Add this procedure to the Result object's trace
        call r%addToTrace("Percolating water on time step #" // trim(str(t)))
    end function

    !> Calculates soil erosion for this timestep, Updates this `GridCell`'s
    !! state variable `erodedSediment` accordingly.
    function erodeMUSLESoilProfile1(me, t) result(r)
        class(SoilProfile1) :: me               !! This `SoilProfile` instance
        integer             :: t                !! The current time step
        type(Result)        :: r                !! The `Result` object to return
        real(dp)            :: t_conc           ! Time of concentration \( t_{\text{tc}} \)
        real(dp)            :: q_peak           ! Peak rainfall \( q_{\text{peak}} \)
        real(dp)            :: Q_surf_hru       ! Surface runoff for the whole HRU per day [m3/day]
        real(dp)            :: S_tot            ! Total eroded sediment

        ! Change units of surface runoff from m/timestep for the GridCell, to m3/day for the HRU
        Q_surf_hru = (me%q_surf/C%timeStep)*me%usle_area_hru*86400       ! [m3/day]
        ! Estimate the time of concentration
        t_conc = (me%usle_L_sb**0.6 * me%usle_n_sb**0.6)/(18 * me%usle_slp_sb) &
                + (0.62 * me%usle_L_ch * me%n_river**0.75)/(me%usle_area_sb**0.125 * me%usle_slp_ch**0.375)
        ! Estimate the peak flow
        q_peak = me%usle_alpha_half(t)*me%q_surf*me%usle_area_sb/(3.6*t_conc)
        ! Bring this all together to calculate eroded sediment, converted to kg/timestep (from metric ton/day)
        S_tot = (118*C%timeStep/864) * (me%q_surf * q_peak * me%usle_area_hru)**0.56 &
                * me%usle_K * me%usle_C(t) * me%usle_P * me%usle_LS * me%usle_CFRG
        ! TODO: Need to convert sediment yield for the HRU to sediment yield for the grid cell.
        ! Simply scaling linearly from HRU area to grid cell area like below isn't realistic
        ! (not everywhere in the grid cell is going to be contributing as much as whatever
        ! HRU we're doing the calculation for).
        me%erodedSediment = me%imposeSizeDistribution(S_tot*me%area/me%usle_area_hru)
        call r%addToTrace("Eroding soil on time step #" // trim(str(t)))
    end function

    !> Calculate the soil erosion for this timestep and updates this `GridCell`'s
    !! `erodedSediment` property accordingly. Soil erosion based on RUSLE, with
    !! R-factor derived from kinetic energy calculated by Davison method:
    !! [Davison et al. 2005](https://doi.org/10.1016/j.scitotenv.2005.02.002).
    !! Europe specific parameterisation include in a_1, a_2, a_3, I30 and b
    !! parameters (provided by input data). K factor based on [modified Morgan
    !! Finney](https://doi.org/10.1002/esp.1530) and in g/J. Using these units, R-factor
    !! is simply equal to kinetic energy (J/m2) and sediment yield is in g/m2.
    function erodeSoilProfile1(me, t) result(rslt)
        class(SoilProfile1) :: me
        integer             :: t
        type(Result)        :: rslt
        real(dp)            :: E_k
        real(dp)            :: K_MMF
        real(dp)            :: erodedSedimentTotal
        type(datetime)      :: currentDate
        integer             :: julianDay

        ! TODO This function only works with daily timesteps

        ! Convert the current date to Julian day number (https://en.wikipedia.org/wiki/Julian_day).
        ! date2num converts to number of days since 0001-01-01, and 1721423 is the Julian day
        ! number of 0001-01-01.
        currentDate = C%startDate + timedelta(days=t)
        julianDay = date2num(currentDate) + 1721423
        ! Then calculate the kinetic energy [J/m2/day]. Precip in [mm/day].
        E_k = (me%erosivity_a1 + me%erosivity_a2 * cos(julianDay * (2*C%pi/365) + me%erosivity_a3)) &
                * (me%q_precip_timeSeries(t)*1.0e3)**me%erosivity_b
        ! Now the modified MMF version of K, dependent on sand, silt and clay content [g/J]
        K_MMF = 0.1*(me%clayContent/100.0_dp) + 0.3*(me%sandContent/100.0_dp) + 0.5*(me%siltContent/100.0_dp)
        ! Total eroded sediment [g/m2/day]
        erodedSedimentTotal = E_k * K_MMF * me%usle_C(t) * me%usle_P * me%usle_LS
        ! Split this into a size distribution and convert to [kg/m2/day]
        me%erodedSediment = me%imposeSizeDistribution(erodedSedimentTotal*1.0e-3)
    end function

    !> Impose a size class distribution on a total mass to split it up
    !! into separate size classes. If no distribution has been specified
    !! for this `SoilProfile`, then a default global size distribution is used
    function imposeSizeDistributionSoilProfile1(me, mass) result(distribution)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        real(dp)            :: mass                             !! The mass to split into size classes
        real(dp)            :: distribution(C%nSizeClassesSpm)  !! The resulting distribution
        integer             :: s                                ! Loop iterator for size classes
        do s = 1, C%nSizeClassesSpm
            distribution(s) = mass*me%distributionSediment(s)*0.01
        end do
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including the allocation of arrays that depend on
    !! this input data
    function parseInputDataSoilProfile1(me) result(r)
        class(SoilProfile1)     :: me                       !! This `SoilProfile` instance
        type(Result)            :: r                        !! `Result` object to return
        type(NcDataset)         :: nc                       ! NetCDF dataset
        type(NcVariable)        :: var                      ! NetCDF variable
        type(NcGroup)           :: grp                      ! NetCDF group
        real(dp), allocatable   :: usle_C_min(:)            ! Minimum cover factor for USLE
        real(dp), allocatable   :: usle_C_av(:)             ! Average cover factor for USLE
        integer                 :: usle_rock                ! % rock in top of soil profile, to calculate usle_CFRG param
        real(dp), allocatable   :: usle_rsd(:)              ! Residue on soil surface [kg/ha]

        ! Allocate the data which are time series. These must be allocatable (as opposed to
        ! being declared that length) to work with the mo_netcdf getData() procedure.
        allocate(usle_C_min(C%nTimeSteps))
        allocate(usle_C_av(C%nTimeSteps))
        allocate(usle_rsd(C%nTimeSteps))
        ! Set the data interfacer's group to this SoilProfile
        call r%addErrors(.errors. DATA%setGroup([character(len=100) :: &
            'Environment', &
            ref('GridCell', me%x, me%y), &
            me%ref &
        ]))
        ! Setup data and soil hydraulic/texture properties
        call r%addErrors([ &
            .errors. DATA%get('n_soil_layers', me%nSoilLayers), &
            .errors. DATA%get('WC_sat', me%WC_sat), &               ! Water content at saturation [m3/m3] TODO check between 0 and 1
            .errors. DATA%get('WC_FC', me%WC_FC), &                 ! Water content at field capacity [m3/m3] TODO check between 0 and 1
            .errors. DATA%get('K_s', me%K_s), &                     ! Saturated hydraulic conductivity [m/s]
            .errors. DATA%get('sand_content', me%sandContent), &
            .errors. DATA%get('silt_content', me%siltContent), &
            .errors. DATA%get('clay_content', me%clayContent), &
            .errors. DATA%get('coarse_frag_content', me%coarseFragContent), &
            .errors. DATA%get('distribution_sediment', me%distributionSediment, C%defaultDistributionSediment) & ! Sediment size class dist, sums to 100
        ])
        allocate(me%colSoilLayers(me%nSoilLayers))

        ! Auditing
        call r%addError( &
            ERROR_HANDLER%equal( &
                value = sum(me%distributionSediment), &
                criterion = 100, &
                message = "Specified size class distribution for sediments " &
                            // "does not sum to 100%." &
            ) &
        )

        ! -- RAINFALL EROSIVITY --------------------------------------------------------!
        ! The defaults are those for central England
        call r%addErrors([ &
            .errors. DATA%get('erosivity_a1', me%erosivity_a1, 6.608_dp), &
            .errors. DATA%get('erosivity_a2', me%erosivity_a2, 0.5_dp), &
            .errors. DATA%get('erosivity_a3', me%erosivity_a3, 2.7_dp), &
            .errors. DATA%get('erosivity_b', me%erosivity_b, 1.204_dp) &
        ])

        ! -- SOIL EROSION DATA ---------------------------------------------------------!
        ! C     Cover and land management factor, defined as the ratio of soil loss
        !       from land cropped under specified conditions to the corresponding loss
        !       from clean-tilled, continuous flow. Should be time-dependent (as crop
        !       cover changes). [-]
        if (DATA%grp%hasVariable('usle_C')) then              ! First, check if time series of C-factors is available
            var = DATA%grp%getVariable('usle_C')
            call var%getData(me%usle_C)
        else                                                    ! Else, we can estimate C
            if (DATA%grp%hasVariable('usle_C_min')) then      ! Use minimum C to estimate C
                var = DATA%grp%getVariable('usle_C_min')
                call var%getData(usle_C_min)
            else if (DATA%grp%hasVariable('usle_C_av')) then  ! Average annual C can be used to estimate minimum C
                var = DATA%grp%getVariable('usle_C_av')
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
            if (DATA%grp%hasVariable('usle_rsd')) then        ! Residue on surface also needed to esimate C [kg/ha]
                var = DATA%grp%getVariable('usle_rsd')
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
        if (DATA%grp%hasVariable('usle_K')) then
            var = DATA%grp%getVariable('usle_K')
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
        if (DATA%grp%hasVariable('usle_P')) then
            var = DATA%grp%getVariable('usle_P')
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
        if (DATA%grp%hasVariable('usle_LS')) then
            var = DATA%grp%getVariable('usle_LS')
            call var%getData(me%usle_LS)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_LS not found in input file." &
            ))
        end if
        ! CFRG  Coase fragment factor, CFRG = exp(0.035 * % rock in first soil layer). [-]
        if (DATA%grp%hasVariable('usle_rock')) then
            var = DATA%grp%getVariable('usle_rock')           ! % rock in top of soil profile [-]
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
        if (DATA%grp%hasVariable('usle_alpha_half')) then
            var = DATA%grp%getVariable('usle_alpha_half')
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
        if (DATA%grp%hasVariable('usle_area_hru')) then
            var = DATA%grp%getVariable('usle_area_hru')
            call var%getData(me%usle_area_hru)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_area_hru not found in input file." &
            ))
        end if
        ! Subbassin area [km2]
        if (DATA%grp%hasVariable('usle_area_sb')) then
            var = DATA%grp%getVariable('usle_area_sb')
            call var%getData(me%usle_area_sb)
        else if (DATA%grp%hasVariable('usle_area_hru')) then  ! Default to area_hru, if that is present
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
        if (DATA%grp%hasVariable('usle_L_sb')) then
            var = DATA%grp%getVariable('usle_L_sb')
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
        if (DATA%grp%hasVariable('usle_n_sb')) then
            var = DATA%grp%getVariable('usle_n_sb')
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
        if (DATA%grp%hasVariable('usle_slp_sb')) then
            var = DATA%grp%getVariable('usle_slp_sb')
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
        if (DATA%grp%hasVariable('usle_slp_ch')) then
            var = DATA%grp%getVariable('usle_slp_ch')
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
        if (DATA%grp%hasVariable('usle_L_ch')) then
            var = DATA%grp%getVariable('usle_L_ch')
            call var%getData(me%usle_L_ch)
        else
            call r%addError(ErrorInstance( &
                code = 201, &
                message = "Value for usle_L_ch not found in input file. " &
            ))
        end if
        
        ! Add this procedure to the trace
        call r%addToTrace('Parsing input data')

    end function
end module

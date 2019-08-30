module Globals
    use mo_netcdf
    use datetime_module
    use mod_strptime, only: f_strptime
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none

    type(ErrorCriteria) :: ERROR_HANDLER
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)

    type, public :: GlobalsType
        ! Config
        character(len=256)  :: inputFile
        character(len=256)  :: flatInputFile
        character(len=256)  :: constantsFile
        character(len=256)  :: outputFile
        character(len=256)  :: outputPath
        character(len=256)  :: logFilePath
        character(len=256)  :: configFilePath
        type(datetime)      :: startDate                        !! Datetime object representing the start date
        integer             :: timeStep                         !! The timestep to run the model on [s]
        integer             :: nTimeSteps                       !! The number of timesteps
        real(dp)            :: epsilon = 1e-10                  !! Used as proximity to check whether variable as equal
        real, allocatable   :: soilLayerDepth(:)                !! Soil layer depth [m]
        real(dp)            :: defaultMeanderingFactor = 1.0_dp !! Default river meandering factor, >1
        real(dp)            :: default_k_att                    !! Default attachment rate
        integer             :: maxRiverReaches = 100            !! Maximum number of RiverReaches a SubRiver can have
        real(dp)            :: default_alpha_hetero             !! Default NP-SPM attachment efficiency [-]
        real(dp)            :: default_alpha_hetero_estuary     !! Default NP-SPM attachment efficiency [-]
        real(dp)            :: default_alpha_resus              !! Resuspension calibration parameter
        real(dp)            :: default_beta_resus               !! Resuspension calibration parameter
        logical             :: includeBioturbation              !! Should bioturbation be modelled?
        logical             :: includePointSources              !! Should point sources be included?
        logical             :: includeBedSediment               !! Should the bed sediment be included?
        logical             :: includeAttachment                !! Should attachment to soil be included?
        integer             :: warmUpPeriod                     !! How long before we start inputting NM (to give flows to reach steady state)?
        real(dp)            :: nanomaterialDensity              !! Density of the nanomaterial modelled
        integer             :: nSoilLayers                      !! Number of soil layers to modelled
        
        ! Calibration and batch run
        logical             :: calibrationRun                   !! Is this model run a calibration run from/to given site?
        character(len=256)  :: siteData                         !! Where is the data about the sampling sites stored?
        character(len=6)    :: startSite                        !! Where does the calibration start from?
        character(len=6)    :: endSite                          !! Where does the calibration end?
        character(len=6), allocatable :: otherSites(:)          !! List of other sites to use from the site data file
        logical             :: isBatchRun = .false.             !! Are we batch running config files?

        ! General
        type(NcDataset)     :: dataset                          !! The NetCDF dataset

        ! Physical constants
        real(dp) :: g = 9.80665_dp          !! Gravitational acceleration [m/s^2]
        real(dp) :: k_B = 1.38064852e-23    !! Boltzmann constant [m2 kg s-2 K-1]
        real(dp) :: pi = 4*atan(1.0_dp)     !! Pi [-]
        real(dp) :: n_river = 0.035_dp      !! Manning's roughness coefficient, for natural streams and major rivers.
                                            !! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).

        ! Temp
        real(dp) :: T = 15.0_dp             !! Temperature [C]
        real(dp) :: defaultWaterTemperature !! Default water temperature [C]

        ! Size class distributions
        real(dp), allocatable :: d_spm(:)           !! Suspended particulate matter size class diameters [m]
        real(dp), allocatable :: d_spm_low(:)       !! Lower bound when treating each size class as distribution [m]
        real(dp), allocatable :: d_spm_upp(:)       !! Upper bound when treating each size class as distribution [m]
        real(dp), allocatable :: d_np(:)            !! Nanoparticle size class diameters [m]
        ! TODO: Get rid of d_pd (probably references to it in BedSediment). Analogous to rho_spm.
        real(dp), allocatable :: d_pd(:)            !! Sediment particle densities [kg m-3]
        real(dp), allocatable :: rho_spm(:)         !! Sediment particle densities [kg m-3]
        integer :: nSizeClassesSpm                  !! Number of sediment particle size classes
        integer :: nSizeClassesNP                   !! Number of nanoparticle size classes
        integer :: nFracCompsSpm                    !! Number of sediment fractional compositions
        integer, allocatable :: defaultDistributionSediment(:)  !! Default imposed size distribution for sediment
        integer, allocatable :: defaultDistributionNP(:)    !! Default imposed size distribution for NPs
        integer, allocatable :: defaultFractionalComp(:)  !! Default fractional composition of sediment
        integer :: npDim(3)                         !! Default dimensions for arrays of NM
        integer :: ionicDim                         !! Default dimensions for ionic metal

        ! Estuarine and tidal data
        real(dp) :: tidalM2                         !! Tidal harmonic coefficient M2 [-]
        real(dp) :: tidalS2                         !! Tidal harmonic coefficient S2 [-]
        real(dp) :: tidalDatum                      !! Datum that tidal harmonics are calculated relative to [m]
        ! real :: estuaryChartedDepthExpA             !! Exponential coef A for charted depth
        ! real :: estuaryChartedDepthExpB             !! Exponential coef B for charted depth
        real :: estuaryMeanDepthExpA                !! Exponential coef A for mean depth
        real :: estuaryMeanDepthExpB                !! Exponential coef B for mean depth
        real :: estuaryWidthExpA                    !! Exponential coef A for estuary width
        real :: estuaryWidthExpB                    !! Exponential coef B for estuary width

      contains
        procedure :: rho_w      ! Density of water
        procedure :: nu_w       ! Kinematic viscosity of water
        procedure :: mu_w       ! Dynamic viscosity of water
    end type

    type(GlobalsType) :: C

  contains

    !> Initialise global variables, such as `ERROR_HANDLER`
    subroutine GLOBALS_INIT()
        type(NcVariable) :: var                             ! NetCDF variable
        type(NcGroup) :: grp                                ! NetCDF group
        real(dp), allocatable :: spmSizeClasses(:)          ! Array of sediment particle sizes
        real(dp), allocatable :: spmFracComps(:)            ! Array of sediment fractional composition
        real(dp), allocatable :: npSizeClasses(:)           ! Array of nanoparticle particle sizes
        integer :: n                                        ! Iterator for size classes
        type(ErrorInstance) :: errors(17)                   ! ErrorInstances to be added to ErrorHandler
        character(len=256) :: configFilePath, batchRunFilePath
        integer :: configFilePathLength, batchRunFilePathLength, nBatches
        ! Values from config file
        character(len=256) :: input_file, flat_input, constants_file, output_file, output_path, log_file_path, start_date, &
            startDateStr, site_data
        character(len=6) :: start_site, end_site
        character(len=6), allocatable :: other_sites(:)
        integer :: default_fractional_comp_size, default_np_forms, default_np_extra_states, warm_up_period
        integer :: timestep, n_timesteps, max_river_reaches, n_soil_layers, n_other_sites
        integer, allocatable :: default_fractional_comp(:)
        real(dp) :: epsilon, default_meandering_factor, default_water_temperature, default_alpha_hetero, &
            default_k_att, default_alpha_hetero_estuary, nanomaterial_density, default_alpha_resus, default_beta_resus
        real, allocatable :: soil_layer_depth(:)
        logical :: error_output, include_bioturbation, include_attachment, include_point_sources, include_bed_sediment, &
            calibration_run
        namelist /calibrate/ calibration_run, site_data, start_site, end_site, other_sites
        namelist /allocatable_array_sizes/ default_fractional_comp_size, default_np_forms, default_np_extra_states, &
                                            n_soil_layers, n_other_sites
        namelist /data/ input_file, flat_input, constants_file, output_file, output_path
        namelist /run/ timestep, n_timesteps, epsilon, error_output, log_file_path, start_date
        namelist /global/ default_fractional_comp, warm_up_period, nanomaterial_density
        namelist /soil/ soil_layer_depth, include_bioturbation, include_attachment, default_k_att
        namelist /river/ max_river_reaches, default_meandering_factor, default_water_temperature, default_alpha_hetero, &
            default_alpha_hetero_estuary, include_bed_sediment, default_alpha_resus, default_beta_resus
        namelist /sources/ include_point_sources

        ! Has a path to the config path been provided as a command line argument?
        call get_command_argument(1, configFilePath, configFilePathLength)
        call get_command_argument(2, batchRunFilePath, batchRunFilePathLength)
        ! Open the config file and read the different config groups
        if (configFilePathLength > 0) then
            open(10, file=trim(configFilePath), status="old")
            C%configFilePath = configFilePath
        else
            open(10, file="config/config.nml", status="old")
            C%configFilePath = "config/config.nml"
        end if

        if (batchRunFilePathLength > 0) then
            C%isBatchRun = .true.
            open(12, file=trim(batchRunFilePath), status="old")
            read(12, '(i2)') nBatches
            print *, nBatches
        end if

        read(10, nml=allocatable_array_sizes)
        ! Use the allocatable array sizes to allocate those arrays (allocatable arrays
        ! must be allocated before being read in to)
        allocate(default_fractional_comp(default_fractional_comp_size))
        allocate(soil_layer_depth(n_soil_layers))
        allocate(other_sites(n_other_sites))
        ! Carry on reading in the different config groups
        read(10, nml=calibrate)
        read(10, nml=data)
        read(10, nml=run)
        read(10, nml=global)
        read(10, nml=soil)
        read(10, nml=river)
        read(10, nml=sources)
        close(10)
        
        ! Store this data in the Globals variable
        C%inputFile = input_file
        C%flatInputFile = flat_input
        C%constantsFile = constants_file
        C%outputFile = output_file
        C%outputPath = output_path
        C%logFilePath = log_file_path
        C%timeStep = timestep
        C%nTimeSteps = n_timesteps
        C%epsilon = epsilon
        startDateStr = start_date
        C%startDate = f_strptime(startDateStr)
        C%nSoilLayers = n_soil_layers
        ! Calibration
        C%calibrationRun = calibration_run
        C%siteData = site_data
        C%startSite = start_site
        C%endSite = end_site
        C%otherSites = other_sites
        C%defaultFractionalComp = default_fractional_comp
        C%soilLayerDepth = soil_layer_depth
        C%defaultMeanderingFactor = default_meandering_factor
        C%maxRiverReaches = max_river_reaches
        C%defaultWaterTemperature = default_water_temperature
        C%default_alpha_hetero = default_alpha_hetero
        C%default_alpha_hetero_estuary = default_alpha_hetero_estuary
        C%default_alpha_resus = default_alpha_resus
        C%default_beta_resus = default_beta_resus
        C%default_k_att = default_k_att
        C%warmUpPeriod = warm_up_period
        C%nanomaterialDensity = nanomaterial_density
        ! Processes to be modelled / data to be included
        C%includeBioturbation = include_bioturbation
        C%includePointSources = include_point_sources
        C%includeAttachment = include_attachment
        C%includeBedSediment = include_bed_sediment
        
        ! General
        errors(1) = ErrorInstance(code=110, message="Invalid object type index in data file.")
        ! File operations
        errors(2) = ErrorInstance(code=200, message="File not found.")
        errors(3) = ErrorInstance(code=201, message="Variable not found in input file.")
        errors(4) = ErrorInstance(code=202, message="Group not found in input file.")
        errors(5) = ErrorInstance(code=203, message="Unknown config file option.")
        ! Numerical calculations
        errors(6) = ErrorInstance(code=300, message="Newton's method failed to converge.")
        ! Grid and geography
        errors(7) = ErrorInstance(code=401, &
            message="Invalid RiverReach inflow reference. Inflow must be from a neighbouring RiverReach.")
        errors(8) = ErrorInstance(code=402, &
            message="Invalid RiverReach inflow reference. If multiple inflows are specified, they must " // &
                        "be inflows to the GridCell and all come from the same GridCell.")
        errors(9) = ErrorInstance(code=403, &
            message="RiverReach cannot have more than 5 inflows.")
        errors(10) = ErrorInstance(code=404, &
            message="RiverReach outflow could not be determined. Reaches must either be specified as " // &
                        "inflow to downstream reach, or have a model domain outflow specified.")
        errors(11) = ErrorInstance(code=405, &
            message="RiverReach lengths specified in input data sum to greater than straight-line river branch " // &
                        "length. Are you sure this is intended?", isCritical=.false.)
        ! River routing
        errors(11) = ErrorInstance(code=500, &
            message="All SPM advected from RiverReach.", isCritical=.false.)
        errors(12) = ErrorInstance(code=501, &
            message="No input data provided for required SubRiver - check nSubRivers is correct.")
        ! Soil
        errors(13) = ErrorInstance(code=600, message="All water removed from SoilLayer.", isCritical=.false.)
        ! General
        errors(14) = ErrorInstance(code=901, message="Invalid RiverReach type index provided.")
        errors(15) = ErrorInstance(code=902, message="Invalid Biota index provided.")
        errors(16) = ErrorInstance(code=903, message="Invalid Reactor index provided.")
        errors(17) = ErrorInstance(code=904, message="Invalid BedSedimentLayer index provided.")

        ! Add custom errors to the error handler
        call ERROR_HANDLER%init(errors=errors, on=error_output)

        ! Get the sediment and nanoparticle size classes from data file
        C%dataset = NcDataset(C%inputFile, "r")             ! Open dataset as read-only
        grp = C%dataset%getGroup("global")                  ! Get the global variables group
        var = grp%getVariable("spm_size_classes")           ! Get the sediment size classes variable
        call var%getData(spmSizeClasses)                    ! Get the variable's data
        allocate(C%d_spm, source=spmSizeClasses)            ! Allocate to class variable
        var = grp%getVariable("np_size_classes")            ! Get the sediment size classes variable
        call var%getData(npSizeClasses)                     ! Get the variable's data
        allocate(C%d_np, source=npSizeClasses)              ! Allocate to class variable
        var = grp%getVariable("spm_particle_densities")     ! Get the sediment particle density variable
        call var%getData(spmFracComps)                      ! Get the variable's data
        allocate(C%d_pd, source=spmFracComps)               ! Allocate to class variable
        allocate(C%rho_spm, source=spmFracComps)
        ! TODO: Check the distribution adds up to 100%
        var = grp%getVariable("tidal_M2")                   ! Tidal harmonic coefficient M2
        call var%getData(C%tidalM2)
        var = grp%getVariable("tidal_S2")                   ! Tidal harmonic coefficient S2
        call var%getData(C%tidalS2)
        ! The following exponential components are used to calculate depth/width and function of distance to mouth,
        ! using an expontential equation of the form A*exp(-Bt)
        ! TODO check this, but I don't think we need charted depth - everything can be calculated as fn of mean depth (see spreadsheet)
        ! var = grp%getVariable("estuary_charted_depth_expA") ! Charted depth exponential A coef
        ! call var%getData(C%estuaryChartedDepthExpA)
        ! var = grp%getVariable("estuary_charted_depth_expB") ! Charted depth exponential A coef
        ! call var%getData(C%estuaryChartedDepthExpB)
        var = grp%getVariable("estuary_mean_depth_expA")    ! Mean depth exponential A coef
        call var%getData(C%estuaryMeanDepthExpA)
        var = grp%getVariable("estuary_mean_depth_expB")    ! Mean depth exponential A coef
        call var%getData(C%estuaryMeanDepthExpB)
        var = grp%getVariable("estuary_width_expA")          ! Width exponential A coef
        call var%getData(C%estuaryWidthExpA)
        var = grp%getVariable("estuary_width_expB")         ! Width exponential A coef
        call var%getData(C%estuaryWidthExpB)

        call C%dataset%close()                              ! Close the dataset
        ! TODO: Get default water temperature "T_water"

        ! Set the number of size classes
        C%nSizeClassesSpm = size(C%d_spm)
        C%nSizeClassesNP = size(C%d_np)
        C%nFracCompsSpm = size(C%rho_spm)
        allocate(C%d_spm_low(C%nSizeClassesSpm))
        allocate(C%d_spm_upp(C%nSizeClassesSpm))
        ! Set the upper and lower bounds of each size class, if treated as a distribution
        do n = 1, C%nSizeClassesSpm
            ! Set the upper and lower limit of the size class's distributions
            if (n == C%nSizeClassesSpm) then
                C%d_spm_upp(n) = 1                                              ! failsafe overall upper size limit
            else
                C%d_spm_upp(n) = C%d_spm(n+1) - (C%d_spm(n+1)-C%d_spm(n))/2     ! Halfway between d_1 and d_2
            end if                
        end do
        do n = 1, C%nSizeClassesSpm
            if (n == 1) then
                C%d_spm_low(n) = 0                                              ! Particles can be any size below d_upp,1
            else
                C%d_spm_low(n) = C%d_spm_upp(n-1)                               ! lower size boundary equals upper size boundary of lower size class
            end if
        end do        

        ! Array to store default NM and ionic array dimensions. NM:
        !   1: NP size class
        !   2: form (core, shell, coating, corona)
        !   3: state (free, bound, heteroaggregated)
        ! Ionic: Form (free ion, solution, adsorbed)
        C%npDim = [C%nSizeClassesNP, default_np_forms, C%nSizeClassesSpm + default_np_extra_states]
        C%ionicDim = 3
        
    end subroutine

    !> Calculate the density of water at a given temperature \( T \):
    !! $$
    !!      \rho_{\text{w}}(T) = 1000 \left( 1 - \frac{T + 288.9414}{508929.2 (T + 68.12963) (T - 3.9863^2)} \right)
    !! $$
    !! and optionally with a given salinity \( S \):
    !! $$
    !!      \rho_{\text{w,s}}(T,S) = \rho_w + AS + BS^{3/2} + CS^2
    !! $$
    !! where \( A = 0.824493 - 0.0040899T + 0.000076438T^2 -0.00000082467T^3 + 0.0000000053675T^4 \),
    !! \( B = -0.005724 + 0.00010227T - 0.0000016546T^2 \) and \( C = 4.8314 \times 10^{-4} \).
    !! Reference:
    !! [D. R. Maidment, Handbook of Hydrology (2012)](https://books.google.co.uk/books/about/Handbook_of_hydrology.html?id=4_9OAAAAMAAJ)
    function rho_w(me, T, S)
        class(GlobalsType), intent(in) :: me                    !! This `Constants` instance
        real(dp), intent(in) :: T                               !! Temperature \( T \) [C]
        real(dp), intent(in), optional :: S                     !! Salinity \( S \) [g/kg]
        real(dp) :: rho_w                                       !! Density of water \( \rho_w \) [kg/m**3].
        if (present(S)) then
            rho_w = 1000.0_dp*(1-(T+288.9414_dp)/(508929.2_dp*(T+68.12963_dp))*(T-3.9863_dp)**2) &
                    + (0.824493_dp - 0.0040899_dp*T + 0.000076438_dp*T**2 - 0.00000082467_dp*T**3 + 0.0000000053675_dp*T**4)*S &
                    + (-0.005724_dp + 0.00010227_dp*T - 0.0000016546_dp*T**2)*S**(3.0_dp/2.0_dp) &
                    + 0.00048314_dp*S**2
        else
            rho_w = 1000.0_dp*(1-(T+288.9414_dp)/(508929.2_dp*(T+68.12963_dp))*(T-3.9863_dp)**2)
        end if
    end function

    !> Calculate the kinematic viscosity of water \( \nu_w \) at given temperature \( T \)
    !! and optionally salinity \( S \):
    !! $$
    !!      \nu_{\text{w}}(T,S) = \frac{1}{\rho_w(T,S)} 2.414\times 10^{-5} \cdot 10^{\frac{247.8}{(T+273.15)-140.0}}
    !! $$
    !! Reference: [T. Al-Shemmeri](http://varunkamboj.typepad.com/files/engineering-fluid-mechanics-1.pdf)
    function nu_w(me, T, S)
        class(GlobalsType), intent(in) :: me                    !! This `Constants` instance
        real(dp), intent(in) :: T                               !! Temperature \( T \) [C]
        real(dp), intent(in), optional :: S                     !! Salinity \( S \) [g/kg]
        real(dp) :: nu_w                                        !! Kinematic viscosity of water \( \nu_{\text{w}} \)
        if (present(S)) then
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T,S)
        else
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T)
        end if
    end function
    
    !> Calculate the dynamic viscosity of water \( \mu_w \) at a given temperature \( T \)
    !! $$
    !!      \nu_{\text{w}}(T,S) = 2.414\times 10^{-5} \cdot 10^{\frac{247.8}{(T+273.15)-140.0}}
    !! $$
    !! Reference: [T. Al-Shemmeri](http://varunkamboj.typepad.com/files/engineering-fluid-mechanics-1.pdf)
    function mu_w(me, T)
        class(GlobalsType), intent(in) :: me
        real(dp), intent(in) :: T
        real(dp) :: mu_w
        mu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))
    end function
end module

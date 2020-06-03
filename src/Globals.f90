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
        ! Data input
        character(len=256)  :: inputFile
        character(len=256)  :: constantsFile
        ! Data output 
        character(len=256)  :: outputPath                       !! Path to directory to store output data
        logical             :: writeCSV                         !! Should output data be written as CSV file?
        logical             :: writeNetCDF                      !! Should output data be written as NetCDF file?
        ! Run
        character(len=256)  :: runDescription                   !! Short description of model run
        character(len=256)  :: logFilePath                      !! Log file path
        character(len=256)  :: configFilePath                   !! Config file path
        type(datetime)      :: startDate                        !! Datetime object representing the start date
        integer             :: timeStep                         !! The timestep to run the model on [s]
        integer             :: nTimeSteps                       !! The number of timesteps
        real(dp)            :: epsilon = 1e-10                  !! Used as proximity to check whether variable as equal
        real, allocatable   :: soilLayerDepth(:)                !! Soil layer depth [m]
        logical             :: includeBioturbation              !! Should bioturbation be modelled?
        logical             :: includePointSources              !! Should point sources be included?
        logical             :: includeBedSediment               !! Should the bed sediment be included?
        logical             :: includeAttachment                !! Should attachment to soil be included?
        integer             :: warmUpPeriod                     !! How long before we start inputting NM (to give flows to reach steady state)?
        integer             :: nSoilLayers                      !! Number of soil layers to be modelled
        integer             :: nSedimentLayers                  !! Number of sediment layers to be modelled
        
        ! Calibration
        logical             :: calibrationRun                   !! Is this model run a calibration run from/to given site?
        character(len=256)  :: siteData                         !! Where is the data about the sampling sites stored?
        character(len=6)    :: startSite                        !! Where does the calibration start from?
        character(len=6)    :: endSite                          !! Where does the calibration end?
        character(len=6), allocatable :: otherSites(:)          !! List of other sites to use from the site data file

        ! Batched run
        integer             :: nBatches = 1                     !! Numbers of batches to run
        logical             :: isBatchRun = .false.             !! Are we batch running config files?
        character(len=256), allocatable :: batchConfigFiles(:)  !! Paths to config files for batches

        ! General
        type(NcDataset) :: dataset                          !! The NetCDF dataset

        ! Physical constants
        real(dp) :: g = 9.80665_dp          !! Gravitational acceleration [m/s^2]
        real(dp) :: k_B = 1.38064852e-23    !! Boltzmann constant [m2 kg s-2 K-1]
        real(dp) :: pi = 4*atan(1.0_dp)     !! Pi [-]
        real(dp) :: n_river = 0.035_dp      !! Manning's roughness coefficient, for natural streams and major rivers.
                                            !! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).

        ! Temp
        real(dp) :: T = 15.0_dp             !! Temperature [C]

        ! Size class distributions
        real, allocatable :: d_spm(:)                       !! Suspended particulate matter size class diameters [m]
        real, allocatable :: d_spm_low(:)                   !! Lower bound when treating each size class as distribution [m]
        real, allocatable :: d_spm_upp(:)                   !! Upper bound when treating each size class as distribution [m]
        real, allocatable :: d_nm(:)                        !! Nanomaterial size class diameters [m]
        real, allocatable :: sedimentParticleDensities(:)   !! Sediment particle densities [kg m-3]
        integer :: nSizeClassesSpm                          !! Number of sediment particle size classes
        integer :: nSizeClassesNM                           !! Number of nanoparticle size classes
        integer :: nFracCompsSpm                            !! Number of sediment fractional compositions
        integer :: nFormsNM                                 !! Number of NM forms (e.g. pristine, transformed, etc)
        integer :: nExtraStatesNM                           !! Number of NM states other than heteroaggregated to SPM
        integer, allocatable :: defaultDistributionSediment(:) !! Default imposed size distribution for sediment
        integer, allocatable :: defaultDistributionNP(:) !! Default imposed size distribution for NPs
        integer :: npDim(3)                                 !! Default dimensions for arrays of NM
        integer :: ionicDim                                 !! Default dimensions for ionic metal

      contains
        procedure :: rho_w      ! Density of water
        procedure :: nu_w       ! Kinematic viscosity of water
        procedure :: mu_w       ! Dynamic viscosity of water
    end type

    type(GlobalsType) :: C

  contains

    !> Initialise global variables, such as `ERROR_HANDLER`
    subroutine GLOBALS_INIT()
        integer :: n, i                                     ! Iterators
        ! Water
        type(ErrorInstance) :: errors(17)                   ! ErrorInstances to be added to ErrorHandler
        character(len=256) :: configFilePath, batchRunFilePath
        integer :: configFilePathLength, batchRunFilePathLength
        ! Values from config file
        character(len=256) :: input_file, constants_file, output_path, log_file_path, start_date, &
            startDateStr, site_data, description
        character(len=6) :: start_site, end_site
        character(len=6), allocatable :: other_sites(:)
        integer :: n_nm_size_classes, n_nm_forms, n_nm_extra_states, warm_up_period, n_spm_size_classes, &
            n_fractional_compositions
        integer :: timestep, n_timesteps, max_river_reaches, n_soil_layers, n_other_sites, n_layers
        real(dp) :: epsilon, default_meandering_factor, default_water_temperature, default_alpha_hetero, &
            default_alpha_hetero_estuary
        real, allocatable :: soil_layer_depth(:), nm_size_classes(:), spm_size_classes(:), sediment_particle_densities(:)
        logical :: error_output, include_bioturbation, include_attachment, include_point_sources, include_bed_sediment, &
            calibration_run, write_csv, write_netcdf
        namelist /allocatable_array_sizes/ n_soil_layers, n_other_sites, n_nm_size_classes, n_spm_size_classes, &
            n_fractional_compositions
        namelist /calibrate/ calibration_run, site_data, start_site, end_site, other_sites
        namelist /nanomaterial/ n_nm_forms, n_nm_extra_states, nm_size_classes
        namelist /data/ input_file, constants_file, output_path, write_csv, write_netcdf
        namelist /run/ timestep, n_timesteps, epsilon, error_output, log_file_path, start_date, warm_up_period, &
            description
        namelist /soil/ soil_layer_depth, include_bioturbation, include_attachment
        namelist /sediment/ spm_size_classes, n_layers, include_bed_sediment, sediment_particle_densities
        namelist /sources/ include_point_sources

        ! Defaults, which will be overwritten if present in config file
        write_csv = .true.
        write_netcdf = .false.
        description = ""

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
            read(12, '(i2)') C%nBatches
            allocate(C%batchConfigFiles(C%nBatches))
            do i = 1, C%nBatches
                read(12, '(A)') C%batchConfigFiles(i)
            end do
            close(12)
        end if

        read(10, nml=allocatable_array_sizes)
        ! Use the allocatable array sizes to allocate those arrays (allocatable arrays
        ! must be allocated before being read in to)
        allocate(soil_layer_depth(n_soil_layers))
        allocate(other_sites(n_other_sites))
        allocate(nm_size_classes(n_nm_size_classes))
        allocate(spm_size_classes(n_spm_size_classes))
        allocate(sediment_particle_densities(n_fractional_compositions))
        ! Carry on reading in the different config groups
        read(10, nml=nanomaterial); rewind(10)
        read(10, nml=calibrate); rewind(10)
        read(10, nml=data); rewind(10)
        read(10, nml=run); rewind(10)
        read(10, nml=soil); rewind(10)
        ! read(10, nml=water); rewind(10)
        read(10, nml=sediment); rewind(10)
        read(10, nml=sources); rewind(10)
        close(10)
        
        ! Store this data in the Globals variable
        ! Nanomaterial
        C%nSizeClassesNM = n_nm_size_classes
        C%nFormsNM = n_nm_forms
        C%nExtraStatesNM = n_nm_extra_states
        allocate(C%d_nm, source=nm_size_classes)
        ! Data
        C%inputFile = input_file
        C%constantsFile = constants_file
        C%outputPath = output_path
        C%writeCSV = write_csv
        C%writeNetCDF = write_netcdf
        ! Run
        C%runDescription = description
        C%logFilePath = log_file_path
        C%timeStep = timestep
        C%nTimeSteps = n_timesteps
        C%epsilon = epsilon
        startDateStr = start_date
        C%startDate = f_strptime(startDateStr)
        ! Calibration
        C%calibrationRun = calibration_run
        if (C%calibrationRun) then
            C%siteData = site_data
            C%startSite = start_site
            C%endSite = end_site
            C%otherSites = other_sites
        end if
        ! Sediment
        C%nSizeClassesSpm = n_spm_size_classes
        C%includeBedSediment = include_bed_sediment
        C%nSedimentLayers = n_layers
        allocate(C%d_spm, source=spm_size_classes)
        C%nFracCompsSpm = n_fractional_compositions
        allocate(C%sedimentParticleDensities, source=sediment_particle_densities)
        ! Soil
        C%nSoilLayers = n_soil_layers
        C%soilLayerDepth = soil_layer_depth
        C%warmUpPeriod = warm_up_period
        C%includeBioturbation = include_bioturbation
        C%includeAttachment = include_attachment
        ! Sources
        C%includePointSources = include_point_sources

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
        C%npDim = [C%nSizeClassesNM, C%nFormsNM, C%nSizeClassesSpm + C%nExtraStatesNM]
        
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
        real, intent(in) :: T                                   !! Temperature \( T \) [deg C]
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
        class(GlobalsType), intent(in) :: me                    !! This Globals instance
        real, intent(in) :: T                                   !! Temperature \( T \) [deg C]
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
        real, intent(in) :: T
        real(dp) :: mu_w
        mu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))
    end function
end module

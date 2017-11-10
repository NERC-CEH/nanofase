module Globals
    use json_module
    use mo_netcdf
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none

    type(ErrorCriteria) :: ERROR_HANDLER
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)

    type, public :: Constants
        ! Physical constants
        real(dp) :: g = 9.80665_dp          !! Gravitational acceleration [m/s^2]
        real(dp) :: n_river = 0.035_dp      !! Manning's roughness coefficient, for natural streams and major rivers.
                                            !! [Reference](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).

        ! Data input
        real(dp) :: T = 15.0_dp             !! Temperature [C]
        character(len=12) :: inputFile = 'data/data.nc'   !! Name of the data input file. TODO: Get this from config file.

        ! Size class distributions
        real(dp), allocatable :: d_spm(:)           !! Suspended particulate matter size class diameters [m]
        real(dp), allocatable :: d_spm_low(:)       !! Lower bound when treating each size class as distribution [m]
        real(dp), allocatable :: d_spm_upp(:)       !! Upper bound when treating each size class as distribution [m]
        real(dp), allocatable :: d_np(:)            !! Nanoparticle size class diameters [m]
        integer :: nSizeClassesSpm                  !! Number of sediment particle size classes
        integer :: nSizeClassesNP                   !! Number of nanoparticle size classes
        integer, allocatable :: defaultDistributionSediment(:)  !! Default imposed size distribution for sediment
        integer, allocatable :: defaultDistributionNP(:)   !! Default imposed size distribution for NPs

        ! Limits
        integer :: maxRiverReaches = 100    !! Maximum number of RiverReaches a SubRiver can have.
                                            !! TODO: Would be good if this was from config file

        ! Structure and time
        real(dp) :: gridCellSize            !! The dimensions of each grid cell [m].
        integer :: timeStep                 !! The timestep to run the model on [s].
        integer :: nTimeSteps               !! The number of timesteps.

      contains
        procedure :: rho_w, nu_w
    end type

    type(Constants) :: C

  contains

    !> Initialise global variables. For the moment, just error
    !! handling, but concievably could deal with constants, files
    !! and other setup tasks in the future.
    subroutine GLOBALS_INIT()
        type(NcDataset) :: NC                               !! NetCDF dataset
        type(NcVariable) :: var                             !! NetCDF variable
        type(NcGroup) :: grp                                !! NetCDF group
        type(json_file) :: config                           !! JSON config file
        logical :: jsonVarFound                             !! Was the JSON variable found?
        real(dp), allocatable :: spmSizeClasses(:)          !! Array of sediment particle sizes
        real(dp), allocatable :: npSizeClasses(:)           !! Array of nanoparticle particle sizes
        integer :: n                                        !! Iterator for size classes

        ! Add custom errors to the error handler
        call ERROR_HANDLER%init(errors=[ &
            ! File operations
            ErrorInstance(code=200, message="File not found."), &
            ErrorInstance(code=201, message="Variable not found in input file."), &
            ErrorInstance(code=202, message="Group not found in input file."), &
            ErrorInstance(code=203, message="Unknown config file option."), &
            ! Numerical calculations
            ErrorInstance(code=300, message="Newton's method failed to converge."), &
            ! Grid and geography
            ErrorInstance(code=401, message="Invalid SubRiver inflow reference. Inflow must be from a neighbouring SubRiver."), &
            ! River routing
            ErrorInstance(code=500, message="All SPM advected from RiverReach.", isCritical=.false.), &
            ErrorInstance(code=501, message="No input data provided for required SubRiver - check nSubRivers is correct."), &
            ! General
            ErrorInstance(code=901, message="Invalid RiverReach type index provided."), &
            ErrorInstance(code=902, message="Invalid Biota index provided."), &
            ErrorInstance(code=903, message="Invalid Reactor index provided."), &
            ErrorInstance(code=904, message="Invalid BedSedimentLayer index provided.") &
        ])

        ! Get config options from the config file
        ! TODO: NOT WORKING
        ! call config%initialize()
        ! call config%load_file(filename = '../config.json')
        ! if (config%failed()) call ERROR_HANDLER%trigger( &
        !     error = ErrorInstance(code=200, message="Config file config.json not found."))
        ! ! call config%get('data.input_file', C%inputFile, jsonVarFound)
        ! ! if (.not. jsonVarFound) call ERROR_HANDLER%queue(201, message="data.input_file variable not found in config file.")
        ! call config%get('run.timestep', C%timeStep, jsonVarFound)
        ! if (.not. jsonVarFound) call ERROR_HANDLER%queue( &
        !     error = ErrorInstance(201, message="run.timestep variable not found in config file."))
        ! call config%get('run.n_timesteps', C%nTimeSteps, jsonVarFound)
        ! if (.not. jsonVarFound) call ERROR_HANDLER%queue( &
        !     error = ErrorInstance(201, message="run.n_timesteps variable not found in config file."))

        ! call config%destroy()

        ! Get the sediment and nanoparticle size classes from data file
        nc = NcDataset(C%inputFile, "r")                    ! Open dataset as read-only
        grp = nc%getGroup("global")                         ! Get the global variables group
        var = grp%getVariable("spm_size_classes")           ! Get the sediment size classes variable
        call var%getData(spmSizeClasses)                    ! Get the variable's data
        allocate(C%d_spm, source=spmSizeClasses)            ! Allocate to class variable
        var = grp%getVariable("np_size_classes")            ! Get the sediment size classes variable
        call var%getData(npSizeClasses)                     ! Get the variable's data
        allocate(C%d_np, source=npSizeClasses)              ! Allocate to class variable
        var = grp%getVariable("defaultDistributionSediment")! Get the default sediment size classes distribution
        call var%getData(C%defaultDistributionSediment)     ! Get the variable's data
        ! TODO: Check the distribution adds up to 100%
        var = grp%getVariable("defaultDistributionNP")      ! Get the sediment size classes variable
        call var%getData(C%defaultDistributionNP)           ! Get the variable's data
        var = grp%getVariable("gridCellSize")               ! Get the size of a grid cell [m]
        call var%getData(C%gridCellSize)
        var = grp%getVariable("timeStep")                   ! Get the timestep to run the model on [s]
        call var%getData(C%timeStep)
        var = grp%getVariable("nTimeSteps")                 ! Get the number of time steps [s]
        call var%getData(C%nTimeSteps)
        ! Set the number of size classes
        C%nSizeClassesSpm = size(C%d_spm)
        C%nSizeClassesNP = size(C%d_np)
        allocate(C%d_spm_low(C%nSizeClassesSpm))
        allocate(C%d_spm_upp(C%nSizeClassesSpm))
        ! Set the upper and lower bounds of each size class, if treated as a distribution
        do n = 1, C%nSizeClassesSpm
            ! Set the upper and lower limit of the size class's distributions
            if (n == 1) then
                C%d_spm_low(n) = 0                                              ! Particles can be any size below d_upp,1
                C%d_spm_upp(n) = C%d_spm(n+1) - (C%d_spm(n+1)-C%d_spm(n))/2     ! Halfway between d_1 and d_2
            else
                C%d_spm_low(n) = C%d_spm(n) - (C%d_spm(n)-C%d_spm(n-1))/2       ! Halfway between d_n-1 and d_n
                C%d_spm_upp(n) = 2*C%d_spm(n) - C%d_spm_low(n)                  ! Halfway between d_n and d_n+1
            end if
        end do
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
    !!  - [D. R. Maidment, Handbook of Hydrology (2012)](https://books.google.co.uk/books/about/Handbook_of_hydrology.html?id=4_9OAAAAMAAJ)
    pure function rho_w(me, T, S)
        class(Constants), intent(in) :: me                      !! This Constants instance.
        real(dp), intent(in) :: T                               !! Temperature \( T \) [C].
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
    !!      \nu_{\text{w}}(T,S) = \frac{1}{\rho_w(T,S)} 2.414\times 10^{-5} 10^{\frac{247.8}{(T+273.15)-140.0}}
    !! $$
    pure function nu_w(me, T, S)
        class(Constants), intent(in) :: me                      !! This Constants instance.
        real(dp), intent(in) :: T                               !! Temperature \( T \) [C].
        real(dp), intent(in), optional :: S                     !! Salinity \( S \) [g/kg]
        real(dp) :: nu_w                                        !! Kinematic viscosity of water \( \nu_{\text{w}} \)
        if (present(S)) then
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T,S)
        else
            nu_w = (2.414e-5_dp * 10.0_dp**(247.8_dp/((T+273.15_dp)-140.0_dp)))/me%rho_w(T)
        end if
    end function
end module
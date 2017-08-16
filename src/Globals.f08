module Globals
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
        character(len=7) :: inputFile = 'data.nc'   !! Name of the data input file. TODO: Get this from config file.
        real(dp), allocatable :: d_spm(:)   !! Suspended particulate matter size class diameters [m]
        real(dp), allocatable :: d_np(:)    !! Nanoparticle size class diameters [m]
        integer :: nSizeClassesSpm          !! Number of sediment particle size classes
        integer :: nSizeClassesNP           !! Number of nanoparticle size classes

        ! Limits
        integer :: maxRiverReaches = 100    !! Maximum number of RiverReaches a SubRiver can have.
                                            !! TODO: Would be good if this was from config file

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
        real(dp), allocatable :: spmSizeClasses(:)          !! Array of sediment particle sizes
        real(dp), allocatable :: npSizeClasses(:)           !! Array of nanoparticle particle sizes

        ! Add custom errors to the error handler
        call ERROR_HANDLER%init(errors=[ &
            ErrorInstance(code=999,message="Invalid biota index provided when creating bed sediment layer."), &
            ErrorInstance(code=998,message="Invalid reactor index provided when creating bed sediment layer."), &
            ErrorInstance(code=997,message="Invalid bed sediment layer index provided when creating bed sediment."), &
            ErrorInstance(code=996,message="Invalid number of bed sediment layers provided. Must be greater than zero."), &
            ! Numerical calculations
            ErrorInstance(code=300, &
                message="Newton's method failed to converge.") &
        ])

        ! Get the sediment and nanoparticle size classes from data file
        nc = NcDataset(C%inputFile, "r")                    ! Open dataset as read-only
        grp = nc%getGroup("global")                         ! Get the global variables group
        var = grp%getVariable("spm_size_classes")           ! Get the sediment size classes variable
        call var%getData(spmSizeClasses)                    ! Get the variable's data
        allocate(C%d_spm, source=spmSizeClasses)            ! Allocate to class variable
        var = grp%getVariable("np_size_classes")            ! Get the sediment size classes variable
        call var%getData(npSizeClasses)                     ! Get the variable's data
        allocate(C%d_np, source=npSizeClasses)              ! Allocate to class variable
        ! Set the number of size classes
        C%nSizeClassesSPM = size(C%d_spm)
        C%nSizeClassesNP = size(C%d_np)
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
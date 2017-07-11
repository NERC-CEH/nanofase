module Globals
    use netcdf
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none

    type(ErrorCriteria) :: ERROR_HANDLER
    integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: qp = selected_real_kind(33, 4931)
    ! integer :: NCID

    type, public :: Constants
        real(dp) :: g = 9.80665_dp              !> Gravitational acceleration [m/s^2]
        real(dp) :: nu = 1.0034e-6_dp           ! Viscosity of water at 10C [m^2/s]
        real(dp) :: rho = 1000.0_dp           ! Density of water at 10C [kg/m^3]
    end type

    type(Constants) :: C

  contains

    !> Initialise global variables. For the moment, just error
    !! handling, but concievably could deal with constants, files
    !! and other setup tasks in the future.
    subroutine GLOBALS_INIT()
        integer :: nfStat
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

        ! Open the data file
        ! nfStat = nf90_open(path = './data', nf90_write, NCID)
    end subroutine
end module
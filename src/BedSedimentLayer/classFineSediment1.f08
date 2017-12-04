!> Module container for `FineSediment1` class. Nonpolymorphic.
module classFineSediment1
    use Globals
    use ErrorInstanceModule
    use ResultModule
    use UtilModule
    implicit none                                                    ! force declaration of all variables
    !> Definition of `FineSediment1` class. Nonpolymorphic.
    type, public :: FineSediment1
        character(len=256) :: name                                   !! A name for the object
        real(dp), private :: M_f_l                                   !! LOCAL fine sediment mass [kg m-2]
        real(dp), private :: V_w_l                                   !! LOCAL volume of water associated with fine sediment [m3 m-2]
        real(dp), allocatable :: f_comp(:)                           !! Fractional composition [-]
        real(dp), allocatable :: pd_comp(:)                          !! LOCAL storage of fractional particle densities [kg m-3]
        integer :: nfComp                                            !! LOCAL number of fractional composition terms
    contains
        procedure, public :: create => createFineSediment1           ! sets up by reading variables required for computations
        procedure, public :: destroy => destroyFineSediment1         ! finalises by doing all necessary deallocations
        procedure, public :: set => setFS1                           ! set properties, using either fine sediment volume or mass
        procedure, public :: V_f => getFSVol1                        ! returns the fine sediment volume [m3 m-2]
        procedure, public :: M_f => getFSMass1                       ! returns the fine sediment mass [kg m-2]
        procedure, public :: V_w => getWVol1                         ! returns the water volume [kg m-2]
        procedure, public :: rho_part => pdens1                      ! returns the fine sediment particle density [kg m-3]
        procedure, public :: audit_comp => audit_fcomp1              ! check the fractional composition
        procedure, public :: IsEmpty => empty1                       ! check for presence of sediment and water
        procedure, public :: ClearAll => ClearAll1                   ! clear all fine sediment and water from the object
        procedure, public :: mix => Mix1                             ! mix this sediment into another
    end type

    !> Result object with operator for FineSediment scalar data
    type, public, extends(Result0D) :: ResultFineSediment0D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment0D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

    !> Result object with operator for data as 1D FineSediment array
    type, public, extends(Result1D) :: ResultFineSediment1D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment1D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

    !> Result object with operator for data as 2D FineSediment array
    type, public, extends(Result2D) :: ResultFineSediment2D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment2D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

    interface Result
        module procedure init0D, init1D, init2D
    end interface

  contains
        !> **Function purpose**                                     <br>
        !! Initialise a `FineSediment` object. Set the object name, number of compositional
        !! fractions, and particle density for each compositional fraction.
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `n (character)`: a name unambiguously identifying the object
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Initialised `FineSediment1` object. Returns `Result` object containing
        !! `ErrorInstance` if no object name has been provided.
        function createFineSediment1(Me, n) result(r)
            class(FineSediment1) :: Me                               !! Self-reference
            character(len=*) :: n                                    !! A name identifier for the object; identifies this object uniquely
            type(Result) :: r                                        !! `Result` object
            type(ErrorInstance) :: er                                ! LOCAL to store errors in
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            integer :: allst                                         ! LOCAL array allocation status
        !
        ! Notes
        ! -------------------------------------------------------------------------------
        ! No notes.
        ! -------------------------------------------------------------------------------
            if (len_trim(n) == 0) then
                call r%addError(ErrorInstance( &
                             code = 1, &
                             message = "An object name has not been &
                                        provided", &
                             trace = ["classFineSediment1%create"])) ! error if name is not provided
                return                                               ! critical error, so exit here
            end if
            Me%name = n                                              ! set object name
            Me%nfComp = C%nFracCompsSpm                              ! set number of compositional fractions from Global
            allocate(Me%pd_comp(Me%nfComp), stat = allst)            ! allocate space for particle densities of compositional fractions
            if (allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                    trace = [Me%name // &
                "%createBedSedimentLayer1%colFineSediment"] &
                                  )                                  ! create error
                call r%addError(er)                                  ! add to Result
                return                                               ! critical error, so exit
            end if
            Me%pd_comp = C%d_pd                                         ! particle densities of compositional fractions from Global
            allocate(Me%f_comp(Me%nfComp), stat = allst)             ! allocate space for compositional fractions
            if (allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                    trace = [Me%name // &
                "%createBedSedimentLayer1%colFineSediment"] &
                                  )                                  ! create error
                call r%addError(er)                                  ! add to Result
                return                                               ! critical error, so exit
            end if
        end function
        !> **Function purpose**                                     <br>
        !! Finalise a `FineSediment` object. Deallocate all class-level allocated variables
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! finalised `FineSediment1` object. Returns `Result` object containing
        !! `ErrorInstance` if deallocation causes error(s).
        function destroyFineSediment1(Me) result (r)
            class(FineSediment1) :: Me                               !! Self-reference
            type(Result) :: r                                        !! `Result` object
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            character(len=18), parameter :: &
                                          ms = "Deallocation error"  ! LOCAL CONSTANT error message
            type(ErrorInstance) :: er                                ! LOCAL Object to store ErrorInstances in
            integer :: allst                                         ! LOCAL array allocation status
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            tr = Me%name // &
                "%destroyFineSediment1"                              ! trace message
            deallocate(Me%f_comp, stat = allst)                      ! deallocate f_comp
            if (allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            deallocate(Me%pd_comp, stat = allst)                     ! deallocate pd_comp
            if (allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
        end function
        !> **Function purpose**                                     <br>
        !! Set one or more of the fundamental properties of a FineSediment1 object
        !! these properties are:
        !! <ol>
        !!  <li>the sediment mass [kg] - which can be set either directly
        !!      or by setting the sediment volume</li>
        !!  <li>the volume of water associated with the sediment within a sediment layer</li>
        !!  <li>the sediment fractional composition [-]</li>
        !! </ol>
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `Mf_in` [optional]: the fine sediment mass [kg m-2]      <br>
        !! `Vf_in` [optional]: the fine sediment volume [m3 m-2]    <br>
        !! `Vw_in` [optional]: the associated water volume [m3 m-2] <br>
        !! `f_comp_in` [optional]: the fractional composition [-]
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! If no critical error(s), function sets the requested properties of this
        !! FineSediment1 object.
        !! A critical error is thrown if:
        !! <ol>
        !!  <li>both Mf_in and Vf_in are specified;</li>
        !!  <li>either Mf_in, Vf_in or Vw_in are equal to or less than zero;</li>
        !!  <li>f_comp_in is specified, but its array dimension does not match the number
        !!      of fractional compositions</li>
        !!  <li>f_comp_in is specified, but its contained values do not sum to unity.</li>
        !! </ol>
        function setFS1(Me, Mf_in, Vf_in, Vw_in, f_comp_in) &
            result(r)
            class(FineSediment1) :: Me                               !! Self-reference
            real(dp), intent(in), optional :: Mf_in                  !! The fine sediment mass
            real(dp), intent(in), optional :: Vf_in                  !! The fine sediment volume
            real(dp), intent(in), optional :: Vw_in                  !! The water volume. Optional; if not present, stored composition is used
            real(dp), intent(in), optional :: f_comp_in(:)           !! Input fractional composition. Optional; if not present, stored composition is used
            type(Result) :: r                                        !! `Result` object
            type(ErrorInstance) :: er                                ! LOCAL ErrorInstance object
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            if ((present(Mf_in)) .and. (present(Vf_in))) then        ! both sediment mass and volume specified - cannot use both
                er = ErrorInstance( &
                    code = 1, &
                    message = "Sediment mass and volume both &
                               specified", &
                    trace = [Me%name] &
                                  )                                  ! compose error
                call r%addError(er)                                  ! add it to the result
                return                                               ! and exit
            end if
            if (present(Mf_in)) then
                if (Mf_in <= 0) then                                 ! Mf_in is invalid
                    er = ErrorInstance( &
                        code = 103, &
                        message = "Sediment mass is out of range", &
                        trace = [Me%name] &
                                      )                              ! compose error
                    call r%addError(er)                              ! add it to the result
                    return                                           ! and exit
                end if
            end if
            if (present(Vf_in)) then
                if (Vf_in <= 0) then                                 ! Vf_in is invalid
                    er = ErrorInstance( &
                        code = 103, &
                        message = "Sediment volume is out of range", &
                        trace = [Me%name] &
                                      )                              ! compose error
                    call r%addError(er)                              ! add it to the result
                    return                                           ! and exit
                end if
            end if
            if (present(Vw_in)) then
                if (Vw_in <= 0) then                                 ! Vw_in is invalid
                    er = ErrorInstance( &
                        code = 103, &
                        message = "Water volume is out of range", &
                        trace = [Me%name] &
                                      )                              ! compose error
                    call r%addError(er)                              ! add it to the result
                    return                                           ! and exit
                end if
            end if
            if (present(f_comp_in)) then
                if (size(f_comp_in) /= Me%NFComp) then
                    er = ErrorInstance( &
                        code = 106, &
                        message = "Size of fractional composition &
                                   array incorrect", &
                        trace = [Me%name // "%SetFS1"] &             ! check size of compositional array against stored no. of fraction
                                      )
                    call r%addError(er)                              ! add it to the result
                    return                                           ! and exit, as this is a critical error
                else                                                 ! if no error thrown, then
                    Me%f_comp = f_comp_in                            ! store the composition locally
                    er = Me%audit_comp()                             ! audit sum (fractional composition) = 1, return error instance
                    if (er%isError()) then                           ! if an error was thrown
                        call er%addToTrace(trim(Me%name) // "%SetFS1") ! add a trace
                        call r%addError(er)                          ! add it to the result
                        return                                       ! and exit, as this is a critical error
                    end if
                end if
            end if
            if (present(Mf_in)) Me%M_f_l = Mf_in                     ! Storing fine sediment mass, if specified
            if (present(Vf_in)) Me%M_f_l = Vf_in * Me%rho_part()     ! Storing fine sediment volume, if specified
            if (present(Vw_in)) Me%V_w_l = Vw_in                     ! Volume of water, if specified
        end function
        !> **Function purpose**                                     <br>
        !! Return the fine sediment volume [m3 m-2] as a derived property
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Fine sediment volume [m3 m-2]
        pure function getFSVol1(Me) result(Vf)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            real(dp) :: Vf                                           !! The return value
                                                                     ! function to return the fine sediment volume [m3 m-2]
                                                                     ! Output: Vf = Fine sediment volume
            Vf = Me%M_f_l / Me%rho_part()                            ! fine sediment volume computation
        end function
        !> **Function purpose**                                     <br>
        !! PropertyGet: return the fine sediment mass [kg m-2]
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Fine sediment mass [kg m-2]
        pure function getFSMass1(Me) result(Mf)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            real(dp) :: Mf                                           !! The return value
                                                                     ! function to return the fine sediment mass [kg m-2]
                                                                     ! Output: Mf = Fine sediment mass
            Mf = Me%M_f_l                                            ! fine sediment mass computation
        end function
        !> **Function purpose**                                     <br>
        !! PropertyGet: return the water volume [m3 m-2]
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! water volume [m3 m-2]
        pure function getWVol1(Me) result(Vw)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            real(dp) :: Vw                                           !! The return value
                                                                     ! function to return the water volume [m3 m-2]
                                                                     ! Output: Vw = water volume
            Vw = Me%V_w_l                                            ! water volume computation
        end function
        !> **Function purpose**                                     <br>
        !! Returns fine sediment particle density as a derived property
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Fine sediment particle density [kg m-3]
        pure function pdens1(Me) result(rho_part)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            real(dp) :: rho_part                                     !! Return value: the particle density [kg m-3]
            integer :: x                                             ! LOCAL loop counter
            rho_part = 0                                             ! initialise output variable
            do x = 1, Me%NFComp
                rho_part = rho_part + Me%f_comp(x) * Me%pd_comp(x)   ! summing contributions to particle density
            end do
        end function
        !> **Function purpose**                                     <br>
        !! Check that the array of fractional compositions sums to unity
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Function returns ErrorInstance with values
        !! 106: if fractional composition does not sum to unity <br>
        !! 0: otherwise
        pure function audit_fcomp1(Me) result(er)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            type(ErrorInstance) :: er                                !! `ErrorInstance` object, returns error if `t_fcomp /= 1`
            integer :: F                                             ! LOCAL loop counter
            real(dp) :: t_fcomp                                      ! LOCAL sum of fractional compositions
            t_fcomp = 0                                              ! Initialise to zero
            do F = 1, Me%NFComp
                t_fcomp = t_fcomp + Me%f_comp(F)                     ! summing fractional compositions
            end do
            if (t_fcomp < (1.0_dp-1.0d-10) .or. t_fcomp > (1.0_dp+1.0d-10)) then
                er = ErrorInstance( &
                    code = 106, &
                    message = "Fractional composition does &
                               not sum to unity. Value: " // trim(str(t_fcomp)) // "." &
                )                                                    ! check t_fcomp = 1
            else
                er = ErrorInstance(0)                                ! Else, no error
            end if
        end function
        !> **Function purpose**                                     <br>
        !! Check whether this object contains any sediment or water
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Function returns .true./.false. depending on whether any sediment or water is
        !! present or not
        pure function Empty1(Me) result(t)
            class(FineSediment1), intent(in) :: Me                   !! Self-reference
            logical :: t
                !! Return value. True: `V_f/M_f = V_w = 0`. False: `V_f/M_f > 0 .or. V_w > 0`
            t = .false.
            if (Me%M_f_l == 0 .and. Me%V_w_l == 0) t = .true.
        end function
        !> **Subroutine purpose**                                   <br>
        !! Clear the sediment mass and water volume of this `FineSediment`
        !!                                                          <br>
        !! **Subroutine outputs/outcomes**                          <br>
        !! Subroutine sets fine sediment mass, water volume and all fractional
        !! compositions to zero. No outputs.
        subroutine ClearAll1(Me)
            class(FineSediment1) :: Me                               !! The `FineSediment` instance
            integer :: X                                             ! LOCAL loop counter
            Me%M_f_l = 0                                             ! clear fine sediment mass
            Me%V_w_l = 0                                             ! clear water volume
            do X = 1, Me%nfComp                                         ! clear fractional composition
                Me%f_comp(X) = 0
            end do
        end subroutine
        !> **Function purpose**                                     <br>
        !! Mix this FineSediment with another and return the result
        !!                                                          <br>
        !! **Function takes as inputs:**
        !! `FS` &tab; FineSediment object with which this one is to be mixed.
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! Function returns `ResultFineSediment0D` object containing the mixed `FineSediment` <br><br>
        !! Function throws critical error if:
        !! <ol>
        !!      <li>fine sediment mass in FS <= 0.</li>
        !!      <li>water volume in FS < 0.</li>
        !!      <li>the number of fractional compositions in FS differs from that in Me.</li>
        !!      <li>the sum of fractional components is FS /= 1.</li>
        !!      <li>any particle density in FS <= 0.</li>
        !!      <li>allocation of f_comp_mix fails.</li>
        !! </ol>
        function Mix1(Me, FS) result(r)
            class(FineSediment1) :: Me                               !! Self-reference
            type(FineSediment1), intent(in) :: FS                    !! `FineSediment1` to be mixed with this one
            type(ResultFineSediment0D) :: r                          !! `Result` object
            type(ErrorInstance) :: er                                ! LOCAL error instance object
            integer :: x                                             ! LOCAL loop counter
            real(dp) :: M_f_mix                                      ! LOCAL mixed sediment mass
            real(dp) :: V_w_mix                                      ! LOCAL mixed water volume
            real(dp), allocatable :: f_comp_mix(:)                   ! LOCAL mixed fractional composition
            integer :: allst                                         ! LOCAL allocation status
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            if (FS%M_f() <= 0) then                                  ! mixing sediment mass is invalid
                er = ErrorInstance( &
                    code = 103, &
                    message = "Mixing sediment mass is invalid", &
                    trace = [Me%name // "%Mix1"] &
                                  )                                  ! compose error
                call r%addError(er)                                  ! add it to the result
            end if                                                   ! note that V_f does not need to be checked
            if (FS%V_w() < 0) then                                   ! mixing water volume is invalid - note procedure does accept that volume can be zero
                er = ErrorInstance( &
                    code = 103, &
                    message = "Mixing water volume is invalid", &
                    trace = [Me%name // "%Mix1"] &
                                  )                                  ! compose error
                call r%addError(er)                                  ! add it to the result
            end if
            if (Me%nfComp /= FS%nfComp) then                         ! inconsistency in number of compositional fractions
                er = ErrorInstance( &
                    code = 106, &
                    message = "Inconsistent numbers of compositional &
                               fractions", &
                    trace = [Me%name // "%Mix1"] &
                                  )                                  ! compose error
                call r%addError(er)                                  ! add it to the result
            end if
            er = FS%audit_comp()                                     ! audit sum (fractional composition) = 1, return error instance
            if (er%isError()) then                                   ! if an error was thrown
                call er%addToTrace(Me%name // "%Mix1")               ! add a trace
                call r%addError(er)                                  ! add it to the result
            end if
            if (r%hasCriticalError()) return                         ! exit if a critical error has been thrown
            do x = 1, FS%nfComp
                if (FS%pd_comp(x) <= 0) then                         ! check all particle densities are valid
                    er = ErrorInstance( &
                        code = 103, &
                        message = "A mixing particle density is &
                                   invalid", &
                        trace = [Me%name // "%Mix1"] &
                                      )                              ! compose error
                    call r%addError(er)                              ! add it to the result
                    return                                           ! and exit
                end if
            end do
            allocate(f_comp_mix(Me%nfComp), stat = allst)         ! allocate f_comp_mix
            if (allst /= 0) then
                er = ErrorInstance(1, &
                            message = "Allocation error", &
                            trace = [Me%name // "%Mix1"] &
                                  )                                  ! create error
                call r%addError(er)                                  ! add to Result
                return                                               ! critical error, so exit
            end if
            do x = 1, Me%nfComp
                f_comp_mix(x) = (Me%M_f() * Me%f_comp(x) + &
                                 FS%M_f() * FS%f_comp(x)) / &
                                (Me%M_f() + FS%M_f())                ! mass-weighted fractional composition of mixed sediment
            end do
            M_f_mix = Me%M_f() + FS%M_f()                            ! mixed sediment mass
            V_w_mix = Me%V_w() + FS%V_w()                            ! mixed water volume
            call r%addErrors(.errors. FS%set(Mf_in = M_f_mix, &
                                        Vw_in = V_w_mix, &
                                        f_comp_in = f_comp_mix &
                                            ) &
                            )                                        ! set the properties of the returned FineSediment object
            if (r%hasCriticalError()) return                         ! exit if critical error thrown
            r = Result(data = FS)                                    ! feed FineSediment object into Result
       end function

! *********************************!
!** ResultFineSediment extension **!
!**********************************!
        !> Initialise the result object with 0D `FineSediment` data.
        pure function init0D(data, error, errors) result(this)
            type(ResultFineSediment0D)                  :: this         !! This `Result` object
            type(FineSediment1), intent(in)             :: data         !! 0D data to store
            type(ErrorInstance), intent(in), optional   :: error        !! An error to store
            type(ErrorInstance), intent(in), optional   :: errors(:)    !! Any errors to store

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Initialise the result object with 1D `FineSediment` data.
        pure function init1D(data, error, errors) result(this)
            type(ResultFineSediment1D)                  :: this         !! This Result object
            type(FineSediment1), intent(in)             :: data(:)      !! 1D data to store
            type(ErrorInstance), intent(in), optional   :: error        !! An error to store
            type(ErrorInstance), intent(in), optional   :: errors(:)    !! Any errors to store

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

        !> Initialise the result object with 2D `FineSediment` data.
        pure function init2D(data, error, errors) result(this)
            type(ResultFineSediment2D)                  :: this         !! This Result object
            type(FineSediment1), intent(in)             :: data(:,:)    !! 2D data to store
            type(ErrorInstance), intent(in), optional   :: error        !! An error to store
            type(ErrorInstance), intent(in), optional   :: errors(:)    !! Any errors to store

            ! Store the given data in this%data and set the errors
            allocate(this%data, source=data)
            call this%setErrors(error, errors)
        end function

       !> Attempt to return the data as a scalar FineSediment1 object
        pure function getDataAsFineSediment0D(this) result(data)
            class(ResultFineSediment0D), intent(in) :: this     !! This Result object
            type(FineSediment1)                     :: data     !! The data as a `FineSediment1` object
            select type (d => this%data)
                type is (FineSediment1)
                    data = d
                class default
                    error stop "Error trying to return 0D data as FineSediment1. Are you sure the data is of type FineSediment1?"
            end select
        end function

        !> Attempt to return the data as a 1D FineSediment1 object array
        pure function getDataAsFineSediment1D(this) result(data)
            class(ResultFineSediment1D), intent(in) :: this                     !! This Result object
            type(FineSediment1)                     :: data(size(this%data))    !! The data as a 1D `FineSediment1` array
            select type (d => this%data)
                type is (FineSediment1)
                    data = d
                class default
                    error stop "Error trying to return 1D data as FineSediment1. Are you sure the data is of type FineSediment1?"
            end select
        end function

        !> Attempt to return the data as a 2D FineSediment1 object array
        pure function getDataAsFineSediment2D(this) result(data)
            class(ResultFineSediment2D), intent(in) :: this                         !! This Result object
            type(FineSediment1)                     :: data(size(this%data,1), &
                                                            size(this%data,2))      !! The data as a 1D `FineSediment1` array
            select type (d => this%data)
                type is (FineSediment1)
                    data = d
                class default
                    error stop "Error trying to return 2D data as FineSediment1. Are you sure the data is of type FineSediment1?"
            end select
        end function
end module

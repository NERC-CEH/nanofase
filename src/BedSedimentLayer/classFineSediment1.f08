!> definition for class FineSediment1. Nonpolymorphic.
module classFineSediment1
    use Globals
    use ResultModule
    implicit none                                                    ! force declaration of all variables
    type, public :: FineSediment1                                    ! type declaration for class
        character(len=256) :: name                                   !! a name for the object
        real(dp), private :: M_f_l                                   !! LOCAL fine sediment mass [kg m-2]
        real(dp), private :: V_w_l                                   !! LOCAL volume of water associated with fine sediment [m3 m-2]
        real(dp), allocatable :: f_comp(:)                           !! fractional composition [-]
        real(dp), allocatable :: pd_comp_l(:)                        !! LOCAL storage of fractional particle densities [kg m-3]
        integer :: nfComp                                            !! LOCAL number of fractional composition terms
        integer :: allst                                             !! array allocation status
    contains
        procedure, public :: create => createFineSediment1           ! sets up by reading variables required for computations
        procedure, public :: set => setFS1                           ! set properties, using either fine sediment volume or mass
!        procedure, public :: setByV => setFSVol1                     ! set properties, using a fine sediment volume
!        procedure, public :: setByM => setFSMass1                    ! set properties, using a fine sediment mass
        procedure, public :: V_f => getFSVol1                        ! returns the fine sediment volume [m3 m-2]
        procedure, public :: M_f => getFSMass1                       ! returns the fine sediment mass [kg m-2]
        procedure, public :: V_w => getWVol1                         ! returns the water volume [kg m-2]
        procedure, public :: rho_part => pdens1                      ! returns the fine sediment particle density [kg m-3]
        procedure, public :: audit_comp => audit_fcomp1              ! check the fractional composition
        procedure, public :: IsEmpty => empty1                       ! check for presence of sediment and water
        procedure, public :: ClearAll => ClearAll1                   ! clear all fine sediment and water from the object
        procedure, public :: mix => Mix1                             ! mix this sediment into another
    end type
    contains
        !> initialise this object
        function createFineSediment1(Me, n, pd_comp_in) &
            result(r)
            implicit none
            class(FineSediment1) :: Me                               !! self-reference
            character(len=256) :: n                                  !! a name identifier for the object; identifies this object uniquely
            real(dp), intent(in), allocatable :: pd_comp_in(:)       !! input array of particle densities for compositional fractions
            real(dp), allocatable :: pd_comp(:)
            type(Result) :: r                                        !! Result object
            type(ErrorInstance) :: er                                ! To store errors in
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! initialise a FineSediment object. Set the object name, number of compositional
            ! fractions, and particle density for each compositional fraction.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! n (character)            a name unambiguously identifying the object
            ! nsc (integer)            the number of size classes of sediment
            ! pd_comp_in(:) (real, dp) 1D array of particle density [kg m-3] for
            !                          compositional fractions
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! initialised FineSediment1 object. Returns Result object containing
            ! ErrorInstance if no object name has been provided.
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
            Me%NFComp = size(pd_comp_in)                             ! set number of compositional fractions
            allocate(pd_comp(Me%nfComp), stat = Me%allst)            ! allocate space for particle densities of compositional fractions
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                    trace = [Me%name // &
                "%createBedSedimentLayer1%colFineSediment"] &
                                  )                                  ! create error
                call r%addError(er)                                  ! add to Result
                return                                               ! critical error, so exit
            end if
            pd_comp = pd_comp_in                                     ! read in particle densities of compositional fractions
        end function
        !> set the properties of the object
        function setFS1(Me, Mf_in, Vf_in, Vw_in, f_comp_in) &
            result(r)
            ! QUERY: can this function be pure?
            class(FineSediment1) :: Me                                ! self-reference
            real(dp), intent(in), optional :: Mf_in                  ! the fine sediment mass
            real(dp), intent(in), optional :: Vf_in                  ! the fine sediment volume
            real(dp), intent(in), optional :: Vw_in                  ! the water volume. Optional; if not present, stored composition is used
            real(dp), intent(in), optional :: f_comp_in(:)           ! input fractional composition. Optional; if not present, stored composition is used
            type(Result) :: r                                        ! Result object
            type(ErrorInstance) :: er                                ! LOCAL ErrorInstance object
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! set one or more of the fundamental properties of a FineSediment1 object
            ! these properties are:
            !   1. the sediment mass [kg] - which can be set either directly
            !      or by setting the sediment volume
            !   2. the volume of water associated with the sediment within a sediment layer
            !   3. the sediment fractional composition [-]
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            !
            ! Mf_in [optional]         the fine sediment mass [kg m-2]
            ! Vf_in [optional]         the fine sediment volume [m3 m-2]
            ! Vw_in [optional]         the associated water volume [m3 m-2]
            ! f_comp_in [optional]     the fractional composition [-]
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! if no critical error(s), function sets the requested properties of this
            ! FineSediment1 object.
            ! a critical error is thrown if:
            !   1. both Mf_in and Vf_in are specified;
            !   2. either Mf_in, Vf_in or Vw_in are equal to or less than zero;
            !   3. f_comp_in is specified, but its array dimension does not match the number
            !      of fractional compositions;
            !   4. f_comp_in is specified, but its contained values do not sum to unity.
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
                        call er%addToTrace(Me%name // "%SetFS1")     ! add a trace
                        call r%addError(er)                          ! add it to the result
                        return                                       ! and exit, as this is a critical error
                    end if
                end if
            end if
            if (present(Mf_in)) Me%M_f_l = Mf_in                     ! Storing fine sediment mass, if specified
            if (present(Vf_in)) Me%M_f_l = Vf_in * Me%rho_part()     ! Storing fine sediment volume, if specified
            if (present(Vw_in)) Me%V_w_l = Vw_in                     ! Volume of water, if specified
        end function
        !> return the fine sediment volume [m3 m-2]
        pure function getFSVol1(Me) result(Vf)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            real(dp) :: Vf                                           !! the return value
                                                                     ! function to return the fine sediment volume [m3 m-2]
                                                                     ! Output: Vf = Fine sediment volume
            Vf = Me%M_f_l / Me%rho_part()                            ! fine sediment volume computation
        end function
        !> return the fine sediment mass [kg m-2]
        pure function getFSMass1(Me) result(Mf)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            real(dp) :: Mf                                           !! the return value
                                                                     ! function to return the fine sediment mass [kg m-2]
                                                                     ! Output: Mf = Fine sediment mass
            Mf = Me%M_f_l                                            ! fine sediment mass computation
        end function
        !> return the water volume [m3 m-2]
        pure function getWVol1(Me) result(Vw)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            real(dp) :: Vw                                           !! the return value
                                                                     ! function to return the water volume [m3 m-2]
                                                                     ! Output: Vw = water volume
            Vw = Me%V_w_l                                            ! water volume computation
        end function
        !> compute particle density from components and their densities
        pure function pdens1(Me) result(rho_part)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            real(dp) :: rho_part                                     !! return value: the particle density [kg m-3]
            integer :: x                                             ! LOCAL loop counter
            rho_part = 0                                             ! initialise output variable
            do x = 1, Me%NFComp
                rho_part = rho_part + Me%f_comp(x) * Me%pd_comp_l(x) ! summing contributions to particle density
            end do
        end function
        !> check that the array of fractional compositions sums to unity
        pure function audit_fcomp1(Me) result(er)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            type(ErrorInstance) :: er                                !! ErrorInstance object, returns error if t_fcomp /= 1
            integer :: F                                             ! LOCAL loop counter
            real(dp) :: t_fcomp                                      ! LOCAL sum of fractional compositions
            do F = 1, Me%NFComp
                t_fcomp = t_fcomp + Me%f_comp(F)                     ! summing fractional compositions
            end do
            if (t_fcomp /= 1) then
                er = ErrorInstance( &
                    code = 106, &
                    message = "Fractional composition does &
                               not sum to unity.", &
                    trace = [trim(Me%name)] &
                )                                                    ! check t_fcomp = 1
            end if
        end function
        !> check whether this object contains any fine sediment or water of the specified size class
        pure function Empty1(Me) result(t)
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            logical :: t                                             !! return value. True= V_f/M_f = V_w = 0. False= V_f/M_f > 0 .or. V_w > 0
            t = .false.
            if (Me%M_f_l == 0 .and. Me%V_w_l == 0) t = .true.
        end function
        !> clear all properties
        subroutine ClearAll1(Me)
            class(FineSediment1) :: Me                               !! the FineSediment instance
            integer :: X                                             ! LOCAL loop counter
            Me%M_f_l = 0                                             ! clear fine sediment mass
            Me%V_w_l = 0                                             ! clear water volume
            do X = 1, Me%nfComp                                         ! clear fractional composition
                Me%f_comp(X) = 0
            end do
        end subroutine
        !> mix two FineSediment objects together
        function Mix1(Me, FS) result(r)
            class(FineSediment1) :: Me                               !! self-reference
            type(FineSediment1), intent(in) :: FS                    !! FineSediment1 to be mixed with this one
            type(Result0D) :: r                                      !! Result object
            type(ErrorInstance) :: er                                ! LOCAL error instance object
            integer :: x                                             ! LOCAL loop counter
            real(dp) :: M_f_mix                                      ! LOCAL mixed sediment mass
            real(dp) :: V_w_mix                                      ! LOCAL mixed water volume
            real(dp), allocatable :: f_comp_mix(:)                   ! LOCAL mixed fractional composition
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! mix this FineSediment with another and return the result
            !
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! FS    FineSediment object with which this one is to be mixed.
            !
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! Function returns Result0D object containing the mixed FineSediment
            !
            ! Function throws critical error if:
            ! 1.    fine sediment mass in FS <= 0.
            ! 2.    water volume in FS < 0.
            ! 3.    the number of fractional compositions in FS differs from that in Me.
            ! 4.    the sum of fractional components in FS /= 1.
            ! 5.    any particle density in FS <= 0.
            ! 6.    allocation of f_comp_mix fails.
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
                if (FS%pd_comp_l(x) <= 0) then                       ! check all particle densities are valid
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
            allocate(f_comp_mix(Me%nfComp), stat = Me%allst)         ! allocate f_comp_mix
            if (Me%allst /= 0) then
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
                       )                                             ! set the properties of the returned FineSediment object
            if (r%hasCriticalError()) return                         ! exit if critical error thrown
            r = Result(data = FS)                                    ! feed FineSediment object into Result
       end function
end module

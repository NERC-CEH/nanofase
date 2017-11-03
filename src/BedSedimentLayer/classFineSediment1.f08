module classFineSediment1                                            !! definition for class FineSediment1. Nonpolymorphic.
    use Globals
    use ResultModule
    implicit none                                                    !! force declaration of all variables
    type, public :: FineSediment1                                    !! type declaration for class
        character(len=256) :: name                                   !! a name for the object
        real(dp), private :: M_f_l                                   !! LOCAL fine sediment mass [kg m-2]
        real(dp), private :: V_w_l                                   !! LOCAL volume of water associated with fine sediment [m3 m-2]
        real(dp), allocatable :: f_comp(:)                           !! fractional composition [-]
        real(dp), allocatable :: pd_comp_l(:)                        !! LOCAL storage of fractional particle densities [kg m-3]
        integer :: NFComp                                            !! LOCAL number of fractional composition terms
    contains
        procedure, public :: create => createFineSediment1           !! sets up by reading variables required for computations
        procedure, public :: set => setFS1                           !! set properties, using either fine sediment volume or mass
!        procedure, public :: setByV => setFSVol1                     !! set properties, using a fine sediment volume
!        procedure, public :: setByM => setFSMass1                    !! set properties, using a fine sediment mass
        procedure, public :: V_f => getFSVol1                        !! returns the fine sediment volume [m3 m-2]
        procedure, public :: M_f => getFSMass1                       !! returns the fine sediment mass [kg m-2]
        procedure, public :: V_w => getWVol1                         !! returns the water volume [kg m-2]
        procedure, public :: rho_part => pdens1                      !! returns the fine sediment particle density [kg m-3]
        procedure, public :: audit_comp => audit_fcomp1              !! check the fractional composition
        procedure, public :: IsEmpty => empty1                       !! check for presence of sediment and water
        procedure, public :: ClearAll => ClearAll1                   !! clear all fine sediment and water from the object
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
            character(len=256) :: tr                                 !! LOCAL name of this procedure, for trace
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! initialise a FineSediment object. Set the object name, number of compositional
            ! fractions, and particle density for each compositional fraction.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
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
                             trace = ["classFineSediment1%create"]))    !! error if name is not provided
                return                                               !! critical error, so exit here
            end if
            Me%name = n                                              !! set object name
            Me%NFComp = size(pd_comp_in)                             !! set number of compositional fractions
            allocate(pd_comp(1:Me%NFComp))                              !! allocate space for particle densities of compositional fractions
            pd_comp = pd_comp_in                                     !! read in particle densities of compositional fractions
        end function
        !> set the properties of the sediment, using fine sediment volume
        !function SetFSVol1(Me, Vf_in, Vw_in, f_comp_in) result(r)
        !    implicit none                                            !! based on the volume [m3 m-2] and particle density [kg m-3]
        !    class(FineSediment1) :: Me                                !! self-reference
        !    type(Result) :: r                                        !! Result object
        !    type(ErrorInstance) :: er                                !! LOCAL ErrorCriteria object
        !    real(dp), intent(in), optional :: Vf_in                  !! the fine sediment volume
        !    real(dp), intent(in), optional :: Vw_in                  !! the water volume
        !    real(dp), intent(in), optional, allocatable :: &
        !                                             f_comp_in(:)    !! input fractional composition. Optional; if not present, stored composition is used
        !                                                             !! function to set properties given a fine sediment volume
        !                                                             !! Input: Vf_in = the value of the fine sediment volume
        !                                                             !! Input: f_comp_in(:) [optional] = the composition of the fine sediment,
        !                                                             !! by mass fraction of each defined component
        !    if (present(f_comp_in)) then
        !        er = &
        !            ERROR_HANDLER%notEqual(Me%NFComp, &
        !                                   size(f_comp_in), &
        !                                   message = "Size of &
        !                                              fractional &
        !                                              composition &
        !                                              array &
        !                                              incorrect", &
        !                              traceMessage = Me%name // &
        !                                             "%SetFSVol1")   !! check size of compositional array against stored no. of fraction
        !        if (er%notError()) then                       !! if no error thrown, then
        !            Me%f_comp = f_comp_in                            !! store the composition locally
        !            er = Me%audit_comp()                            !! audit sum (fractional composition) = 1, return error instance
        !            if (er%isError()) then                   !! if an error was thrown
        !                call er%addToTrace(Me%name // "%SetFSVol1")       !! add a trace
        !                call r%addError(er)                               !! add it to the result
        !                return                                       !! and exit, as this is a critical error
        !            end if
        !        else                                                 !! if fractional composition array size threw and error,
        !            call r%addError(er)                                   !! add it to the result
        !            return                                           !! and exit, as this is a critical error
        !        end if
        !    end if
        !    if (present(Vf_in)) Me%M_f_l = Vf_in * Me%rho_part()        !! Computation of fine sediment mass from volume and particle density
        !    if (present(Vw_in))  Me%V_w_l = Vw_in                    !! Volume of water
        !end function
        !> set the properties of the object
        function setFS1(Me, Mf_in, Vf_in, Vw_in, f_comp_in) &
            result(r)
            ! QUERY: can this function be pure?
            implicit none
            type(FineSediment1) :: Me                                !! self-reference
            real(dp), intent(in), optional :: Mf_in                  !! the fine sediment mass
            real(dp), intent(in), optional :: Vf_in                  !! the fine sediment volume
            real(dp), intent(in), optional :: Vw_in                  !! the water volume. Optional; if not present, stored composition is used
            real(dp), intent(in), optional, allocatable :: &
                                                     f_comp_in(:)    !! input fractional composition. Optional; if not present, stored composition is used
            type(Result) :: r                                        !! Result object
            type(ErrorInstance) :: er                                !! LOCAL ErrorInstance object
                                                                     !! function to set properties given a fine sediment mass
                                                                     !! Input: Mf_in [optional] = the value of the fine sediment mass
                                                                     !! Input: Vf_in [optional] = the value of the fine sediment volume
                                                                     !! Input: Vw_in [optional] = the value of the water volume
                                                                     !! Input: f_comp_in(:) [optional] = the composition of the fine sediment,
                                                                     !! by mass fraction of each defined component
            if ((present(Mf_in)) .and. (present(Vf_in))) then        !! both sediment mass and volume specified - cannot use both
                er = ErrorInstance( &
                    code = 1, &
                    message = "Sediment mass and volume both &
                               specified", &
                    traceMessage = [trim(Me%name)] &
                                  )                                  !! compose error
                return                                               !! and exit
            end if
            if (present(f_comp_in)) then
                if (size(f_comp_in) /= Me%NFComp) then
                    er = ErrorInstance( &
                        code = 106, &
                        message = "Size of fractional composition &
                                   array incorrect", &
                        traceMessage = Me%name // "%SetFS1")         !! check size of compositional array against stored no. of fraction
                                      )
                call r%addError(er)                                  !! add it to the result
                return                                               !! and exit, as this is a critical error
 !               er = &
 !                   ERROR_HANDLER%notEqual(Me%NFComp, &
 !                                          size(f_comp_in), &
 !                                          message = "Size of &
 !                                                     fractional &
 !                                                     composition &
 !                                                     array &
 !                                                     incorrect", &
 !                                     traceMessage = Me%name // &
 !                                                    "%SetFSMass1")  !! check size of compositional array against stored no. of fraction
                else                                                 !! if no error thrown, then
                    Me%f_comp = f_comp_in                            !! store the composition locally
                    er = Me%audit_comp1()                            !! audit sum (fractional composition) = 1, return error instance
                    if (er%isError()) then                           !! if an error was thrown
                        call er%addToTrace(Me%name // "%SetFS1")     !! add a trace
                        call r%addError(er)                          !! add it to the result
                        return                                       !! and exit, as this is a critical error
                    end if
                end if
            end if
            if (present(Mf_in)) Me%M_f_l = Mf_in                     !! Storing fine sediment mass, if specified
            if (present(Vf_in)) Me%M_f_l = Vf_in * Me%pdens1()       !! Storing fine sediment volume, if specified
            if (present(Vw_in)) Me%V_w_l = Vw_in                     !! Volume of water, if specified
        end function
        !> return the fine sediment volume [m3 m-2]
        pure function getFSVol1(Me) result(Vf)
            implicit none
            type(FineSediment1), intent(in) :: Me                    !! self-reference
            real(dp) :: Vf                                           !! the return value
                                                                     !! function to return the fine sediment volume [m3 m-2]
                                                                     !! Output: Vf = Fine sediment volume
            Vf = Me%M_f_l / Me%rho_part()                            !! fine sediment volume computation
        end function
        !> return the fine sediment mass [kg m-2]
        pure function getFSMass1(Me) result(Mf)
            implicit none
            type(FineSediment), intent(in) :: Me                     !! self-reference
            real(dp) :: Mf                                           !! the return value
                                                                     !! function to return the fine sediment mass [kg m-2]
                                                                     !! Output: Mf = Fine sediment mass
            Mf = Me%M_f_l                                            !! fine sediment mass computation
        end function
        !> return the water volume [m3 m-2]
        pure function getWVol1(Me) result(Vw)
            implicit none
            type(FineSediment1), intent(in) :: Me                    !! self-reference
            real(dp) :: Vw                                           !! the return value
                                                                     !! function to return the water volume [m3 m-2]
                                                                     !! Output: Vw = water volume
            Vw = Me%V_w_l                                            !! water volume computation
        end function
        !> compute particle density from components and their densities
        pure function pdens1(Me) result(rho_part)
            implicit none
            type(FineSediment1), intent(in) :: Me                    !! self-reference
            real(dp) :: rho_part                                     !! return value: the particle density [kg m-3]
            integer :: x                                             !! LOCAL loop counter
            rho_part = 0                                             !! initialise output variable
            do x = 1, Me%NFComp
                rho_part = rho_part + Me%f_comp(x) * Me%pd_comp_l(x) !! summing contributions to particle density
            end do
        end function
        !> check that the array of fractional compositions sums to unity
        function audit_fcomp1(Me) result(er)
            ! QUERY: could this a pure function?
            ! QUERY: implicit none required or not?
            implicit none
            class(FineSediment1), intent(in) :: Me                   !! self-reference
            type(ErrorInstance) :: er                                !! ErrorInstance object, returns error if t_fcomp /= 1
            integer :: F                                             !! LOCAL loop counter
            real(dp) :: t_fcomp                                      !! LOCAL sum of fractional compositions
            do F = 1, Me%NFComp
                t_fcomp = t_fcomp + Me%f_comp(F)                     !! summing fractional compositions
            end do
            if (t_fcomp /= 1) then
                er = ErrorInstance( &
                    code = 106, &
                    message = "Fractional composition does &
                               not sum to unity.", &
                    trace = [trim(Me%name)] &
                )                                                    !! check t_fcomp = 1
            end if
        end function
        !> check whether this object contains any fine sediment or water of the specified size class
        function Empty1(Me) result(t)
            implicit none
            type(FineSediment1), intent(in) :: Me                    !! self-reference
            logical :: t                                             !! return value. True= V_f/M_f = V_w = 0. False= V_f/M_f > 0 .or. V_w > 0
            t = .false.
            if (Me%M_f_l == 0 .and. Me%V_w_l == 0) t = .true.
        end function
        !> clear all properties
        subroutine ClearAll1(Me)
            implicit none
            class(FineSediment1) :: Me                                !! the FineSediment instance
            integer :: X                                             !! LOCAL loop counter
            Me%M_f_l = 0                                             !! clear fine sediment mass
            Me%V_w_l = 0                                             !! clear water volume
            do X = 1, C%nSizeClassesSpm                              !! clear fractional composition
                Me%f_comp(X) = 0
            end do
        end subroutine
end module

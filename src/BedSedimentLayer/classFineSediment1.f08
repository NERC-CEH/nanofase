module classFineSediment1                                            !! definition for derived class FineSediment1
    implicit none                                                    !! force declaration of all variables
    type, public, extends(FineSediment) :: FineSediment1             !! type declaration for class
    contains
        procedure, public :: create => createFineSediment1           !! sets up by reading variables required for computations
        procedure, public :: SetV => setFSVol1                       !! set properties, using a fine sediment volume
        procedure, public :: SetM => setFSMass1                      !! set properties, using a fine sediment mass
        procedure, public :: V_f => getFSVol1                        !! returns the fine sediment volume [m3 m-2]
        procedure, public :: M_f => getFSMass1                       !! returns the fine sediment mass [kg m-2]
        procedure, public :: V_w => getWVol1                         !! returns the water volume [kg m-2]
        procedure, public :: rho_part => pdens1                      !! returns the fine sediment particle density [kg m-3]
        procedure, public :: audit_comp => audit_fcomp1              !! check the fractional composition
        procedure, public :: IsEmpty => empty1                       !! check for presence of sediment and water
     end type
    contains
        function createFineSediment1(Me, pd_comp_in(:)) result(r)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            real(dp), intent(in), allocatable :: pd_comp_in(:)       !! input array of particle densities for compositional fractions
            type(Result) :: r                                        !! Result object
            Me%NFComp = size(pd_comp_in)
            ! CRITICAL ERROR HERE: size(pd_comp) /= Me%NFComp
            pd_comp = pd_comp_in                                     !! read in particle densities of compositional fractions
        end function
        function SetFSVol1(Me, Vf_in, Vw_in, f_comp_in(:)) result(r) !! set the properties of the sediment, using fine sediment volume
            implicit none                                            !! based on the volume [m3 m-2] and particle density [kg m-3]
            class(FineSediment) :: Me                                !! the FineSediment instance
            type(Result) :: r                                        !! Result object
            real(dp), intent(in), optional :: Vf_in                  !! the fine sediment volume
            real(dp), intent(in), optional :: Vw_in                  !! the fine sediment volume
            real(dp), intent(in), optional, allocatable :: &
                                                     f_comp_in(:)    !! input fractional composition. Optional; if not present, stored composition is used
                                                                     !! function to set properties given a fine sediment volume
                                                                     !! Input: Vf_in = the value of the fine sediment volume
                                                                     !! Input: f_comp_in(:) [optional] = the composition of the fine sediment,
                                                                     !! by mass fraction of each defined component
            if present(f_comp_in) then
                if Me%NFComp = size(f_comp_in) then                  !! update the composition if supplied
                    Me%f_comp = f_comp_in                            !! store the composition locally
                    audit_fcomp(Me)                                  !! check that sum of f_comp = 1
               else
                    ! CRITICAL ERROR HERE: Me%NFComp <> size(f_comp_in)
               end if
            end if
            if present(Mf_in) then M_f_l = Vf_in * Me%pdens1         !! Computation of fine sediment mass from volume and particle density
            if present(Vw_in) then Me%V_w_l = Vw_in                  !! Volume of water
        end function
        function setFSMass1(Me, Mf_in, Vw_in, f_comp_in(:)) &
            result(r)                                                !! set properties using the fine sediment mass [kg m-2]
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            type(Result) :: r                                        !! Result object
            real(dp), intent(in), optional :: Mf_in                  !! the fine sediment mass
            real(dp), intent(in), optional :: Vw_in                  !! the fine sediment volume
            real(dp), intent(in), optional, allocatable :: &
                                                     f_comp_in(:)    !! input fractional composition. Optional; if not present, stored composition is used
                                                                     !! function to set properties given a fine sediment mass
                                                                     !! Input: Mf_in = the value of the fine sediment mass
                                                                     !! Input: f_comp_in(:) [optional] = the composition of the fine sediment,
                                                                     !! by mass fraction of each defined component
            if (present(f_comp_in)) then
                if (Me%NFComp = size(f_comp_in)) then                !! update the composition if supplied
                    Me%f_comp = f_comp_in                            !! store the composition locally
                    r%AddError(.error. Me%audit_fcomp(Me))           !! check that sum of f_comp = 1, add error to existing if tripped
                else
                    ! CRITICAL ERROR HERE: Me%NFComp <> size(f_comp_in)

                    r%AddError()

                end if
            end if
            if (present(Mf_in)) Me%M_f_l = Mf_in                     !! Storing fine sediment mass
            if (present(Vw_in))  Me%V_w_l = Vw_in                    !! Volume of water
        end function
        pure function GetMflayer(Me) result (Mf_layer)               !! return the sediment mass in the layer
        !
        !
        !
        !?
        end function
            implicit none
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            real(dp), intent(out) :: Mf_layer                        !! return value
            integer :: x                                             !! loop counter
            do x = 1, Me%nSizeClasses
                Mf_layer = Mf_layer + &
                           Me%colFineSediment(x)%item%M_f            !! sum masses from all size classes
            end do
        end function
        pure function getFSVol1(Me) result(Vf)                       !! return the fine sediment volume [m3 m-2]
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            real(dp), intent(out) :: Vf                              !! the return value
                                                                     !! function to return the fine sediment volume [m3 m-2]
                                                                     !! Output: Vf = Fine sediment volume
            Vf = Me%M_f_l / Me%pdens1                                !! fine sediment volume computation
        end function
        pure function getFSMass1(Me) result(Mf)                      !! return the fine sediment mass [kg m-2]
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            real(dp), intent(out) :: Mf                              !! the return value
                                                                     !! function to return the fine sediment mass [kg m-2]
                                                                     !! Output: Mf = Fine sediment mass
            Mf = Me%M_f_l                                            !! fine sediment mass computation
        end function
        !> return the water volume [m3 m-2]
        pure function getWVol1(Me) result(Vw)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            real(dp), intent(out) :: Vw                              !! the return value
                                                                     !! function to return the water volume [m3 m-2]
                                                                     !! Output: Vw = water volume
            Vw = Me%V_w_l                                            !! water volume computation
        end function
        !> compute particle density from components and their densities
        pure function pdens1(Me) result(rho_part)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            real(dp), intent(out) :: rho_part                        !! return value: the particle density [kg m-3]
            rho_part = 0                                             !! initialise output variable
            do c = 1, Me%NFComp
                rho_part = rho_part + f_comp(x) * pd_comp_l(x)       !! summing contributions to particle density
            end do
        end function
        !> check that the array of fractional compositions sums to unity
        function audit_fcomp1(Me) result(r)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            type(Result) :: r                                        !! Result object
            integer :: F                                             !! LOCAL loop counter
            real(dp) :: t_fcomp                                      !! LOCAL sum of fractional compositions
            do F = 1, Me%NFComp
                t_fcomp = t_fcomp + Me%f_comp(F)                     !! summing fractional compositions
            end do
            r%AddError(error_handler%NotEqual(t_fcomp, 1.0))         !! check t_fcomp /= 1, add error if so
        end function
        !> check whether this object contains any fine sediment or water of the specified size class
        function Empty1(Me) result(t)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            logical :: t                                             !! return value. True= V_f/M_f = V_w = 0. False= V_f/M_f > 0 .or. V_w > 0
            t = .false.
            if (Me%M_f_l == 0 .and. Me%V_w_l == 0) t = .true.
        end function
        !> clear all properties
        subroutine ClearAll(Me)
            implicit none
            class(FineSediment) :: Me                                !! the FineSediment instance
            integer :: X                                             !! LOCAL loop counter
            Me%M_f_l = 0                                             !! clear fine sediment mass
            Me%V_w_l = 0                                             !! clear water volume
            do X = 1, nSizeClasses                                   !!
                Me%f_comp(X) = 0
                Me%pd_comp_l(X) = 0
            end do
        end subroutine
end module

module spcFineSediment                                              !! abstract superclass definition for FineSediment
                                                                    !! defines the properties and methods shared by all BedSedimentLayer objects
                                                                    !! objects of this class cannot be instantiated, only objects of its subclasses
    use Globals
    use netcdf                                                      !! input/output handling
    use mo_netcdf                                                   !! input/output handling
    use ResultModule                                                !! error handling classes, required for
    use ErrorInstanceModule                                         !! generation of trace error messages
    implicit none                                                   !! force declaration of all variables
    type, abstract, public :: FineSediment                          !! type declaration for superclass
        character(len=256) :: name                                  !! a name for the object
        real(dp) :: M_f_l                                           !! fine sediment mass [kg m-2]
        real(dp) :: V_w_l                                           !! LOCAL volume of water associated with fine sediment [m3 m-2]
        real(dp), allocatable :: f_comp(:)                          !! fractional composition [-]
        real(dp), allocatable :: pd_comp_l(:)                       !! LOCAL storage of fractional particle densities [kg m-3]
        integer :: NFComp                                           !! LOCAL number of fractional composition terms

      contains
                                                                    ! deferred methods: must be defined in all subclasses
        procedure, public, deferred :: create => createFineSediment !! sets up by reading variables required for computations
        procedure, public, deferred :: SetV => setFSVol             !! set properties by assigning a fine sediment volume
        procedure, public, deferred :: SetM => setFSMass            !! set properties by assigning a fine sediment mass
        procedure, public, deferred :: V_f => getFSVol              !! returns the fine sediment volume [m3 m-2]
        procedure, public, deferred :: M_f => getFSMass             !! returns the fine sediment mass [kg m-2]
        procedure, public, deferred :: V_w => getWVol               !! returns the water volume [kg m-2]
        procedure, public, deferred :: rho_part => pdens            !! returns the fine sediment particle density [kg m-3]
        procedure, public, deferred :: audit_comp => audit_fcomp    !! audits the fractional composition
        procedure, public, deferred :: IsEmpty => Empty             !! check for presence of sediment and water
        procedure, public, deferred :: Clear => ClearAll            !! clear all fine sediment and water from the object
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
    end type
    abstract interface
        !> initialise this object
        function createFineSediment1(Me, n, pd_comp_in(:)) &
            result(r)
            implicit none
            class(FineSediment) :: Me                                !! self-reference
            character(len=256) :: n                                  !! a name for the object
            real(dp), intent(in), allocatable :: pd_comp_in(:)       !! input array of particle densities for compositional fractions
            type(Result) :: r                                        !! Result object
        end function
        !> set the properties, using fine sediment volume [m3 m-2]
        function setFSVol(Me, Vf_in, Vw_in, f_comp_in(:)) result(r)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            type(Result) :: r                                       !! Result object
            real(dp), optional :: Vf_in                             !! the fine sediment volume
            real(dp), optional :: Vw_in                             !! the water volume
            real(dp), optional, allocatable :: f_comp_in(:)         !! input fractional composition. Optional; if not present, stored composition is used
         end function
        !> set the properties, using fine sediment mass [kg m-2]
        function setFSMass(Me, Mf_in, Vw_in, f_comp_in(:)) result(r)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            type(Result) :: r                                       !! Result object
            real(dp), optional :: Mf_in                             !! the fine sediment mass
            real(dp), optional :: Vw_in                             !! the water volume
            real(dp), optional, allocatable :: f_comp_in(:)         !! input fractional composition. Optional; if not present, stored composition is used
        end function
        !> return the fine sediment volume [m3 m-2]
        function getFSVol(Me) result(Vf)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            real(dp), intent(out) :: Vf                             !! the return value
        end function
        !> return the fine sediment mass [kg m-2]
        function getFSMass(Me) result(Mf)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            real(dp), intent(out) :: Mf                             !! the return value
        end function
        !> return the water volume [m3 m-2]
        function getWVol(Me) result(Vw)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            real(dp), intent(out) :: Vw                             !! the return value
        end function
        !> return the particle density [kg m-3]
        function pdens(Me) result(rp)
            implicit none
            class(FineSediment) :: Me                               !! self-reference
            real(dp) :: rp                                          !! return value: the particle density [kg m-3]
        end function
        !> audit the fractional composition
        function Audit_fcomp(Me)
            implicit none
            class(FineSediment) :: Me                                !! self-reference
            integer :: F                                             !! LOCAL loop counter
            real(dp) :: t_fcomp                                      !! LOCAL sum of fractional compositions
        end function
        !> check whether this object contains any fine sediment or water of the specified size class
        function Empty(Me) result(t)
            implicit none
            class(FineSediment) :: Me                                !! self-reference
            logical :: t                                             !! return value. True= V_f/M_f = V_w = 0. False= V_f/M_f > 0 .or. V_w > 0
        end function
        !> clear all fine sediment and water from the object
        subroutine ClearAll(Me)
            implicit none
            class(FineSediment) :: Me                                !! self-reference
            integer :: X                                             !! LOCAL loop counter
        end subroutine
    end interface
end module


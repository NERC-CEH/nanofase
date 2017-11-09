!> abstract superclass definition for BedSedimentLayer
!! defines the properties and methods shared by all BedSedimentLayer objects
!! objects of this class cannot be instantiated, only objects of its subclasses
module spcBedSedimentLayer
    use Globals
    use ResultModule                                                 ! error handling classes, required for
    use ErrorInstanceModule                                          ! generation of trace error messages
    use classFineSediment1                                           ! USEs all subclasses of FineSediment
    implicit none                                                    ! force declaration of all variables
    type FineSedimentElement
        class(FineSediment1), allocatable :: item                    !! Storing polymorphic class(FineSediment) in derived type so that a set of
    end type                                                         ! different extended types of FineSediment can be stored in an array.
    type, abstract, public :: BedSedimentLayer                       !! type declaration for superclass
        character(len=256) :: name                                   !! a name for the object
                                                                     ! define variables for 'has a' objects: Biota and Reactor
                                                                     ! properties
        real(dp), allocatable :: C_f_l(:)                            !! capacity for fine sediment [m3 m-2]
        real(dp), allocatable :: C_w_l(:)                            !! capacity for water [m3 m-2]
        class(FineSedimentElement), allocatable :: &
        colFineSediment(:)                                           !! collection of FineSediment objects
        real(dp) :: C_total                                          !! total capacity [m3 m-2]
        real(dp) :: V_c                                              !! coarse material volume [m3 m-2]
        real(dp), allocatable :: pd_comp(:)                          !! particle densities of sediment components [kg m-3]
        integer :: nSizeClasses                                      !! number of sediment size classes
        integer :: nfComp                                            !! number of fractional composition terms for sediment
        integer :: allst                                             !! array allocation status
    contains
                                                                     ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: A_f => GetAf                            ! available capacity for a fine sediment size fraction
        procedure, public :: A_w => GetAw                            ! available capacity for water associated with a fine sediment size fraction
        procedure, public :: C_f => GetCf                            ! return total capacity for a fine sediment size fraction
        procedure, public :: C_w => GetCw                            ! return total capacity for water associated with a fine sediment size fraction
        procedure, public :: volSLR => GetvolSLR                     ! return volumetric solid:liquid ratio for this layer; applies to all size classes
        procedure, public :: C_f_layer => GetCflayer                 ! return total fine sediment capacity in the layer
        procedure, public :: M_f_layer => GetMflayer                 ! return total fine sediment mass in the layer
        procedure, public :: V_f_layer => GetVflayer                 ! return total fine sediment volume in the layer
        procedure, public :: V_w_layer => GetVwlayer                 ! return total water volume in the layer
        procedure, public :: C_w_layer => GetCwlayer                 ! return total water capacity in the layer
        procedure, public :: V_m_layer => GetVmlayer                 ! return total fine sediment and water volume in the layer
        procedure, public :: V_layer => GetVlayer                    ! return sum of fine sediment, water and coarse material volumes in the layer
                                                                     ! deferred methods: must be defined in all subclasses
        procedure(createBedSedimentLayer), public, deferred :: &
        create                                                       ! constructor method
        procedure(destroyBedSedimentLayer), public, deferred :: &
        destroy                                                      ! finaliser method
        procedure(AddSedimentToLayer), public, deferred :: &
        AddSediment                                                  ! add fine sediment to the layer
        procedure(RemoveSedimentFromLayer), public, deferred :: &
        RemoveSediment                                               ! remove fine sediment from layer
    end type
    abstract interface
        !> create a BedSedimentLayer object and its incorporated BedSediment objects:
        !!  - sets number of particle size classes
        !!  - reads in fixed layer volume
        !!  - reads in volumes of fine sediment and water in each size class
        !!  - sets volume of coarse material
        function createBedSedimentLayer(Me, &
                                        n, &
                                        nsc, &
                                        FSType, &
                                        C_tot, &
                                        f_comp, &
                                        pd_comp, &
                                        Porosity, &
                                        V_f, &
                                        M_f) result(r)
                                                                     
            use Globals
            import BedSedimentLayer, ErrorInstance, FineSediment1, Result
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            character(len=256) :: n                                  !! a name for the object
            integer, intent(in) :: nsc                               !! the number of particle size classes
            integer, intent(in) :: FSType                            !! the type identification number of the FineSediment(s)
            real(dp), intent(in) :: C_tot                            !! the total volume of the layer
            real(dp), intent(in), optional :: V_f(:)                 !! set of fine sediment volumes, if being used to define layer
            real(dp), intent(in), optional :: M_f(:)                 !! set of fine sediment masses, if being used to define layer
            real(dp), intent(in) :: f_comp(:,:)                      !! set of fractional compositions. Index 1 = size class, Index 2 = compositional fraction
            real(dp), intent(in), allocatable :: pd_comp(:)          !! set of fractional particle densities
            real(dp), intent(in), optional :: Porosity               !! layer porosity, if being used to define layer
            type(Result) :: r                                        !! The Result object.
            type(ErrorInstance) :: er                                ! LOCAL ErrorCriteria object for error handling.
            type(FineSediment1) :: fs1                               ! LOCAL object of type FineSediment1, for implementation of polymorphism
            real(dp) :: slr                                          ! LOCAL volumetric solid:liquid ratio
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            logical :: criterr                                       ! LOCAL .true. if one or more critical errors tripped
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! initialise a BedSedimentLayer object and its constituent
            ! FineSediment objects
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! n (character)         a name unambiguously identifying the object
            ! nsc (integer)         the number of size classes of sediment
            ! FStype (integer)      the subtype of the spcFineSediment Superclass to use
            !                       to create fine sediment objects
            ! C_tot (real, dp)      The total volume of the layer [m3 m-2]
            ! V_f(:) (real, dp)     OPTIONAL array of initial fine sediment volumes [m3 m-2]
            ! M_f(:) (real, dp)     OPTIONAL array of initial fine sediment masses [kg m-2]
            ! F_comp(:,:) (real, dp) array of fractional compositions for each size class
            ! Porosity (real, dp)   OPTIONAL sediment porosity
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! No specific outputs: results are initialisation of variables and objects
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! This function fills all available space in the layer with fine sediment,
            ! water and coarse material. There are two calling conventions:
            ! 1.    Specify V_f(:) or M_f(:) and porosity. Water volumes are computed from
            !       porosity. Any remaining capacity is filled by coarse material.
            ! 2.    Specify V_f(:) or M_f(:) only. Space not occupied by fine sediment is
            !       occupied by water.
            ! If both V_f and M_f are specified then V_f will be used.
            ! -------------------------------------------------------------------------------
        end function
        !> destroy this object
        function destroyBedSedimentLayer(Me) result (r)
            import BedSedimentLayer, Result
            class(BedSedimentLayer) :: Me
            type(Result) :: r
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Deallocate all allocated variables in this object.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! no inputs
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! F returns the amounts of sediment and water that could not be added
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
        end function
        !> add sediment and water to this layer
        function addSedimentToLayer(Me, S, F) result(r)
            use Globals
            import BedSedimentLayer, FineSediment1, Result
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(inout) :: F                  !! FineSediment - holds material to be added
            type(Result) :: r                                        !! The Result object
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Add fine sediment of a specified size fraction, and water, to this layer.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! S (integer)             the size class to which sediment is to be added
            ! F (FineSedimentElement) object representing the FineSediment to be added
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! F returns the amounts of sediment and water that could not be added
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
        end function
        !> remove sediment and water from this layer
        function RemoveSedimentFromLayer(Me, S, G) result(r)
            use Globals
            import BedSedimentLayer, FineSediment1, Result1D
            class(BedSedimentLayer) :: Me                                   !! the BedSedimentLayer instance
            integer, intent(in) :: S                                        !! the particle size class
            type(FineSediment1), intent(in) :: G                            !! fine sediment to be removed; returns fine sediment that could not be removed
            type(Result1D) :: r                                             !! The Result object. Result%data(1) = fine sediment that was removed; Result%data(2) = fine sediment that could not be removed
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Removes fine sediment and associated water from this layer.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! S (integer)             the size class from which sediment is to be removed
            ! G (FineSedimentElement) object representing the sediment to be removed,
            !                         returns sediment that could not be removed
            ! F (FineSedimentElement) object returning the sediment that was removed
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! F returns the sediment that was removed
            ! G returns the sediment could not be removed
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
        end function
    end interface
  contains
        !> return the available capacity for fine sediment of a specified size class
        pure function GetAf(Me, s) result(A_f)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            real(dp) :: A_f                                          !! return value
            A_f = Me%C_f_l(s) - Me%colFineSediment(s)%item%V_f()     !! compute capacity
            ! CRITICAL ERROR if A_f < 0
        end function
        !> return the available capacity for water associated with fine sediment of a specified size class
        pure function GetAw(Me, s) result(A_w)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            real(dp) :: A_w                                          !! return value
            A_w = Me%C_w_l(s) - Me%colFineSediment(s)%item%V_w()     !! compute capacity
            ! CRITICAL ERROR if A_w < 0
        end function
        !> return the total capacity for fine sediment of a specified size class
        pure function GetCf(Me, s) result(C_f)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            real(dp) :: C_f                                          !! return value
            C_f = Me%C_f_l(s)                                        !! compute capacity
        end function
        !> returns the total capacity for water associated with fine sediment of a specified size class
        pure function GetCw(Me, s) result(C_w)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            real(dp) :: C_w                                          !! return value
            C_w = Me%C_w_l(s)                                        !! compute capacity
        end function
        !> return the volumetric solid:liquid ratio for the layer
        pure function GetvolSLR(Me) result(volSLR)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: volSLR                                       !! return value
            volSLR = Me%C_f_l(1) / Me%C_w_l(1)                       ! compute ratio
        end function
        !> return the sediment mass in the layer across all size fractions
        pure function GetMflayer(Me) result (Mf_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Mf_layer                                     !! return value
            integer :: x                                             ! LOCAL loop counter
            do x = 1, Me%nSizeClasses
                Mf_layer = Mf_layer + &
                            Me%colFineSediment(x)%item%M_f()         ! sum across all size classes
            end do
        end function
        !> return the sediment capacity in the layer across all size fractions
        pure function GetCflayer(Me) result (Cf_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Cf_layer                                     !! return value
            integer :: s                                             ! loop counter
            do s = 1, Me%nSizeClasses
                Cf_layer = Cf_layer +  Me%C_f(s)                     ! sum across all size classes
            end do
        end function
        !> return the sediment volume in the layer across all size fractions
        pure function GetVflayer(Me) result (Vf_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Vf_layer                                     !! return value
            integer :: s                                             ! loop counter
            do s = 1, Me%nSizeClasses
                Vf_layer = Vf_layer + &
                            Me%colFineSediment(s)%item%V_f()         ! sum across all size classes
            end do
        end function
        !> return the water volume in the layer across all sediment size fractions
        pure function GetVwlayer(Me) result (Vw_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Vw_layer                                     !! return value
            integer :: s                                             ! loop counter
            do s = 1, Me%nSizeClasses
                Vw_layer = Vw_layer + &
                            Me%colFineSediment(s)%item%V_w()         ! sum across all size classes
            end do
        end function
        !> return the water capacity in the layer across all sediment size fractions
        pure function GetCwlayer(Me) result (Cw_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Cw_layer                                     !! return value
            integer :: s                                             ! loop counter
            do s = 1, Me%nSizeClasses
                Cw_layer = Cw_layer + Me%C_w_l(s)                    ! sum across all size classes
            end do
        end function
        !> return the fine sediment & water volume in the layer across all size fractions
        pure function GetVmlayer(Me) result (Vm_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: Vm_layer                                     !! return value
            integer :: s                                             ! loop counter
            do s = 1, Me%nSizeClasses
                Vm_layer = Vm_layer + &
                           Me%colFineSediment(s)%item%V_f() + &
                           Me%colFineSediment(s)%item%V_w()          ! sum across all size classes
            end do
        end function
        !> return the total volume of the layer
        pure function GetVlayer(Me) result (V_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            real(dp) :: V_layer                                      !! return value
            integer :: s                                             ! loop counter
            V_layer = Me%V_c                                         ! start by adding coarse material volume
            do s = 1, Me%nSizeClasses
                V_layer = V_layer + &
                          Me%colFineSediment(s)%item%V_f() + &
                          Me%colFineSediment(s)%item%V_w()           ! sum across all size classes
            end do
        end function
end module

!> abstract superclass definition for BedSedimentLayer
!! defines the properties and methods shared by all BedSedimentLayer objects
!! objects of this class cannot be instantiated, only objects of its subclasses
module spcBedSedimentLayer
    use Globals
    use ResultModule                                                 ! error handling classes, required for
    use ErrorInstanceModule                                          ! generation of trace error messages
    use classFineSediment1                                           ! USEs all subclasses of FineSediment
    implicit none                                                    ! force declaration of all variables
    type, abstract, public :: BedSedimentLayer                       !! type declaration for superclass
        character(len=256) :: name                                   !! a name for the object
                                                                     ! define variables for 'has a' objects: Biota and Reactor
                                                                     ! properties
        real(dp), allocatable :: C_f_l(:)                            !! capacity for fine sediment [m3 m-2]
        real(dp), allocatable :: C_w_l(:)                            !! capacity for water [m3 m-2]
        class(FineSediment1), allocatable :: &
        colFineSediment(:)                                           !! collection of FineSediment objects
        real(dp) :: C_total                                          !! total capacity [m3 m-2]
        real(dp) :: V_c                                              !! coarse material volume [m3 m-2]
        real(dp), allocatable :: pd_comp(:)                          !! particle densities of sediment components [kg m-3]
        integer :: nSizeClasses                                      !! number of sediment size classes
        integer :: nfComp                                            !! number of fractional composition terms for sediment
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
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! initialise a BedSedimentLayer object and its constituent
        !! FineSediment objects
        !!  - sets number of particle size classes
        !!  - reads in fixed layer volume
        !!  - reads in masses of fine sediment in each size class
        !!  - sets associated water volume for each size class
        !!  - sets volume of coarse material
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! layerGroup (NcGroup)   reference to the group holding this layer's data in the
        !!                        netCDF input file
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! No specific outputs: results are initialisation of variables and objects
        !!
        !! -------------------------------------------------------------------------------
        function createBedSedimentLayer1(Me, Parent, layerGroup) result(r)
            use Globals                                              !! global declarations
            use mo_netcdf                                            !! input/output handling
            use ResultModule                                         !! error handling classes, required for
            use ErrorInstanceModule                                  !! generation of trace error messages
            import BedSedimentLayer, ErrorInstance, FineSediment1, Result
            class(BedSedimentLayer1) :: Me                           !! the BedSedimentLayer instance
            character(len=*) :: Parent                               !! name of parent object
            type(NcGroup) :: layerGroup                              !! NetCDF group referring to the inputs for this layer
            type(Result) :: r                                        !! The Result object.
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! This function fills all available space in the layer with fine sediment,
            ! water and coarse material. There are two calling conventions:
            ! 1.    Specify M_f(:) and porosity. Water volumes are computed from
            !       porosity. Any remaining capacity is filled by coarse material.
            ! 2.    Specify M_f(:) only. Space not occupied by fine sediment is
            !       occupied by water.
            ! -------------------------------------------------------------------------------
        end function
        !>Function purpose
        !! -------------------------------------------------------------------------------
        !! Deallocate all allocated variables in this object.
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !! no inputs
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! all allocated variables deallocated
        !!
        !! -------------------------------------------------------------------------------
        function destroyBedSedimentLayer(Me) result (r)
            import BedSedimentLayer, Result
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            type(Result) :: r                                        !! The Result object
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
        end function
        !> Function purpose
        ! -----------------------------------------------------------------------------------
        !  add fine sediment of a specified size fraction, and associated water,
        !  to a bed sediment layer
        !
        ! Function inputs
        ! -----------------------------------------------------------------------------------
        !
        ! S (integer)       the size class to which sediment is to be added
        ! F (FineSediment1) object representing the FineSediment to be added
        !
        ! Function outputs/outcomes
        ! -----------------------------------------------------------------------------------
        ! r (FineSediment1) returns the amounts of sediment and water that could not be added
        ! -----------------------------------------------------------------------------------
        function addSedimentToLayer(Me, S, F) result(r)
            use Globals
            import BedSedimentLayer, FineSediment1, Result
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(in) :: F                     !! FineSediment - holds material to be added
            type(Result0D) :: r                                      !! The Result object. Return data type = FineSediment1
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! remove sediment of a specified size fraction, and associated water,
        !! from a bed sediment layer
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class from which sediment is to be removed
        !! G (FineSediment1) sediment to be removed
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r(1) (FineSediment1) returns the sediment that was removed
        !! r(2) (FineSediment1) returns the sediment that could not be removed
        !! -------------------------------------------------------------------------------
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

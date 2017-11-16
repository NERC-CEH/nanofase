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
        procedure, public :: clearAll => clearAllBedSedimentLayer    ! clear all sediment and water in the layer
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
        function createBedSedimentLayer(Me, Parent, layerGroup) result(r)
            import BedSedimentLayer, Result, NcGroup
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            character(len=*) :: Parent                               !! name of parent object
            type(NcGroup) :: layerGroup                              !! NetCDF group referring to the inputs for this layer
            type(Result) :: r                                        !! The Result object.
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
        end function
        !> Function purpose
        !! -----------------------------------------------------------------------------------
        !!  add fine sediment of a specified size fraction, and associated water,
        !!  to a bed sediment layer
        !!
        !! Function inputs
        !! -----------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class to which sediment is to be added
        !! F (FineSediment1) object representing the FineSediment to be added
        !!
        !! Function outputs/outcomes
        !! -----------------------------------------------------------------------------------
        !! r (FineSediment1) returns the amounts of sediment and water that could not be added
        !! -----------------------------------------------------------------------------------
        function addSedimentToLayer(Me, S, F) result(r)
            import BedSedimentLayer, FineSediment1, ResultFineSediment0D
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(in) :: F                     !! FineSediment - holds material to be added
            type(ResultFineSediment0D) :: r                          !! The Result object. Return data type = FineSediment1
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
            import BedSedimentLayer, FineSediment1, ResultFineSediment1D
            class(BedSedimentLayer1) :: Me                           !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(in) :: G                     !! fine sediment to be removed
            type(ResultFineSediment1D) :: r                          !! The Result object. Result%data(1) = fine sediment that was removed; Result%data(2) = fine sediment that could not be removed
        end function
    end interface
  contains
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the available capacity for fine sediment of a specified size class
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !! r returns critical error 103 if A_f < 0
        !!
        !! -------------------------------------------------------------------------------
        pure function GetAf(Me, s) result(r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            type(Result0D) :: r                                      !! return value
            real(dp) :: A_f                                          !  LOCAL internal storage
            A_f = Me%C_f_l(s) - Me%colFineSediment(s)%V_f()          ! compute capacity
            r = Result(data = A_f)                                   ! add to Result
            if (A_f < 0) then                                        ! CRITICAL ERROR if A_f < 0
                call r%addError(ErrorInstance(code = 103, &
                            message = "Fine sediment unoccupied " &
                                      "capacity less than zero", &
                            trace = ["spcBedSedimentLayer%GetAf"] &
                                             ) &
                               )                                     ! create if critical error thrown
            end if
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the available capacity for water associated with a specified size class
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !! r returns critical error 103 if A_w < 0
        !!
        !! -------------------------------------------------------------------------------
        pure function GetAw(Me, s) result(r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            type(Result0D) :: r                                      !! return value
            real(dp) :: A_w                                          ! LOCAL internal storage
            A_w = Me%C_w_l(s) - Me%colFineSediment(s)%V_w()          ! compute capacity
            r = Result(data = A_w)                                   ! add to Result
            if (A_w < 0) then                                        ! CRITICAL ERROR if A_w < 0
                call r%addError(ErrorInstance(code = 103, &
                            message = "Water unoccupied " &
                                      "capacity less than zero", &
                            trace = ["spcBedSedimentLayer%GetAw"] &
                                             ) &
                               )                                     ! create if critical error thrown
            end if
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the total capacity for fine sediment of a specified size class
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !! r returns critical error 103 if C_f < 0
        !!
        !! -------------------------------------------------------------------------------
        pure function GetCf(Me, s) result(C_f)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            type(Result0D) :: r                                      !! return value
            real(dp) :: C_f                                          ! LOCAL internal storage
            C_f = Me%C_f_l(s)                                        ! compute capacity
            r = Result(data = C_f)                                   ! add to Result
            if (C_f < 0) then                                        ! CRITICAL ERROR if C_f < 0
                call r%addError(ErrorInstance(code = 103, &
                            message = "Sediment total " &
                                      "capacity less than zero", &
                            trace = ["spcBedSedimentLayer%GetCf"] &
                                             ) &
                               )                                     ! create if critical error thrown
            end if
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the total capacity for water associated with fine sediment of a
        !! specified size class
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !! r returns critical error 103 if C_w < 0
        !!
        !! -------------------------------------------------------------------------------
        pure function GetCw(Me, s) result(r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve capacity
            type(Result0D) :: r                                      !! return value
            real(dp) :: C_w                                          ! LOCAL internal storage
            C_w = Me%C_w_l(s)                                        ! compute capacity
            r = Result(data = C_w)                                   ! add to Result
            if (C_w < 0) then                                        ! CRITICAL ERROR if C_f < 0
                call r%addError(ErrorInstance(code = 103, &
                            message = "Sediment total " &
                                      "capacity less than zero", &
                            trace = ["spcBedSedimentLayer%GetCf"] &
                                             ) &
                               )                                     ! create if critical error thrown
            end if
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the volumetric solid:liquid ratio for the layer
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !! r returns critical error if ratio cannot be computed
        !!
        !! -------------------------------------------------------------------------------
        pure function GetvolSLR(Me) result(r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: volSLR                                       ! LOCAL internal storage
            if (Me%C_f_l(1) > 0 .and. Me%C_w_l(1) > 0) then
                volSLR = Me%C_f_l(1) / Me%C_w_l(1)                   ! compute ratio
                r = Result(Data = volSLR)                            ! add to result object
            else
                call r%addError(ErrorInstance(code = 1, &
                            message = "Error in computing solid-" &
                                      "liquid ratio", &
                            trace = ["spcBedSedimentLayer%volSLR"] &
                                             ) &
                               )                                     ! create if critical error thrown
            end if
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the sediment mass in the layer across all size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetMflayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Mf_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            do S = 1, Me%nSizeClasses
                Mf_layer = Mf_layer + &
                            Me%colFineSediment(S)%M_f()              ! sum across all size classes
            end do
            r = Result(data = Mf_layer)
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the sediment capacity in the layer across all size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetCflayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Cf_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            do S = 1, Me%nSizeClasses
                Cf_layer = Cf_layer +  Me%C_f(S)                     ! sum across all size classes
            end do
            r = Result(data = Cf_layer)
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the sediment volume in the layer across all size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetVflayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Vf_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            do S = 1, Me%nSizeClasses
                Vf_layer = Vf_layer + &
                            Me%colFineSediment(S)%V_f()              ! sum across all size classes
            end do
            r = Result(data = Vf_layer)
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the water volume in the layer across all sediment size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetVwlayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Vw_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            do S = 1, Me%nSizeClasses
                Vw_layer = Vw_layer + &
                            Me%colFineSediment(S)%V_w()              ! sum across all size classes
            end do
            r = Result(data = Vw_layer)
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the water capacity in the layer across all sediment size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetCwlayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Cw_layer                                     ! LOCAL internal storage
            integer :: S                                             ! loop counter
            do S = 1, Me%nSizeClasses
                Cw_layer = Cw_layer + Me%C_w_l(S)                    ! sum across all size classes
            end do
            r = Result(data = Cw_layer)
        end function
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the fine sediment & water volume in the layer across all size fractions
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetVmlayer(Me) result (r)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: Vm_layer                                     ! LOCAL internal storage
            integer :: S                                             ! loop counter
            do S = 1, Me%nSizeClasses
                Vm_layer = Vm_layer + &
                           Me%colFineSediment(S)%V_f() + &
                           Me%colFineSediment(S)%V_w()                ! sum across all size classes
            end do
             r = Result(data = Vm_layer)
        end function
        !>
        !> Function purpose
        !! -------------------------------------------------------------------------------
        !! return the total volume of the layer
        !!
        !! Function inputs
        !! -------------------------------------------------------------------------------
        !!
        !! S (integer)       the size class
        !!
        !! Function outputs/outcomes
        !! -------------------------------------------------------------------------------
        !! r (Result0D) returns the result
        !!
        !! -------------------------------------------------------------------------------
        pure function GetVlayer(Me) result (V_layer)
            class(BedSedimentLayer), intent(in) :: Me                !! the BedSedimentLayer instance
            type(Result0D) :: r                                      !! return value
            real(dp) :: V_layer                                      ! LOCAL internal storage
            integer :: S                                             ! loop counter
            V_layer = Me%V_c                                         ! start by adding coarse material volume
            do S = 1, Me%nSizeClasses
                V_layer = V_layer + &
                          Me%colFineSediment(S)%V_f() + &
                          Me%colFineSediment(S)%V_w()                ! sum across all size classes
            end do
              r = Result(data = V_layer)
        end function
end module

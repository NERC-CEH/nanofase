!> Module container for `AbstractBedSedimentLayer` abstract superclass
module AbstractBedSedimentLayerModule
    use GlobalsModule
    use ResultModule, only: Result, Result0D                        ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    use FineSedimentModule                                          ! USEs all subclasses of FineSediment
    implicit none                                                   ! force declaration of all variables

    !> Abstract superclass definition for `BedSedimentLayer`
    !! defines the properties and methods shared by all `BedSedimentLayer` objects
    !! objects of this class cannot be instantiated, only objects of its subclasses
    type, abstract, public :: AbstractBedSedimentLayer
        character(len=256) :: name                                  !! A name for the object
        integer :: l                                                !! Index of this layer. 1-based, so 1 is the top-most layer.
        real(dp), allocatable :: C_f_l(:)                           !! Capacity for fine sediment [m3 m-2]
        real(dp), allocatable :: C_w_l(:)                           !! Capacity for water [m3 m-2]
        type(FineSediment), allocatable :: colFineSediment(:)       !! Collection of `FineSediment` objects
        real(dp) :: C_total                                         !! Total capacity [m3 m-2]
        real(dp) :: V_c                                             !! Coarse material volume [m3 m-2]
        real(dp), allocatable :: pd_comp(:)                         !! Particle densities of sediment components [kg m-3]
        integer :: nSizeClasses                                     !! Number of sediment size classes
        integer :: nfComp                                           !! Number of fractional composition terms for sediment
    contains
                                                                    ! non-deferred methods: defined here. Can be overwritten in subclasses
        procedure, public :: A_f => GetAf                           ! available capacity for a fine sediment size fraction
        procedure, public :: A_w => GetAw                           ! available capacity for water associated with a fine sediment size fraction
        procedure, public :: C_f => GetCf                           ! return total capacity for a fine sediment size fraction
        procedure, public :: C_w => GetCw                           ! return total capacity for water associated with a fine sediment size fraction
        procedure, public :: volSLR => GetvolSLR                    ! return volumetric solid:liquid ratio for this layer; applies to all size classes
        procedure, public :: C_f_layer => GetCflayer                ! return total fine sediment capacity in the layer
        procedure, public :: M_f_layer => GetMflayer                ! return total fine sediment mass in the layer
        procedure, public :: V_f_layer => GetVflayer                ! return total fine sediment volume in the layer
        procedure, public :: V_w_layer => GetVwlayer                ! return total water volume in the layer
        procedure, public :: C_w_layer => GetCwlayer                ! return total water capacity in the layer
        procedure, public :: V_m_layer => GetVmlayer                ! return total fine sediment and water volume in the layer
        procedure, public :: V_layer => GetVlayer                   ! return sum of fine sediment, water and coarse material volumes in the layer
        ! procedure, public :: clearAll => clearAllBedSedimentLayer   ! clear all sediment and water in the layer
                                                                    ! deferred methods: must be defined in all subclasses
        procedure(createAbstractBedSedimentLayer), public, deferred :: &
        create                                                      ! constructor method
        procedure(destroyAbstractBedSedimentLayer), public, deferred :: &
        destroy                                                     ! finaliser method
        procedure(AddSedimentToLayer), public, deferred :: &
        AddSediment                                                 ! add fine sediment to the layer, outputting via intent(inout)
        procedure(RemoveSedimentFromLayer), public, deferred :: &
        RemoveSediment                                              ! remove fine sediment from layer, outputting via intent(inout)
        procedure(clearAllFromLayer), public, deferred :: clearAll  ! set all fine sediment masses, all water volume and all fractional compositions to zero
        procedure(ReportMassesToConsole), public, deferred :: &
            repmass                                                 ! report all fine sediment masses to the console
    end type
    abstract interface
        !> **Function purpose**                                     <br>
        !! Initialise a `AbstractBedSedimentLayer` object and its constituent
        !! `FineSediment` objects
        !! <ul>
        !!  <li>sets number of particle size classes</li>
        !!  <li>reads in fixed layer volume</li>
        !!  <li>reads in masses of fine sediment in each size class</li>
        !!  <li>sets associated water volume for each size class</li>
        !!  <li>sets volume of coarse material</li>
        !! </ul>
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `layerGroup (NcGroup)` reference to the group holding this layer's data in the
        !!                        netCDF input file
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! No specific outputs: results are initialisation of variables and objects
        function createAbstractBedSedimentLayer(Me, l) result(r)
            use ResultModule, only: Result
            import AbstractBedSedimentLayer, NcGroup
            class(AbstractBedSedimentLayer) :: Me                           !! The `AbstractBedSedimentLayer` instance
            integer :: l                                            !! Index for this bed sediment layer
            type(Result) :: r                                       !! The `Result` object.
        end function
        !> **Function purpose**                                     <br>
        !! Deallocate all allocated variables in this object.
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! All allocated variables deallocated
        function destroyAbstractBedSedimentLayer(Me) result (r)
            use ResultModule, only: Result
            import AbstractBedSedimentLayer
            class(AbstractBedSedimentLayer) :: Me                            !! The `AbstractBedSedimentLayer` instance
            type(Result) :: r                                        !! The `Result` object
        end function
        !> **Function purpose**                                     
        !!  Add fine sediment of a specified size fraction, and associated water,
        !!  to a bed sediment layer
        !!                                                          
        !! **Function inputs**                                      
        !! `S (integer)`:       the size class to which sediment is to be added
        !! `F (FineSediment)`: object representing the FineSediment to be added
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! `F (FineSediment)`: Returns the amounts of sediment and water that could not be added
        function addSedimentToLayer(Me, S, F) result(r)
            use ResultModule, only: Result, Result0D
            import AbstractBedSedimentLayer, FineSediment
            class(AbstractBedSedimentLayer) :: Me                            !! The `AbstractBedSedimentLayer` instance
            integer, intent(in) :: S                                 !! The particle size class
            type(FineSediment), intent(inout) :: F                  !! `FineSediment` - holds material to be added, returns material that could not be added
            type(Result) :: r                                        !! The `Result` object. Return data type = `FineSediment`
        end function

        !> **Function purpose**                                     
        !! ALTERNATIVE REMOVESEDIMENT
        !! Remove sediment of a specified size fraction, and associated water,
        !! from a bed sediment layer
        !!                                                          
        !! **Function inputs**                                      
        !! `S (integer)`:       the size class from which sediment is to be removed
        !! `G (FineSediment)`: sediment to be removed
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! in this laternative version, both objects returned via inout
        !! `G (FineSediment)` returns the sediment that could not be removed
        !! `H (FineSediment)` returns the sediment that was removed 
        function RemoveSedimentFromLayer(Me, S, G, H) result(r)
            use ResultModule, only: Result
            use GlobalsModule, only: dp
            import AbstractBedSedimentLayer, FineSediment         
            class(AbstractBedSedimentLayer) :: Me                            !! The `AbstractBedSedimentLayer` instance
            integer, intent(in) :: S                                 !! The particle size class
            type(FineSediment), intent(inout) :: G                  !! Fine sediment to be removed, returns fine sediment that could not be removed
            type(FineSediment), intent(inout) :: H                  !! Returns fine sediment that was removed
            type(Result) :: r                                        !! The Result object = fine sediment that was removed AND fine sediment that could not be removed
        end function
        !> **Subroutine purpose**                                   <br>
        !! Remove sediment of a specified size fraction, and associated water,
        !! from a bed sediment layer
        !!                                                          <br>
        !! **Subroutine inputs**                                    <br>
        !! `S (integer)`: the size class from which sediment is to be removed <br>
        !! `G (FineSediment)`: sediment to be removed
        !!                                                          <br>
        !! **Subroutine outcomes**                                  <br>
        !! Values of sediment mass, water volume and fractional composition set to zero
        !! for all sediments in this layer
        subroutine clearAllFromLayer(Me)
            import AbstractBedSedimentLayer
            class(AbstractBedSedimentLayer) :: Me                            !! This `AbstractBedSedimentLayer` object
        end subroutine
        !> **Function purpose**                                   
        !! 1. Report the mass of fine sediment in each size fraction to the console
        !! 2. report the total mass of fine sediment in the layer to the console
        !!                                                          
        !! **Function inputs**                                      
        !! none
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! 
        subroutine ReportMassesToConsole(Me)
            import AbstractBedSedimentLayer
            class(AbstractBedSedimentLayer) :: Me                            !! The `AbstractBedSedimentLayer` instance
            integer :: n                                             !! LOCAL loop counter 
        end subroutine
    end interface
  contains
        !> **Function purpose**                                     <br>
        !! Return the available capacity for fine sediment of a specified size class
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: The size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result                        <br>
        !! `r` returns critical error 103 if `A_f < 0`
        function GetAf(Me, s) result(A_f)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! the AbstractBedSedimentLayer instance
            integer, intent(in) :: s                                 !! size class for which to retrieve available capacity
            real(dp) :: A_f                                          !  LOCAL internal storage
            A_f = Me%C_f_l(s) - Me%colFineSediment(s)%V_f()          ! compute capacity
        end function
        !> **Function purpose**                                     <br>
        !! return the available capacity for water associated with a specified size class
        !!
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)`: returns the result                       <br>
        !! `r` returns critical error 103 if `A_w < 0`
        function GetAw(Me, s) result(A_w)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            integer, intent(in) :: s                                 !! Size class for which to retrieve available capacity
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: A_w                                          ! LOCAL internal storage
            A_w = Me%C_w_l(s) - Me%colFineSediment(s)%V_w()          ! compute capacity
        end function
        !> **Function purpose**                                     <br>
        !! Return the total capacity for fine sediment of a specified size class
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)`: returns the result                       <br>
        !! `r` returns critical error 103 if `C_f < 0`
        function GetCf(Me, s) result(C_f)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            integer, intent(in) :: s                                 !! Size class for which to retrieve available capacity
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: C_f                                          ! LOCAL internal storage
            C_f = Me%C_f_l(s)                                        ! compute capacity
        end function
        !> **Function purpose**                                     <br>
        !! Return the total capacity for water associated with fine sediment of a
        !! specified size class
        !!
        !! Function inputs                                          <br>
        !! `S (integer)`: the size class
        !!
        !! Function outputs/outcomes                                <br>
        !! `r (Result0D)` returns the result                        <br>
        !! `r` returns critical error 103 if `C_w < 0`
        function GetCw(Me, s) result(C_w)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            integer, intent(in) :: s                                 !! Size class for which to retrieve capacity
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: C_w                                          ! LOCAL internal storage
            C_w = Me%C_w_l(s)                                        ! compute capacity
        end function
        !> **Function purpose**                                     <br>
        !! Return the volumetric solid:liquid ratio for the layer
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result                        <br>
        !! `r` returns critical error if ratio cannot be computed
        function GetvolSLR(Me, S) result(volSLR)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            integer :: S                                             !! size class for which volumetric SLR is to be computed
            real(dp) :: volSLR                                       ! LOCAL internal storage
            volSLR = Me%C_f_l(S) / Me%C_w_l(S)                   ! compute ratio
        end function

        !> Return the sediment mass in the layer across all size fractions
        function GetMflayer(me) result(Mf_layer)
            class(AbstractBedSedimentLayer), intent(in) :: me                !! The `AbstractBedSedimentLayer` instance
            real(dp) :: Mf_layer                                     !! Sediment mass in this layer
            integer :: i                                             ! Iterator over SPM size classes
            Mf_layer = 0
            do i = 1, me%nSizeClasses
                Mf_layer = Mf_layer + Me%colFineSediment(i)%M_f_l
            end do
        end function

        !> **Function purpose**                                     <br>
        !! Return the sediment capacity in the layer across all size fractions
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result
        function GetCflayer(Me) result(Cf_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: Cf_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            Cf_layer = 0
            do S = 1, Me%nSizeClasses
                Cf_layer = Cf_layer + Me%C_f(S)                 ! sum across all size classes
            end do
            ! r = Result(data = Cf_layer)
        end function
        !> **Function purpose**                                     <br>
        !! Return the sediment volume in the layer across all size fractions
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result
        function GetVflayer(Me) result(Vf_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: Vf_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            Vf_layer = 0                                             ! initialise local variable 
            do S = 1, Me%nSizeClasses
                Vf_layer = Vf_layer + Me%colFineSediment(S)%V_f()              ! sum across all size classes
            end do
            ! r = Result(data = Vf_layer)
        end function
        !> **Function purpose**                                     <br>
        !! Return the water volume in the layer across all sediment size fractions
        !!                                                          <br>
        !! **Function inputs**
        !! `S (integer)`: the size class     
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result
        function GetVwlayer(Me) result(Vw_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: Vw_layer                                     ! LOCAL internal storage
            integer :: S                                             ! LOCAL loop counter
            Vw_layer = 0
            do S = 1, Me%nSizeClasses
                Vw_layer = Vw_layer + Me%colFineSediment(S)%V_w()   ! sum across all size classes
            end do
            ! r = Result(data = Vw_layer)
        end function
        !> **Function purpose**                                     <br>
        !! Return the water capacity in the layer across all sediment size fractions
        !!                                                          <br>
        !! **Function inputs**
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result
        function GetCwlayer(Me) result(Cw_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: Cw_layer                                     ! LOCAL internal storage
            integer :: S                                             ! loop counter
            Cw_layer = 0
            do S = 1, Me%nSizeClasses
                Cw_layer = Cw_layer + Me%C_w_l(S)                    ! sum across all size classes
            end do
            ! r = Result(data = Cw_layer)
        end function
        !> **Function purpose**                                     <br>
        !! Return the fine sediment & water volume in the layer across all size fractions
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! `r (Result0D)` returns the result
        function GetVmlayer(Me) result(Vm_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            ! type(Result0D) :: r                                      !! Return value
            real(dp) :: Vm_layer                                     ! LOCAL internal storage
            integer :: S                                             ! loop counter
            Vm_layer = 0                                             ! initialise local variable
            do S = 1, Me%nSizeClasses
                Vm_layer = Vm_layer + &
                           Me%colFineSediment(S)%V_f() + &
                           Me%colFineSediment(S)%V_w()                ! sum across all size classes
            end do
            !  r = Result(data = Vm_layer)
        end function
        !> **Function purpose**                                     <br>
        !! Return the total volume of the layer
        !!                                                          <br>
        !! **Function inputs**                                      <br>
        !! `S (integer)`: the size class
        !!                                                          <br>
        !! Function outputs/outcomes                                <br>
        !! `r (Result0D)` returns the result
        function GetVlayer(Me) result (V_layer)
            class(AbstractBedSedimentLayer), intent(in) :: Me                !! The `AbstractBedSedimentLayer` instance
            type(Result0D) :: r                                      !! Return value
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

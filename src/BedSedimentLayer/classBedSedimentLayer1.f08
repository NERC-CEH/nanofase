!> class definition for BedSedimentLayer1
module classBedSedimentLayer1                                        
    use spcBedSedimentLayer                                          ! use BedSedimentLayer superclass
    use Globals
    use UtilModule
    use ResultModule
    use ErrorInstanceModule
    implicit none                                                    ! force declaration of all variables
    type, public, extends(BedSedimentLayer) :: &
        BedSedimentLayer1                                            ! type declaration for class - extends abstract superclass
        ! private real(dp), allocatable :: C_f_l(:)                    ! LOCAL capacity for fine sediment [m3 m-2]
        ! private real(dp), allocatable :: C_w_l(:)                    ! LOCAL capacity for water [m3 m-2]
        contains                                                     ! methods deferred from superclass
            procedure, public :: &
            create => createBedSedimentLayer1                        ! constructor method
            procedure, public :: &
            destroy => destroyBedSedimentLayer1                      ! finaliser method
            procedure, public :: &
            addSediment => addSediment1                              ! add fine sediment to the layer
            procedure, public :: &
            removeSediment => removeSediment1                        ! remove fine sediment from layer
    end type
    contains
        !> initialise a BedSedimentLayer object:
        !!  - sets number of particle size classes
        !!  - reads in fixed layer volume
        !!  - reads in volumes of fine sediment and water in each size class
        !!  - sets volume of coarse material
        function createBedSedimentLayer1(Me, &
                                         n, &
                                         nsc, &
                                         FSType, &
                                         C_tot, &
                                         f_comp, &
                                         pd_comp, &
                                         Porosity, &
                                         V_f, &
                                         M_f) result(r)
                                                                     
            class(BedSedimentLayer1) :: Me                           !! the BedSedimentLayer instance
            type(Result) :: r                                        !! The Result object.
            character(len=256) :: n                                  !! a name for the object
            integer, intent(in) :: nsc                               !! the number of particle size classes
            integer, intent(in) :: FSType                            !! the type identification number of the FineSediment(s)
            real(dp), intent(in) :: C_tot                            !! the total volume of the layer
            real(dp), intent(in) :: f_comp(:,:)                      !! set of fractional compositions. Index 1 = size class, Index 2 = compositional fraction
            real(dp), intent(in), allocatable :: pd_comp(:)          !! set of fractional particle densities
            real(dp), intent(in), optional :: Porosity               !! layer porosity, if being used to define layer
            real(dp), intent(in), optional :: V_f(:)                 !! set of fine sediment volumes, if being used to define layer
            real(dp), intent(in), optional :: M_f(:)                 !! set of fine sediment masses, if being used to define layer
            type(ErrorInstance) :: er                                ! LOCAL ErrorCriteria object for error handling.
            type(FineSediment1), allocatable :: fs1                  ! LOCAL object of type FineSediment1, for implementation of polymorphism
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            real(dp) :: fwr                                          ! LOCAL fine sediment to water ratio
            integer :: S                                             ! LOCAL loop counter
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
            tr = "create"                                            ! procedure binding name as trace
            if (len_trim(n) == 0) then
                call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "An object name has not &
                                           been provided", &
                            trace = ["classBedSedimentLayer1%create"] &
                            ))                                       ! error if name is not provided
                return                                               ! critical error, so exit here
            end if
            Me%name = n
            if (nsc <= 0) then                                       ! CRITICAL ERROR HERE: nsc <= 0
                call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "Invalid number of particle &
                                      size classes" &
                            ))
            end if
            if (C_tot == 0) then                                     ! CRITICAL ERROR HERE: C_tot = 0
                call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "Layer volume is zero" &
                               ))
            end if
            if ((.not. present(V_f)) .and. (.not. present(M_f))) then
                call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "One of fine sediment volume &
                                           and mass must be specified" &
                               ))                                    ! create error instance
            end if
            if (present(V_f)) then
                if (size(V_f) /= nsc) then                           ! array of fine sediment masses must have correct size
                    call r%addError(ErrorInstance( &
                                code = 1, &
                                message = "Array of fine sediment &
                                               volumes is the wrong &
                                               size" &
                                   ))                                 ! create error instance
                end if
            end if
            if (present(M_f)) then
                if (size(M_f) /= nsc) then                           ! array of fine sediment masses must have correct size
                    call r%AddError(ErrorInstance( &
                                code = 1, &
                                message = "Array of fine sediment &
                                               masses is the wrong &
                                               size" &
                                                 ) &
                                   )                                 ! create error instance
                end if
            end if
            if (size(f_comp, 1) /= nsc) then                         ! number of size classes must be consistent
               call r%AddError(ErrorInstance( &
                          code = 1, &
                          message = "Array of fractional &
                                          compositions is the wrong &
                                          size" &
                                            ) &
                              )                                      ! create error instance
            end if
            if (size(f_comp, 2) /= size(pd_comp)) then               ! number of compositional fractions must be consistent
                call r%AddError(ErrorInstance( &
                                code = 1, &
                                message = "Arrays of fractional &
                                           compositions and particle &
                                           densities have different &
                                           sizes" &
                                             ) &
                               )                                     ! create error instance
            end if
            if (present(Porosity)) then
                if (Porosity <= 0 .or. Porosity >= 1) then
                call r%AddError(ErrorInstance( &
                                code = 1, &
                                message = "Porosity is out of range" &
                                             ) &
                               )                                     ! create error instance
                end if
            end if
            tr = Me%name // "%" // tr                                ! add name to trace string
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add trace to Result
                return                                               ! exit, as a critical error has occurred
            end if
            Me%nSizeClasses = nsc                                    ! store number of size classes
            Me%nfComp = size(f_comp, 2)                              ! store number of fractional composition terms
            Me%C_total = C_tot                                       ! assign the total volume
            tr = Me%name // &
                "%createBedSedimentLayer1%colFineSediment"           ! trace message
            allocate(Me%colFineSediment(1:nsc), stat=Me%allst)       ! set up fine sediment collection
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%createBedSedimentLayer1%C_f_l"                     ! trace message
            allocate(Me%C_f_l(1:nsc), stat=Me%allst)                 ! allocate space for fine sediment capacity
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%createBedSedimentLayer1%C_w_l"                     ! trace message
            allocate(Me%C_w_l(1:nsc), stat=Me%allst)                 ! allocate space for water capacity
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%createBedSedimentLayer1%pd_comp"                   ! trace message
            allocate(Me%pd_comp(1:size(pd_comp)), stat=Me%allst)     ! allocate space for particle densities of components
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   "Allocation error", &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            if (r%hasCriticalError()) return                            ! exit if allocation has thrown an error
            do S = 1, nsc
                associate (O => Me%colFineSediment(S)%item)          ! association for the FineSediment object we are working with
                                                                     ! this is done to reduce code length and increase readability
                    select case (FSType)                             ! loop through possible FineSediment object types
                        case(1)                                      ! Type FineSediment1
            tr = Me%name // &
                "%createBedSedimentLayer1%pd_comp"                   ! trace message
                            allocate (fs1, stat=Me%allst)            ! create FineSediment1 object
                            if (Me%allst /= 0) then
                                er = ErrorInstance(1, &
                                                "Allocation error", &
                                                .false., &
                                                [tr] &
                                                  )                  ! create warning if error thrown
                                call r%addError(er)                  ! add to Result
                                if (r%hasCriticalError()) return     ! exit if allocation has thrown an error
                            end if
                            call r%addErrors( &
                                .errors. fs1%create( &               ! run constructor for this object
                                    Me%name, &
                                    Me%pd_comp &
                                ) &    
                            )
                            call move_alloc(fs1, &
                              Me%colFineSediment(S)%item)            ! move the object into the colFineSediment collection; this deallocates fs1
                        case default                                 ! not a recognised FineSediment type
                            call r%addError(ErrorInstance( &
                                        code = 1, &
                                        message = "Invalid &
                                                   FineSediment &
                                                   object type &
                                                   specified" &
                                                         ) &
                                           )                         ! add ErrorInstance
                            call r%addToTrace(tr)                    ! add trace to Result
                            return                                   ! critical error, so exit
                    end select
                    if (present(V_f)) then
                        call r%addErrors(.errors. &
                                    O%set(Vf_in = V_f(S), &
                                         f_comp_in = f_comp(S,:) &
                                         ) &
                                        )                            ! if V_f values are defined, set up FineSediment using V_f
                        if (r%hasCriticalError()) then               ! if a critical error has been thrown
                            call r%addToTrace(tr)                    ! add trace to Result
                            return                                   ! exit, as a critical error has occurred
                        end if
                    elseif (present(M_f)) then
                        ! QUERY: can we return from Set_M into an ErrorCriteria instance and then test for criticality
                        !        in order to determine whether to exit immediately?
                        call r%addErrors(.errors. &
                                    O%set(Mf_in = M_f(S), &
                                         f_comp_in = f_comp(S,:) &
                                         ) &
                                        )                            ! otherwise if M_f values are defined, set up FineSediment using M_f
                        if (r%hasCriticalError()) then               ! if a critical error has been thrown
                            call r%addToTrace(tr)                    ! add trace to Result
                            return                                   ! exit, as a critical error has occurred
                        end if
                    end if
                end associate
                Me%C_f_l(S) = V_f(S)                                 ! set the sediment capacities, using the local value
            end do
            if (Me%V_f_layer() > C_tot) then                         ! CRITICAL ERROR HERE: if Me%V_f_layer > C_tot
                call r%addError(ErrorInstance( &
                                  code = 1, &
                                  message = "Fine sediment volume &
                                             exceeds capacity" &
                                             ) &
                               )                                     ! add ErrorInstance
                return                                               ! critical error, so exit
            end if
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add trace to Result
                return                                               ! exit, as a critical error has occurred
            end if
            if (present(Porosity)) then                              ! has a porosity value been supplied?
                fwr = Porosity / (1 - Porosity)                      ! yes, use porosity to compute factor for sediment:water ratio
            else                                                     !
                fwr = (Me%C_total - Me%C_f_layer()) / Me%C_f_layer() ! no, so use C_total and C_f_layer to compute factor for
            end if                                                   ! sediment:water ratio
            do S = 1, nsc                                            ! compute V_w for each size fraction using the sediment:water ratio
                associate (O => Me%colFineSediment(S)%item)          ! association for the FineSediment object we are working with
                    call r%addErrors(.errors. &
                        O%set(Vw_in = O%V_f() * fwr))                ! is used to compute the volume of associated water
                        if (r%hasCriticalError()) then               ! if a critical error has been thrown
                            call r%addToTrace(tr)                    ! add trace to Result
                            return                                   ! exit, as a critical error has occurred
                        end if
                end associate
            end do
            do S = 1, nsc                                            ! loop through all size fractions
                Me%C_w_l(S) = Me%colFineSediment(S)%item%V_w()       ! set the water capacities, using the local variable
            end do
            if (Me%V_m_layer() > C_tot) then                         ! CRITICAL ERROR HERE: if Me%V_m_layer > C_tot
                call r%addError(ErrorInstance( &
                                    code = 1, &
                                     message = "Fine sediment & &
                                                water volume &
                                                exceeds capacity" &
                                             ) &
                               )                                     ! add ErrorInstance
                call r%addToTrace(tr)                                ! add trace to Result
                return                                               ! critical error, so exit
            end if
            Me%V_c = C_tot - Me%V_m_layer()                          ! set the coarse material volume
        end function
        !> destroy this object
        function destroyBedSedimentLayer1(Me) result(r)
            class(BedSedimentLayer1) :: Me                           !! the BedSedimentLayer instance
            type(Result) :: r                                        !! The Result object
            type(ErrorInstance) :: er                                ! LOCAL ErrorCriteria object for error handling.
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            character(len=18), parameter :: &
                                          ms = "Deallocation error"  ! LOCAL CONSTANT error message
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
            tr = Me%name // &
                "%destroyBedSedimentLayer1%colFineSediment"          ! trace message
            deallocate(Me%colFineSediment, stat = Me%allst)          ! deallocate all allocatable variables
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%destroyBedSedimentLayer1%pd_comp"                  ! trace message
            deallocate(Me%pd_comp, stat = Me%allst)
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%destroyBedSedimentLayer1%C_f_l"                    ! trace message
            deallocate(Me%C_f_l, stat = Me%allst)
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
            tr = Me%name // &
                "%destroyBedSedimentLayer1%C_w_l"                    ! trace message
            deallocate(Me%C_w_l, stat = Me%allst)
            if (Me%allst /= 0) then
                er = ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                  )                                  ! create warning if error thrown
                call r%addError(er)                                  ! add to Result
            end if
        end function
        !> add sediment and water to this layer
        function addSediment1(Me, S, F) result(r)
            implicit none
            class(BedSedimentLayer1) :: Me                           !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(inout) :: F                  !! FineSediment - holds material to be added
            type(Result) :: r                                        !! The Result object
            real(dp) :: add_M_f                                      ! LOCAL mass of fine sediment being added
            real(dp) :: add_V_f                                      ! LOCAL volume of fine sediment to be added
            real(dp) :: add_V_w                                      ! LOCAL volume of water to be added
            real(dp) :: M_f_SC                                       ! LOCAL mass of fine sediment in receiving size class
            real(dp) :: V_f_SC                                       ! LOCAL volume of fine sediment in receiving size class
            real(dp) :: A_f_SC                                       ! LOCAL capacity for fine sediment in receiving size class
            real(dp) :: V_w_SC                                       ! LOCAL volume of water in receiving size class
            real(dp) :: A_w_SC                                       ! LOCAL capacity for water in receiving size class
            real(dp) :: V_f_added                                    ! LOCAL volume of water added
            real(dp) :: Mf                                           ! LOCAL temporary variable
            real(dp), allocatable :: t_comp(:)                       ! LOCAL temporary variable
            integer :: x                                             ! LOCAL loop counter
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            !
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Add fine sediment of a specified size fraction, and water, to this layer.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! S (integer)       the size class to which sediment is to be added
            ! F (FineSediment1) object representing the FineSediment to be added
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! F returns the amounts of sediment and water that could not be added
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            tr = Me%name // "%addSediment1"                          ! trace for this procedure
            if (S <= 0 .or. S > Me%nSizeClasses) then                ! CRITICAL ERROR HERE: if S <= 0 or S > nSizeClasses
                call r%addError(ErrorInstance(code = 1, &
                                message = "The size class is out of &
                                           range" &
                               ))
            end if
            if (size(F%f_comp) /= Me%nFComp) then                    ! CRITICAL ERROR HERE: if S <= 0 or S > nSizeClasses
                call r%addError(ErrorInstance(code = 1, &
                                message = "The number of &
                                           compositional fractions &
                                           in input is incorrect" &
                               ))
            end if
            add_V_f = F%V_f()                                        ! static local copy of added fine sediment volume
            if (add_V_f <= 0) then                                   ! CRITICAL ERROR HERE: if add_V_f < 0
                call r%addError(ErrorInstance(code = 1, &
                                message = "The added fine sediment &
                                           volume in size class " &
                                           // trim(str(S)) // &
                                          " is less than zero" &
                               ))
            end if
            add_V_w = F%V_w()                                        ! static local copy of added water volume
            if (add_V_w <= 0) then                                   ! CRITICAL ERROR HERE: if add_V_w < 0
                call r%addError(ErrorInstance(code = 1, &
                                message = "The added water &
                                           volume in size class " &
                                           // trim(str(S)) // &
                                          " is less than zero" &
                               ))
            end if
            if (r%hasCriticalError()) then                           ! if AddSediment throws a critical error
                call r%addToTrace(tr)                                ! add trace to all errors
                return                                               ! and exit
            end if
            tr = Me%name // &
                "%createBedSedimentLayer1%t_comp"                    ! trace message
            allocate(t_comp(1:Me%nfComp), stat = Me%allst)           ! for storage of modified fractional composition of modified sediment
            if (Me%allst /= 0) then
                call r%addError(ErrorInstance(1, &
                                   "Allocation error", &
                                   .false., &
                                   [tr] &
                                  ))                                 ! create warning if error thrown
                return                                               ! critical error, so return
            end if
            A_f_SC = Me%A_f(S)                                       ! static local copy of fine sediment capacity
            A_w_SC = Me%A_w(S)                                       ! static local copy of water capacity
            associate(O => Me%colFineSediment(S)%item)
                M_f_SC = O%M_f()                                     ! fine sediment mass in layer
                V_f_SC = O%V_f()                                     ! fine sediment volume in layer
                V_w_SC = O%V_w()                                     ! water volume in layer
                if (add_V_f > A_f_SC) then                           ! added volume exceeds the available capacity; cannot all be added
                    V_f_SC = Me%C_f_l(S)                             ! set fine sediment volume to capacity
                    add_V_f = add_V_f - A_f_SC                       ! volume that could not be added
                    V_f_added = V_f_SC - A_f_SC                      ! volume added
                else                                                 ! added volume does not exceed the fine sediment capacity; can all be added
                    V_f_SC = V_f_SC + add_V_f                        ! addition of fine sediment volume
                    add_V_f = 0                                      ! return zero volume not added
                    V_f_added = add_V_f                              ! volume added
                end if
                if (add_V_w > A_w_SC) then                           ! added volume exceeds the available capacity; cannot all be added
                    V_w_SC = Me%C_w_l(S)                             ! set water volume to capacity
                    add_V_w = add_V_w - A_w_SC                       ! volume that could not be added
                else                                                 ! added volume does not exceed the fine sediment capacity; can all be added
                    V_w_SC = V_w_SC + add_V_w                        ! addition of water volume
                    add_V_w = 0                                      ! return zero volume not added
                end if
                Mf = V_f_added * F%rho_part()                        ! read in added mass - prevents multiple calls to object
                do x = 1, Me%nfComp                                  ! in this subsequent loop
                    t_comp(x) = M_f_SC * O%f_comp(x)
                    t_comp(x) = t_comp(x) + Mf * F%f_comp(x)
                    t_comp(x) = t_comp(x) / (M_f_SC + Mf)            ! modified fraction of component no. x
                end do
                call r%addErrors(.errors. O%set(Vf_in = V_f_SC, &
                                               Vw_in = V_w_SC, &
                                           f_comp_in = t_comp &
                                              ) &
                               )                                     ! copy modified properties to fine sediment, add any error to Result object
                if (r%hasCriticalError()) then                       ! if a critical error has been thrown
                    call r%addToTrace(tr)                            ! add trace to all errors
                    return                                           ! and exit
                end if
            end associate
            call r%addErrors(.errors. F%set(Vf_in = add_V_f, &
                                           Vw_in = add_V_w &
                                          ) &
                           )                                         ! return volumes of fine sediment and water not added, add any error to Result object
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add trace to all errors
                return                                               ! and exit
            end if
        end function
        !> remove sediment and water from this layer
        function removeSediment1(Me, S, G) result(r)
            implicit none
            class(BedSedimentLayer1) :: Me                           ! the BedSedimentLayer instance
            integer, intent(in) :: S                                 ! the particle size class
            type(FineSediment1), intent(in) :: G                     ! fine sediment to be removed
            type(Result1D) :: r                                      ! The Result object. Result%data(1) = fine sediment that was removed; Result%data(2) = fine sediment that could not be removed
            type(FineSediment1) :: F                                 ! LOCAL returns fine sediment that was removed
            real(dp) :: V_f_SC                                       ! LOCAL fine sediment volume in layer
            real(dp) :: V_f_SC_r                                     ! LOCAL fine sediment volume removed
            real(dp) :: V_w_SC                                       ! LOCAL water volume in layer
            real(dp) :: V_w_SC_r                                     ! LOCAL water volume removed
            character(len=256) :: tr                                 ! LOCAL error trace
            ! Function purpose
            ! -------------------------------------------------------------------------------
            ! Removes fine sediment and associated water from this layer.
            !
            ! Function inputs
            ! -------------------------------------------------------------------------------
            ! Function takes as inputs:
            ! S (integer)       the size class from which sediment is to be removed
            ! G (FineSediment1) sediment to be removed
            !
            ! Function outputs/outcomes
            ! -------------------------------------------------------------------------------
            ! r(1) returns the sediment that was removed
            ! r(2) returns the sediment that could not be removed
            !
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            tr = Me%name // "%removeSediment1"                       ! trace for this procedure
            if (S <= 0 .or. S > Me%nSizeClasses) then                ! CRITICAL ERROR HERE: if S <= 0 or S > nSizeClasses
                call r%addError(ErrorInstance( &
                  code = 1, &
                  message = "The size class is out of &
                            range" &
                  ))
            end if
            V_f_SC_r = G%V_f()                                       ! static local copy of fine sediment volume to be removed
            if (V_f_SC_r <= 0) then                                  ! CRITICAL ERROR HERE: if V_f_SC_r < 0
                call r%addError(ErrorInstance( &
                  code = 1, &
                  message = "The removed fine sediment &
                              volume in size class " &
                              // trim(str(S)) // &
                             " is less than zero" &
                ))
            end if
            V_w_SC_r = G%V_w()                                       ! static local copy of water volume to be removed
            if (V_w_SC_r <= 0) then                                  ! CRITICAL ERROR HERE: if V_w_SC_r < 0
                call r%addError(ErrorInstance( &
                  code = 1, &
                  message = "The removed water volume &
                              in size class " &
                              // trim(str(S)) // &
                             " is less than zero" &
                ))
            end if
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add a trace message to any errors
                return                                               ! exit here
            end if
            associate (O => Me%colFineSediment(S)%item)
                V_f_SC = O%V_f()                                     ! static local copy of fine sediment volume
                V_w_SC = O%V_w()                                     ! static local copy of water volume
                if (V_f_SC_r > V_f_SC) V_f_SC_r = V_f_SC             ! set actual volume of fine sediment to be removed
                if (V_w_SC_r > V_w_SC) V_w_SC_r = V_w_SC             ! set actual volume of water to be removed
                call r%addErrors(.errors. O%set( &
                                    Vf_in = V_f_SC - V_f_SC_r, &
                                    Vw_in = V_w_SC - V_w_SC_r &
                                         ) &
                          )                                          ! update fine sediment in layer
                                                                     ! fractional composition unchanged
                if (r%hasCriticalError()) then                       ! if a critical error has been thrown
                    call r%addToTrace(tr)                            ! add a trace message to any errors
                    return                                           ! exit here
                end if
                tr = Me%name //  "%removeSediment1%r"                ! trace message
                call r%addErrors(.errors. F%set( &
                                    Vf_in = V_f_SC_r, &
                                    Vw_in = V_w_SC_r, &
                                    f_comp_in = O%f_comp &
                                              ) &
                          )                                          ! set properties of the sediment being removed, including fractional composition
                if (r%hasCriticalError()) then                       ! if a critical error has been thrown
                    call r%addToTrace(tr)                            ! add a trace message to any errors
                    return                                           ! exit here
                end if
            end associate
            call r%addErrors(.errors. G%set( &
                                Vf_in = G%V_f() - V_f_SC_r, &
                                Vw_in = G%V_w() - V_w_SC_r &
                                             ) &
                      )                                              ! return the volume that could not be removed
            if (r%hasCriticalError()) then                           ! if a critical error has been thrown
                call r%addToTrace(tr)                                ! add a trace message to any errors
                return                                               ! exit here
            end if
            r = Result(data = [F,G])                                 ! Result%data(1) = fine sediment that was removed; Result%data(2) = fine sediment that could not be removed
        end function
end module

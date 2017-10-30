module classBedSedimentLayer1                                        !! class definition for BedSedimentLayer1
    use spcBedSedimentLayer                                          !! use BedSedimentLayer superclass
    use Globals
    use UtilModule
    use ResultModule
    use ErrorInstanceModule
    implicit none                                                    !! force declaration of all variables
    type, public, extends(BedSedimentLayer) :: &
        BedSedimentLayer1                                            !! type declaration for class - extends abstract superclass
        contains                                                     !! methods deferred from superclass
            procedure, public :: &
            create => createBedSedimentLayer1                        !! constructor method
            procedure, public :: &
            destroy => destroyBedSedimentLayer1                      !! finaliser method
            procedure, public :: &
            AddSediment => AddSedimentToLayer1                       !! add fine sediment to the layer
            procedure, public :: &
            AddWater => AddWaterToLayer1                             !! add water to layer
            procedure, public :: &
            RemoveSediment => RemoveSedimentFromLayer1               !! remove fine sediment from layer
            procedure, public :: &
            RemoveWater => RemoveWaterFromLayer1                     !! remove water from layer
    end type
    contains
        !> initialise a BedSedimentLayer object
        function createBedSedimentLayer1(Me, &
                                         n, &
                                         nsc, &
                                         FSType, &
                                         C_tot, &
                                         V_f(:), &
                                         M_f(:), &
                                         f_comp(::), &
                                         pd_comp(:), &
                                         Porosity) result(r)
                                                                     !! sets number of particle size classes
                                                                     !! reads in fixed layer volume
                                                                     !! reads in volumes of fine sediment and water in each size class
                                                                     !! sets volume of coarse material
            implicit none
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            character(len=256) :: n                                  !! a name for the object
            integer, intent(in) :: nsc                               !! the number of particle size classes
            integer, intent(in) :: FSType                            !! the type identification number of the FineSediment(s)
            real(dp), intent(in) :: C_tot                            !! the total volume of the layer
            real(dp), intent(in), optional, allocatable :: V_f(:)    !! set of fine sediment volumes, if being used to define layer
            real(dp), intent(in), optional, allocatable :: M_f(:)    !! set of fine sediment masses, if being used to define layer
            real(dp), intent(in), allocatable :: f_comp(::)          !! set of fractional compositions. Index 1 = size class, Index 2 = compositional fraction
            real(dp), intent(in), allocatable :: pdcomp(:)           !! set of fractional particle densities
            real(dp), intent(in), optional :: Porosity               !! layer porosity, if being used to define layer
            type(Result), intent(out) :: r                           !! The Result object.
            type(ErrorCriteria) :: er                                !! LOCAL ErrorCriteria object for error handling.
            type(FineSediment1) :: fs1                               !! LOCAL object of type FineSediment1, for implementation of polymorphism
            character(len=*) :: tr                                   !! LOCAL name of this procedure, for trace
            logical :: criterr                                       !! LOCAL .true. if one or more critical errors tripped
            real(dp) :: fwr                                          !! LOCAL fine sediment to water ratio
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
            ! F_comp(::) (real, dp) array of fractional compositions for each size class
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
            tr = "create"                                            !! procedure binding name as trace
            criterr = .false.                                        !! initial setting
            if len_trim(n) = 0 then
                r%addError(message = "An object name has not been &
                                      provided" &
                             trace = "classBedSedimentLayer1%create") !! error if name is not provided
                return                                               !! critical error, so exit here
            end if
            Me%name = n


            if (nsc <= 0) then                                       !! CRITICAL ERROR HERE: nsc <= 0
                r%addError(message = "Invalid number of particle &
                                      size classes" &
                          )
                criterr = .true.                                     !! critical error
            end if
            if (C_tot == 0) then                                     !! CRITICAL ERROR HERE: C_tot = 0
                r%addError(message = "Layer volume is zero" &
                          )
                criterr = .true.                                     !! critical error
            end if
            if ((.not. present(V_f)) .and. (.not. present(M_f)) then
                r%AddError(message = "One of fine sediment volume &
                                      and mass must be specified", &
                          )                                          !! create error instance
                criterr = .true.                                     !! critical error
            end if
            if (present(V_f)) then
                if (size(V_f) /= nsc) then                           !! array of fine sediment masses must have correct size
                    r%AddError(message = "Array of fine sediment &
                                          volumes is the wrong &
                                          size", &
                              )                                      !! create error instance
                    criterr = .true.                                 !! critical error
                end if
            end if
            if (present(M_f)) then
                if (size(M_f) /= nsc) then                           !! array of fine sediment masses must have correct size
                    r%AddError(message = "Array of fine sediment &
                                          masses is the wrong &
                                          size", &
                              )                                      !! create error instance
                    criterr = .true.                                 !! critical error
                end if
            end if
            if (size(f_comp, 1) /= nsc) then                         !! number of size classes must be consistent
                r%AddError(message = "Array of fractional &
                                      compositions is the wrong &
                                      size", &
                          )                                          !! create error instance
                criterr = .true.                                     !! critical error
            end if
            if (size(f_comp, 2) /= size(pd_comp)) then               !! number of compositional fractions must be consistent
                r%AddError(message = "Arrays of fractional &
                                      compositions and particle &
                                      densities have different &
                                      sizes", &
                          )                                          !! create error instance
                criterr = .true.                                     !! critical error
            end if
            if (present(Porosity)) then
                if (Porosity <= 0 .or. Porosity >= 1) then
                r%AddError(message = "Porosity is out of range" &
                          )                                          !! create error instance
                criterr = .true.                                     !! critical error
                end if
            end if
            tr = Me%name // "%" // tr                                !! add name to trace string
            if (criterr = .true.) then                               !! if a critical error has been thrown
                r%addToTrace(tr)                                     !! add trace to Result
                return                                               !! exit, as a critical error has occurred
            end if
            Me%nSizeClasses = nsc                                    !! store number of size classes
            Me%nFComp = size(f_comp, 2)                              !! store number of fractional composition terms
            Me%C_total = C_tot                                       !! assign the total volume
            allocate(Me%colFineSediment(1:nsc, stat=Me%allst)        !! set up fine sediment collection
            allocate(Me%colFineSedimentResusp(1:nsc, stat=Me%allst)  !! set up fine sediment collection to hold resuspension
            allocate(Me%C_f_l(1:nsc, stat=Me%allst))                 !! allocate space for fine sediment capacity
            allocate(Me%C_w_l(1:nsc, stat=Me%allst))                 !! allocate space for water capacity
            allocate(Me%pd_comp(1:size(pd_comp), stat=Me%allst))     !! allocate space for particle densities of components
            do x = 1, nsc
                associate (O => Me%colFineSediment(x)%item)          !! association for the FineSediment object we are working with
                                                                     !! this is done to reduce code length and increase readability
                    select case (FSType)                             !! loop through possible FineSediment object types
                        case(1)                                      !! Type FineSediment1
                            allocate (fs1, stat=Me%allst)            !! create FineSediment1 object
                            call fs1%create(Me%pd_comp)              !! run constructor for this object
                            call move_alloc(fs1, O)                  !! move the object into the colFineSediment collection; this deallocates fs1
                        case default                                 !! not a recognised FineSediment type
                            r%AddError(.errors. &
                                        message = "Invalid &
                                                   FineSediment &
                                                   object type &
                                                   specified", &
                                          trace = tr &
                                      )                              !! add ErrorInstance
                            return                                   !! critical error, so exit
                    end select
                    if (present(V_f)) then
                        r%addError(.errors. &
                                    O%SetV(Vf_in = V_f(x), &
                                    f_comp_in = f_comp(x,:)) &
                                  )                                  !! if V_f values are defined, set up FineSediment using V_f
                    elseif (present(M_f)) then
                        ! QUERY: can we return from Set_M into an ErrorCriteria instance and then test for criticality
                        !        in order to determine whether to exit immediately?
                        r%addError(.errors. &
                                    O%SetM(Mf_in = M_f(x), &
                                    f_comp_in = f_comp(x,:)) &
                                  )                                  !! otherwise if M_f values are defined, set up FineSediment using M_f
                    end if
                end associate
                Me%C_f_l(x) = V_f(x)                                 !! set the sediment capacities, using the local value
            end do
            if (Me%V_f_layer > C_tot) then                           !! CRITICAL ERROR HERE: if Me%V_f_layer > C_tot
                r%addError(.errors. message = "Fine sediment volume &
                                               exceeds capacity", &
                                      trace = tr &
                          )                                          !! add ErrorInstance
                return                                               !! critical error, so exit
            end if
            if (present(Porosity)) then                              !! has a porosity value been supplied?
                fwr = Porosity / (1 - Porosity)                      !! yes, use porosity to compute factor for sediment:water ratio
            else                                                     !!
                fwr = (Me%C_total - Me%C_f_layer) / Me%C_f_layer     !! no, so use C_total and C_f_layer to compute factor for
            end if                                                   !! sediment:water ratio
            do x = 1, nsc                                            !! compute V_w for each size fraction using the sediment:water ratio
                associate (O => Me%colFineSediment(x)%item)          !! association for the FineSediment object we are working with
                    r%addError(.errors. &
                        O%SetV(V_w_in = O%V_f * fwr))                !! is used to compute the volume of associated water
                end associate
            end do
            do x = 1, nsc                                            !! loop through all size fractions
                Me%C_w_l(x) = Me%colFineSediment(x)%item%V_w         !! set the water capacities, using the local variable
            end do
            if (Me%V_m_layer > C_tot) then                           !! CRITICAL ERROR HERE: if Me%V_f_layer > C_tot
                r%addError(.errors. message = "Fine sediment & water &
                                               volume exceeds &
                                               capacity", &
                                      trace = tr &
                          )                                          !! add ErrorInstance
                return                                               !! critical error, so exit
            end if
            Me%V_c = C_tot - Me%V_m_layer                            !! set the coarse material volume
        end function
        !> destroy this object
        subroutine destroyBedSedimentLayer1(Me)
            deallocate(Me%colFineSediments)                          !! deallocate all allocatable variables
            deallocate(pd_comp)
            deallocate(C_f_l)
            deallocate(C_w_l)
        end subroutine
        !> add sediment and water to this layer
        function AddSediment1(Me, S, F) result(r)
            implicit none
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSediment1), intent(inout) :: F                  !! FineSediment - holds material to be added
            type(Result), intent(out) :: r                           !! The Result object
            type(ErrorCriteria) :: er                                !! LOCAL ErrorCriteria object for error handling.
            real(dp) :: add_M_f                                      !! LOCAL mass of fine sediment being added
            real(dp) :: add_V_f                                      !! LOCAL volume of fine sediment to be added
            real(dp) :: add_V_w                                      !! LOCAL volume of water to be added
            real(dp) :: M_f_SC                                       !! LOCAL mass of fine sediment in receiving size class
            real(dp) :: V_f_SC                                       !! LOCAL volume of fine sediment in receiving size class
            real(dp) :: A_f_SC                                       !! LOCAL capacity for fine sediment in receiving size class
            real(dp) :: V_w_SC                                       !! LOCAL volume of water in receiving size class
            real(dp) :: A_w_SC                                       !! LOCAL capacity for water in receiving size class
            real(dp) :: Mf                                           !! LOCAL temporary variable
            real(dp), allocatable :: t_comp(:)                       !! LOCAL temporary variable
            integer :: x                                             !! LOCAL loop counter
            logical :: criterr                                       !! LOCAL .true. if one or more critical errors tripped
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
            criterr = .false.                                        !! initial setting
            if (S <= 0 .or. S > Me%nSizeClasses) then                !! CRITICAL ERROR HERE: if S <= 0 or S > nSizeClasses
                r%addError(message = "The size class is out of &
                                      range" &
                          )
                criterr = .true.                                     !! critical error
            end if
            add_V_f = F%item%V_f                                     !! static local copy of added fine sediment volume
            if (add_V_f <= 0) then                                   !! CRITICAL ERROR HERE: if add_V_f < 0
                r%addError(message = "The added fine sediment &
                                      volume in size class " &
                                      // S // &
                                     " is less than zero" &
                          )
                criterr = .true.                                     !! critical error
            end if
            add_V_w = F%item%V_w                                     !! static local copy of added water volume
            if (add_V_w <= 0) then                                   !! CRITICAL ERROR HERE: if add_V_w < 0
                r%addError(message = "The added water &
                                      volume in size class " &
                                      // S // &
                                     " is less than zero" &
                          )
                criterr = .true.                                     !! critical error
            end if
            if criterr = .true. then                                 !! if a critical error has been thrown
                r%addToTrace(Me%name // "%addSediment")              !! add a trace message to any errors
                return                                               !! exit here
            end if
            allocate t_comp(1:Me%NFComp)                             !! for storage of modified fractional composition of modified sediment
            A_f_SC = Me%A_f(S)                                       !! static local copy of fine sediment capacity
            A_w_SC = Me%A_w(S)                                       !! static local copy of water capacity
            associate (O => Me%colFineSediments(S)%item)
                M_f_SC = O%M_f                                       !! fine sediment mass in layer
                V_f_SC = O%V_f                                       !! fine sediment volume in layer
                V_w_SC = O%V_w                                       !! water volume in layer
                if (add_V_f > A_f_SC) then                           !! added volume exceeds the available capacity; cannot all be added
                    V_f_SC = Me%C_f_l(S)                             !! set fine sediment volume to capacity
                    add_V_f = add_V_f - A_f_SC                       !! volume that could not be added
                    V_f_added = V_f_SC - A_f_SC                      !! volume added
                else                                                 !! added volume does not exceed the fine sediment capacity; can all be added
                    V_f_SC = V_f_SC + add_V_f                        !! addition of fine sediment volume
                    add_V_f = 0                                      !! return zero volume not added
                    V_f_added = add_V_f                              !! volume added
                end if
                if (add_V_w > A_w_SC) then                           !! added volume exceeds the available capacity; cannot all be added
                    V_w_SC = Me%C_w_l(S)                             !! set water volume to capacity
                    add_V_w = add_V_w - A_w_SC                       !! volume that could not be added
                else                                                 !! added volume does not exceed the fine sediment capacity; can all be added
                    V_w_SC = V_w_SC + add_V_w                        !! addition of water volume
                    add_V_w = 0                                      !! return zero volume not added
                end if
                Mf = V_f_added * F%item%rho_part(S)                  !! read in added mass - prevents multiple calls to object
                do x = 1, size(Me%nFComp)                            !! in this subsequent loop
                    t_comp(x) = M_f_SC * O%f_comp(x)
                    t_comp(x) = t_comp(x) + Mf * F%item%f_comp(x)
                    t_comp(x) = t_comp(x) / (M_f_SC + Mf)            !! modified fraction of component no. x
                end do
                r%addError(.errors. O%SetV(V_f_SC, V_w_SC, t_comp))  !! copy modified properties to fine sediment, add any error to Result object
                ! QUERY: is there a way to find out whether there is a critical error instance in the Result object?
                !        this could be used to immediately Return from the procedure
            end associate
            r%addError(.errors. F%item%SetV(add_V_f, add_V_w))       !! return volumes of fine sediment and water not added, add any error to Result object
        end function
        !> remove sediment and water from this layer
        function RemoveSediment1(Me, S, G, F) result(r)
            implicit none
            class(BedSedimentLayer) :: Me                            !! the BedSedimentLayer instance
            integer, intent(in) :: S                                 !! the particle size class
            type(FineSedimentElement), intent(inout) :: G            !! fine sediment to be removed; returns fine sediment that could not be removed
            type(FineSedimentElement), intent(inout) :: F            !! returns fine sediment that was removed
            type(Result), intent(out) :: r                           !! The Result object
            real(dp) :: V_f_SC                                       !! LOCAL fine sediment volume in layer
            real(dp) :: V_f_SC_r                                     !! LOCAL fine sediment volume removed
            real(dp) :: V_w_SC                                       !! LOCAL water volume in layer
            real(dp) :: V_w_SC_r                                     !! LOCAL water volume removed
            logical :: criterr                                       !! LOCAL .true. if one or more critical errors tripped
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
            criterr = .false.                                        !! initial setting
            if (S <= 0 .or. S > Me%nSizeClasses) then                !! CRITICAL ERROR HERE: if S <= 0 or S > nSizeClasses
                r%addError(message = "The size class is out of &
                                      range" &
                          )
                criterr = .true.                                     !! critical error
            end if
            V_f_SC_r = G%item%V_f                                    !! static local copy of fine sediment volume to be removed
            if (V_f_SC_r <= 0) then                                  !! CRITICAL ERROR HERE: if V_f_SC_r < 0
                r%addError(message = "The removed fine sediment &
                                      volume in size class " &
                                      // S // &
                                     " is less than zero" &
                          )
                criterr = .true.                                     !! critical error
            end if
            V_w_SC_r = G%item%V_w                                    !! static local copy of water volume to be removed
            if (V_w_SC_r <= 0) then                                  !! CRITICAL ERROR HERE: if V_w_SC_r < 0
                r%addError(message = "The removed water volume &
                                      in size class " &
                                      // S // &
                                     " is less than zero" &
                          )
                criterr = .true.                                     !! critical error
            end if
            if criterr = .true. then                                 !! if a critical error has been thrown
                r%addToTrace(Me%name // "%removeSediment")           !! add a trace message to any errors
                return                                               !! exit here
            end if
            associate (O => Me%colFineSediments(S)%item)
                V_f_SC = O%V_f                                       !! static local copy of fine sediment volume
                V_w_SC = O%V_w                                       !! static local copy of water volume
                if (V_f_SC_r > V_f_SC) V_f_SC_r = V_f_SC             !! set actual volume of fine sediment to be removed
                if (V_w_SC_r > V_w_SC) V_w_SC_r = V_w_SC             !! set actual volume of water to be removed
                r%AddError(.errors. O%SetV(V_f_SC - V_f_SC_r, &
                                           V_w_SC - V_w_SC_r)        !! update fine sediment in layer
                r%AddError(.errors. F%item%SetV(V_f_SC_r, &
                                     V_w_SC_r, O%f_comp))            !! set properties of the sediment being removed, including fractional composition
                r%AddError(.errors. G%item%SetV(G%item%V_f - V_f_SC_r, &
                                     G%item%V_w - V_w_SC_r))         !! return the volume that could not be removed
            end associate
        end function
end module

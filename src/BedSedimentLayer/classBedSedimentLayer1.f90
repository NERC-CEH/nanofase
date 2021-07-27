!> Module container for `BedSedimentLayer1` class
module classBedSedimentLayer1
    use spcBedSedimentLayer                                          ! use BedSedimentLayer superclass
    use Globals
    use UtilModule
    use ResultModule, only: Result, Result0D
    use classDatabase, only: DATASET
    use classFineSediment1
    use ErrorInstanceModule
    implicit none                                                    ! force declaration of all variables
    !> Class definition for `BedSedimentLayer1`. Extends abstract
    !! superclass `BedSedimentLayer`
    type, public, extends(BedSedimentLayer) :: &
        BedSedimentLayer1
        contains                                                     ! methods deferred from superclass
            procedure, public :: &
            create => createBedSedimentLayer1                        ! constructor method
            procedure, public :: &
            destroy => destroyBedSedimentLayer1                      ! finaliser method
            procedure, public :: &
            addSediment => addSediment2                              ! add fine sediment to the layer - version returning output via intent(inout)
            procedure, public :: &
            removeSediment => removeSediment2                        ! remove fine sediment from layer - version returning outputs via intent(inout)
            procedure, public :: clearAll => clearAllSediment1       ! set all fine sediment masses, all water volume and all fractional compositions to zero
            procedure, public :: repmass => ReportMassesToConsole1   ! print all fine sediment masses to the console
    end type
    contains
        !> **Function purpose**                                     
        !! Initialise a `BedSedimentLayer` object and its constituent
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
        function createBedSedimentLayer1(Me, l) result(r)
            class(BedSedimentLayer1) :: Me                           !! The `BedSedimentLayer` instance
            integer :: l                                             !! Index of this layer 
            type(Result) :: r                                        !! The `Result` object.
            real(dp) :: Porosity                                     ! LOCAL layer porosity
            real(dp), allocatable :: M_f(:)                          ! LOCAL set of fine sediment masses, index = size class
            real(dp), allocatable :: f_comp(:,:)                     ! LOCAL set of fractional compositions. Index 1 = size class, Index 2 = compositional fraction
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            character(len=16), parameter :: ms = &
                                            "Allocation error"       ! LOCAL allocation error message
            real(dp) :: fwr
            real(dp) :: V_m_layer_l                                  ! LOCAL copy of fines+water volume, avoids repeated property calls
            integer :: S                                             ! LOCAL loop counter
            integer :: allst                                         ! LOCAL array allocation status
            character(len=256) :: allms                              ! LOCAL array allocation message
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
            ! TODO remove local versions of me%nSizeClasses and nfComp to free up memory
            Me%nSizeClasses = C%nSizeClassesSpm                      ! set number of size classes from global value
            Me%nfComp = C%nFracCompsSpm                              ! set number of fractional compositions from global value
            me%l = l                                        ! Index for this layer
            me%name = ref('Layer', me%l)                    ! Name for this layer
            tr = trim(Me%name) // "%create"                          ! add name to trace string
            ! Get the layer capacity [m3/m2] from data for this layer
            me%C_total = C%sedimentLayerDepth(me%l)
            if (Me%C_total == 0) then                                ! CRITICAL ERROR HERE: C_total == 0
                call r%addError(ErrorInstance( &
                            code = 1, &
                            message = "Layer capacity is zero", &
                            trace = [tr] &
                                            ) &
                               )
            end if
            ! var = layerGroup%getVariable("initial_mass")             ! Get the sediment initial masses
            allocate(M_f, source=DATASET%sedimentInitialMass)
            ! M_f = DATASET%sedimentInitialMass                       ! Get the initial sediment mass from the dataset
            if (size(M_f) /= Me%nSizeClasses) then                   ! array of fine sediment masses must have correct size
                call r%AddError(ErrorInstance( &
                            code = 1, &
                            message = "Array of fine sediment " &
                                       // "masses is the wrong size", &
                            trace = [tr] &
                                             ) &
                               )                                     ! create error instance
            end if
            
            ! Code for old NetCDF data had a check whether there was a porosity variable.
            ! Now we'll presume that there is a porosity variable. Auditing will be moved to classDatabase.
            Porosity = DATASET%sedimentPorosity(me%l) 
            ! if (layerGroup%hasVariable("porosity")) then             ! has a porosity value been supplied?
            !     var = layerGroup%getVariable("porosity")             ! Get the porosity
            !     call var%getData(Porosity)                           ! Put porosity into local variable
            !     if (Porosity <= 0 .or. Porosity >= 1) then
            !         call r%AddError(ErrorInstance( &
            !                 code = 1, &
            !                 message = "Porosity is out of range", &
            !                 trace = [tr] &
            !                              ) &
            !                )                                         ! create error instance
            !     end if
            !     if (r%hasCriticalError()) return                     ! exit if a critical error has occurred
            ! end if                                                   ! sediment:water ratio
            ! grp = layerGroup%getGroup("fractional_compositions")     ! get fractional composition group
                                                                     ! SH: The fractional comps could be stored as 2D arrays in the NetCDF/JSON file
                                                                     ! to simplify this a bit
            ! allocate(f_comp_sc(Me%nfComp), stat = allst, &
                                        !    errmsg = allms)           ! allocate the temporary array of fractional composition (note on this below)

            ! Get the fractional composition distribution from data
            allocate(f_comp(Me%nSizeClasses, Me%nfComp))             ! allocate space for fractional compositions
            do s = 1, me%nSizeClasses
                f_comp(s,:) = DATASET%sedimentFractionalComposition
            end do

           
            ! if (allst /= 0) then
            !      call r%addError(ErrorInstance( &
            !                            code = 1, &
            !                         message = allms, &
            !                           trace = [tr] &
            !                                   ) &
            !                     )                                    ! add to Result
            ! end if
            ! TODO (DONE REQUIRES CHECKING) line above requires proper error checking
            ! do S = 1, Me%nSizeClasses                                ! loop through size classes
            !     var = grp%getVariable(trim(ref("f", "s", S)))        ! get the fractional composition data for each size class, variable name f_s_1, f_s_2 etc.
            !                                                          ! NetCDF doesn't like array slices in getData(), so we have to store them in
            !                                                          ! another allocatable variable, f_comp_sc, before retrieving them
            !     f_comp_sc = f_comp(S,:)
            !     call var%getData(f_comp_sc)                          ! put the data into the relevant size class of f_comp(:,:)
            !     f_comp(S,:) = f_comp_sc
            ! end do

            tr = trim(Me%name) // &
                "%createBedSedimentLayer1%colFineSediment"           ! trace message
            allocate(Me%colFineSediment(Me%nSizeClasses), &
                stat = allst, errmsg = allms)                        ! set up fine sediment collection
             if (allst /= 0) then
                 call r%addError(ErrorInstance( &
                                       code = 1, &
                                    message = allms, &
                                      trace = [tr] &
                                              ) &
                                )                                    ! add to Result
             end if
            tr = trim(Me%name) // &
                "%createBedSedimentLayer1%C_f_l"                     ! trace message
            allocate(Me%C_f_l(Me%nSizeClasses), &
                stat = allst, errmsg = allms)                        ! allocate space for fine sediment capacity
            if (allst /= 0) then
                call r%addError(ErrorInstance( &
                                      code = 1, &
                                   message = allms, &
                                     trace = [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
            tr = trim(Me%name) // &
                "%createBedSedimentLayer1%C_w_l"                     ! trace message
            allocate(Me%C_w_l(Me%nSizeClasses), &
                stat = allst, errmsg = allms)                        ! allocate space for water capacity
            if (allst /= 0) then
                call r%addError(ErrorInstance( &
                                      code = 1, &
                                   message = allms, &
                                     trace = [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
            ! TOOD is me%pd_comp used or 
            tr = trim(Me%name) // &
                "%createBedSedimentLayer1%pd_comp"                   ! trace message
            allocate(Me%pd_comp(Me%nfComp), &
                stat = allst, errmsg = allms)                        ! allocate space for particle densities of components
            if (allst /= 0) then
                call r%addError(ErrorInstance( &
                                      code = 1, &
                                   message = allms, &
                                     trace = [tr] &
                                             ) &
                               )                                     ! add to Result
            end if

            if (r%hasCriticalError()) return                         ! exit if allocation has thrown an error
            do S = 1, Me%nSizeClasses
                call me%colFineSediment(s)%create(trim(ref(me%name, "s", s)), me%nfComp)
                call me%colFineSediment(S)%set(Mf_in=M_f(S), f_comp_in=f_comp(S,:))
                Me%C_f_l(S) = Me%colFineSediment(S)%V_f()            ! set the sediment capacities to the volumes
            end do

            if (Me%V_f_layer() > Me%C_total) then               ! CRITICAL ERROR HERE: if layer volume exceeds capacity
                call r%addError(ErrorInstance( &
                               code = 1, &
                               message = "Fine sediment volume &
                                          exceeds capacity" &
                                             ) &
                               )                                    ! add ErrorInstance
                return                                              ! critical error, so exit
            end if
            if (r%hasCriticalError()) then                          ! if a critical error has been thrown
                call r%addToTrace(tr)                               ! add trace to Result
                return                                              ! exit, as a critical error has occurred
            end if
            
            ! Database will set porosity to 0 if it isn't in input data, so perform that check here
            if (.not. isZero(Porosity)) then                          ! has a porosity value been supplied?
                fwr = Porosity / (1 - Porosity)                     ! yes, use porosity to compute factor for sediment:water ratio
            else   
                ! TODO I don't think this defaulting is being done correctly, outputs value > 1.
                ! Not an issue for the moment because porosity always supplied, but worth checking back.
                fwr = (Me%C_total - Me%C_f_layer()) / Me%C_f_layer()! no, so use C_total and C_f_layer to compute factor for
            end if                                                  ! sediment:water ratio

            do S = 1, Me%nSizeClasses                               ! compute V_w for each size fraction using the sediment:water ratio
                call Me%colFineSediment(S)%set(Vw_in = &
                        Me%colFineSediment(S)%V_f() * fwr)          ! is used to compute the volume of associated water
            end do
            do S = 1, Me%nSizeClasses                                ! loop through all size fractions
                Me%C_w_l(S) = Me%colFineSediment(S)%V_w()            ! set the water capacities, using the local variable
            end do
            V_m_layer_l = Me%V_m_layer()                            ! temporary storage of fines+water volume
            if (V_m_layer_l > Me%C_total) then                       ! CRITICAL ERROR HERE: if Me%V_m_layer > C_tot
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
            Me%V_c = Me%C_total - V_m_layer_l                        ! set the coarse material volume
        end function
        !> **Function purpose**                                     <br>
        !! Deallocate all allocated variables in this object.
        !!                                                          <br>
        !! **Function outputs/outcomes**                            <br>
        !! All allocated variables deallocated
        function destroyBedSedimentLayer1(Me) result(r)
            class(BedSedimentLayer1) :: Me                           !! The `BedSedimentLayer` instance
            type(Result) :: r                                        !! The `Result` object
            character(len=256) :: tr                                 ! LOCAL name of this procedure, for trace
            integer :: allst                                         ! LOCAL array allocation status
            character(len=18), parameter :: &
                                          ms = "Deallocation error"  ! LOCAL CONSTANT error message
            ! Notes
            ! -------------------------------------------------------------------------------
            ! No notes.
            ! -------------------------------------------------------------------------------
            tr = trim(Me%name) // &
                "%destroyBedSedimentLayer1%colFineSediment"          ! trace message
            deallocate(Me%colFineSediment, stat = allst)             ! deallocate all allocatable variables
            if (allst /= 0) then
                call r%addError(ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
            tr = trim(Me%name) // &
                "%destroyBedSedimentLayer1%pd_comp"                  ! trace message
            deallocate(Me%pd_comp, stat = allst)
            if (allst /= 0) then
                call r%addError(ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
            tr = trim(Me%name) // &
                "%destroyBedSedimentLayer1%C_f_l"                    ! trace message
            deallocate(Me%C_f_l, stat = allst)
            if (allst /= 0) then
                call r%addError(ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
            tr = trim(Me%name) // &
                "%destroyBedSedimentLayer1%C_w_l"                    ! trace message
            deallocate(Me%C_w_l, stat = allst)
            if (allst /= 0) then
                call r%addError(ErrorInstance(1, &
                                   ms, &
                                   .false., &
                                   [tr] &
                                             ) &
                               )                                     ! add to Result
            end if
        end function

        !> Add fine sediment of a specified size fraction, and associated water,
        !! to a bed sediment layer
        !!  Returns sediment that could not be added
        !! in the passed intent(inout) variable F, instead of in the Result object
        !!                                                          
        !! **Function inputs**                                      
        !! `S (integer)`:       the size class to which sediment is to be added
        !! `F (FineSediment1)`: object representing the FineSediment to be added
        !!                                                          
        !! **Function outputs/outcomes**                            
        !! `F (FineSediment1)`: Returns the amounts of sediment and water that could not be added
        function addSediment2(Me, S, F) result(r)
            class(BedSedimentLayer1) :: Me                           !! The `BedSedimentLayer` instance
            integer, intent(in) :: S                                 !! The particle size class
            type(FineSediment1), intent(inout) :: F                  !! `FineSediment` - holds material to be added, returns material that could not be added
            type(Result) :: r                                        !! The `Result` object. Return data type = `FineSediment1`
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
            integer :: i                                             ! LOCAL loop counter
            character(len=100) :: tr                                 ! LOCAL name of this procedure, for trace

            tr = (Me%name) // "%addSediment1"                        ! trace for this procedure
            add_V_f = max(F%V_f(), 0.0_dp)                           ! static local copy of added fine sediment volume, ensuring it's 0 or greater
            add_V_w = max(F%V_w(), 0.0_dp)                           ! static local copy of added water volume, ensuring it's 0 or greater
            allocate(t_comp(1:Me%nfComp))                           ! for storage of modified fractional composition of modified sediment
            A_f_SC = me%A_f(s)                                      ! static local copy of fine sediment capacity
            A_w_SC = me%A_w(s)                                      ! static local copy of water capacity
            associate(O => Me%colFineSediment(S))                    ! association for brevity
                M_f_SC = O%M_f()                                     ! fine sediment mass in layer
                V_f_SC = O%V_f()                                     ! fine sediment volume in layer
                V_w_SC = O%V_w()                                     ! water volume in layer
                if (add_V_f > A_f_SC) then                           ! added volume exceeds the available capacity; cannot all be added
                    V_f_SC = Me%C_f_l(S)                             ! set fine sediment volume to capacity
                    V_f_added = A_f_SC                               ! volume added
                    add_V_f = add_V_f - A_f_SC                       ! volume that could be added
                else                                                    ! added volume does not exceed the fine sediment capacity; can all be added
                    V_f_SC = V_f_SC + add_V_f                           ! addition of fine sediment volume
                    V_f_added = add_V_f                                 ! volume added
                    add_V_f = 0                                         ! return zero volume not added
                end if
                if (add_V_w > A_w_SC) then                              ! added volume exceeds the available capacity; cannot all be added
                    V_w_SC = Me%C_w_l(S)                                ! set water volume to capacity
                    add_V_w = add_V_w - A_w_SC                          ! volume that could not be added
                else                                                    ! added volume does not exceed the fine sediment capacity; can all be added
                    V_w_SC = V_w_SC + add_V_w                           ! addition of water volume
                    add_V_w = 0                                         ! return zero volume not added
                end if
                Mf = V_f_added * F%rho_part()                           ! read in added mass - prevents multiple calls to object
                do i = 1, Me%nfComp                                     ! in this subsequent loop
                    t_comp(i) = M_f_SC * O%f_comp(i)
                    t_comp(i) = t_comp(i) + Mf * F%f_comp(i)
                    t_comp(i) = flushToZero(t_comp(i))
                    M_f_SC = flushToZero(M_f_SC)
                    Mf = flushToZero(Mf)
                    t_comp(i) = divideCheckZero(t_comp(i), M_f_SC + Mf)               ! modified fraction of component no. i
                end do
                call O%set(Vf_in = V_f_SC, &
                           Vw_in = V_w_SC, &
                           f_comp_in = t_comp)                          ! copy modified properties to fine sediment, add any error to Result object
            end associate
            call F%set(Vf_in = add_V_f, &
                       Vw_in = add_V_w)                                 ! return volumes of fine sediment and water not added, add any error to Result object
        end function

        !> Remove sediment of a specified size fraction, and associated water,
        !! from a bed sediment layer
        !!                                                         
        !! **Function inputs**                                     
        !! `S (integer)`:       the size class from which sediment is to be removed
        !! `G (FineSediment1)`: sediment to be removed
        !!                                                         
        !! **Function outputs/outcomes**                           
        !! in this alternative version, both objects returned via inout
        !! `G (FineSediment1)` returns the sediment that could not be removed
        !! `H (FineSediment1)` returns the sediment that was removed
        function removeSediment2(Me, S, G, H) result(r)
            class(BedSedimentLayer1) :: Me                           !! The `BedSedimentLayer` instance
            integer, intent(in) :: S                                 !! The particle size class
            type(FineSediment1), intent(inout) :: G                  !! Fine sediment to be removed, returns fine sediment that could not be removed
            type(FineSediment1), intent(inout) :: H                  !! Returns fine sediment that was removed
            type(Result) :: r                                        !! The Result object
            real(dp) :: V_f_SC                                       ! LOCAL fine sediment volume in layer
            real(dp) :: V_f_SC_r                                     ! LOCAL fine sediment volume removed
            real(dp) :: V_f_SC_r_2                                   ! LOCAL fine sediment volume to be removed
            real(dp) :: V_w_SC                                       ! LOCAL water volume in layer
            real(dp) :: V_w_SC_r                                     ! LOCAL water volume removed
            character(len=256) :: tr                                 ! LOCAL error trace

            tr = trim(Me%name) // "%removeSediment2"                 ! trace for this procedure
            ! Make two copies of fine sediment volume to be removed, and set to 0 if it is <0
            V_f_SC_r = max(G%V_f(), 0.0_dp)
            V_f_SC_r_2 = V_f_SC_r
            associate (O => Me%colFineSediment(S))
                V_f_SC = O%V_f()                                     ! static local copy of fine sediment volume
                V_w_SC = O%V_w()                                     ! static local copy of water volume
                if (V_f_SC_r > V_f_SC) then
                    V_f_SC_r = V_f_SC                                ! amount of sediment to be removed exceeds amount in layer, 
                    V_w_SC_r = V_w_SC                                ! so set volumes of sediment and water to be removed to the layer totals
                else                                                 ! need to compute volume of water to be removed - equal proportion of water present as to sediment present
                    if (isZero(G%V_w())) then
                        V_w_SC_r = V_f_SC_r / Me%volSLR(S)          ! water volume to be removed, computed from the solid:liquid ratio for the layer, if no value is supplied
                    else
                        V_w_SC_r = G%V_w()                           ! water volume as supplied
                    end if
                end if
                call O%set( &
                    Vf_in = V_f_SC - V_f_SC_r, &
                    Vw_in = V_w_SC - V_w_SC_r &
                )                                                   ! updating the amounts in the bed layer  
                call G%set( &
                    Vf_in = V_f_SC_r_2 - V_f_SC_r, &
                    Vw_in =  0.0_dp &
                )                                                   ! setting G to return the sediment that could not be removed, and reset the water requirement to zero
                tr = trim(Me%name) //  "%removeSediment1%"           ! trace message
                call H%set( &                  ! populate H, holding the removed sediment
                    Vf_in = V_f_SC_r, &
                    Vw_in = V_w_SC_r, &
                    f_comp_in = O%f_comp &
                )                                   ! set properties of the sediment being removed, including fractional composition
            end associate
        end function
        !> **Subroutine purpose**                                   <br>
        !! Remove sediment of all size classes, and associated water,
        !! from a bed sediment layer
        !!                                                          <br>
        !! **Subroutine inputs**                                    <br>
        !! `S (integer)`: the size class from which sediment is to be removed <br>
        !!                                                          <br>
        !! **Subroutine outcomes**                                  <br>
        !! Values of sediment mass, water volume and fractional composition set to zero
        !! for all sediments in this layer
        subroutine clearAllSediment1(Me)
            class(BedSedimentLayer1) :: Me                           !! This `BedSedimentLayer1` object
            integer :: i                                             ! Loop iterator for FineSediment objects
            do i = 1, size(Me%colFineSediment)
                call Me%colFineSediment(i)%ClearAll()
            end do
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
        subroutine ReportMassesToConsole1(me)
            class(BedSedimentLayer1) :: me                          !! The `BedSedimentLayer` instance
            integer :: n                                            !! LOCAL loop counter 
            print *, trim(me%name)                                  ! the name of this layer
            do n=1, me%nSizeClasses
                print *, "Size class ", n, " : ", &
                    Me%colFineSediment(n)%M_f()                     ! print out mass of FS in each size class [kg/m2]
            end do
            print *, "Total: ", me%M_f_layer()                      ! print out mass of FS in layer [kg/m2]
        end subroutine
end module

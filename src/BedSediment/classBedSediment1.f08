module classBedSediment1                                             ! class definition for BedSediment1
    use Globals
    use ResultModule
    use spcBedSediment                                               ! use BedSediment superclass
    implicit none                                                    ! force declaration of all variables
    private

    type, public, extends(BedSediment) :: &
        BedSediment1                                                 ! type declaration for class - extends abstract superclass
      contains
        procedure, public :: create => createBedSediment1            !! constructor method
        procedure, public :: destroy => destroyBedSediment1          !! finaliser method
        procedure, public :: deposit => DepositSediment1             !! deposit sediment from water column
        procedure, public :: resuspend => ResuspendSediment1         !! resuspend sediment to water column
!        procedure, public :: Resuspension => calculateResuspensionBedSediment1
!        procedure, public :: StreamPower => calculateStreamPowerBedSediment1
    end type
  contains
    function createBedSediment1
        ! TODO: everything
    end function
    function destroyBedSediment1
        ! TODO: everything
    end function
    !> compute resuspension from bed sediment
    function ResuspendSediment1(Me, M_resusp) result(r)
        class(BedSediment) :: Me                                     !! self-reference
        real(dp), intent(in), allocatable :: M_resusp(:)             !! array of sediment masses to be resuspended [kg m-2]. Index = size class[1,...,S]
        real(dp), intent(inout), allocatable :: FS(::)               !! array returning resuspended fine sediment. Index 1 = size class, Index 2 = layer
        real(dp), intent(inout), allocatable :: W(::)                !! array returning resuspended water. Index 1 = size class, Index 2 = layer
        real(dp), intent(inout), allocatable :: C(:::)               !! array returning resuspended fractional composition. Index 1 = size class, Index 2 = layer, Index 3 = fc
        ! TODO: rework FS(::) as a set of dp arrays, not FineSediment object
                                                                     !! Index 1 = size class, Index 2 = layer derived from
        type(Result), intent(out) :: r                               !! returned Result object
        type(FineSediment1), allocatable :: F                        !! LOCAL FineSediment object representing material that has been resuspended
        type(FineSediment1), allocatable :: G                        !! LOCAL FineSediment object representing material to be resuspended
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        character(len=*) :: tr                                       !! LOCAL name of this procedure, for trace
        !
        ! Function purpose
        ! ----------------------------------------------------------------------------------
        ! Resuspend specified masses of fine sediment in each size class, and their
        ! associated water
        !
        ! Function inputs
        ! ----------------------------------------------------------------------------------
        ! Function takes as inputs:
        ! M_resusp (real, dp)      1D array of fine sediment masses to be resuspended
        !
        ! Function outputs/outcomes
        ! ----------------------------------------------------------------------------------
        !
        ! returns a warning if the resuspended mass in a size class exceeds the mass in the
        ! sediment bed
        !
        ! FS(::) returns masses of resuspended fine sediment, split by size class and layer
        !
        ! FS(::) returns volumes of resuspended water, split by size class and layer
        !
        ! C(:::) returns fractional composition of resuspended fine sediment, split by
        !        size class, layer and fraction
        !
        ! Notes
        ! ----------------------------------------------------------------------------------
        ! QUESTION: FS, W and C do not bring any inputs into the function, so they are
        !           allocated here - but is this possible???
        ! ----------------------------------------------------------------------------------
        tr = Me%name // "ResuspendSediment1"                         !! error trace for this procedure
        if (size(M_resusp) /= Me%nSizeClasses) then                  !! number of size classes must be consistent
            call r%AddError(message = "Number of resuspended &
                                      sediment classes does not &
                                      match number of size classes &
                                      in sediment", &
                              trace = tr &
                           )                                         !! create error instance
            return
        end if                                                       !! exit if a critical error has been thrown
        allocate(F, stat = Me%allst)                                 !! set up FineSediment1 variable F
        allocate(G, stat = Me%allst)                                 !! set up FineSediment1 variable G
        allocate(FS(1:Me%nSizeClasses,1:Me%nLayers) &
                                             stat = Me%allst)        !! allocation of output array FS
        allocate(W(1:Me%nSizeClasses,1:Me%nLayers), &
                                             stat = Me%allst)        !! allocation of output array W
        allocate(C(1:Me%nSizeClasses,1:Me%nLayers, 1:Me%nFComp), &
                                             stat = Me%allst)        !! allocation of output array C
        do S = 1, Me%nSizeClasses                                    !! loop through all size classes
            call r%addErrors(.errors. G%setByM(Mf_in = M_resusp(S)) &
                            )                                        !! top layer: set up the temporary object G, with the resuspended mass
            if (r%hasCriticalError) then
                call r%addToTrace(tr)
                return                                               !! exit if a critical error has been thrown
            end if
            L = 1                                                    !! start with top layer
            associate(O => Me%colBedSedimentLayers(L))               !! association for brevity
                do while (M_resusp(S) > 0 .and. L <= Me%nLayers)     !! loop through layers until all sediment resuspended or all layers considered
                    call r%addErrors(.errors. &
                         G%setByM(Vw_in = M_resusp(S) / O%volSLR), &
                                    )                                !! add the water content to G
                                                                     !! and the volume of water to be resuspended along with the fine sediment
                    if (r%hasCriticalError) then
                        call r%addToTrace(tr)
                        return                                       !! exit if a critical error has been thrown
                    end if
                    call r%addErrors(.errors. &
                                        O%RemoveSediment(S, G, F), &
                                    )                                !! remove the resuspended sediment, put the resuspended sediment into F
                    if (r%hasCriticalError) then
                        call r%addToTrace(tr)
                        return                                       !! exit if a critical error has been thrown
                    end if
                    FS(L, S) = F%M_f                                 !! set fine sediment mass in output array
                    W(L, S) = F%V_w                                  !! set water volume in output array
                    C(L, S, :) = F%f_comp                            !! set fractional composition in output array
                    M_resusp(S) = M_resusp(S) - FS(L, S)             !! keep count of fine sediment that has been resuspended
                    L = L + 1                                        !! the residual resuspended sediment into G. Repeat until all sediment has been
                end do                                               !! resuspended, or sediment has been removed from all layers
                if (M_resusp(S) > 0) then
                    er = ErrorInstance(666, &
                                       "All sediment of size class " &
                                       // S // " resuspended", &
                                       .false., &
                                       tr &
                                      )                              !! warning (noncritical error) if bed has been stripped of size class S
                    call r%addError(er)                              !! add the error to the Result
                end if
            end do
        end associate
        deallocate(F)                                                !! release memory allocated to F
        deallocate(G)                                                !! release memory allocated to G
    end function
    !> compute deposition to bed sediment, including burial and downward shifting of fine sediment and water
    function DepositSediment1(Me, M_dep, f_comp_dep, V_w_tot) result (r)
        class(BedSediment) :: Me                                     !! self-reference
        real(dp), intent(in), allocatable :: M_dep(:)                !! Depositing sediment mass by size class
        real(dp), intent(in), allocatable :: f_comp_dep(::)          !! Depositing sediment fractional composition by size class
                                                                     !! Index 1 = size class, Index 2 = compositional fraction
        real(dp), intent(out) :: V_w_tot                             !! water requirement from the water column [m3 m-2]
        type(Result) :: r                                            !! returned Result object
        type(ErrorCriteria) :: er                                    !! LOCAL ErrorCriteria object for error handling
        type(FineSediment1), allocatable :: DS(:)                    !! LOCAL FineSediment objects holding deposited material
        type(FineSediment1), allocatable :: B                        !! LOCAL object to receive sediment being buried
        type(FineSediment1), allocatable :: T                        !! LOCAL object to receive sediment being buried
        type(FineSediment1), allocatable :: U                        !! LOCAL object to receive sediment that has been buried
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        integer :: A                                                 !! LOCAL second counter for layers
        real(dp) :: A_f_sed                                          !! LOCAL available fine sediment capacity for size class
        real(dp) :: tempV                                            !! LOCAL volume variable
        real(dp) :: V_f_b                                            !! LOCAL available fine sediment capacity in the receiving layer [m3 m-2]
        real(dp) :: V_w_b                                            !! LOCAL available water capacity in the receiving layer [m3 m-2]
        character(len=*) :: tr                                       !! LOCAL name of this procedure, for trace
        !
        ! Function purpose
        ! -------------------------------------------------------------------------------
        ! Deposit specified masses of fine sediment in each size class, and their
        ! associated water. Function buries sediment and shifts remaining sediment down
        ! to make space for deposition, if required
        !
        ! Function inputs
        ! -------------------------------------------------------------------------------
        ! Function takes as inputs:
        ! D (FineSedimentElement)  FineSediment object representing the depositing fine
        !                          sediment and water
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------------------------
        !
        ! Notes
        ! -------------------------------------------------------------------------------
        ! 1.    Currently does not account fully for sediment burial, in the sense that
        !       it does not tally mass, volume and composition of buried material. This
        !       will need to be added before burial losses of a chemical vector can be
        !       computed.
        !       ACTION: add code to mix FineSediments together and return a single
        !       FineSediment object. This code can be used to tally up the sediment that
        !       is lost through burial.
        ! 2.    The FineSediment objects in D should not contain any water, but if they
        !       do it is not a problem as it will be overwritten.
        ! -------------------------------------------------------------------------------
        tr = Me%name // "%DepositSediment1"                          !! object and procedure binding name as trace
        if (size(M_dep) /= Me%nSizeClasses) &
            call r%AddError(message = "The number of fine &
                                       sediment masses does &
                                       not equal the number &
                                       of size classes"   &
                           )                                         !! CRITICAL ERROR if size(D) <> nSizeClasses
        if (size(f_comp_dep, 1) /= nSizeClasses) &                   !! number of size classes must be consistent
            call r%AddError(message = "Input number of fractional &
                                       compositions does not match &
                                       number of size classes", &
                           )
        if (size(f_comp_dep, 2) /= nFComp) &                         !! number of compositional fractions must be consistent
            call r%AddError(message = "Input number of fractional &
                                       compositions does not match &
                                       required number", &
                           )
        if (r%hasCriticalError) then                                 !! if a critical error has been thrown
            call r%AddToTrace(tr)                                    !! add trace to Result
            return                                                   !! and exit
        end if
        allocate(DS(1:nSizeClasses), stat = Me%allst)                !! allocate space for FineSediment1 objects
        do S = 1, nSizeClasses                                       !! compose FineSediment1 objects from the inputs
            DS(S)%setByM(Mf_in = M_dep(S), &
                     f_comp_in = f_comp_dep(S,:) &
                        )                                            !! populate DS with depositing sediment and its fractional composition
        end do
        do S = 1, nSizeClasses
            call r%AddErrors(.errors. DS(S)%fcomp_audit)             !! audits fractional composition of deposition, returns error instance
            if (r%hasCriticalError) then                             !! if fcomp_audit throws a critical error
                call r%addToTrace(tr)                                !! add trace to all errors
                return                                               !! and exit
            end if
        end do
        allocate(B, stat = Me%allst)                                 !! set up B
        allocate(T, stat = Me%allst)                                 !! set up T
        allocate(U, stat = Me%allst)                                 !! set up U
        do S = 1, nSizeClasses                                       !! loop through all size classes
            if (int(DS(S)%V_f / Me%Cf_sediment(S)) > 0) then         !! check whether the depositing sediment in each size class exceeds the total
                do L = 1, nLayers                                    !! capacity in the layer. If so, then remove all fine sediment, water and
                    call Me%colBedSedimentLayers(L)%ClearAll         !! fractional compositions from all layers for this size class
                    ! TODO: tally up the sediment being buried at this point
                end do
            end if
        end do
        do S = 1, nSizeClasses
            A_f_sed = Me%Af_sediment(S)                              !! local copy of the capacity for this sediment size class in the whole bed
            if (DS(S)%V_f > A_f_sed) then                            !! do we need to bury sediment to create available capacity for deposition?
                call T%setByV(Vf_in = DS(S)%V_f - A_f_sed, &
                              Vw_in = 0)                             !! set up temporary FineSediment object with volume of fine sediment requiring burial
                                                                     !! to compute the volume of water requiring burial, we must loop through layers
                                                                     !! from the top, computing for each layer the volume of fine sediment that must be
                                                                     !!  removed to allow space for deposition, and the volume of water associated with the
                                                                     !! fine sediment
                L = 1                                                !! loop through layers, downwards from the top
                do while (T%V_f > 0)                                 !! use fine sediment volume in T as a counter
                    associate (O => Me%colBedSedimentLayers(L)%item) !! association to layer L
                        if (T%V_f > O%C_f(S)) then                   !! does the depositing fine sediment fit into this layer,
                                                                     !! after accounting for the capacity in layers above?
                                                                     !! no, so increase water removal requirement by the water capacity of this layer
                                                                     !! and decrease the count of remaining depositing fine sediment by the capacity
                            call T%setByV(Vf_in = T%V_f - O%C_f(S), &
                                          Vw_in = T%V_w + O%C_w(S) &
                                         )
                        else                                         !! yes,
                                                                     !! so increase the water burial requirement by the amount required to maintain the SLR in this layer
                                                                     !! and set the count of fine sediment to zero, to jump out of the loop
                            tempV = T%V_f / O%volSLR                 !! temporary variable
                            T%setByV(Vf_in = 0, &
                                     Vw_in = T%V_w + tempV &
                                    )
                        end if
                    end associate
                    L = L + 1
                end do
                                                                     !! now to actually bury fine sediment and water
                call T%setByV(Vf_in = DS(S)%item%V_f - A_f_sed)      !! reset the fine sediment burial requirement, still using temporary object T
                L = nLayers                                          !! start with the bottom layer
                do while (L > 0 .and. T%V_f + T%V_w > 0)             !! loop through each layer, while there is still material to bury
                    if (deltaV_f_temp > 0) Then
                        associate(O => &
                            Me%colBedSedimentLayers(L)%item)         !! association reference to layer L object
                            call r%addErrors(.errors. &
                                O%RemoveSediment(S, B, T))           !! remove sediment from this layer into B. T returns unburied sediment
                        end associate
                        if (r%hasCriticalError) then                 !! if RemoveSediment throws a critical error
                            call r%addToTrace(tr)                    !! add trace to all errors
                            return                                   !! and exit
                        end if
                    end If
                    L = L - 1                                        !! move up to next layer
                end do                                               !! finished burial. temporary object T can be reused
                do L = nLayers, 2, -1                                !! downward shift of fine sediment. loop through the layers, starting at the bottom
                                                                     !! and working upwards
                    assoc1 : associate &
                        (O => Me%colBedSedimentLayers(L)%item)       !! association to "receiving" layer L
                        A = L - 1                                    !! counter for donating layers - initially the layer above
                        assoc2 : associate &
                            (P => Me%colBedSedimentLayers(A)%item)   !! association to "donating" layer A
                            call T%setByV(Vf_in = O%A_f(S), &
                                          Vw_in = O%A_w(S) &
                                         )                           !! set FineSediment object T to hold sediment requiring removal
                            do while (A > 0 .or. &
                                      T%IsEmpty .eqv. .false.)       !! loop through "donating" layers, moving upwards
                                if (P%colFineSediments(S)%V_f > 0) &
                                    then                             !! if there is sediment in the "donating" layer
                                    call r%addErrors(.errors. &
                                         P%RemoveSediment(S, U, T))  !! remove sediment (T) from the "donating" layer
                                                                     !! sediment that cannot be removed is returned in T
                                                                     !! for removal on the next pass through the loop
                                                                     !! sediment that is removed is returned in U
                                    if (r%hasCriticalError) then     !! if RemoveSediment throws a critical error
                                        call r%addToTrace(tr)        !! add trace to all errors
                                        return                       !! and exit
                                    end if
                                    call r%addErrors(.errors. &
                                        O%AddSediment(S, U))         !! add the sediment in U to the "receiving" layer
                                    if (r%hasCriticalError) then     !! if AddSediment throws a critical error
                                        call r%addToTrace(tr)        !! add trace to all errors
                                        return                       !! and exit
                                    end if
                                end if
                                ! TODO: tally up removed sediment from U on each loop
                                A = A - 1                            !! shift up to next "donating" layer
                            end do
                        end associate assoc2
                  end associate assoc1
                end do
            end if
        end do
        do S = 1, nSizeClasses                                       !! deposit sediment from the water column
            L = nLayers                                              !! start with the bottom layer and work upwards
            do
                associate(O => Me%colBedSedimentLayers(L)%item)      !! size class S in Layer L
                    if (O%A_f > 0 .or. O%A_w > 0) then               !! if there is available capacity in this layer, add deposition here
                        V_w_b = DS(S)%V_f / O%volSLR                 !! the volume of water needed to maintain SLR in the "receiving" layer,
                        DS(S)%setByV(Vw_in = V_w_b)                  !! if all deposition fits into this layer
                        call r%addError(.errors. &
                            O%AddSediment(S, DS(S)))                 !! add the fine sediment in deposition. DS(S) returns volumes
                                                                     !! that could not be added
                        if (r%hasCriticalError) then                 !! if AddSediment throws a critical error
                            call r%addToTrace(tr)                    !! add trace to all errors
                            return                                   !! and exit
                        end if
                    end if
                    V_w_tot = V_w_tot + V_w_b - DS(S)%V_w            !! tally up V_w_b to compute water requirement to take from the water column
                    L = L - 1
                end associate
            end do
        end do
        deallocate(Q)                                                !! deallocate Q
        deallocate(T)                                                !! deallocate T
        deallocate(U)                                                !! deallocate U
    end function
    !> Calculate resuspension from bed sediment using
    !! [Bussi](http://www.sciencedirect.com/science/article/pii/S0022169416305625):
    !! $$
    !!      m_{\text{ent}} = a m_{\text{bed}} \alpha \omega \frac{R_\text{h}}{R_{\text{h,max}}}
    !! $$
    !function calculateResuspensionBedSediment1(me, a, m_bed, alpha, omega, R_h, R_hmax) result(r)
    !    class(BedSediment1) :: me
    !    real(dp) :: a                                   !! Calibration factor [s2/kg]
    !    real(dp) :: m_bed                               !! Bed mass per unit area [kg/m2]
    !    real(dp) :: alpha                               !! Proportion of size class that can be resuspended [-]
    !    real(dp) :: omega                               !! Stream power per unit area of stream bed [J/s/m2]
    !    real(dp) :: R_h                                 !! Actual hydraulic radius [m]
    !    real(dp) :: R_hmax                              !! Maximum hydraulic radius [m]
    !    real(dp) :: f                                   !! Friction factor [-]
    !    type(Result0D) :: r
    !    f = R_h/R_hmax                                  ! Calculate the friction factor
    !    r = Result( &
    !        data = a * m_bed * alpha * omega * f &       ! Calculate the resuspension
    !    )
    !end function

    !> Calculate the stream power (per unit area of stream bed) using Bagnold's
    !! stream power equation:
    !! $$
    !!      \omega = \frac{\rho g Q S}{W}
    !! $$
    !! Reference: [Bagnold, 1966](https://www.uvm.edu/~wbowden/Teaching/Stream_Geomorph_Assess/Resources/Private/Documents/1966_Bagnold_river_sediments.pdf)
    !function calculateStreamPowerBedSediment1(me, rho_water, g, Q, W, S) result(r)
    !    class(BedSediment1) :: me
    !    real(dp) :: rho_water                           !! Density of water [kg/m3]
    !    real(dp) :: g                                   !! Gravitational acceleration [m/s]
    !    real(dp) :: Q                                   !! Discharge [m3/s]
    !    real(dp) :: W                                   !! River width [m]
    !    real(dp) :: S                                   !! River slope [m/m]
    !    type(Result0D) :: r
    !    r = Result( &
    !        data = rho_water * g * Q * S / W &
    !    )
    !end function
end module

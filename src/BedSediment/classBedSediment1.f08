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
        real(dp), intent(inout), allocatable :: M_resusp(:)          !! array of sediment masses to be resuspended [kg m-2]
        !type(FineSedimentElement), intent(out), allocatable :: FS(::) !! array of FineSediment objects returning resuspended material.
                                                                     !! Index 1 = size class, Index 2 = layer derived from
        type(Result2D), intent(out) :: r                             !! returned Result object
        type(FineSediment1), allocatable :: G                        !! LOCAL FineSediment object representing material to be resuspended
        real(dp) :: V_w_resusp                                       !! LOCAL the volume of water to be resuspended along with the fine sediment
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        character(len=*) :: tr                                       !! LOCAL name of this procedure, for trace
        logical :: criterr                                           !! LOCAL .true. if one or more critical errors tripped
        !
        ! Function purpose
        ! -------------------------------------------------------------------------------
        ! Resuspend specified masses of fine sediment in each size class, and their
        ! associated water
        !
        ! Function inputs
        ! -------------------------------------------------------------------------------
        ! Function takes as inputs:
        ! M_resusp (real, dp)      1D array of fine sediment masses to be resuspended
        ! FS (FineSedimentElement) 2D array of FineSediment objects representing the
        !                          resuspended fine sediment and water
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------------------------
        ! Result object R returns the amounts of fine sediment and water resuspended from each size
        ! class as a "d array of FineSediment1.
        ! M_resusp() returns the mass of sediment that could not be resuspended, so if
        ! M_resusp(S) > 0 then the bed has been stripped of sediment of size class S.
        !
        ! Notes
        ! -------------------------------------------------------------------------------
        ! QUESTION: FS does not bring any inputs into the function. Should it be
        !           allocated inside or outside the function.
        ! NOTE:     the 2-dimensional structure of FS is not necessary to consider
        !           only resuspension of fine sediment and water, but it is implemented
        !           for use with a chemical vector.
        ! -------------------------------------------------------------------------------
        if (size(M_resusp) /= nSizeClasses) then                     !! number of size classes must be consistent
            r%AddError(message = "Number of resuspended sediment &
                                  classes does not match &
                                  number of size classes in &
                                  sediment", &
                         trace = Me%name // "ResuspendSediment1" &
                      )                                              !! create error instance
            return                                                   !! critical error, so exit immediately
        end if
        allocate(G)                                                  !! set up FineSedimentElement variable G
        allocate(FS(1:Me%nSizeClasses,1:Me%nLayers))                 !! allocation of FS (single layer)
        associate (O => Me%colBedSedimentLayers(L)%item)             !! association for brevity
            do S = 1, Me%nSizeClasses                                !! loop through all size classes
                L = 1                                                !! start with top layer
                V_w_resusp = M_resusp(S) / O%volSLR                  !! the volume of water to be resuspended along with the fine sediment
                r%addError(.errors. G%item%SetM(M_resusp(S), &
                           V_w_resusp, &
                           O%colFineSediments(S)%item%f_comp)        !! top layer: set up the temporary object G, with the resuspended mass and the same
                                                                     !! fractional composition as the layer
                do while (G%item%M_f > 0 .and. L <= Me%nLayers)
                    if L > 1 then
                        r%addError(.errors. G%item%SetM &
                       (f_comp_in = &
                        O%colFineSediments(S)%item%f_comp)           !! sublayer: set up G with the fractional composition of the layer
                    end if                                           !! the mass and water to be resuspended are already in the object
                    O%RemoveSediment(S, G, FS(S, L)%item)            !! remove the resuspended sediment, put the resuspended sediment into FS(S, L) and
                    M_resusp(s) = M_resusp(s) - FS(S)%item%M_f       !! keep count of fine sediment that has been resuspended
                   L = L + 1                                         !! the residual resuspended sediment into G. Repeat until all sediment has been
                end do                                               !! resuspended, or sediment has been removed from all layers
            end do
        end associate
        deallocate(G)                                                !! release memory allocated to G
        r = Result(data = FS)                                        !! add data output to Result object
    end function
    !> compute deposition to bed sediment, including burial and downward shifting of fine sediment and water
    function DepositSediment1(Me, D, F_comp_D) result (r)
        class(BedSediment) :: Me                                     !! self-reference
        real(dp), intent(in), allocatable :: D(:)                    !! Depositing sediment mass by size class
        real(dp), intent(in), allocatable :: f_comp_D(::)            !! Depositing sediment fractional composition by size class.
                                                                     !! Index 1 = size class, Index 2 = compositional fraction
        type(Result) :: r                                            !! returned Result object
        type(ErrorCriteria) :: er                                    !! LOCAL ErrorCriteria object for error handling.
        type(FineSedimentElement), allocatable :: DS(:)              !! LOCAL FineSediment objects holding deposited material
        type(FineSedimentElement), allocatable :: Q                  !! LOCAL object to receive sediment being buried
        type(FineSedimentElement), allocatable :: T                  !! LOCAL object to receive sediment being buried
        type(FineSedimentElement), allocatable :: U                  !! LOCAL object to receive sediment that has been buried
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        integer :: A                                                 !! LOCAL second counter for layers
        real(dp) :: A_f_sed                                          !! LOCAL available fine sediment capacity for size class
        real(dp) :: deltaV_f_temp                                    !! LOCAL volume of fine sediment requiring burial to create space for deposition
        real(dp) :: deltaV_w_temp                                    !! LOCAL volume of water requiring burial to create space for deposition
        real(dp) :: V_f_b                                            !! LOCAL available fine sediment capacity in the receiving layer [m3 m-2]
        real(dp) :: V_w_b                                            !! LOCAL available water capacity in the receiving layer [m3 m-2]
        character(len=*) :: tr                                       !! LOCAL name of this procedure, for trace
        logical :: criterr                                           !! LOCAL .true. if one or more critical errors tripped
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
        er = ERROR_HANDLER%notEqual(size(D), nSizeClasses, &         !! CRITICAL ERROR if size(D) <> nSizeClasses
                                    message = "The number of fine &
                                               sediment masses does &
                                               not equal the number &
                                               of size classes" &
                                   )                                 !! error if added fine sediment volume is less than zero
        if (er%GetCode) == 0) then                                   !! note condition here is ==, not /=
            er%AddToTrace(tr)                                        !! add trace to error
                    r%AddError(error = er)                           !! add error to result if tripped
                    criterr = .true.                                 !! critical error
        end if
        if (size(f_comp_D, 1) /= nSizeClasses) then                  !! number of size classes must be consistent
            r%AddError(message = "Input number of fractional &
                                  compositions does not match &
                                  number of size classes", &
                       trace = tr &
                      )                                              !! create error instance
            criterr = .true.                                         !! critical error
        end if
        if (size(f_comp_D, 2) /= nFComp) then                        !! number of compositional fractions must be consistent
            r%AddError(message = "Input number of fractional &
                                  compositions does not match &
                                  required number", &
                       trace = tr &
                      )                                              !! create error instance
            criterr = .true.                                         !! critical error
        end if
        if (criterr == .true). return                                !! exit here if a critical error has been thrown
        allocate(DS(1:nSizeClasses), stat = Me%allst)                !! allocate space for FineSediment objects
        do S = 1, nSizeClasses                                       !! compose FineSediment objects from the inputs
            DS(S)%item%SetM(Mf_in = D(S), &
                            f_comp_in = f_comp_D(S,))                !! populate each FineSediment object
        end do
        do S = 1, nSizeClasses                                       !! CRITICAL ERROR if D(S)%item%M_f < 0
            er = D%item%fcomp_audit                                  !! audits the fractional composition of the depositing sediment, returns error instance
            if (er%GetCode) == 0) then                               !! note condition here is ==, not /=
                er%AddToTrace(tr)                                    !! add trace to error
                r%AddError(error = er)                               !! add error to result if tripped
                criterr = .true.                                     !! critical error
            end if
        end do
        allocate(Q, stat = Me%allst)                                 !! set up Q
        allocate(T, stat = Me%allst)                                 !! set up T
        allocate(U, stat = Me%allst)                                 !! set up U
        do S = 1, nSizeClasses                                       !! loop through all size classes
            if (int(D(S)%item%V_f/Me%Cf_sediment(S))) then           !! check whether the depositing sediment in each size class exceeds the total
                do L = 1, nLayers                                    !! capacity in the layer. If so, then remove all fine sediment, water and
                    Me%colBedSedimentLayers(L)%item%Clear            !! fractional compositions from the layer for this size class
                end do
            end if
        end do
        do S = 1, nSizeClasses
            A_f_sed = Me%Af_sediment(S)                              !! local copy of the capacity for this sediment size class in the whole bed
            if (D(S)%item%V_f > A_f_sed) then                        !! do we need to bury sediment to create available capacity for deposition?
                deltaV_f_temp = D(S)%item%Vf - A_f_sed               !! Yes. deltaV_f_temp is the volume of fine sediment capacity we need to clear
                deltaV_w_temp = 0                                    !! The corresponding volume of water needs to be calculated
                L = 1
                do while (deltaV_w_temp > 0)
                    associate (O => Me%colBedSedimentLayers(L)%item) !! association to layer L
                        if (deltaV_f_temp > O%C_f(S)) then           !! does the depositing fine sediment fit into this layer,
                                                                     !! after accounting for the capacity in higher layers?
                            deltaV_w_temp = deltaV_w_temp + O%C_w(S) !! NO, so increase water removal requirement by the water capacity of this layer
                            deltaV_f_temp = deltaV_f_temp - O%C_f(S) !! and decrease the count of remaining depositing fine sediment by the capacity
                        else                                         !! YES,
                            deltaV_w_temp = deltaV_w_temp + _
                                deltaV_f_temp / O%volSLR             !! so increase the water removal requirement by the amount required to maintain the SLR in this layer
                            deltaV_f_temp = 0                        !! and set the count of fine sediment to zero to jump out of the loop
                        end if
                    end associate
                    L = L + 1
                end do
                L = nLayers                                          !! start with the bottom layer
                deltaV_f_temp = D(S)%item%V_f - A_f_sed              !! reset the fine sediment burial requirement
                do while (L > 0 .and. &
                         deltaV_f_temp + deltaV_w_temp > 0)          !! loop through each layer
                    If (deltaV_f_temp > 0) Then &
        Me%colBedSedimentLayers(L)%item%RemoveSediment(S, D(S), Q)
                                                                     !! remove sediment from this layer into Q
                    L = L - 1                                        !! move up to next layer
                end do
                do L = nLayers, 2, -1                                !! downward shift of fine sediment. loop through the layers, starting at the bottom
                    assoc1 : associate &
                        (O => Me%colBedSedimentLayers(L)%item)       !! association to layer L
                        A = L - 1                                    !! counter for donating layers
                        assoc2 : associate &
                            (P => Me%colBedSedimentLayers(A)%item)   !! association to layer A
                            V_f_b = O%A_f(S)                         !! available fine sediment capacity in the receiving layer [m3 m-2]
                            V_w_b = O%A_w(S)                         !! available water capacity in the receiving layer [m3 m-2]
                            T%item%SetV(V_f_b, V_w_b, O%f_comp)      !! set FineSediment object T to hold sediment requiring removal
                            do while (A > 0 .or. &
                                      T%item%IsEmpty == .false.)
                                if (P%colFineSediments(S)%item%V_f &
                                    > 0) &
                                    P%RemoveSediment(S, T, U)        !! attempt to remove sediment from layer A. Sediment that cannot be removed
                                                                     !! is returned in T, sediment that is removed is returned in U
                                    ! TODO: tally up removed sediment from U on each loop
                                A = A - 1                            !! shift up to next layer
                            end do
                        end associate assoc2
                  end associate assoc1
                end do
            end if
        end do
        do S = 1, nSizeClasses
            L = nLayers                                              !! start with the bottom layer and work upwards
            do
                associate(O => Me%colBedSedimentLayers(L)%item)
                    if (O%A_f > 0 .or. O%A_w > 0) then
                        O%AddSediment(S, D(S))                       !! add the fine sediment in deposition. This has to be done twice, firstly to get
                        V_w_b = O%colFineSediments(S)%item%V_f / &
                                O%volSLR - &
                                O%colFineSediments%item%V_w          !! the volume of water required to maintain the required SLR in the layer
                                                                     !! compute the volume of water that needs to be added to maintain the required SLR
                        D(S)%item%SetV(V_w_in = V_w_b)               !! add this water into the depositing FineSediment object
                        r%AddError(.errors. O%AddSediment(S, D(S)))  !! and add the fine sediment and water in deposition
                    end if
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

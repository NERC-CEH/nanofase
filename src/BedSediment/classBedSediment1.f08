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
    function ResuspendSediment1(Me, M_resusp, FS) result(R)
        class(BedSediment) :: Me                                     !! self-reference
        type(Result), intent(out) :: R                               !! Result object
        real(dp), intent(in), allocatable :: M_resusp(:)             !! array of sediment masses to be resuspended [kg m-2]
        type(FineSedimentElement), intent(out), allocatable :: FS(:) !! array of FineSediment objects returning resuspended material
        type(FineSedimentElement), allocatable :: G                  !! LOCAL FineSediment object representing material to be resuspended
        real(dp) :: V_w_resusp                                       !! LOCAL the volume of water to be resuspended along with the fine sediment
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        !
        ! Function purpose
        ! -------------------------------------------------------------------------------
        ! Resuspend specified masses of fine sediment in each size class, and their
        ! associated water
        !
        ! Function inputs
        ! -------------------------------------------------------------------------------
        ! Function takes as inputs:
        ! M_resusp (real, dp)      array of fine sediment masses to be resuspended
        ! FS (FineSedimentElement) array of FineSediment objects representing the
        !                          resuspended fine sediment and water
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------------------------
        ! FS returns the amounts of fine sediment and water resuspended from each size
        ! class.
        !
        ! Notes
        ! -------------------------------------------------------------------------------
        ! QUESTION: FS does not bring any inputs into the function. Should it be
        !           allocated inside or outside the function
        ! -------------------------------------------------------------------------------
        ! CRITICAL ERROR if size(M_resusp) <> nSizeClasses
        ! CRITICAL ERROR if size(FS) <> nSizeClasses assuming FS is already allocated
        ! WARNING if M_resusp(S) > Mf_sediment(S) for any size class S, i.e. if the
        !         resuspension strips all fine sediment out
        ! NOTE    if M_resusp(S) > Mf_sediment(S) then the resuspension and deposition
        !         timestep must be reduced to prevent this situation. This should
        !         actually be checked, and the timestep adjusted if necessary, in the
        !         calling routine
        allocate(G)                                                  !! set up FineSedimentElement variable G
        ! QUESTION: is this the correct syntax?
        do S = 1, Me%nSizeClasses                                    !! loop through all size classes
            L = 1                                                    !! start with top layer
            V_w_resusp = M_resusp(S) / O%volSLR                      !! the volume of water to be resuspended along with the fine sediment
            G%item%SetM(M_resusp(S), V_w_resusp, &
                O%colFineSediments(S)%item%f_comp)                   !! top layer: set up the temporary object G, with the resuspended mass and the same
                                                                     !! fractional composition as the layer
            do while G%item%M_f > 0
                ! TODO: insert code for condition where whole bed is resuspended
                associate (O => Me%colBedSedimentLayers(L)%item)     !! association for brevity
                    if L > 1 then
                            G%item%SetM &
                        (f_comp_in=O%colFineSediments(S)%item%f_comp) !! sublayer: set up G with the fractional composition of the layer
                    end if                                           !! the mass and water to be resuspended are already in the object
                    O%RemoveSediment(S, G, FS(S)%item)               !! remove the resuspended sediment, put the resuspended sediment into FS(S) and
                end associate
                L = L + 1
            end do                                                   !! the residual resuspended sediment into G. Repeat until all sediment has been
        end do                                                       !! resuspended
        deallocate(G)                                                !! release memory allocated to G
    end function
    !> compute deposition to bed sediment
    function DepositSediment1(Me, D)
        ! TODO: replace D with real array to represent SPM *masses* only
        class(BedSediment) :: Me                                     !! self-reference
        type(FineSedimentElement), intent(in), allocatable :: D(:)   !! Depositing sediment by size class
        type(FineSedimentElement), allocatable :: Q                  !! LOCAL object to receive sediment being buried
        type(FineSedimentElement), allocatable :: T                  !! LOCAL object to receive sediment being buried
        type(FineSedimentElement), allocatable :: U                  !! LOCAL object to receive sediment that has been buried
        integer :: S                                                 !! LOCAL loop counter for size classes
        integer :: L                                                 !! LOCAL counter for layers
        real(dp) :: A_f_sed                                          !! LOCAL available fine sediment capacity for size class
        real(dp) :: deltaV_f_temp                                    !! LOCAL volume of fine sediment requiring burial to create space for deposition
        real(dp) :: deltaV_w_temp                                    !! LOCAL volume of water requiring burial to create space for deposition
        real(dp) :: V_f_b
        real(dp) :: V_w_b
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
        ! CRITICAL ERROR if size(D) <> nSizeClasses
        do S = 1, nSizeClasses
            ! CRITICAL ERROR if D(S)%item%M_f < 0
            D%item%fcomp_audit                                       !! audits the fractional composition of the depositing sediment
        end do
        ! allocate(Q)                                                !! set up Q
        ! allocate(T)                                                !! set up T
        allocate(U)                                                  !! set up U
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
                    If deltaV_f_temp > 0 Then &
                        Me%colBedSedimentLayers(L)%item%RemoveSediment(S, D(S), Q) !! ****
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
                        O%AddSediment(S, D(S))                       !! and add the fine sediment and water in deposition
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

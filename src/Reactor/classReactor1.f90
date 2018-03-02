module classReactor1
    use Globals
    use ResultModule
    use spcReactor

    implicit none
    
    type, public, extends(Reactor) :: Reactor1
        contains
        procedure :: create => createReactor1
        procedure :: update => updateReactor1
        ! Processes
        procedure :: heteroaggregation => heteroaggregationReactor1
        procedure :: parseInputData => parseInputDataReactor1
        ! Calculations
        procedure :: calculateCollisionRate => calculateCollisionRateReactor1
    end type
        
    contains
    
    !> Run initialising procedures for the `Reactor` object
    function createReactor1(me, x, y, alpha_hetero) result(r)
        class(Reactor1) :: me           !! This `Reactor1` object
        integer :: x                    !! The containing `GridCell` x reference
        integer :: y                    !! The containing `GridCell` x reference
        real(dp) :: alpha_hetero        !! Attachment efficiency, 0-1 [-]
        type(Result) :: r               !! The `Result` object to return
        
        ! Set the grid references
        me%x = x
        me%y = y
        me%alpha_hetero = alpha_hetero
        me%rho_np = 4230                ! HACK: Manually setting rho_np to TiO2 density [kg/m3]

        ! Allocate size class arrays to the correct size
        allocate(me%W_settle_np(C%nSizeClassesNP))
        allocate(me%W_settle_spm(C%nSizeClassesSpm))
        allocate(me%k_hetero(C%nSizeClassesNP, C%nSizeClassesSpm))
        allocate(me%C_spm_particle(C%nSizeClassesSpm))

        ! Allocate the NP mass matrix to correct number of state/form elements.
        ! States: 1. free, 2. bound to solid, 3+ heteroaggreated (per SPM size class). 
        ! Forms: 1. core, 2. shell, 3. coating, 4. corona.
        allocate(me%m_np( &
            C%nSizeClassesNP, &             ! Number of NP size classes
            4, &                            ! Number of different forms
            C%nSizeClassesSpm + 2 &         ! Number of different states
        ))
        ! Allocate ionic metal array. 1. free ion, 2. solution, 3. adsorbed.
        allocate(me%m_ionic(3))
        
        ! HACK: Set this from data
        me%m_np = 100.0_dp
    end function
    
    !> Run the `Reactor`'s simulation for the current time step
    function updateReactor1(me, t, C_spm, T_water, W_settle_np, W_settle_spm, G) result(r)
        class(Reactor1) :: me           !! This `Reactor1` object
        integer :: t                    !! The current time step
        real(dp) :: C_spm(C%nSizeClassesSpm)    !! The current mass concentration of SPM [kg/m3]
        real(dp) :: T_water             !! The current water temperature [C]
        real(dp) :: W_settle_np(C%nSizeClassesNP)   !! NP settling velocity [m/s]
        real(dp) :: W_settle_spm(C%nSizeClassesSpm) !! SPM settling velocity [m/s]
        real(dp) :: G                   !! Shear rate [s-1]
        type(Result) :: r               !! The `Result` object to return
        integer :: s                    ! Iterator for SPM size classes
        
        
        ! Set some state variables
        me%T_water = T_water
        me%W_settle_np = W_settle_np
        me%W_settle_spm = W_settle_spm
        me%G = G

        ! Calculate the SPM particle concentration, assuming SPM is spherical
        do s = 1, C%nSizeClassesSpm
            ! HACK: Sort this out to use propper fractional comp densities
            ! C_spm_particle = C_spm / volume of particle
            me%C_spm_particle(s) = C_spm(s)/(sum(C%rho_spm)/C%nFracCompsSpm*(4/3)*C%pi*(C%d_spm(s)/2)**3)
        end do
        
        print *, "C_spm (mass): ", C_spm(1)

        ! Heteroaggregate NPs to SPM
        call r%addErrors(.errors. me%heteroaggregation())
    end function
    
    !> Perform the heteroaggregation calculation for this time step
    function heteroaggregationReactor1(me) result(r)
        class(Reactor1) :: me
        real(dp) :: beta(4,C%nSizeClassesSpm + 2)
        type(Result) :: r
        real(dp) :: k_coll(C%nSizeClassesNp,C%nSizeClassesSpm)
        integer :: s, n
        real(dp) :: T(C%nSizeClassesNP, C%nSizeClassesSpm + 2, C%nSizeClassesSpm + 2)

        ! Calculate the collision rate
        k_coll = me%calculateCollisionRate( &
            me%T_water, &
            me%G, &
            me%W_settle_np, &
            me%W_settle_spm &
        )
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nSizeClassesNP
                me%k_hetero(n,s) = k_coll(n,s) * me%alpha_hetero * me%C_spm_particle(s)
            end do
        end do
        
        print *, "C_spm_particle: ", me%C_spm_particle(1)
        print *, "k_hetero: ", me%k_hetero(1,1)
        print *, "m_np: ", me%m_np(1,1,1)

        ! Construct transformation matrix
        ! TODO: Eventually this will be moved to update() function as it will
        ! deal with other processes
        do n = 1, C%nSizeClassesNP
            T(n,1,1) = 1 - sum(me%k_hetero(n,:))*C%timeStep     ! Amount removed from free NPs
            T(n,2,2) = 1                                        ! Don't change bound concentration
            do s = 1, C%nSizeClassesSpm
                T(n,s,s) = 1 + me%k_hetero(n,s)*C%timeStep      ! Amount to add to heteroaggregated NPs
            end do
            me%m_np(n,:,:) = matmul(me%m_np(n,:,:),T(n,:,:))
        end do
        
        print *, "Mass of core free NPs in SC 1: ", me%m_np(1,1,1)
        print *, " "

        ! Assume on each time step, 10% of free mass is transferred to heteroaggregated,
        ! split equally between the size classes
        ! beta(0) = [[0.9,0,0],[0,1,0],[0,0,1-(0.9/me%nSizeClassesSpm)]]
        ! me%n_np = me%n_np
    end function
    
    !> Parse the input data for this Reactor
    function parseInputDataReactor1(me) result(r)
        class(Reactor1) :: me
        type(Result) :: r
        
        ! TODO: Is there actually going to be input data,
        ! or is this going to be passed from RiverReach?
    end function
    
    !> Calculate the collision rate between NPs and SPM.
    !! Reference: [Praetorious et al, 2012](http://dx.doi.org/10.1021/es204530n)
    function calculateCollisionRateReactor1(me, T_water, G, W_settle_np, W_settle_spm) result(k_coll)
        class(Reactor1) :: me           !! This `Reactor1` instance
        real(dp) :: T_water             !! Temperature of the water [C]
        real(dp) :: G                   !! Shear rate [s-1]
        real(dp) :: W_settle_np(:)      !! NP settling velocity [m/s]
        real(dp) :: W_settle_spm(:)     !! SPM settling velocity [m/s]
        real(dp) :: k_coll(C%nSizeClassesNp,C%nSizeClassesSpm)
        integer :: n, s                 ! Iterators for SPM and NP size classes
        
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nSizeClassesNp
                k_coll(n,s) = (2*C%k_B*(T_water+273.15_dp)/(3*C%nu_w(T_water))) * (C%d_spm(s)/2 + C%d_np(n)/2)**2/(C%d_spm(s)/2*C%d_np(n)/2) &
                                + (4.0_dp/3.0_dp)*G*(C%d_np(n)/2 + C%d_spm(s)/2)**3 + C%pi*(C%d_spm(s)/2+C%d_np(n)/2)**2 * &
                                abs(W_settle_np(n) - W_settle_spm(s))
            end do
        end do
    end function
    
end module

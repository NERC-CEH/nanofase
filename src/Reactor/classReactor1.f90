module classReactor1
    use Globals
    use UtilModule
    use ResultModule
    use spcReactor
    use classDatabase, only: DATASET

    implicit none
    
    type, public, extends(Reactor) :: Reactor1
        contains
        procedure :: create => createReactor1
        procedure :: update => updateReactor1
        ! Processes
        procedure :: heteroaggregation => heteroaggregationReactor1
        procedure :: dissolution => dissolutionReactor1
        procedure :: transformation => transformationReactor1
        procedure :: parseInputData => parseInputDataReactor1
        ! Calculations
        procedure :: calculateCollisionRate => calculateCollisionRateReactor1
        procedure :: calculateParticleConcentration => calculateParticleConcentrationReactor1
    end type
        
    contains
    
    !> Run initialising procedures for the `Reactor` object
    function createReactor1(me, x, y, alpha_hetero) result(r)
        class(Reactor1) :: me           !! This `Reactor1` object
        integer :: x                    !! The containing `GridCell` x reference
        integer :: y                    !! The containing `GridCell` x reference
        real(dp) :: alpha_hetero        !! Attachment efficiency, 0-1 [-]
        type(Result) :: r               !! The `Result` object to return
        integer :: n                    !! NP size class iterator
        
        ! Set the grid references
        me%x = x
        me%y = y
        me%alpha_hetero = alpha_hetero
        me%rho_np = DATASET%nmDensity 
        me%volume = 0                   ! No river to begin with...

        call r%addErrors(.errors. me%parseInputData())

        ! Allocate size class arrays to the correct size
        allocate(me%W_settle_np(C%nSizeClassesNM))
        allocate(me%W_settle_spm(C%nSizeClassesSpm))
        allocate(me%k_hetero(C%nSizeClassesNM, C%nSizeClassesSpm))
        allocate(me%C_spm_particle(C%nSizeClassesSpm))
        allocate(me%C_np_free_particle(C%nSizeClassesNM))
        allocate(me%individualNPMass(C%nSizeClassesNM))

        ! Allocate the NP mass matrix to correct number of state/form elements.
        ! States: 1. free, 2. bound to solid, 3+ heteroaggreated (per SPM size class). 
        ! Forms: 1. core, 2. shell, 3. coating, 4. corona.
        allocate(me%m_np( &
            C%nSizeClassesNM, &             ! Number of NP size classes
            4, &                            ! Number of different forms
            C%nSizeClassesSpm + 2 &         ! Number of different states
        ))
        allocate(me%m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))

        ! Set the mass of individual particles (used for converting between
        ! particle concentrations and masses)
        ! TODO: Maybe set NP individual masses in globals?? Actually, don't think it's needed...
        !do n = 1, C%nSizeClassesNM
        !    me%individualNPMass(n) = me%rho_np * (4/3)*C%pi*(C%d_spm(n)/2)**3
        !end do
        
        
    end function
    
    !> Run the `Reactor`'s simulation for the current time step
    function updateReactor1(me, t, m_np, m_transformed, m_dissolved, C_spm, T_water, W_settle_np, W_settle_spm, G, volume) result(r)
        class(Reactor1) :: me                                       !! This `Reactor1` object
        integer         :: t                                        !! The current time step
        real(dp)        :: m_np(C%npDim(1), C%npDim(2), C%npDim(3)) !! Mass of NM for this timestep [kg]
        real(dp)        :: m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)) !! Mass of NM for this timestep [kg]
        real(dp)        :: m_dissolved                              !! Mass of dissolved NM for this timestep [kg]
        real(dp)        :: C_spm(C%nSizeClassesSpm)                 !! The current mass concentration of SPM [kg/m3]
        real            :: T_water                                  !! The current water temperature [C]
        real(dp)        :: W_settle_np(C%nSizeClassesNM)            !! NM settling velocity [m/s]
        real(dp)        :: W_settle_spm(C%nSizeClassesSpm)          !! SPM settling velocity [m/s]
        real            :: G                                        !! Shear rate [s-1]
        real(dp)        :: volume                                   !! Volume of the reach on this time step [m3]
        type(Result)    :: r                                        !! The `Result` object to return
        integer         :: s                                        ! Iterator for SPM size classes
        integer         :: n                                        ! Iterator for NP size classes

        ! Set current mass of NP in reactor to that given. Reactor doesn't deal
        ! with inflows/outflows and just takes a mass on each timestep and transforms
        ! that mass
        me%m_np = m_np
        me%m_transformed = m_transformed
        me%m_dissolved = m_dissolved
        me%volume = volume                  ! Volume of the container
        me%T_water = T_water                ! Current water temperature
        me%W_settle_np = W_settle_np        ! Settling rates
        me%W_settle_spm = W_settle_spm
        me%G = G                            ! Shear rate

        ! Calculate the SPM particle concentration, assuming SPM is spherical
        do s = 1, C%nSizeClassesSpm
            ! HACK: Sort this out to use propper fractional comp densities
            ! C_spm_particle = C_spm / mass of particle
            me%C_spm_particle(s) = me%calculateParticleConcentration( &
                C_spm(s), &
                sum(C%sedimentParticleDensities)/C%nFracCompsSpm, &
                C%d_spm(s) &
            )
        end do

        ! Heteroaggregate NPs to SPM, which updates m_np accordingly
        call r%addErrors(.errors. me%heteroaggregation())
        call r%addErrors(.errors. me%dissolution())
        call r%addErrors(.errors. me%transformation())
        
        ! The mass will have now been adjusted according to transformation.
        ! Reactor doesn't deal with inflows/outflows, so this updated mass
        ! must be picked up by the containing object and dealt with accordingly.        
    end function
    
    !> Perform the heteroaggregation calculation for this time step
    function heteroaggregationReactor1(me) result(r)
        class(Reactor1) :: me                               !! This `Reactor1` instance
        !real(dp) :: beta(4,C%nSizeClassesSpm + 2) 
        type(Result) :: r                                   !! The `Result` object to return any errors in
        real(dp) :: k_coll(C%nSizeClassesNM,C%nSizeClassesSpm)  ! Collision frequency [s-1]
        integer :: s, n                                     ! Iterators for NM and SPM size classes
        real(dp) :: T(C%nSizeClassesNM, C%nSizeClassesSpm + 2, C%nSizeClassesSpm + 2)
        real(dp) :: dm_hetero           ! Mass of NPs heteroaggregated on this timestep [kg/timestep]

        ! Calculate the collision rate and then heteroaggregation rate constant
        k_coll = me%calculateCollisionRate( &
            me%T_water, &
            me%G, &
            me%W_settle_np, &
            me%W_settle_spm &
        )
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nSizeClassesNM
                me%k_hetero(n,s) = k_coll(n,s) * me%alpha_hetero * me%C_spm_particle(s)
            end do
        end do
        
        do n = 1, C%nSizeClassesNM
            ! Calculate mass heteroaggregated (dm_hetero)
            ! first so that, if all NPs are heteroaggregated on one timestep, the mass can be split
            ! amongst SPM size classes correctly (rather than using k_hetero for each SPM size class,
            ! when we add to the heteroaggregated mass, which would result in too much mass being added)
            dm_hetero = min(sum(me%k_hetero(n,:))*C%timeStep*me%m_np(n,1,1), me%m_np(n,1,1))
            me%m_np(n,1,1) = me%m_np(n,1,1) - dm_hetero             ! Remove heteroaggregated mass from free NPs
            ! Add heteroaggregated mass to each size class of SPM
            do s = 1, C%nSizeClassesSpm
                if (.not. isZero(me%k_hetero(n,s))) then
                    dm_hetero = dm_hetero*(me%k_hetero(n,s)/sum(me%k_hetero(n,:)))  ! Fraction of heteroaggregated mass to add to this SPM size class
                    me%m_np(n,1,s+2) = me%m_np(n,1,s+2) + dm_hetero                 ! Add that heteroaggregated NPs
                else
                    dm_hetero = 0.0_dp
                end if
            end do
            
            ! Transformed NM
            dm_hetero = min(sum(me%k_hetero(n,:))*C%timeStep*me%m_transformed(n,1,1), me%m_transformed(n,1,1))
            me%m_transformed(n,1,1) = me%m_transformed(n,1,1) - dm_hetero             ! Remove heteroaggregated mass from free NPs
            do s = 1, C%nSizeClassesSpm
                if (.not. isZero(me%k_hetero(n,s))) then
                    dm_hetero = dm_hetero*(me%k_hetero(n,s)/sum(me%k_hetero(n,:)))  ! Fraction of heteroaggregated mass to add to this SPM size class
                    me%m_transformed(n,1,s+2) = me%m_transformed(n,1,s+2) + dm_hetero
                else
                    dm_hetero = 0.0_dp
                end if
            end do

        end do
    end function

    function dissolutionReactor1(me) result(rslt)
        class(Reactor1) :: me
        type(Result)    :: rslt
        real(dp)        :: dm_diss(C%npDim(1), C%npDim(2), C%npDim(3))  ! Mass of NM dissolving on each time step [kg/timestep]
        ! Dissolution of pristine NM
        dm_diss = min(me%k_diss_pristine * C%timeStep * me%m_np, me%m_np)
        me%m_np = me%m_np - dm_diss
        me%m_dissolved = me%m_dissolved + sum(dm_diss)
        ! Dissolution of transformed NM
        dm_diss = min(me%k_diss_transformed * C%timeStep * me%m_transformed, me%m_transformed)
        me%m_transformed = me%m_transformed - dm_diss
        me%m_dissolved = me%m_dissolved + sum(dm_diss)
    end function

    function transformationReactor1(me) result(rslt)
        class(Reactor1) :: me
        type(Result)    :: rslt
        real(dp)        :: dm_transform(C%npDim(1), C%npDim(2), C%npDim(3))
        
        ! Transformation (e.g. sulphidation) of pristine NM
        dm_transform = min(me%k_transform_pristine * C%timeStep * me%m_np, me%m_np)
        me%m_np = me%m_np - dm_transform
        me%m_transformed = me%m_transformed + dm_transform

    end function
    
    !> Parse the input data for this Reactor
    function parseInputDataReactor1(me) result(r)
        class(Reactor1) :: me
        type(Result) :: r
        
        me%k_diss_pristine = DATASET%water_k_diss_pristine
        me%k_diss_transformed = DATASET%water_k_diss_transformed
        me%k_transform_pristine = DATASET%water_k_transform_pristine
        ! TODO: Is there actually going to be input data,
        ! or is this going to be passed from RiverReach?
    end function
    
    !> Calculate the collision rate between NPs and SPM.
    !! Reference: [Praetorious et al, 2012](http://dx.doi.org/10.1021/es204530n)
    function calculateCollisionRateReactor1(me, T_water, G, W_settle_np, W_settle_spm) result(k_coll)
        class(Reactor1) :: me                       !! This `Reactor1` instance
        real            :: T_water                  !! Temperature of the water [deg C]
        real            :: G                        !! Shear rate [/s]
        real(dp)        :: W_settle_np(:)           !! NP settling velocity [m/s]
        real(dp)        :: W_settle_spm(:)          !! SPM settling velocity [m/s]
        real(dp)        :: k_coll(C%nSizeClassesNM, C%nSizeClassesSpm)      !! The collision frequency to return [/s]
        integer         :: n, s                     ! Iterators for SPM and NP size classes
        
        do s = 1, C%nSizeClassesSpm
            do n = 1, C%nSizeClassesNM
                k_coll(n,s) = (2*C%k_B*(T_water+273.15_dp)/(3*C%mu_w(T_water))) &
                              * (C%d_spm(s)/2 + C%d_nm(n)/2)**2/((C%d_spm(s)/2)*(C%d_nm(n)/2)) &
                              + (4.0_dp/3.0_dp)*G*(C%d_nm(n)/2 + C%d_spm(s)/2)**3 &
                              + C%pi*(C%d_spm(s)/2+C%d_nm(n)/2)**2 &
                              * abs(W_settle_np(n) - W_settle_spm(s))
            end do
        end do
        
    end function

    !> Calculate a particle concentration from a mass concentration
    function calculateParticleConcentrationReactor1(me, C_mass, rho_particle, d) result(C_particle)
        class(Reactor1) :: me
        real(dp) :: C_mass
        real :: rho_particle
        real :: d
        real(dp) :: C_particle
        C_particle = C_mass / (rho_particle*(4.0_dp/3.0_dp)*C%pi*(d/2)**3)
    end function
    
end module

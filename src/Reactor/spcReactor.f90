module spcReactor
    use Globals
    implicit none
    
    !> A `Reactor` objects deals with nanoparticle transformations
    !! within any environmental compartment
    type, abstract, public :: Reactor
        character(len=100) :: ref
        integer :: x
        integer :: y
        real(dp), allocatable :: m_np(:,:,:)
            !! Matrix of NP masses, each element representing a different NP size class (1st dimension),
            !! state (2nd dimension) and form (3rd dimension). States: free, bound to solid, heteroaggreated
            !! (per SPM size class). Forms: core, shell, coating, corona.
        real(dp), allocatable :: m_ionic(:)
            !! Array of ionic metal masses: Free ion, solution, adsorbed.
        real(dp), allocatable :: C_np_free_particle(:)      !! Particle concentration of free NPs
        real(dp) :: T_water                     !! Temperature of the water [C]
        real(dp), allocatable :: W_settle_np(:) !! NP settling velocity [m/s]
        real(dp), allocatable :: W_settle_spm(:)!! SPM settling velocity [m/s]
        real(dp) :: G                           !! Shear rate of the water [s-1]
        real(dp) :: alpha_hetero                !! Attachment efficiency, 0-1 [-]
        real(dp), allocatable :: k_hetero(:,:)
            !! Heteroaggregation rate constant [s-1]. 1st dimension: NP size class.
            !! 2nd dimesion: SPM size class
        real(dp) :: rho_np ! HACK: This needs to be set based on density of the specific NP in question
        real(dp), allocatable :: individualNPMass(:)    !! Mass of individual nanoparticles
        real(dp), allocatable :: C_spm_particle(:)      !! Particle concentration of SPM [m-3]
        real(dp) :: volume                              !! Volume of the container the Reactor is in [m3]
        
        contains
        procedure(createReactor), deferred :: create
        procedure(updateReactor), deferred :: update
        ! Processes
        procedure(heteroaggregationReactor), deferred :: heteroaggregation
        procedure(parseInputDataReactor), deferred :: parseInputData
        ! Calculators
        procedure(calculateCollisionRateReactor), deferred :: calculateCollisionRate
        procedure(calculateParticleConcentrationReactor), deferred :: calculateParticleConcentration
    end type
      
    abstract interface
        
        !> Run initialising procedures for the `Reactor` object
        function createReactor(me, x, y, alpha_hetero) result(r)
            use Globals
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me                !! This `Reactor` object
            integer :: x                        !! The containing `GridCell` x reference
            integer :: y                        !! The containing `GridCell` y reference
            real(dp) :: alpha_hetero            !! Attachment efficiency, 0-1 [-]
            type(Result) :: r                   !! The `Result` object to 
        end function
    
        !> Run the `Reactor`'s simulation for the current time step
        function updateReactor(me, t, m_np, C_spm, T_water, W_settle_np, W_settle_spm, G, volume, Q_out) result(r)
            use Globals
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me                        !! This `Reactor1` object
            integer :: t                                !! The current time step
            real(dp) :: m_np(C%nSizeClassesNP, 4, 2 + C%nSizeClassesSpm)     !! Mass of NP for this timestep [kg]
            real(dp) :: C_spm(C%nSizeClassesSpm)        !! The current mass concentration of SPM [kg/m3]
            real(dp) :: T_water                         !! The current water temperature [C]
            real(dp) :: W_settle_np(C%nSizeClassesNP)   !! NP settling velocity [m/s]
            real(dp) :: W_settle_spm(C%nSizeClassesSpm) !! SPM settling velocity [m/s]
            real(dp) :: G                               !! Shear rate [s-1]
            real(dp) :: volume                          !! `RiverReach` volume on this timestep [s-1]
            real(dp) :: Q_out                           !! Outflow for the containing water body [m3/timestep]
            type(Result) :: r
        end function
        
        function finaliseUpdateReactor(me) result(r)
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me
            type(Result) :: r
        end function
        
        !> Perform the heteroaggregation calculation for this time step
        function heteroaggregationReactor(me) result(r)
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me
            type(Result) :: r
        end function
        
        !> Parse the input data for this Reactor object
        function parseInputDataReactor(me) result(r)
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me
            type(Result) :: r
        end function
        
        !> Calculate the collision rate of NPs to SPM
        function calculateCollisionRateReactor(me, T_water, G, W_settle_np, W_settle_spm) result(k_coll)
            use Globals
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me
            real(dp) :: T_water             !! Temperature of the water [C]
            real(dp) :: G                   !! Shear rate [s-1]
            real(dp) :: W_settle_np(:)      !! NP settling velocity [m/s]
            real(dp) :: W_settle_spm(:)     !! SPM settling velocity [m/s]
            real(dp) :: k_coll(C%nSizeClassesNp,C%nSizeClassesSpm)
        end function

        !> Calculate a particle concentration from a mass concentration
        function calculateParticleConcentrationReactor(me, C_mass, rho_particle, d) result(C_particle)
            use Globals
            import Reactor
            class(Reactor) :: me
            real(dp) :: C_mass
            real(dp) :: rho_particle
            real(dp) :: d
            real(dp) :: C_particle
        end function
    
    end interface

end module
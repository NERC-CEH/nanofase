module spcReactor
    use Globals
    implicit none
    
    !> A `Reactor` objects deals with nanoparticle transformations
    !! within any environmental compartment
    type, abstract, public :: Reactor
        character(len=100) :: ref
        integer :: x
        integer :: y
        real(dp), allocatable :: m_np(:,:)
            !! Array of NP masses, each element representing a different state (1st dimension)
            !! and form (2nd dimension). States: free, heteroaggreated (per SPM size class).
            !! Forms: core, shell, coating, corona, adsorbed, dissolved
        
        contains
        procedure(createReactor), deferred :: create
        procedure(updateReactor), deferred :: update
    end type
      
    abstract interface
        
        !> Run initialising procedures for the `Reactor` object
        function createReactor(me, x, y) result(r)
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me                !! This `Reactor` object
            integer :: x                        !! The containing `GridCell` x reference
            integer :: y                        !! The containing `GridCell` y reference
            type(Result) :: r                   !! The `Result` object to 
        end function
    
        !> Run the `Reactor`'s simulation for the current time step
        function updateReactor(me, t) result(r)
            use ResultModule, only: Result
            import Reactor
            class(Reactor) :: me                !! This `Reactor` object
            integer :: t                        !! The current time step
            type(Result) :: r                   !! The `Result` object to return
        end function
    
    end interface

end module
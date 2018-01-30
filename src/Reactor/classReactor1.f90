module classReactor1
    use Globals
    use ResultModule
    use spcReactor

    implicit none
    
    type, public, extends(Reactor) :: Reactor1
        contains
        procedure :: create => createReactor1
        procedure :: update => updateReactor1
    end type
        
    contains
    
    !> Run initialising procedures for the `Reactor` object
    function createReactor1(me, x, y) result(r)
        class(Reactor1) :: me           !! This `Reactor1` object
        integer :: x                    !! The containing `GridCell` x reference
        integer :: y                    !! The containing `GridCell` x reference
        type(Result) :: r               !! The `Result` object to return
        
        ! Allocate the NP mass matrix to correct number of state/form elements.
        ! States: 1. free, 2. bound to solid, 3+ heteroaggreated (per SPM size class). 
        ! Forms: 1. core, 2. shell, 3. coating, 4. corona.
        allocate(me%m_np( &
            C%nSizeClassesNP, &             ! Number of NP size classes
            4, &                            ! Number of different forms
            C%nSizeClassesSpm + 2 &         ! Number of different states
        ))
        ! Allocate dissolved metal array. 1. free ion, 2. solution, 3. adsorbed.
        allocate(me%m_ionic(3))
    end function
    
    !> Run the `Reactor`'s simulation for the current time step
    function updateReactor1(me, t) result(r)
        class(Reactor1) :: me           !! This `Reactor1` object
        integer :: t                    !! The current time step
        type(Result) :: r               !! The `Result` object to return
    end function
    
end module

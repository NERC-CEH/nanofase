module classEstuaryReach1
    use Globals
    use ResultModule
    use spcEstuaryReach
    implicit none
    
    type, public, extends(EstuaryReach) :: EstuaryReach1
        
      contains
        procedure :: create => createEstuaryReach1
        procedure :: update => updateEstuaryReach1
    end type
    
  contains
    
    function createEstuaryReach1(me) result(r)
        class(EstuaryReach1) :: me
        type(Result) :: r
        ! Create stuff
    end function
    
    function updateEstuaryReach1(me, t) result(r)
        class(EstuaryReach1) :: me
        integer :: t
        type(Result) :: r
        ! Update stuff
    end function
    
end module
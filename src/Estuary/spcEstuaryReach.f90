module spcEstuaryReach
    use ResultModule
    implicit none
    

    type, public, abstract :: EstuaryReach
    
      contains
        procedure(createEstuaryReach), deferred :: create
        procedure(updateEstuaryReach), deferred :: update
    end type
    
    abstract interface
        function createEstuaryReach(me) result(r)
            use ResultModule
            import EstuaryReach, Result
            class(EstuaryReach) :: me
            type(Result) :: r
        end function
        
        function updateEstuaryReach(me, t) result(r)
            use ResultModule
            import EstuaryReach, Result
            class(EstuaryReach) :: me
            integer :: t
            type(Result) :: r
        end function
        
    end interface
    
    
end module
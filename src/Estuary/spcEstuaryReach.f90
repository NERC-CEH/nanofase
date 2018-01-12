module spcEstuaryReach
	use ResultModule
	implicit none
	

	type, public :: EstuaryReach
		integer :: integervariable = 1
	
	  contains
	    ! Create/destory
          procedure(createEstuaryReach), deferred :: create
          ! Simulators
          procedure(updateEstuaryReach), deferred :: update
	end type
	
      abstract interface
	    function createEstuaryReach(me) result(r)
		    import EstuaryReach, Result
		    class(EstuaryReach) :: me
			type(Result) :: r
		end function
		
		function updateEstuaryReach(me, t) result(r)
		    import EstuaryReach, Result
		    class(EstuaryReach) :: me
			integer :: t
			type(Result) :: r
		end function
		
	end abstract interface
	
	
end module
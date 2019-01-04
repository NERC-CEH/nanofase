module spcEstuaryReach
    use spcWaterBody
    implicit none
    

    type, public, abstract, extends(WaterBody) :: EstuaryReach
    
        integer :: a = 1
    end type
      
    type, public :: EstuaryReachElement
        class(EstuaryReach), allocatable :: item    
    end type
    
end module
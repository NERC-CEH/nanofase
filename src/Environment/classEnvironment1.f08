module classEnvironment1
    use Globals
    use ResultModule
    implicit none
    private

    type, public :: Environment1
        private

      contains
        procedure :: create => createEnvironment1
        procedure, public :: getQ
        procedure, public :: getSpm
    end type

  contains
    !> Create the environment, which sets up the grid and river structure:
    !!  - Get grid structure from data file and set up GridCell collection
    function createEnvironment1(me) result(r)
        class(Environment1) :: me
        type(Result) :: r

        ! Set up the grid structure
    end function

    !> DUMMY FUNCTION for the time being.
    function getQ(me, gridX, gridY, subRiverN) result(r)
        class(Environment1) :: me
        integer :: gridX
        integer :: gridY
        integer :: subRiverN
        type(Result0D) :: r
        r = Result(data=1.0_dp)
    end function

    !> DUMMY FUNCTION for the time being.
    function getSpm(me, gridX, gridY, subRiverN, iSizeClass) result(r)
        class(Environment1) :: me
        integer :: gridX
        integer :: gridY
        integer :: subRiverN
        integer :: iSizeClass
        type(Result0D) :: r
        r = Result(data=1.0_dp)
    end function
end module
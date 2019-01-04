module classEstuaryReach1
    use Globals
    use ResultModule
    use spcEstuaryReach
    implicit none
    
    type, public, extends(EstuaryReach) :: EstuaryReach1
        
      contains
        procedure :: create => createEstuaryReach1
        procedure :: destroy => destroyEstuaryReach1
        procedure :: update => updateEstuaryReach1
        procedure :: finaliseUpdate => finaliseUpdateEstuaryReach1
        procedure :: parseInputData => parseInputDataEstuaryReach1
    end type
    
  contains
    
    function createEstuaryReach1(me, x, y, i, gridCellArea) result(rslt)
        class(EstuaryReach1) :: me
        integer :: x, y, i
        real(dp) :: gridCellArea
        type(Result) :: rslt
        ! Create stuff
    end function

    function destroyEstuaryReach1(me) result(rslt)
        class(EstuaryReach1) :: me
        type(Result) :: rslt
        ! Destroy stuff
    end function
    
    function updateEstuaryReach1(me, t, j_spm_runoff, j_np_runoff) result(rslt)
        class(EstuaryReach1) :: me
        integer :: t
        real(dp), optional :: j_spm_runoff(:)                   !! Eroded sediment runoff to this water body [kg/timestep]
        real(dp), optional :: j_np_runoff(:,:,:)                !! Eroded NP runoff to this water body [kg/timestep]
        type(Result) :: rslt
        ! Update stuff
    end function

    !> Set temporary flow variables to real flow variables
    function finaliseUpdateEstuaryReach1(me) result(rslt)
        class(EstuaryReach1) :: me
        type(Result) :: rslt
    end function

    function parseInputDataEstuaryReach1(me) result(rslt)
        class(EstuaryReach1) :: me
        type(Result) :: rslt
        ! Parse input data
    end function
    
end module
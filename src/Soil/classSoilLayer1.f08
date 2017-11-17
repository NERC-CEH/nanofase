!> Module containing definition of SoilLayer1 class.
module classSoilLayer1
    use Globals
    use UtilModule
    use spcSoilLayer
    implicit none

    !> SoilLayer is responsible for routing percolation through
    !! the SoilProfile in which it is contained.
    type, public, extends(SoilLayer) :: SoilLayer1
      contains
        procedure :: create => createSoilLayer1
        procedure :: destroy => destroySoilLayer1
        procedure :: update => updateSoilLayer1
        procedure :: parseInputData => parseInputDataSoilLayer1
    end type

  contains
    !> Create this SoilLayer
    function createSoilLayer1(me, x, y, p, l) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        integer, intent(in) :: x                        !! Containing GridCell x index
        integer, intent(in) :: y                        !! Containing GridCell y index
        integer, intent(in) :: p                        !! Containing SoilProfile index
        integer, intent(in) :: l                        !! Layer index
        type(Result) :: r                               !! The Result object to return

        ! Set the metadata
        me%x = x
        me%y = y 
        me%p = p
        me%l = l
        me%ref = ref("SoilLayer", x, y, p, l)

        ! Parse the input data into the object properties
        r = me%parseInputData()
        print *, me%depth

        ! Add this procedure to the Result trace
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Destroy this SoilLayer
    function destroySoilLayer1(me) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        type(Result) :: r                               !! The Result object to return
    end function

    !> Update the SoilLayer on a given timestep \( t \)
    function updateSoilLayer1(me, t) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        integer :: t                                    !! The current timestep \( t \)
        type(Result) :: r                               !! The Result object to return
        
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including allocation of arrays that depend on
    !! input data
    function parseInputDataSoilLayer1(me) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        type(Result) :: r
            !! The Result object to return any errors relating to the input data file
        type(NcDataset) :: nc                           ! NetCDF dataset
        type(NcVariable) :: var                         ! NetCDF variable
        type(NcGroup) :: grp                            ! NetCDF group

        ! Open the dataset
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")                        ! Get the Environment group
        grp = grp%getGroup(ref("GridCell",me%x,me%y))           ! Get the containing GridCell's group
        grp = grp%getGroup(ref("SoilProfile",me%x,me%y,me%p))  ! Get the containing SoilProfile's group
        me%ncGroup = grp%getGroup("SoilLayer_" // trim(str(me%l)))  ! Get this SoilLayer's group

        ! Get the depth of the SoilLayer, if present, otherwise default
        ! without warning
        if (me%ncGroup%hasVariable('depth')) then
            var = me%ncGroup%getVariable('depth')
            call var%getData(me%depth)
        else
            me%depth = C%defaultSoilLayerDepth
        end if

    end function

end module
!> Module containing definition of `SoilLayer1` class.
module classSoilLayer1
    use Globals
    use UtilModule
    use spcSoilLayer
    implicit none

    !> `SoilLayer1` is responsible for routing percolation through
    !! the `SoilProfile` in which it is contained.
    type, public, extends(SoilLayer) :: SoilLayer1
      contains
        procedure :: create => createSoilLayer1
        procedure :: destroy => destroySoilLayer1
        procedure :: update => updateSoilLayer1
        procedure :: addPooledWater => addPooledWaterSoilLayer1
        procedure :: parseInputData => parseInputDataSoilLayer1
    end type

  contains
    !> Create this `SoilLayer` and call the input data parsing procedure
    function createSoilLayer1(me, x, y, p, l, WC_sat, WC_FC, K_s) result(r)
        class(SoilLayer1) :: me                         !! This `SoilLayer1` instance
        integer, intent(in) :: x                        !! Containing `GridCell` x index
        integer, intent(in) :: y                        !! Containing `GridCell` y index
        integer, intent(in) :: p                        !! Containing `SoilProfile` index
        integer, intent(in) :: l                        !! Layer index
        real(dp), intent(in) :: WC_sat                  !! Water content at saturation [m3/m3]
        real(dp), intent(in) :: WC_FC                   !! Water content at field capacity [m3/m3]
        real(dp), intent(in) :: K_s                     !! Saturated hydraulic conductivity [m/s]
        type(Result) :: r                               !! The `Result` object to return, with any errors

        ! Set the metadata
        me%x = x
        me%y = y 
        me%p = p
        me%l = l
        me%ref = ref("SoilLayer", x, y, p, l)

        ! Parse the input data into the object properties
        r = me%parseInputData()

        ! Set saturation and field capacity volumes [m3/m2] based on depth of layer
        me%V_sat = WC_sat*me%depth
        me%V_FC = WC_FC*me%depth
        me%K_s = K_s                                    ! Hydraulic conductivity [m/s]
        me%V_w = 0                                      ! Set initial water content to 0 [m3/m2]

        ! Add this procedure to the Result trace
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Destroy this `SoilLayer1`
    function destroySoilLayer1(me) result(r)
        class(SoilLayer1) :: me                         !! This `SoilLayer1` instance
        type(Result) :: r                               !! The `Result` object to return
    end function

    !> Update the `SoilLayer1` on a given time step, based on specified inflow.
    !! Calculate percolation to next layer and, if saturated, the amount
    !! to pool to the above layer (or surface runoff, if this is the top layer)
    function updateSoilLayer1(me, t, Q_in) result(r)
        class(SoilLayer1) :: me                         !! This `SoilLayer1` instance
        integer :: t                                    !! The current time step [s]
        real(dp) :: Q_in                                !! Water into the layer on this time step, from percolation and pooling [m3 s-1]
        type(Result) :: r
            !! The `Result` object to return. Contains warning if all water on this time step removed.
        
        ! Setting volume of water, pooled water and excess water, based on inflow
        if (me%V_w + Q_in*C%timeStep < me%V_sat) then           ! If water volume below V_sat after inflow
            me%V_pool = 0                                       ! No pooled water
            me%V_w = me%V_w + Q_in*C%timeStep                   ! Update the volume based on inflow
            me%V_excess = max(me%V_w - me%V_FC, 0.0_dp)         ! Volume of water above V_FC
        else if (me%V_w + Q_in*C%timeStep > me%V_sat) then      ! Else, water pooled above V_sat
            me%V_pool = me%V_w + Q_in*C%timeStep - me%V_sat     ! Water pooled above V_sat
            me%V_w = me%V_sat                                   ! Volume of water must be V_sat
            me%V_excess = me%V_w - me%V_FC                      ! Volume must be above FC and so there is excess
        end if
        ! Calculate volume percolated on this timestep [m3 m-2]
        me%V_perc = min(me%V_excess * &                          
                        (1-exp(-C%timeStep*me%K_s/(me%V_sat-me%V_FC))), &   ! Up to a maximum of V_w
                        me%V_w)
        me%V_w = me%V_w - me%V_perc                              ! Get rid of the percolated water

        ! Emit a warning if all water removed
        call r%addError( &
            ERROR_HANDER%nonZero( &
                value = me%V_w, &
                message = "All water removed from SoilProfile.", &
                isCritical = .false. &
            ) &
        )
        ! Add this procedure to the error trace
        call r%addToTrace("Updating " // trim(me%ref) // " on time step #" // trim(str(t)))
    end function

    !> Add a volume \( V_{\text{pool}} \) of pooled water to the layer.
    !! No percolation occurs as pooled water never really leaves the `SoilLayer`.
    function addPooledWaterSoilLayer1(me, V_pool) result(r)
        class(SoilLayer1) :: me                         !! This SoilLayer1 instance
        real(dp) :: V_pool                              !! Volume of pooled water to add, \( V_{\text{pool}} \) [m3/m2]
        type(Result) :: r                               !! The Result object to return, with no data
        if (me%V_w + V_pool < me%V_sat) then            ! If the pooled water doesn't push volume above saturation, just add it
            me%V_w = me%V_w + V_pool
            me%V_pool = 0
        else                                            ! Else, add as much as possible and then pool the rest
            me%V_w = me%V_sat                           ! Volume of water must be at saturation
            me%V_pool = me%V_w + V_pool - V%sat         ! Add pooled water
        end if
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
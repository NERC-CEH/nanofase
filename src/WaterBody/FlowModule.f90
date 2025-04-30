!> The FlowModule contains types which define material flows within the surface water network.
!! Separate types are provided for water, SPM, NM and dissolved species flows.
module FlowModule
    use GlobalsModule, only: dp, C
    use ContaminantModule
    use ResultModule
    use ErrorInstanceModule
    use ErrorHandlerModule
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    implicit none

    type, public :: WaterFlows
        real(dp) :: inflow
        real(dp) :: runoff
        real(dp) :: transfers
        real(dp) :: demands
        real(dp) :: outflow
    contains
        procedure :: init => initWaterFlows
        procedure :: empty => emptyWaterFlows
        procedure :: addInflow => addInflowWaterFlows
        procedure :: asArray => asArrayWaterFlows
        procedure :: assignWaterFlows
        generic :: assignment(=) => assignWaterFlows
    end type

    type, public :: SPMFlows
        real(dp), allocatable :: inflow(:)
        real(dp), allocatable :: soilErosion(:)
        real(dp), allocatable :: bankErosion(:)
        real(dp), allocatable :: transfers(:)
        real(dp), allocatable :: demands(:)
        real(dp), allocatable :: deposition(:)
        real(dp), allocatable :: resuspension(:)
        real(dp), allocatable :: outflow(:)
    contains
        procedure :: init => initSPMFlows
        procedure :: empty => emptySPMFlows
        procedure :: addInflow => addInflowSPMFlows
        procedure :: asArray => asArraySPMFlows
        procedure :: assignSPMFlows
        generic :: assignment(=) => assignSPMFlows
    end type

contains

    subroutine initWaterFlows(me)
        class(WaterFlows), intent(inout) :: me
        call me%empty()
    end subroutine

    subroutine addInflowWaterFlows(me, q_in)
        class(WaterFlows), intent(inout) :: me
        real(dp), intent(in) :: q_in
        me%inflow = me%inflow + q_in
    end subroutine

    subroutine initSPMFlows(me)
        class(SPMFlows), intent(inout) :: me
        allocate(me%inflow(C%nSizeClassesSpm))
        allocate(me%soilErosion(C%nSizeClassesSpm))
        allocate(me%bankErosion(C%nSizeClassesSpm))
        allocate(me%transfers(C%nSizeClassesSpm))
        allocate(me%demands(C%nSizeClassesSpm))
        allocate(me%deposition(C%nSizeClassesSpm))
        allocate(me%resuspension(C%nSizeClassesSpm))
        allocate(me%outflow(C%nSizeClassesSpm))
        call me%empty()
    end subroutine

    subroutine addInflowSPMFlows(me, j_in)
        class(SPMFlows), intent(inout) :: me
        real(dp), intent(in) :: j_in(:)
        type(ErrorInstance) :: err(1)
        if (size(j_in) /= C%nSizeClassesSpm) then
            err(1) = ErrorInstance(code=900, message="Size mismatch in SPMFlows addInflow")
            call LOGR%toFile(errors=err)
            error stop "Critical error in addInflowSPMFlows"
        end if
        me%inflow = me%inflow + j_in
    end subroutine

    subroutine emptyWaterFlows(me)
        class(WaterFlows), intent(inout) :: me
        me%inflow = 0.0_dp
        me%runoff = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%outflow = 0.0_dp
    end subroutine

    subroutine emptySPMFlows(me)
        class(SPMFlows), intent(inout) :: me
        if (allocated(me%inflow)) me%inflow = 0.0_dp
        if (allocated(me%soilErosion)) me%soilErosion = 0.0_dp
        if (allocated(me%bankErosion)) me%bankErosion = 0.0_dp
        if (allocated(me%transfers)) me%transfers = 0.0_dp
        if (allocated(me%demands)) me%demands = 0.0_dp
        if (allocated(me%deposition)) me%deposition = 0.0_dp
        if (allocated(me%resuspension)) me%resuspension = 0.0_dp
        if (allocated(me%outflow)) me%outflow = 0.0_dp
    end subroutine

    function asArrayWaterFlows(me) result(arr)
        class(WaterFlows), intent(in) :: me
        real(dp) :: arr(5)
        arr = [me%inflow, me%runoff, me%transfers, me%demands, me%outflow]
    end function

    function asArraySPMFlows(me) result(arr)
        class(SPMFlows), intent(in) :: me
        real(dp) :: arr(8,C%nSizeClassesSpm)
        arr(1,:) = me%inflow
        arr(2,:) = me%soilErosion
        arr(3,:) = me%bankErosion
        arr(4,:) = me%transfers
        arr(5,:) = me%demands
        arr(6,:) = me%deposition
        arr(7,:) = me%resuspension
        arr(8,:) = me%outflow
    end function

    subroutine assignWaterFlows(obj, arr)
        class(WaterFlows), intent(out) :: obj
        real(dp), intent(in) :: arr(5)
        obj%inflow = arr(1)
        obj%runoff = arr(2)
        obj%transfers = arr(3)
        obj%demands = arr(4)
        obj%outflow = arr(5)
    end subroutine

    subroutine assignSPMFlows(obj, arr)
        class(SPMFlows), intent(out) :: obj
        real(dp), intent(in) :: arr(8,C%nSizeClassesSpm)
        obj%inflow = arr(1,:)
        obj%soilErosion = arr(2,:)
        obj%bankErosion = arr(3,:)
        obj%transfers = arr(4,:)
        obj%demands = arr(5,:)
        obj%deposition = arr(6,:)
        obj%resuspension = arr(7,:)
        obj%outflow = arr(8,:)
    end subroutine
end module
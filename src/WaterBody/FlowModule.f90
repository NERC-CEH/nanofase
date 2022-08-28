!> The FlowModule contains types which define material flows within the surface water network.
!! Separate types are provided for water, SPM, NM and dissolved species flows.
module FlowModule
    use GlobalsModule, only: dp, C
    implicit none

    !> The WaterFlows object stores information on water flows in and out of a reach.
    type, public :: WaterFlows
        real(dp) :: inflow
        real(dp) :: runoff
        real(dp) :: transfers
        real(dp) :: demands
        real(dp) :: outflow
      contains
        procedure :: init => initWaterFlows
        procedure :: empty => emptyWaterFlows
        procedure :: asArray => asArrayWaterFlows
        procedure :: assignWaterFlows
        generic :: assignment(=) => assignWaterFlows
    end type

    !> The SPMFlows object stores information on SPM flows in and out of a reach.
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
        procedure :: asArray => asArraySPMFlows
        procedure :: assignSPMFlows
        generic :: assignment(=) => assignSPMFlows
    end type

    !> The NMFlows object stores information on NM flows in and out of a reach.
    type, public :: NMFlows
        real(dp), allocatable :: inflow(:,:,:)
        real(dp), allocatable :: soilErosion(:,:,:)
        real(dp), allocatable :: bankErosion(:,:,:)
        real(dp), allocatable :: transfers(:,:,:)
        real(dp), allocatable :: demands(:,:,:)
        real(dp), allocatable :: deposition(:,:,:)
        real(dp), allocatable :: resuspension(:,:,:)
        real(dp), allocatable :: outflow(:,:,:)
        real(dp), allocatable :: pointSources(:,:,:)
        real(dp), allocatable :: diffuseSources(:,:,:)
      contains
        procedure :: init => initNMFlows
        procedure :: empty => emptyNMFlows
        procedure :: asArray => asArrayNMFlows
        procedure :: assignNMFlows
        generic :: assignment(=) => assignNMFlows
    end type

    !> The DissolvedFlows object stores information on dissolved species flows in and out of a reach.
    type, public :: DissolvedFlows
        real(dp), allocatable :: inflow
        real(dp), allocatable :: transfers
        real(dp), allocatable :: demands
        real(dp), allocatable :: outflow
        real(dp), allocatable :: pointSources
        real(dp), allocatable :: diffuseSources
      contains
        procedure :: init => initDissolvedFlows
        procedure :: empty => emptyDissolvedFlows
        procedure :: asArray => asArrayDissolvedFlows
        procedure :: assignDissolvedFlows
        generic :: assignment(=) => assignDissolvedFlows
    end type

  contains

    subroutine initWaterFlows(me)
        class(WaterFlows) :: me
        call me%empty()
    end subroutine

    subroutine initSPMFlows(me)
        class(SPMFlows) :: me
        allocate(me%inflow(C%nSizeClassesSPM))
        allocate(me%soilErosion(C%nSizeClassesSPM))
        allocate(me%bankErosion(C%nSizeClassesSPM))
        allocate(me%transfers(C%nSizeClassesSPM))
        allocate(me%demands(C%nSizeClassesSPM))
        allocate(me%deposition(C%nSizeClassesSPM))
        allocate(me%resuspension(C%nSizeClassesSPM))
        allocate(me%outflow(C%nSizeClassesSPM))
        call me%empty()
    end subroutine

    subroutine initNMFlows(me)
        class(NMFlows) :: me
        allocate(me%inflow(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%soilErosion(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%bankErosion(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%transfers(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%demands(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%deposition(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%resuspension(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%outflow(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%pointSources(C%npDim(1),C%npDim(2),C%npDim(3)))
        allocate(me%diffuseSources(C%npDim(1),C%npDim(2),C%npDim(3)))
        call me%empty()
    end subroutine

    subroutine initDissolvedFlows(me)
        class(DissolvedFlows) :: me
        call me%empty()
    end subroutine

    subroutine emptyWaterFlows(me)
        class(WaterFlows) :: me
        me%inflow = 0.0_dp
        me%runoff = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%outflow = 0.0_dp
    end subroutine

    subroutine emptySPMFlows(me)
        class(SPMFlows) :: me
        me%inflow = 0.0_dp
        me%soilErosion = 0.0_dp
        me%bankErosion = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%deposition = 0.0_dp
        me%resuspension = 0.0_dp
        me%outflow = 0.0_dp
    end subroutine

    subroutine emptyNMFlows(me)
        class(NMFlows) :: me
        me%inflow = 0.0_dp
        me%soilErosion = 0.0_dp
        me%bankErosion = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%deposition = 0.0_dp
        me%resuspension = 0.0_dp
        me%outflow = 0.0_dp
        me%pointSources = 0.0_dp
        me%diffuseSources = 0.0_dp
    end subroutine

    subroutine emptyDissolvedFlows(me)
        class(DissolvedFlows) :: me
        me%inflow = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%outflow = 0.0_dp
        me%diffuseSources = 0.0_dp
        me%pointSources = 0.0_dp
    end subroutine

    function asArrayWaterFlows(me) result(arr)
        class(WaterFlows)   :: me
        real(dp)            :: arr(5)
        arr = [me%inflow, me%runoff, me%transfers, me%demands, me%outflow]
    end function

    function asArraySPMFlows(me) result(arr)
        class(SPMFlows) :: me
        real(dp)        :: arr(8,C%nSizeClassesSpm)
        arr(1,:) = me%inflow
        arr(2,:) = me%soilErosion
        arr(3,:) = me%bankErosion
        arr(4,:) = me%transfers
        arr(5,:) = me%demands
        arr(6,:) = me%deposition
        arr(7,:) = me%resuspension
        arr(8,:) = me%outflow
    end function

    function asArrayNMFlows(me) result(arr)
        class(NMFlows)  :: me
        real(dp)        :: arr(10,C%npDim(1),C%npDim(2),C%npDim(3))
        arr(1,:,:,:) = me%inflow
        arr(2,:,:,:) = me%soilErosion
        arr(3,:,:,:) = me%bankErosion
        arr(4,:,:,:) = me%transfers
        arr(5,:,:,:) = me%demands
        arr(6,:,:,:) = me%deposition
        arr(7,:,:,:) = me%resuspension
        arr(8,:,:,:) = me%outflow
        arr(9,:,:,:) = me%pointSources
        arr(10,:,:,:) = me%diffuseSources
    end function

    function asArrayDissolvedFlows(me) result(arr)
        class(DissolvedFlows)   :: me
        real(dp)                :: arr(6)
        arr = [me%inflow, me%transfers, me%demands, me%outflow, me%pointSources, me%diffuseSources]
    end function

    subroutine assignWaterFlows(obj, arr)
        class(WaterFlows), intent(out)  :: obj
        real(dp), intent(in)            :: arr(5)
        obj%inflow = arr(1)
        obj%runoff = arr(2)
        obj%transfers = arr(3)
        obj%demands = arr(4)
        obj%outflow = arr(5)
    end subroutine

    subroutine assignSPMFlows(obj, arr)
        class(SPMFlows), intent(out)    :: obj
        real(dp), intent(in)            :: arr(8,C%nSizeClassesSpm)
        obj%inflow = arr(1,:)
        obj%soilErosion = arr(2,:)
        obj%bankErosion = arr(3,:)
        obj%transfers = arr(4,:)
        obj%demands = arr(5,:)
        obj%deposition = arr(6,:)
        obj%resuspension = arr(7,:)
        obj%outflow = arr(8,:)
    end subroutine

    subroutine assignNMFlows(obj, arr)
        class(NMFlows), intent(out) :: obj
        real(dp), intent(in)        :: arr(10,C%npDim(1),C%npDim(2),C%npDim(3))
        obj%inflow = arr(1,:,:,:)
        obj%soilErosion = arr(2,:,:,:)
        obj%bankErosion = arr(3,:,:,:)
        obj%transfers = arr(4,:,:,:)
        obj%demands = arr(5,:,:,:)
        obj%deposition = arr(6,:,:,:)
        obj%resuspension = arr(7,:,:,:)
        obj%outflow = arr(8,:,:,:)
        obj%pointSources = arr(9,:,:,:)
        obj%diffuseSources = arr(10,:,:,:)
    end subroutine

    subroutine assignDissolvedFlows(obj, arr)
        class(DissolvedFlows), intent(out)  :: obj
        real(dp), intent(in)                :: arr(6)
        obj%inflow = arr(1)
        obj%transfers = arr(2)
        obj%demands = arr(3)
        obj%outflow = arr(4)
        obj%pointSources = arr(5)
        obj%diffuseSources = arr(6)
    end subroutine

end module
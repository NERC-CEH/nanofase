!> The FlowModule contains types which define material flows within the surface water network.
!! P-FASE version: water and SPM flows remain physical carrier flows, while
!! contaminant flows are PFAS species x form x phase Contaminant objects.
module FlowModule
    use GlobalsModule, only: dp, C
    use ContaminantModule
    use ResultModule
    use ErrorInstanceModule
    use LoggerModule, only: LOGR
    implicit none

    type, public :: ContaminantFlows
        type(Contaminant) :: inflow
        type(Contaminant) :: soilErosion
        type(Contaminant) :: bankErosion
        type(Contaminant) :: transfers
        type(Contaminant) :: demands
        type(Contaminant) :: deposition
        type(Contaminant) :: resuspension
        type(Contaminant) :: outflow
        type(Contaminant) :: pointSources
        type(Contaminant) :: diffuseSources
        type(Contaminant) :: foam
        type(Contaminant) :: atmosphere
        type(Contaminant) :: biota
    contains
        procedure :: init => initContaminantFlows
        procedure :: empty => emptyContaminantFlows
        procedure :: finalise => finaliseContaminantFlows
        procedure :: asArray => asArrayContaminantFlows
        procedure :: assignContaminantFlows
        generic :: assignment(=) => assignContaminantFlows
    end type

    type, public :: WaterFlows
        real(dp) :: inflow = 0.0_dp
        real(dp) :: runoff = 0.0_dp
        real(dp) :: transfers = 0.0_dp
        real(dp) :: demands = 0.0_dp
        real(dp) :: outflow = 0.0_dp
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
        procedure :: finalise => finaliseSPMFlows
        procedure :: addInflow => addInflowSPMFlows
        procedure :: asArray => asArraySPMFlows
        procedure :: assignSPMFlows
        generic :: assignment(=) => assignSPMFlows
    end type

contains

    subroutine initContaminantFlows(me)
        class(ContaminantFlows), intent(inout) :: me
        type(Result) :: r
        call me%finalise()
        call r%addErrors(.errors. me%inflow%create())
        call r%addErrors(.errors. me%soilErosion%create())
        call r%addErrors(.errors. me%bankErosion%create())
        call r%addErrors(.errors. me%transfers%create())
        call r%addErrors(.errors. me%demands%create())
        call r%addErrors(.errors. me%deposition%create())
        call r%addErrors(.errors. me%resuspension%create())
        call r%addErrors(.errors. me%outflow%create())
        call r%addErrors(.errors. me%pointSources%create())
        call r%addErrors(.errors. me%diffuseSources%create())
        call r%addErrors(.errors. me%foam%create())
        call r%addErrors(.errors. me%atmosphere%create())
        call r%addErrors(.errors. me%biota%create())
        if (r%hasCriticalError()) call LOGR%toFile(errors=.errors.r)
    end subroutine

    subroutine emptyContaminantFlows(me)
        class(ContaminantFlows), intent(inout) :: me
        call me%inflow%empty()
        call me%soilErosion%empty()
        call me%bankErosion%empty()
        call me%transfers%empty()
        call me%demands%empty()
        call me%deposition%empty()
        call me%resuspension%empty()
        call me%outflow%empty()
        call me%pointSources%empty()
        call me%diffuseSources%empty()
        call me%foam%empty()
        call me%atmosphere%empty()
        call me%biota%empty()
    end subroutine

    subroutine finaliseContaminantFlows(me)
        class(ContaminantFlows), intent(inout) :: me
        call me%inflow%finalise()
        call me%soilErosion%finalise()
        call me%bankErosion%finalise()
        call me%transfers%finalise()
        call me%demands%finalise()
        call me%deposition%finalise()
        call me%resuspension%finalise()
        call me%outflow%finalise()
        call me%pointSources%finalise()
        call me%diffuseSources%finalise()
        call me%foam%finalise()
        call me%atmosphere%finalise()
        call me%biota%finalise()
    end subroutine

    function asArrayContaminantFlows(me) result(arr)
        class(ContaminantFlows), intent(in) :: me
        real(dp) :: arr(13)
        arr(1) = mass(me%inflow)
        arr(2) = mass(me%soilErosion)
        arr(3) = mass(me%bankErosion)
        arr(4) = mass(me%transfers)
        arr(5) = mass(me%demands)
        arr(6) = mass(me%deposition)
        arr(7) = mass(me%resuspension)
        arr(8) = mass(me%outflow)
        arr(9) = mass(me%pointSources)
        arr(10)= mass(me%diffuseSources)
        arr(11)= mass(me%foam)
        arr(12)= mass(me%atmosphere)
        arr(13)= mass(me%biota)
    contains
        pure function mass(c) result(m)
            type(Contaminant), intent(in) :: c
            real(dp) :: m
            if (allocated(c%c)) then
                m = sum(c%c)
            else
                m = 0.0_dp
            end if
        end function
    end function

    subroutine assignContaminantFlows(lhs, rhs)
        class(ContaminantFlows), intent(out) :: lhs
        type(ContaminantFlows), intent(in) :: rhs
        lhs%inflow = rhs%inflow
        lhs%soilErosion = rhs%soilErosion
        lhs%bankErosion = rhs%bankErosion
        lhs%transfers = rhs%transfers
        lhs%demands = rhs%demands
        lhs%deposition = rhs%deposition
        lhs%resuspension = rhs%resuspension
        lhs%outflow = rhs%outflow
        lhs%pointSources = rhs%pointSources
        lhs%diffuseSources = rhs%diffuseSources
        lhs%foam = rhs%foam
        lhs%atmosphere = rhs%atmosphere
        lhs%biota = rhs%biota
    end subroutine

    subroutine initWaterFlows(me)
        class(WaterFlows), intent(inout) :: me
        call me%empty()
    end subroutine

    subroutine emptyWaterFlows(me)
        class(WaterFlows), intent(inout) :: me
        me%inflow = 0.0_dp
        me%runoff = 0.0_dp
        me%transfers = 0.0_dp
        me%demands = 0.0_dp
        me%outflow = 0.0_dp
    end subroutine

    subroutine addInflowWaterFlows(me, q_in)
        class(WaterFlows), intent(inout) :: me
        real(dp), intent(in) :: q_in
        me%inflow = me%inflow + q_in
    end subroutine

    function asArrayWaterFlows(me) result(arr)
        class(WaterFlows), intent(in) :: me
        real(dp) :: arr(5)
        arr = [me%inflow, me%runoff, me%transfers, me%demands, me%outflow]
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

    subroutine initSPMFlows(me)
        class(SPMFlows), intent(inout) :: me
        call me%finalise()
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

    subroutine finaliseSPMFlows(me)
        class(SPMFlows), intent(inout) :: me
        if (allocated(me%inflow)) deallocate(me%inflow)
        if (allocated(me%soilErosion)) deallocate(me%soilErosion)
        if (allocated(me%bankErosion)) deallocate(me%bankErosion)
        if (allocated(me%transfers)) deallocate(me%transfers)
        if (allocated(me%demands)) deallocate(me%demands)
        if (allocated(me%deposition)) deallocate(me%deposition)
        if (allocated(me%resuspension)) deallocate(me%resuspension)
        if (allocated(me%outflow)) deallocate(me%outflow)
    end subroutine

    subroutine addInflowSPMFlows(me, j_in)
        class(SPMFlows), intent(inout) :: me
        real(dp), intent(in) :: j_in(:)
        type(ErrorInstance) :: err(1)
        if (.not. allocated(me%inflow)) call me%init()
        if (size(j_in) /= size(me%inflow)) then
            err(1) = ErrorInstance(code=900, message="Size mismatch in SPMFlows%addInflow")
            call LOGR%toFile(errors=err)
            return
        end if
        me%inflow = me%inflow + j_in
    end subroutine

    function asArraySPMFlows(me) result(arr)
        class(SPMFlows), intent(in) :: me
        real(dp) :: arr(8,C%nSizeClassesSpm)
        arr = 0.0_dp
        if (.not. allocated(me%inflow)) return
        arr(1,:) = me%inflow
        arr(2,:) = me%soilErosion
        arr(3,:) = me%bankErosion
        arr(4,:) = me%transfers
        arr(5,:) = me%demands
        arr(6,:) = me%deposition
        arr(7,:) = me%resuspension
        arr(8,:) = me%outflow
    end function

    subroutine assignSPMFlows(obj, arr)
        class(SPMFlows), intent(out) :: obj
        real(dp), intent(in) :: arr(8,C%nSizeClassesSpm)
        call obj%init()
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

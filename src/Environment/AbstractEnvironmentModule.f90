!> Module container for the `AbstractEnvironment` abstract base class
module AbstractEnvironmentModule
    use GlobalsModule
    use ResultModule
    use ContaminantModule
    use AbstractGridCellModule
    use ReachModule, only: ReachPointer
    use mo_netcdf
    implicit none
    private

    type, public :: EnvironmentPointer
        class(AbstractEnvironment), pointer :: item => null()
    end type

    type, public, abstract :: AbstractEnvironment
        integer, allocatable                :: gridDimensions(:)
        type(GridCellElement), allocatable  :: colGridCells(:,:)
        integer                             :: nGridCells = 0
        type(ReachPointer), allocatable     :: headwaters(:)
        type(ReachPointer), allocatable     :: routedReaches(:)
        integer                             :: nHeadwaters = 0
        integer                             :: nWaterbodies = 0
        type(NcGroup)                       :: ncGroup

        type(Contaminant), allocatable :: contaminant_water_t(:)
        type(Contaminant), allocatable :: contaminant_sediment_t(:)
        real(dp), allocatable          :: m_sediment_t_byLayer(:,:,:)
    contains
        procedure(createEnvironment), deferred :: create
        procedure(updateEnvironment), deferred :: update
        procedure(updateReachEnvironment), deferred :: updateReach
        procedure(determineStreamOrderEnvironment), deferred :: determineStreamOrder
        procedure(parseNewBatchDataEnvironment), deferred :: parseNewBatchData
        procedure(parseInputDataEnvironment), deferred :: parseInputData
        procedure(get_m_contaminantEnvironment), deferred :: get_m_contaminant
        procedure(get_C_contaminant_soilEnvironment), deferred :: get_C_contaminant_soil
        procedure(get_C_contaminant_waterEnvironment), deferred :: get_C_contaminant_water
        procedure(get_C_contaminant_sedimentEnvironment), deferred :: get_C_contaminant_sediment
        procedure(getBedSedimentAreaEnvironment), deferred :: getBedSedimentArea
        procedure(get_m_sediment_byLayerEnvironment), deferred :: get_m_sediment_byLayer
        procedure :: finalise => finaliseEnvironment
    end type

    abstract interface
        function createEnvironment(me) result(r)
            use ResultModule, only: Result
            import AbstractEnvironment
            class(AbstractEnvironment), target :: me
            type(Result) :: r
        end function

        subroutine updateEnvironment(me, t, tInBatch, isWarmUp)
            import AbstractEnvironment
            class(AbstractEnvironment), target :: me
            integer, intent(in) :: t
            integer, intent(in) :: tInBatch
            logical, intent(in) :: isWarmUp
        end subroutine

        subroutine updateReachEnvironment(me, t, reach, isWarmUp)
            use ReachModule, only: ReachPointer
            import AbstractEnvironment
            class(AbstractEnvironment), target :: me
            integer, intent(in) :: t
            type(ReachPointer), intent(inout) :: reach
            logical, intent(in) :: isWarmUp
        end subroutine

        function parseInputDataEnvironment(me) result(r)
            use ResultModule, only: Result
            import AbstractEnvironment
            class(AbstractEnvironment), intent(inout) :: me
            type(Result) :: r
        end function

        subroutine determineStreamOrderEnvironment(me)
            import AbstractEnvironment
            class(AbstractEnvironment), intent(inout) :: me
        end subroutine

        subroutine parseNewBatchDataEnvironment(me)
            import AbstractEnvironment
            class(AbstractEnvironment), intent(inout) :: me
        end subroutine

        function get_m_contaminantEnvironment(me) result(m_contaminant)
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            type(Contaminant) :: m_contaminant
        end function

        function get_C_contaminant_soilEnvironment(me) result(C_contaminant_soil)
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            type(Contaminant) :: C_contaminant_soil
        end function

        function get_C_contaminant_waterEnvironment(me) result(C_contaminant_water)
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            type(Contaminant) :: C_contaminant_water
        end function

        function get_C_contaminant_sedimentEnvironment(me) result(C_contaminant_sediment)
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            type(Contaminant) :: C_contaminant_sediment
        end function

        function getBedSedimentAreaEnvironment(me) result(bedArea)
            use GlobalsModule, only: dp
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            real(dp) :: bedArea
        end function

        function get_m_sediment_byLayerEnvironment(me) result(m_sediment_byLayer)
            use GlobalsModule, only: dp
            import AbstractEnvironment
            class(AbstractEnvironment), intent(in) :: me
            real(dp), allocatable :: m_sediment_byLayer(:,:)
        end function
    end interface

contains

    subroutine finaliseEnvironment(me)
        class(AbstractEnvironment), intent(inout) :: me
        integer :: i

        if (allocated(me%contaminant_water_t)) then
            do i = 1, size(me%contaminant_water_t)
                call me%contaminant_water_t(i)%finalise()
            end do
            deallocate(me%contaminant_water_t)
        end if

        if (allocated(me%contaminant_sediment_t)) then
            do i = 1, size(me%contaminant_sediment_t)
                call me%contaminant_sediment_t(i)%finalise()
            end do
            deallocate(me%contaminant_sediment_t)
        end if

        if (allocated(me%m_sediment_t_byLayer)) deallocate(me%m_sediment_t_byLayer)

        if (allocated(me%colGridCells)) deallocate(me%colGridCells)
        if (allocated(me%headwaters)) deallocate(me%headwaters)
        if (allocated(me%routedReaches)) deallocate(me%routedReaches)
        if (allocated(me%gridDimensions)) deallocate(me%gridDimensions)
    end subroutine 

end module 

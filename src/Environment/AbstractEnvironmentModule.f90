!> Module container for the `AbstractEnvironment` abstract base class
module AbstractEnvironmentModule
    use GlobalsModule
    use ResultModule
    use ContaminantModule
    use AbstractGridCellModule
    use mo_netcdf
    implicit none
    private

    type, public :: EnvironmentPointer
        class(AbstractEnvironment), pointer :: item => null()           !! Pointer to polymorphic AbstractEnvironment object
    end type

    !> Abstract base class definition for `AbstractEnvironment`.
    type, public, abstract :: AbstractEnvironment
        integer, allocatable                :: gridDimensions(:)        !! Size of the grid as defined in input data file (must be allocatable for mo_netcdf)
        type(GridCellElement), allocatable  :: colGridCells(:,:)        !! Array of `GridCellElement` objects to hold polymorphic `GridCell`s
        integer                             :: nGridCells = 0           !! Number of grid cells in the Environment
        type(ReachPointer), allocatable     :: headwaters(:)            !! Array of `GridCell`s that contain headwaters
        type(ReachPointer), allocatable     :: routedReaches(:)         !! Array of pointers to routed reaches
        integer                             :: nHeadwaters = 0          !! The number of headwaters in the Environment
        integer                             :: nWaterbodies = 0         !! The number of waterbodies in the Environment
        type(NcGroup)                       :: ncGroup                  !! NetCDF group for this `Environment` object
        ! Summary statistics
        type(Contaminant), allocatable :: contaminant_water_t(:)        ! Contaminant state in water per timestep
        type(Contaminant), allocatable :: contaminant_sediment_t(:)     ! Contaminant state in sediment per timestep
        real(dp), allocatable          :: m_sediment_t_byLayer(:,:,:)   ! Sediment mass in each layer per timestep [kg]
      contains
        procedure(createEnvironment), deferred :: create
        procedure(updateEnvironment), deferred :: update
        procedure(updateReachEnvironment), deferred :: updateReach
        procedure(determineStreamOrderEnvironment), deferred :: determineStreamOrder
        procedure(parseNewBatchDataEnvironment), deferred :: parseNewBatchData
        ! Getters
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
    
        !> Interface to create an `AbstractEnvironment` object
        function createEnvironment(me) result(r)
            use ResultModule, only: Result
            import AbstractEnvironment
            class(AbstractEnvironment), target :: me            !! This `AbstractEnvironment` instance
            type(Result) :: r                           !! `Result` object containing any errors
        end function
        
        !> Interface to perform simulations in `AbstractEnvironment`
        subroutine updateEnvironment(me, t, tInBatch, isWarmUp)
            import AbstractEnvironment
            class(AbstractEnvironment), target :: me            !! This `AbstractEnvironment` instance
            integer :: t                                !! The current time step
            integer :: tInBatch                         !! The current time step in the batch run
            logical :: isWarmUp                         !! Are we in the warm up period?
        end subroutine

        subroutine updateReachEnvironment(me, t, reach, isWarmUp)
            use ReachModule, only: ReachPointer
            import AbstractEnvironment
            class(AbstractEnvironment), target  :: me           !! This AbstractEnvironment instance
            integer                     :: t            !! The current timestep
            type(ReachPointer)          :: reach        !! The reach to update
            logical                     :: isWarmUp     !! Are we in a warm up period?
        end subroutine
        
        !> Interface to import and parse input data for the `AbstractEnvironment` object
        function parseInputDataEnvironment(me) result(r)
            use ResultModule, only: Result
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
            type(Result) :: r
        end function

        !> Determine the stream order of water bodies in the AbstractEnvironment
        subroutine determineStreamOrderEnvironment(me)
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
        end subroutine

        subroutine parseNewBatchDataEnvironment(me)
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
        end subroutine
        
        function get_m_contaminantEnvironment(me) result(m_contaminant)
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
            type(Contaminant) :: m_contaminant
        end function

        function get_C_contaminant_soilEnvironment(me) result(C_contaminant_soil)
            !use GlobalsModule, only: C, dp
            use ContaminantModule
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
            !real(dp), allocatable :: C_contaminant_soil(:,:,:)
            type(Contaminant) :: C_contaminant_soil
        end function

        function get_C_contaminant_waterEnvironment(me) result(C_contaminant_water)
            use GlobalsModule, only: C, dp
            import AbstractEnvironment, Contaminant
            class(AbstractEnvironment) :: me
            type(Contaminant) :: C_contaminant_water
        end function

        function get_C_contaminant_sedimentEnvironment(me) result(C_contaminant_sediment)
            use GlobalsModule, only: C, dp
            import AbstractEnvironment, Contaminant
            class(AbstractEnvironment) :: me
            type(Contaminant) :: C_contaminant_sediment
        end function

        function getBedSedimentAreaEnvironment(me) result(bedArea)
            use GlobalsModule, only: dp
            import AbstractEnvironment
            class(AbstractEnvironment) :: me
            real(dp) :: bedArea
        end function

        function get_m_sediment_byLayerEnvironment(me) result(m_sediment_byLayer)
            use GlobalsModule, only: dp, C
            import AbstractEnvironment
            class(AbstractEnvironment)      :: me
            real(dp), allocatable :: m_sediment_byLayer(:,:)  ! (nSedimentLayers, C%contaminantDim(1))
        end function

    end interface

    contains
    subroutine finaliseEnvironment(me)
        class(AbstractEnvironment) :: me
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
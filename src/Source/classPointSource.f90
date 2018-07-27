module classPointSource
    use mo_netcdf
    use Globals
    use UtilModule
    use ResultModule
    implicit none
    
    type, public :: PointSource
        integer :: x                    !! GridCell x reference
        integer :: y                    !! GridCell y reference
        integer :: s                    !! PointSource reference
        character(:), allocatable :: parents(:)
            !! Array of character references to parent environmental compartments, e.g.
            !! ['GridCell_1_1', 'RiverReach_1_1_1']
        type(NcGroup) :: ncGroup        !! NetCDF group for this `PointSource` object
        integer :: fixedMassFrequency   !! Frequency of fixed mass releases; how many timesteps apart are releases? [timestep]
        real(dp), allocatable :: fixedMass(:,:,:)                   !! Fixed mass to be released according to fixedMassFrequency [kg]
        real(dp), allocatable :: variableMass_timeSeries(:,:,:,:)   !! Time series of input masses [kg/timestep]
        real(dp), allocatable :: j_np_pointsource(:,:,:)            !! Nanomaterial inflow for a given timestep [kg/timestep]

    contains
        ! Proceudres
        procedure :: create => createPointSource
        procedure :: update => updatePointSource
        procedure :: parseInputData => parseInputDataPointSource
    end type

    contains
    
    !> Initialise the PointSource object
    function createPointSource(me, x, y, s, parents) result(r)
        class(PointSource) :: me
        integer :: x
        integer :: y
        integer :: s
        character(len=*) :: parents(:)
        type(Result) :: r
        
        allocate(me%parents, source=parents)
        me%x = x
        me%y = y
        me%s = s
        ! Allocate some arrays
        allocate( &
            me%fixedMass(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2), &
            me%variableMass_timeSeries(C%nTimesteps, C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2), &
            me%j_np_pointsource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2) &
        )
        ! Set some defaults
        me%fixedMass = 0.0_dp
        me%variableMass_timeSeries = 0.0_dp
        
        ! Parse the input data for this point source
        call r%addErrors(.errors. me%parseInputData())
        
        ! Add this procedure to the error trace
        call r%addToTrace("Creating PointSource")
    end function
    
    function updatePointSource(me, t) result(r)
        class(PointSource) :: me
        integer :: t
        type(Result) :: r
        
        me%j_np_pointsource = 0      ! Reset from the last timestep
        ! Check if this is a timestep where the fixed mass is to be input
        if (me%fixedMassFrequency /= 0 .and. mod(t,me%fixedMassFrequency) == 0) then
            me%j_np_pointsource = me%fixedMass
        end if
        ! Add this timestep's input from the variable mass input
        me%j_np_pointsource = me%j_np_pointsource + me%variableMass_timeSeries(t,:,:,:)
        
        call r%addToTrace("Updating PointSource on time step " // trim(str(t)))
    end function
    
    !> Parse input data for the PointSource
    function parseInputDataPointSource(me) result(r)
        class(PointSource) :: me
        type(Result) :: r
        type(NcDataset) :: nc               ! NetCDF dataset
        type(NcVariable) :: var             ! NetCDF variable
        type(NcGroup) :: grp                ! NetCDF group
        integer :: i                        ! Loop iterator
        
        nc = NcDataset(C%inputFile, "r")                        ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(trim(ref("GridCell",me%x,me%y)))
        ! Loop through the parent groups        
        do i = 1, size(me%parents)
            grp = grp%getGroup(trim(me%parents(i)))
        end do
        ! The containing waterbody should have already checked this PointSource exists
        if (me%s == 1 .and. grp%hasGroup("PointSource")) then
            me%ncGroup = grp%getGroup("PointSource")
        else
            me%ncGroup = grp%getGroup("PointSource_" // trim(str(me%s)))
        end if
        
        ! If a fixed mass input has been specified
        if (me%ncGroup%hasVariable("fixed_mass")) then
            var = me%ncGroup%getVariable("fixed_mass")
            call var%getData(me%fixedMass)
            ! If a fixed mass frequency has been specified, use it, otherwise default to daily
            if (me%ncGroup%hasVariable("fixed_mass_frequency")) then
                var = me%ncGroup%getVariable("fixed_mass_frequency")
                call var%getData(me%fixedMassFrequency)
            else
                call r%addError(ErrorInstance( &
                    message = "Fixed mass input specified with no frequency of input. Defaulting to daily input.", &
                    isCritical = .false. &
                ))
                me%fixedMassFrequency = 1
            end if
        end if
        
        ! If a time series of inputs has been specified
        if (me%ncGroup%hasVariable("variable_mass")) then
            var = me%ncGroup%getVariable("variable_mass")
            call var%getData(me%variableMass_timeSeries)
        end if
        
        ! Add this procedure to the error trace
        call r%addToTrace("Parsing input data")             
    end function

end module
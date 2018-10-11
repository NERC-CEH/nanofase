module classPointSource
    use mo_netcdf
    use Globals
    use UtilModule
    use ResultModule
    use classDataInterfacer
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

        if (C%includePointSources) then
            ! Check if this is a timestep where the fixed mass is to be input
            if (me%fixedMassFrequency /= 0 .and. mod(t,me%fixedMassFrequency) == 0) then
                me%j_np_pointsource = me%fixedMass
            end if
            ! Add this timestep's input from the variable mass input
            me%j_np_pointsource = me%j_np_pointsource + me%variableMass_timeSeries(t,:,:,:)
        end if
        
        call r%addToTrace("Updating PointSource on time step " // trim(str(t)))
    end function
    
    !> Parse input data for the PointSource
    function parseInputDataPointSource(me) result(r)
        class(PointSource) :: me
        type(Result) :: r
        integer :: i                        ! Loop iterator
        

        call r%addErrors(.errors. DATA%setGroup([character(len=100) :: &
            'Environment', &
            ref('GridCell', me%x, me%y) &
        ]))
        do i = 1, size(me%parents)
            DATA%grp = DATA%grp%getGroup(trim(me%parents(i)))
        end do

        ! The containing waterbody should have already checked this PointSource exists
        if (me%s == 1 .and. DATA%grp%hasGroup("PointSource")) then
            DATA%grp = DATA%grp%getGroup("PointSource")
        else
            DATA%grp = DATA%grp%getGroup("PointSource_" // trim(str(me%s)))
        end if
        
        ! Get fixed mass, otherwise set to 0 [kg]
        call r%addErrors(.errors. DATA%get("fixed_mass", me%fixedMass, 0.0_dp))

        ! If a fixed mass frequency has been specified, use it, otherwise default to daily
        call r%addErrors(.errors. DATA%get("fixed_mass_frequency", me%fixedMassFrequency, 1))
        ! If a time series of inputs has been specified
        call r%addErrors(.errors. DATA%get("variable_mass", me%variableMass_timeSeries, 0.0_dp))
        
        ! Add this procedure to the error trace
        call r%addToTrace("Parsing input data")             
    end function

end module
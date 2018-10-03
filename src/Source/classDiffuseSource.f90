module classDiffuseSource
    use Globals
    use UtilModule
    use ResultModule
    use classDataInterfacer
    implicit none
    private

    type, public :: DiffuseSource
        integer :: x                                !! `GridCell` x reference
        integer :: y                                !! `GridCell` y reference
        integer :: s                                !! Reference for this `DiffuseSource`
        character(100), allocatable :: parents(:)
            !! Array of character references to parent environmental compartments, e.g.
            !! ['GridCell_1_1', 'RiverReach_1_1_1']
        type(NcGroup) :: ncGroup                    !! NetCDF group for this `PointSource` object
        real(dp), allocatable :: inputMass_timeSeries(:,:,:,:)  !! Time series of input nanomaterial masses [kg/m2]
        real(dp), allocatable :: j_np_diffusesource(:,:,:)      !! Nanomaterial input for a given time step [(kg/m2)/timestep]

      contains
        procedure :: create => createDiffuseSource
        procedure :: update => updateDiffuseSource
        procedure :: parseInputData => parseInputDataDiffuseSource
    end type

  contains

    !> Create this DiffuseSource object and retrieve data from the input data.
    function createDiffuseSource(me, x, y, s, parents) result(r)
        class(DiffuseSource) :: me          !! This `DiffuseSource` object
        integer :: x                        !! The containing `GridCell` x reference
        integer :: y                        !! The containing `GridCell` y reference
        integer :: s                        !! Reference for this `DiffuseSource`
        character(len=*), optional :: parents(:)    !! Array of refs for parent environmental compartments
        type(Result) :: r                   !! The `Result` object to return any errors in

        me%x = x
        me%y = y
        me%s = s
        ! Allocate nanomaterial arrays
        allocate(me%inputMass_timeSeries(C%nTimesteps, C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2))
        allocate(me%j_np_diffusesource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2))
        if (.not. present(parents)) allocate(me%parents(0))    ! If no parents given (i.e. we're in a GridCell), set to empty

        ! Parse the input data
        call r%addErrors(.errors. me%parseInputData())        
    end function

    !> Update this diffuse source's NP flux for the current timestep.
    function updateDiffuseSource(me, t) result(r)
        class(DiffuseSource) :: me
        integer :: t
        type(Result) :: r
        ! Get this time step's input mass
        me%j_np_diffusesource = me%inputMass_timeSeries(t,:,:,:)        ! [kg/m2/timestep]
    end function

    !> Parse the input data to get input masses from this diffuse source.
    function parseInputDataDiffuseSource(me) result(r)
        class(DiffuseSource) :: me          !! This `DiffuseSource` object
        type(Result) :: r                   !! Result object to return with any errors
        integer :: i                        ! Loop iterator
        real(dp), allocatable :: atmosphericInput(:,:) ! Atmospheric input, only core, free NPs

        ! Get the GridCell group, followed by any parent groups (if there are any).
        ! Parent groups are things like SoilProfile.
        call r%addErrors(.errors. DATA%setGroup([character(len=100) :: &
            'Environment', &
            ref('GridCell', me%x, me%y) &
        ]))
        do i = 1, size(me%parents)
            DATA%grp = DATA%grp%getGroup(trim(me%parents(i)))
        end do

        ! Get the DiffuseSource group. The containing object should have already
        ! checked that it exists
        if (me%s == 1 .and. DATA%grp%hasGroup("DiffuseSource")) then
            DATA%grp = DATA%grp%getGroup("DiffuseSource")
        else
            DATA%grp = DATA%grp%getGroup("DiffuseSource_" // trim(str(me%s)))
        end if

        ! If a fixed mass input has been specified, get it.
        call r%addErrors(.errors. DATA%get('input_mass', me%inputMass_timeSeries, 0.0_dp))      ! [kg/m2/s]
        me%inputMass_timeSeries = me%inputMass_timeSeries*C%timeStep                            ! Convert to kg/m2/timestep

        ! If an atmospheric fixed mass input has been specified, get it
        if (DATA%grp%hasVariable('input_mass_atmospheric')) then
            call r%addErrors(.errors. DATA%get('input_mass_atmospheric', atmosphericInput))     ! [kg/m2/s]
            me%inputMass_timeSeries(:,:,1,1) = atmosphericInput                                 ! Only add to free, core NP
        end if
    end function

end module
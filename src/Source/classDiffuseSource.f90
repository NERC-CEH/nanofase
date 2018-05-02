module classDiffuseSource
    use Globals
    use ResultModule
    implicit none

    type, public :: DiffuseSource
        integer :: x                                !! `GridCell` x reference
        integer :: y                                !! `GridCell` y reference
        logical :: exists = .true.                  !! Does this PointSource exist in data?
        type(NcGroup) :: ncGroup                    !! NetCDF group for this `PointSource` object
        real(dp), allocatable :: inputMass_timeSeries(:,:,:,:)  !! Time series of input nanomaterial masses
        real(dp), allocatable :: j_np_diffusesource(:,:,:)      !! Nanomaterial input for a given time step [kg/timestep]

      contains
        procedure :: create => createDiffuseSource
        procedure :: update => updateDiffuseSource
        procedure :: parseInputData => parseInputDataDiffuseSource
    end type

    contains

    function createDiffuseSource(me, x, y) result(r)
        class(DiffuseSource) :: me          !! This `DiffuseSource` object
        integer :: x                        !! The containing `GridCell` x reference
        integer :: y                        !! The containing `GridCell` y reference
        type(Result) :: r                   !! The `Result` object to return any errors in

        me%x = x
        me%y = y
        ! Allocate nanomaterial arrays
        allocate( &
            me%input_mass(C%nTimesteps, C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2), &
            me%j_np_diffusesource(C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2) &
        )
        ! Parse the input data
        call r%addErrors(.errors. me%parseInputData())        
    end function

    function updateDiffuseSource(me, t) result(r)
        class(DiffuseSource) :: me
        integer :: t
        type(Result) :: r
        ! If there's data for this diffuse source, then get it for this time step
        if (me%exists) then
            me%j_np_diffusesource = me%inputMass_timeSeries(t)
        else
            me%j_np_diffusesource = 0
        end if
    end function

    function parseInputDataDiffuseSource(me) result(r)
        class(DiffuseSource) :: me          !! This `DiffuseSource` object
        type(Result) :: r                   !! Result object to return with any errors
        type(NcDataset) :: nc               ! NetCDF dataset
        type(NcVariable) :: var             ! NetCDF variable
        type(NcGroup) :: grp                ! NetCDF group
        integer :: i                        ! Loop iterator
        ! Open dataset and get this object's group
        nc = NcDataset(C%inputFile, "r")
        grp = nc%getGroup("Environment")
        grp = grp%getGroup(trim(ref("GridCell",me%x,me%y)))
        if (grp%hasGroup("DiffuseSource")) then
            me%ncGroup = grp%getGroup("DiffuseSource")    
        else
            me%exists = .false.             ! If we can't find the data set, DiffuseSource mustn't exist
        end if

        ! Get data if this PointSource exists
        if (me%exists) then
            ! If a fixed mass input has been specified
            if (me%ncGroup%hasVariable("input_mass")) then
                var = me%ncGroup%getVariable("input_mass")
                call var%getData(me%inputMass_timeSeries)
            end if
        end if
    end function

end module
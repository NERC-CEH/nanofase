module classPointSource2
    use Globals
    use ResultModule
    use classDatabase
    implicit none
    
    type, public :: PointSource2
        integer :: x                        !! Grid cell x reference
        integer :: y                        !! Grid cell y reference
        integer :: s                        !! Point source reference
        character(len=11) :: compartment    !! Which environmental compartment is this source for?
        real(dp), allocatable :: j_np_pointSource(:,:,:)            !! Nanomaterial inflow for a given timestep [kg/timestep]
        real(dp) :: j_dissolved_pointSource
        real(dp) :: j_transformed_pointSource
      contains
        procedure :: create => createPointSource
        procedure :: update => updatePointSource
    end type

  contains
    
    !> Initialise the PointSource object
    subroutine createPointSource(me, x, y, s, compartment) result(r)
        class(PointSource) :: me
        integer :: x
        integer :: y
        integer :: s
        character(len=*) :: parents(:)
        type(Result) :: r
        ! Allocate and initialise
        me%x = x
        me%y = y
        me%s = s
        me%compartment = compartment
        allocate(me%j_np_pointSource(C%npDim(1), C%npDim(2), C%npDim(3)))
    end function
    
    subroutine updatePointSource(me, t) result(r)
        class(PointSource) :: me
        integer :: t
        type(Result) :: r
        ! Default to zero
        me%j_np_pointSource = 0
        me%j_dissolved_pointSource = 0
        me%j_transformed_pointSource = 0

        ! Only include point sources if config says we're meant to, and we're not in the
        ! warm up period
        if (C%includePointSources .and. t .ge. C%warmUpPeriod) then
            ! DO STUFF
        end if
    end subroutine

end module
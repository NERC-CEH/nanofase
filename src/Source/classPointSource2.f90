module classPointSource2
    use Globals
    use ResultModule
    use classDatabase
    implicit none
    
    type, public :: PointSource2
        integer :: x                        !! Grid cell x reference
        integer :: y                        !! Grid cell y reference
        integer :: s                        !! Point source reference
        real :: x_coord                     !! Exact eastings of this point source
        real :: y_coord                     !! Exact northings of this point source
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
    subroutine createPointSource(me, x, y, s, compartment)
        class(PointSource2) :: me
        integer :: x
        integer :: y
        integer :: s
        character(len=*) :: compartment
        ! Allocate and initialise
        me%x = x
        me%y = y
        me%s = s
        me%compartment = compartment
        allocate(me%j_np_pointSource(C%npDim(1), C%npDim(2), C%npDim(3)))
        ! Get the exact coordinates of this point source
        if (.not. DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 1) == nf90_fill_double) then
            me%x_coord = DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 1)
            me%y_coord = DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 2)
        end if
    end subroutine
    
    subroutine updatePointSource(me, t)
        class(PointSource2) :: me   ! This point source
        integer :: t                ! Current time step
        integer :: i                ! Iterator
        ! Default to zero
        me%j_np_pointSource = 0
        me%j_dissolved_pointSource = 0
        me%j_transformed_pointSource = 0

        ! Only include point sources if config says we're meant to, and we're not in the
        ! warm up period
        if (C%includePointSources .and. t .ge. C%warmUpPeriod) then
            ! There are only point sources to water (for the moment)
            if (trim(me%compartment) == 'water') then
                ! Pristine
                if (.not. DATASET%emissionsPointWaterPristine(me%x, me%y, t, me%s) == nf90_fill_double) then
                    me%j_np_pointSource(:,1,1) = DATASET%emissionsPointWaterPristine(me%x, me%y, t, me%s) &
                        * DATASET%defaultNMSizeDistribution
                end if
                ! Matrix-embedded
                if (.not. DATASET%emissionsPointWaterMatrixEmbedded(me%x, me%y, t, me%s) == nf90_fill_double) then
                    do i = 1, DATASET%nSizeClassesNM
                        me%j_np_pointSource(i,1,3:) = DATASET%emissionsPointWaterMatrixEmbedded(me%x, me%y, t, me%s) &
                            * DATASET%defaultMatrixEmbeddedDistributionToSpm * DATASET%defaultNMSizeDistribution(i)
                    end do
                end if
                ! Dissolved
                if (.not. DATASET%emissionsPointWaterDissolved(me%x, me%y, t, me%s) == nf90_fill_double) then
                    me%j_dissolved_pointSource = DATASET%emissionsPointWaterDissolved(me%x, me%y, t, me%s)
                end if
                ! Transformed
                if (.not. DATASET%emissionsPointWaterTransformed(me%x, me%y, t, me%s) == nf90_fill_double) then
                    me%j_transformed_pointSource = DATASET%emissionsPointWaterTransformed(me%x, me%y, t, me%s)
                end if
            end if
        end if
    end subroutine

end module
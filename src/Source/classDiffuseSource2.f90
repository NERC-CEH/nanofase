module classDiffuseSource2
    use Globals
    use ResultModule
    use netcdf, only: nf90_fill_double
    use classDatabase
    implicit none
    private

    type, public :: DiffuseSource2
        integer :: x                                                    !! Grid cell x reference
        integer :: y                                                    !! Grid cell y reference
        integer :: s                                                    !! Diffuse source reference
        character(len=11) :: compartment                                !! Which environmental compartment is this source for?
        real(dp), allocatable :: j_np_diffuseSource(:,:,:)              !! NM input for given timestep [kg/m2/timestep]
        real(dp), allocatable :: j_transformed_diffuseSource(:,:,:)     !! Transformed input for given timestep [kg/m2/timestep]
        real(dp) :: j_dissolved_diffuseSource                           !! Dissolved input for given timestep [kg/m2/timestep]
      contains
        procedure :: create => createDiffuseSource2
        procedure :: update => updateDiffuseSource2
    end type

  contains

    subroutine createDiffuseSource2(me, x, y, s, compartment)
        class(DiffuseSource2) :: me
        integer :: x
        integer :: y
        integer :: s
        character(len=*) :: compartment
        ! Allocate and initialise
        me%x = x
        me%y = y
        me%s = s
        me%compartment = compartment
        allocate(me%j_np_diffuseSource(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%j_transformed_diffuseSource(C%npDim(1), C%npDim(2), C%npDim(3)))
    end subroutine

    subroutine updateDiffuseSource2(me, t)
        class(DiffuseSource2)   :: me       !! This diffuse source
        integer                 :: t        !! Current time step
        integer                 :: i        ! Iterator
        ! Default to zero
        me%j_np_diffuseSource = 0.0_dp
        me%j_dissolved_diffuseSource = 0.0_dp
        me%j_transformed_diffuseSource = 0.0_dp
        ! Check the environmental compartment we're in, get the corresponding areal source
        ! data and impose the default NM size distribution on it. Data already kg/m2/timestep.
        ! If data doesn't exist, classDatabase has already created array filled with nf90_fill_double
        if (trim(me%compartment) == 'soil') then
            ! Pristine NM
            if (.not. DATASET%emissionsArealSoilPristine(me%x, me%y) == nf90_fill_double) then
                me%j_np_diffuseSource(:,1,1) = DATASET%emissionsArealSoilPristine(me%x, me%y) * DATASET%defaultNMSizeDistribution
            end if
            ! Matrix-embedded NM
            if (.not. DATASET%emissionsArealSoilMatrixEmbedded(me%x, me%y) == nf90_fill_double) then
                me%j_np_diffuseSource(:,1,2) = DATASET%emissionsArealSoilMatrixEmbedded(me%x, me%y) &
                    * DATASET%defaultNMSizeDistribution
            end if 
            ! Dissolved
            if (.not. DATASET%emissionsArealSoilDissolved(me%x, me%y) == nf90_fill_double) then
                me%j_dissolved_diffuseSource = DATASET%emissionsArealSoilDissolved(me%x, me%y)
            end if
            ! Transformed
            if (.not. DATASET%emissionsArealSoilTransformed(me%x, me%y) == nf90_fill_double) then
                me%j_transformed_diffuseSource(:,1,1) = DATASET%emissionsArealSoilTransformed(me%x, me%y) &
                    * DATASET%defaultNMSizeDistribution
            end if
        else if (trim(me%compartment) == 'water') then
            ! Pristine NM
            if (.not. DATASET%emissionsArealWaterPristine(me%x, me%y) == nf90_fill_double) then
                me%j_np_diffuseSource(:,1,1) = DATASET%emissionsArealWaterPristine(me%x, me%y) * DATASET%defaultNMSizeDistribution
            end if
            ! Matrix-embedded NM
            if (.not. DATASET%emissionsArealWaterMatrixEmbedded(me%x, me%y) == nf90_fill_double) then
                do i = 1, C%nSizeClassesNM
                    me%j_np_diffuseSource(i,1,3:) = DATASET%emissionsArealWaterMatrixEmbedded(me%x, me%y) &
                        * DATASET%defaultMatrixEmbeddedDistributionToSpm * DATASET%defaultNMSizeDistribution(i)
                end do
            end if 
            ! Dissolved
            if (.not. DATASET%emissionsArealWaterDissolved(me%x, me%y) == nf90_fill_double) then
                me%j_dissolved_diffuseSource = DATASET%emissionsArealWaterDissolved(me%x, me%y)
            end if
            ! Transformed
            if (.not. DATASET%emissionsArealWaterTransformed(me%x, me%y) == nf90_fill_double) then
                me%j_transformed_diffuseSource(:,1,1) = DATASET%emissionsArealWaterTransformed(me%x, me%y) &
                     * DATASET%defaultNMSizeDistribution
            end if
        else if (trim(me%compartment) == 'atmospheric') then
            ! Dry depo
            ! Pristine NM
            if (.not. DATASET%emissionsAtmosphericDryDepoPristine(me%x, me%y, t) == nf90_fill_double) then
                me%j_np_diffuseSource(:,1,1) = DATASET%emissionsAtmosphericDryDepoPristine(me%x, me%y, t) &
                    * DATASET%defaultNMSizeDistribution
            end if
            ! Matrix-embedded NM
            if (.not. DATASET%emissionsAtmosphericDryDepoMatrixEmbedded(me%x, me%y, t) == nf90_fill_double) then
                do i = 1, C%nSizeClassesNM
                    me%j_np_diffuseSource(i,1,3:) = DATASET%emissionsAtmosphericDryDepoMatrixEmbedded(me%x, me%y, t) &
                        * DATASET%defaultMatrixEmbeddedDistributionToSpm * DATASET%defaultNMSizeDistribution(i)
                end do
            end if
            ! Dissolved
            if (.not. DATASET%emissionsAtmosphericDryDepoDissolved(me%x, me%y, t) == nf90_fill_double) then
                me%j_dissolved_diffuseSource = DATASET%emissionsAtmosphericDryDepoDissolved(me%x, me%y, t)
            end if
            ! Transformed
            if (.not. DATASET%emissionsAtmosphericDryDepoTransformed(me%x, me%y, t) == nf90_fill_double) then
                me%j_transformed_diffuseSource(:,1,1) = DATASET%emissionsAtmosphericDryDepoTransformed(me%x, me%y, t) &
                    * DATASET%defaultNMSizeDistribution
            end if
            ! Wet depo
            ! Pristine NM
            if (.not. DATASET%emissionsAtmosphericWetDepoPristine(me%x, me%y, t) == nf90_fill_double) then
                me%j_np_diffuseSource(:,1,1) = me%j_np_diffuseSource(:,1,1) &
                    + DATASET%emissionsAtmosphericWetDepoPristine(me%x, me%y, t) * DATASET%defaultNMSizeDistribution
            end if
            ! Matrix-embedded NM
            if (.not. DATASET%emissionsAtmosphericWetDepoMatrixEmbedded(me%x, me%y, t) == nf90_fill_double) then
                do i = 1, C%nSizeClassesNM
                    me%j_np_diffuseSource(i,1,3:) = me%j_np_diffuseSource(i,1,3:) &
                        + DATASET%emissionsAtmosphericWetDepoMatrixEmbedded(me%x, me%y, t) &
                        * DATASET%defaultMatrixEmbeddedDistributionToSpm * DATASET%defaultNMSizeDistribution(i)
                end do
            end if 
            ! Dissolved
            if (.not. DATASET%emissionsAtmosphericWetDepoDissolved(me%x, me%y, t) == nf90_fill_double) then
                me%j_dissolved_diffuseSource = me%j_dissolved_diffuseSource &
                    + DATASET%emissionsAtmosphericWetDepoDissolved(me%x, me%y, t)
            end if
            ! Transformed
            if (.not. DATASET%emissionsAtmosphericWetDepoTransformed(me%x, me%y, t) == nf90_fill_double) then
                me%j_transformed_diffuseSource(:,1,1) = me%j_transformed_diffuseSource(:,1,1) &
                    + DATASET%emissionsAtmosphericWetDepoTransformed(me%x, me%y, t) * DATASET%defaultNMSizeDistribution
            end if
        end if
    end subroutine

end module
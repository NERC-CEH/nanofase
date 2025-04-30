module DiffuseSourceModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT, SPM_CONTAMINANT_START
    use ResultModule
    use netcdf, only: nf90_fill_double
    use DataInputModule
    use ContaminantModule
    implicit none
    private
    
    type, public :: DiffuseSource
        integer :: x                                                    !! Grid cell x reference
        integer :: y                                                    !! Grid cell y reference
        integer :: s                                                    !! Diffuse source reference
        character(len=11) :: compartment                                !! Which environmental compartment is this source for?
        type(Contaminant) :: j_contaminant
      contains
        procedure :: create => createDiffuseSource
        procedure :: update => updateDiffuseSource
    end type

  contains

    !> Create the diffuse source
    subroutine createDiffuseSource(me, x, y, s, compartment)
        class(DiffuseSource)    :: me                   !! This diffuse source
        integer                 :: x                    !! Grid cell x index
        integer                 :: y                    !! Grid cell y index
        integer                 :: s                    !! Source index
        character(len=*)        :: compartment          !! Soil, water or atmospheric
        type(Result)            :: r
        
        me%x = x
        me%y = y
        me%s = s
        me%compartment = compartment
        r = me%j_contaminant%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
        end if
    end subroutine

    !> Update the diffuse source on time step t
    subroutine updateDiffuseSource(me, t)
        class(DiffuseSource) :: me
        integer, intent(in) :: t
        type(Result) :: r
        integer :: i, j
        real(dp) :: total_emission
        real(dp), allocatable :: form_fraction(:)
        real(dp) :: spm_distribution(C%nSizeClassesSpm)

        call me%j_contaminant%finalise()  ! Reset to zero
        r = me%j_contaminant%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if
        allocate(form_fraction(C%contaminantDim(2)+1))  ! Forms + dissolved
        form_fraction = DATASET%defaultContaminantFormDistribution
        spm_distribution = DATASET%defaultMatrixEmbeddedDistributionToSpm
        select case (trim(me%compartment))
        case ('soil')
            if (DATASET%emissionsArealSoilContaminant(me%x, me%y, 1, 1, FREE_CONTAMINANT) /= nf90_fill_double) then
                total_emission = sum(DATASET%emissionsArealSoilContaminant(me%x, me%y, :, :, :))
                do i = 1, C%contaminantDim(2)
                    me%j_contaminant%c(:,i,FREE_CONTAMINANT) = total_emission * form_fraction(i) * &
                        DATASET%defaultDistributionContaminant
                    me%j_contaminant%c(:,i,ATTACHED_CONTAMINANT) = total_emission * form_fraction(C%contaminantDim(2)+1) * &
                        DATASET%defaultDistributionContaminant / C%contaminantDim(2)
                end do
                me%j_contaminant%m_dissolved = total_emission * form_fraction(C%contaminantDim(2)+1)
            end if
            if (DATASET%emissionsArealSoilDissolvedContaminant(me%x, me%y) /= nf90_fill_double) then
                me%j_contaminant%m_dissolved = me%j_contaminant%m_dissolved + &
                    DATASET%emissionsArealSoilDissolvedContaminant(me%x, me%y)
            end if
        case ('water')
            if (DATASET%emissionsArealWaterContaminant(me%x, me%y, 1, 1, FREE_CONTAMINANT) /= nf90_fill_double) then
                total_emission = sum(DATASET%emissionsArealWaterContaminant(me%x, me%y, :, :, :))
                do i = 1, C%contaminantDim(2)
                    me%j_contaminant%c(:,i,FREE_CONTAMINANT) = total_emission * form_fraction(i) * &
                        DATASET%defaultDistributionContaminant
                    do j = 1, C%nSizeClassesSpm
                        me%j_contaminant%c(:,i,SPM_CONTAMINANT_START+j-1) = total_emission * &
                            form_fraction(C%contaminantDim(2)+1) * DATASET%defaultDistributionContaminant * &
                            spm_distribution(j) / C%contaminantDim(2)
                    end do
                end do
                me%j_contaminant%m_dissolved = total_emission * form_fraction(C%contaminantDim(2)+1)
            end if
            if (DATASET%emissionsArealWaterDissolvedContaminant(me%x, me%y) /= nf90_fill_double) then
                me%j_contaminant%m_dissolved = me%j_contaminant%m_dissolved + &
                    DATASET%emissionsArealWaterDissolvedContaminant(me%x, me%y)
            end if
        case ('atmospheric')
            total_emission = 0.0_dp
            if (DATASET%emissionsAtmosphericDryDepoContaminant(me%x, me%y, t, 1, 1, FREE_CONTAMINANT) /= nf90_fill_double) then
                total_emission = total_emission + sum(DATASET%emissionsAtmosphericDryDepoContaminant(me%x, me%y, t, :, :, :))
            end if
            if (DATASET%emissionsAtmosphericWetDepoContaminant(me%x, me%y, t, 1, 1, FREE_CONTAMINANT) /= nf90_fill_double) then
                total_emission = total_emission + sum(DATASET%emissionsAtmosphericWetDepoContaminant(me%x, me%y, t, :, :, :))
            end if
            if (total_emission > 0.0_dp) then
                do i = 1, C%contaminantDim(2)
                    me%j_contaminant%c(:,i,FREE_CONTAMINANT) = total_emission * form_fraction(i) * &
                        DATASET%defaultDistributionContaminant
                    do j = 1, C%nSizeClassesSpm
                        me%j_contaminant%c(:,i,SPM_CONTAMINANT_START+j-1) = total_emission * &
                            form_fraction(C%contaminantDim(2)+1) * DATASET%defaultDistributionContaminant * &
                            spm_distribution(j) / C%contaminantDim(2)
                    end do
                end do
                me%j_contaminant%m_dissolved = total_emission * form_fraction(C%contaminantDim(2)+1)
            end if
            if (DATASET%emissionsAtmosphericDryDepoDissolvedContaminant(me%x, me%y, t) /= nf90_fill_double) then
                me%j_contaminant%m_dissolved = me%j_contaminant%m_dissolved + &
                    DATASET%emissionsAtmosphericDryDepoDissolvedContaminant(me%x, me%y, t)
            end if
            if (DATASET%emissionsAtmosphericWetDepoDissolvedContaminant(me%x, me%y, t) /= nf90_fill_double) then
                me%j_contaminant%m_dissolved = me%j_contaminant%m_dissolved + &
                    DATASET%emissionsAtmosphericWetDepoDissolvedContaminant(me%x, me%y, t)
            end if
        case default
            call r%addError(ErrorInstance(code=900, message="Invalid compartment: "//trim(me%compartment)))
        end select
        if (.not. allocated(me%j_contaminant%c)) then
            call r%addError(ErrorInstance(code=105, message="Contaminant array not allocated"))
        end if
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
        end if
    end subroutine
end module
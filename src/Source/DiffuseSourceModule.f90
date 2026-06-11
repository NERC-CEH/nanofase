module DiffuseSourceModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT, SPM_CONTAMINANT_START
    use ResultModule
    use netcdf, only: nf90_fill_double
    use DataInputModule
    use ContaminantModule
    use PFASEConstantsModule
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
        me%compartment = adjustl(compartment)

        r = me%j_contaminant%create()
        if (r%hasCriticalError()) call ERROR_HANDLER%trigger(errors=.errors.r)
    end subroutine

    !> Update the diffuse source on time step t
    subroutine updateDiffuseSource(me, t)
        class(DiffuseSource), intent(inout) :: me
        integer, intent(in)                 :: t
        type(Result)                        :: r

        call me%j_contaminant%finalise()
        r = me%j_contaminant%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if

        select case (trim(me%compartment))
        case ('soil')
            call load_areal_soil(me)
        case ('water','estuary')
            call load_areal_water(me)
        case ('atmospheric')
            call load_atmospheric_deposition(me, t)
        case default
            call r%addError(ErrorInstance(code=900, message='Invalid diffuse-source compartment: '//trim(me%compartment)))
        end select

        if (allocated(me%j_contaminant%c)) then
            me%j_contaminant%m_dissolved = sum(me%j_contaminant%c(:,:,PFAS_AQ))
        else
            call r%addError(ErrorInstance(code=105, message='DiffuseSource contaminant array not allocated'))
        end if

        if (r%hasCriticalError()) call ERROR_HANDLER%trigger(errors=.errors.r)
    end subroutine updateDiffuseSource


    subroutine load_areal_soil(me)
        class(DiffuseSource), intent(inout) :: me
        integer :: ns, nf, np
        ns = size(me%j_contaminant%c,1)
        nf = size(me%j_contaminant%c,2)
        np = size(me%j_contaminant%c,3)

        if (allocated(DATASET%emissionsArealSoilContaminant)) then
            if (has_xy_5d(DATASET%emissionsArealSoilContaminant, me%x, me%y)) then
                call copy_pfas_tensor( &
                    DATASET%emissionsArealSoilContaminant(me%x,me%y,1:min(ns,size(DATASET%emissionsArealSoilContaminant,3)), &
                                                        1:min(nf,size(DATASET%emissionsArealSoilContaminant,4)), &
                                                        1:min(np,size(DATASET%emissionsArealSoilContaminant,5))), &
                    me%j_contaminant%c(1:min(ns,size(DATASET%emissionsArealSoilContaminant,3)), &
                                       1:min(nf,size(DATASET%emissionsArealSoilContaminant,4)), &
                                       1:min(np,size(DATASET%emissionsArealSoilContaminant,5))) )
            end if
        end if

        if (allocated(DATASET%emissionsArealSoilDissolvedContaminant)) then
            if (has_xy_2d(DATASET%emissionsArealSoilDissolvedContaminant, me%x, me%y)) then
                if (DATASET%emissionsArealSoilDissolvedContaminant(me%x,me%y) /= nf90_fill_double) &
                    call add_scalar_to_aq(me%j_contaminant, DATASET%emissionsArealSoilDissolvedContaminant(me%x,me%y))
            end if
        end if
    end subroutine load_areal_soil


    subroutine load_areal_water(me)
        class(DiffuseSource), intent(inout) :: me
        integer :: ns, nf, np
        ns = size(me%j_contaminant%c,1)
        nf = size(me%j_contaminant%c,2)
        np = size(me%j_contaminant%c,3)

        if (allocated(DATASET%emissionsArealWaterContaminant)) then
            if (has_xy_5d(DATASET%emissionsArealWaterContaminant, me%x, me%y)) then
                call copy_pfas_tensor( &
                    DATASET%emissionsArealWaterContaminant(me%x,me%y,1:min(ns,size(DATASET%emissionsArealWaterContaminant,3)), &
                                                         1:min(nf,size(DATASET%emissionsArealWaterContaminant,4)), &
                                                         1:min(np,size(DATASET%emissionsArealWaterContaminant,5))), &
                    me%j_contaminant%c(1:min(ns,size(DATASET%emissionsArealWaterContaminant,3)), &
                                       1:min(nf,size(DATASET%emissionsArealWaterContaminant,4)), &
                                       1:min(np,size(DATASET%emissionsArealWaterContaminant,5))) )
            end if
        end if

        if (allocated(DATASET%emissionsArealWaterDissolvedContaminant)) then
            if (has_xy_2d(DATASET%emissionsArealWaterDissolvedContaminant, me%x, me%y)) then
                if (DATASET%emissionsArealWaterDissolvedContaminant(me%x,me%y) /= nf90_fill_double) &
                    call add_scalar_to_aq(me%j_contaminant, DATASET%emissionsArealWaterDissolvedContaminant(me%x,me%y))
            end if
        end if
    end subroutine load_areal_water


    subroutine load_atmospheric_deposition(me, t)
        class(DiffuseSource), intent(inout) :: me
        integer, intent(in)                 :: t
        integer :: ns, nf, np
        ns = size(me%j_contaminant%c,1)
        nf = size(me%j_contaminant%c,2)
        np = size(me%j_contaminant%c,3)

        if (allocated(DATASET%emissionsAtmosphericDryDepoContaminant)) then
            if (has_xyt_6d(DATASET%emissionsAtmosphericDryDepoContaminant, me%x, me%y, t)) then
                call copy_pfas_tensor( &
                    DATASET%emissionsAtmosphericDryDepoContaminant(me%x,me%y,t, &
                                                        1:min(ns,size(DATASET%emissionsAtmosphericDryDepoContaminant,4)), &
                                                          1:min(nf,size(DATASET%emissionsAtmosphericDryDepoContaminant,5)), &
                                                          1:min(np,size(DATASET%emissionsAtmosphericDryDepoContaminant,6))), &
                    me%j_contaminant%c(1:min(ns,size(DATASET%emissionsAtmosphericDryDepoContaminant,4)), &
                                       1:min(nf,size(DATASET%emissionsAtmosphericDryDepoContaminant,5)), &
                                       1:min(np,size(DATASET%emissionsAtmosphericDryDepoContaminant,6))) )
            end if
        end if

        if (allocated(DATASET%emissionsAtmosphericWetDepoContaminant)) then
            if (has_xyt_6d(DATASET%emissionsAtmosphericWetDepoContaminant, me%x, me%y, t)) then
                call copy_pfas_tensor( &
                    DATASET%emissionsAtmosphericWetDepoContaminant(me%x,me%y,t, &
                                                            1:min(ns,size(DATASET%emissionsAtmosphericWetDepoContaminant,4)), &
                                                            1:min(nf,size(DATASET%emissionsAtmosphericWetDepoContaminant,5)), &
                                                            1:min(np,size(DATASET%emissionsAtmosphericWetDepoContaminant,6))), &
                    me%j_contaminant%c(1:min(ns,size(DATASET%emissionsAtmosphericWetDepoContaminant,4)), &
                                       1:min(nf,size(DATASET%emissionsAtmosphericWetDepoContaminant,5)), &
                                       1:min(np,size(DATASET%emissionsAtmosphericWetDepoContaminant,6))) )
            end if
        end if

        if (allocated(DATASET%emissionsAtmosphericDryDepoDissolvedContaminant)) then
            if (has_xyt_3d(DATASET%emissionsAtmosphericDryDepoDissolvedContaminant, me%x, me%y, t)) then
                if (DATASET%emissionsAtmosphericDryDepoDissolvedContaminant(me%x,me%y,t) /= nf90_fill_double) &
                    call add_scalar_to_aq(me%j_contaminant, DATASET%emissionsAtmosphericDryDepoDissolvedContaminant(me%x,me%y,t))
            end if
        end if
        if (allocated(DATASET%emissionsAtmosphericWetDepoDissolvedContaminant)) then
            if (has_xyt_3d(DATASET%emissionsAtmosphericWetDepoDissolvedContaminant, me%x, me%y, t)) then
                if (DATASET%emissionsAtmosphericWetDepoDissolvedContaminant(me%x,me%y,t) /= nf90_fill_double) &
                    call add_scalar_to_aq(me%j_contaminant, DATASET%emissionsAtmosphericWetDepoDissolvedContaminant(me%x,me%y,t))
            end if
        end if
    end subroutine load_atmospheric_deposition


    subroutine copy_pfas_tensor(src, dst)
        real(dp), intent(in)    :: src(:,:,:)
        real(dp), intent(inout) :: dst(:,:,:)
        integer :: i, f, p
        do p = 1, size(src,3)
            do f = 1, size(src,2)
                do i = 1, size(src,1)
                    if (src(i,f,p) /= nf90_fill_double) dst(i,f,p) = dst(i,f,p) + src(i,f,p)
                end do
            end do
        end do
    end subroutine copy_pfas_tensor


    subroutine add_scalar_to_aq(cont, mass)
        type(Contaminant), intent(inout) :: cont
        real(dp), intent(in)             :: mass
        if (.not. allocated(cont%c)) return
        if (mass <= 0.0_dp) return
        cont%c(1,1,PFAS_AQ) = cont%c(1,1,PFAS_AQ) + mass
    end subroutine add_scalar_to_aq


    logical function has_xy_2d(a, x, y)
        real(dp), intent(in) :: a(:,:)
        integer, intent(in) :: x, y
        has_xy_2d = x>=1 .and. y>=1 .and. x<=size(a,1) .and. y<=size(a,2)
    end function has_xy_2d

    logical function has_xyt_3d(a, x, y, t)
        real(dp), intent(in) :: a(:,:,:)
        integer, intent(in) :: x, y, t
        has_xyt_3d = x>=1 .and. y>=1 .and. t>=1 .and. x<=size(a,1) .and. y<=size(a,2) .and. t<=size(a,3)
    end function has_xyt_3d

    logical function has_xy_5d(a, x, y)
        real(dp), intent(in) :: a(:,:,:,:,:)
        integer, intent(in) :: x, y
        has_xy_5d = x>=1 .and. y>=1 .and. x<=size(a,1) .and. y<=size(a,2)
    end function has_xy_5d

    logical function has_xyt_6d(a, x, y, t)
        real(dp), intent(in) :: a(:,:,:,:,:,:)
        integer, intent(in) :: x, y, t
        has_xyt_6d = x>=1 .and. y>=1 .and. t>=1 .and. x<=size(a,1) .and. y<=size(a,2) .and. t<=size(a,3)
    end function has_xyt_6d
end module

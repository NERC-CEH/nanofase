module PointSourceModule
    use GlobalsModule
    use ResultModule
    use DataInputModule, only: DATASET
    use ContaminantModule
    use PFASEConstantsModule
    use netcdf, only: nf90_fill_double
    implicit none
   
    type, public :: PointSource
        integer                     :: x                                !! Grid cell x reference
        integer                     :: y                                !! Grid cell y reference
        integer                     :: s                                !! Point source reference
        real                        :: x_coord                          !! Exact eastings of this point source
        real                        :: y_coord                          !! Exact northings of this point source
        character(len=11)           :: compartment                      !! Which environmental compartment is this source for?
        type(Contaminant)           :: j_contaminant_pointSource        !! Contaminant input for a given time step
        real(dp)                    :: j_dissolved_pointSource          !! Dissolved species input for a given time step [kg/timestep]
      contains
        procedure :: create => createPointSource
        procedure :: update => updatePointSource
    end type

  contains
    
    subroutine createPointSource(me, x, y, s, compartment)
        class(PointSource), intent(inout) :: me
        integer, intent(in)               :: x, y, s
        character(len=*), intent(in)      :: compartment
        type(Result)                      :: r

        me%x = x
        me%y = y
        me%s = s
        me%compartment = adjustl(compartment)
        me%j_dissolved_pointSource = 0.0_dp

        r = me%j_contaminant_pointSource%create()
        if (r%hasCriticalError()) call ERROR_HANDLER%trigger(errors=.errors.r)

        if (allocated(DATASET%emissionsPointWaterCoords)) then
            if (has_point_coord_index(me%x, me%y, me%s)) then
                if (DATASET%emissionsPointWaterCoords(me%x,me%y,me%s,1) /= nf90_fill_double) then
                    me%x_coord = DATASET%emissionsPointWaterCoords(me%x,me%y,me%s,1)
                    me%y_coord = DATASET%emissionsPointWaterCoords(me%x,me%y,me%s,2)
                end if
            end if
        end if
    end subroutine createPointSource


    subroutine updatePointSource(me, t)
        class(PointSource), intent(inout) :: me
        integer, intent(in)               :: t
        integer                           :: ns, nf, np
        integer                           :: ns_in, nf_in, np_in
        type(Result)                      :: r

        call me%j_contaminant_pointSource%finalise()
        r = me%j_contaminant_pointSource%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if

        me%j_dissolved_pointSource = 0.0_dp

        ! Note the warm up period is not checked here - the timestep index restarts at 1 for
        ! the main run, so it can't be distinguished from a warm up timestep by t alone.
        ! Suppressing sources during warm up is the caller's job (see Reach%update).
        if (.not. C%includePointSources) return
        if (trim(me%compartment) /= 'water' .and. trim(me%compartment) /= 'estuary') return
        if (.not. allocated(me%j_contaminant_pointSource%c)) return

        ns = size(me%j_contaminant_pointSource%c,1)
        nf = size(me%j_contaminant_pointSource%c,2)
        np = size(me%j_contaminant_pointSource%c,3)

        if (allocated(DATASET%emissionsPointWaterContaminant)) then
            if (point_array_has_index(me%x, me%y, t, me%s)) then
                ns_in = min(ns, size(DATASET%emissionsPointWaterContaminant,5))
                nf_in = min(nf, size(DATASET%emissionsPointWaterContaminant,6))
                np_in = min(np, size(DATASET%emissionsPointWaterContaminant,7))

                call copy_pfas_point_tensor( &
                    DATASET%emissionsPointWaterContaminant(me%x,me%y,t,me%s,1:ns_in,1:nf_in,1:np_in), &
                    me%j_contaminant_pointSource%c(1:ns_in,1:nf_in,1:np_in) )
            end if
        end if

        if (allocated(DATASET%emissionsPointWaterDissolvedContaminant)) then
            if (dissolved_point_array_has_index(me%x, me%y, t)) then
                if (DATASET%emissionsPointWaterDissolvedContaminant(me%x,me%y,t) /= nf90_fill_double) then
                    me%j_dissolved_pointSource = DATASET%emissionsPointWaterDissolvedContaminant(me%x,me%y,t)
                    call add_scalar_to_aq(me%j_contaminant_pointSource, me%j_dissolved_pointSource)
                end if
            end if
        end if

        me%j_contaminant_pointSource%m_dissolved = sum(me%j_contaminant_pointSource%c(:,:,PFAS_AQ))
    end subroutine updatePointSource


    subroutine finalisePointSource(me)
        class(PointSource), intent(inout) :: me
        call me%j_contaminant_pointSource%finalise()
        me%x = 0
        me%y = 0
        me%s = 0
        me%x_coord = 0.0
        me%y_coord = 0.0
        me%compartment = ''
        me%j_dissolved_pointSource = 0.0_dp
    end subroutine finalisePointSource


    logical function has_point_coord_index(x, y, s)
        integer, intent(in) :: x, y, s
        has_point_coord_index = x >= 1 .and. y >= 1 .and. s >= 1 .and. &
            x <= size(DATASET%emissionsPointWaterCoords,1) .and. &
            y <= size(DATASET%emissionsPointWaterCoords,2) .and. &
            s <= size(DATASET%emissionsPointWaterCoords,3) .and. &
            size(DATASET%emissionsPointWaterCoords,4) >= 2
    end function has_point_coord_index


    logical function point_array_has_index(x, y, t, s)
        integer, intent(in) :: x, y, t, s
        point_array_has_index = x >= 1 .and. y >= 1 .and. t >= 1 .and. s >= 1 .and. &
            x <= size(DATASET%emissionsPointWaterContaminant,1) .and. &
            y <= size(DATASET%emissionsPointWaterContaminant,2) .and. &
            t <= size(DATASET%emissionsPointWaterContaminant,3) .and. &
            s <= size(DATASET%emissionsPointWaterContaminant,4)
    end function point_array_has_index


    logical function dissolved_point_array_has_index(x, y, t)
        integer, intent(in) :: x, y, t
        dissolved_point_array_has_index = x >= 1 .and. y >= 1 .and. t >= 1 .and. &
            x <= size(DATASET%emissionsPointWaterDissolvedContaminant,1) .and. &
            y <= size(DATASET%emissionsPointWaterDissolvedContaminant,2) .and. &
            t <= size(DATASET%emissionsPointWaterDissolvedContaminant,3)
    end function dissolved_point_array_has_index


    subroutine copy_pfas_point_tensor(src, dst)
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
    end subroutine copy_pfas_point_tensor


    subroutine add_scalar_to_aq(cont, mass)
        type(Contaminant), intent(inout) :: cont
        real(dp), intent(in)             :: mass
        if (.not. allocated(cont%c)) return
        if (mass <= 0.0_dp) return
        cont%c(1,1,PFAS_AQ) = cont%c(1,1,PFAS_AQ) + mass
    end subroutine add_scalar_to_aq

end module 
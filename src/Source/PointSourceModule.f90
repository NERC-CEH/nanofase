module PointSourceModule
    use GlobalsModule
    use ResultModule
    use DataInputModule, only: DATASET
    use ContaminantModule
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
        class(PointSource)  :: me               !! This point source
        integer             :: x                !! Grid cell x index
        integer             :: y                !! Grid cell y index
        integer             :: s                !! Point source index
        character(len=*)    :: compartment      !! Compartment type (only water at the moment)
        type(Result)        :: r                !! Result object for error handling
        ! Allocate and initialize
        me%x = x
        me%y = y
        me%s = s
        me%compartment = compartment
        ! Initialize the Contaminant object
        r = me%j_contaminant_pointSource%create()
        if (r%hasCriticalError()) call ERROR_HANDLER%trigger(errors=.errors.r)
        me%j_dissolved_pointSource = 0.0_dp
        ! Get the exact coordinates of this point source
        if (DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 1) /= nf90_fill_double) then
            me%x_coord = DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 1)
            me%y_coord = DATASET%emissionsPointWaterCoords(me%x, me%y, me%s, 2)
        end if
    end subroutine
    
    subroutine updatePointSource(me, t)
        class(PointSource)  :: me           !! This point source
        integer             :: t            !! Current time step
        integer             :: n, s, f      !! Iterators for size classes, SPM states, and forms
        type(Result)        :: r            !! Result object for error handling
        ! Default to zero
        call me%j_contaminant_pointSource%finalise() ! Reset to zero
        r = me%j_contaminant_pointSource%create()   ! Reallocate
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if
        me%j_dissolved_pointSource = 0.0_dp
        ! Only include point sources if config says we're meant to, and we're not in the
        ! warm up period
        if (C%includePointSources .and. t >= C%warmUpPeriod) then
            if (trim(me%compartment) == 'water') then
                ! Pristine and transformed contaminants
                do n = 1, C%nContaminantSizeClasses
                    do f = 1, C%contaminantDim(2) ! Forms (pristine, transformed)
                        ! Free contaminant (state = FREE_CONTAMINANT)
                        if (DATASET%emissionsPointWaterContaminant(me%x, me%y, t, me%s, n, f, FREE_CONTAMINANT) &
                            /= nf90_fill_double) then
                            me%j_contaminant_pointSource%c(n, f, FREE_CONTAMINANT) = &
                                DATASET%emissionsPointWaterContaminant(me%x, me%y, t, me%s, n, f, FREE_CONTAMINANT)
                        end if
                        ! Matrix-embedded (attached to SPM)
                        do s = 1, C%nSizeClassesSpm
                            if (DATASET%emissionsPointWaterContaminant(me%x, me%y, t, me%s, n, f, &
                                SPM_CONTAMINANT_START + s - 1) /= nf90_fill_double) then
                                me%j_contaminant_pointSource%c(n, f, SPM_CONTAMINANT_START + s - 1) = &
                                    DATASET%emissionsPointWaterContaminant(me%x, me%y, t, me%s, n, f, &
                                    SPM_CONTAMINANT_START + s - 1)
                            end if
                        end do
                    end do
                end do
                ! Dissolved
                if (DATASET%emissionsPointWaterDissolvedContaminant(me%x, me%y, t) /= nf90_fill_double) then
                    me%j_dissolved_pointSource = DATASET%emissionsPointWaterDissolvedContaminant(me%x, me%y, t)
                else
                    me%j_dissolved_pointSource = 0.0_dp
                end if

                ! FIX: Add the dissolved scalar to the Contaminant object so the Reach receives it
                me%j_contaminant_pointSource%m_dissolved = me%j_dissolved_pointSource
            end if
        end if
    end subroutine

end module
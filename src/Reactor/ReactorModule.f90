module ReactorModule
    use ContaminantModule 
    use GlobalsModule
    use ResultModule
    use AbstractReactorModule
    use DataInputModule, only: DATASET
    implicit none

    type, public, extends(AbstractReactor) :: Reactor
        character(len=100) :: compartment
        real(dp) :: T_water  
        real(dp), allocatable :: C_spm(:)
        real(dp), allocatable :: W_settle_spm(:)
        real :: G  
        real(dp), allocatable :: k_att(:)
        real(dp) :: alpha_att
        real(dp) :: velocity  
      contains
        procedure :: create => createReactor
        procedure :: update => updateReactor
        procedure :: finalise => finaliseReactor
        procedure :: parseInputData => parseInputDataReactor
    end type

  contains

   function createReactor(me, x, y, compartment, contaminant_in, volume, T_water, &
                       C_spm, W_settle_spm, G, k_att, alpha_att, velocity) result(r)
        class(Reactor), intent(inout) :: me
        integer, intent(in)           :: x, y
        character(len=*), intent(in)  :: compartment
        type(Contaminant), target, intent(in) :: contaminant_in
        real(dp), intent(in)          :: volume
        real(dp), intent(in)          :: T_water
        real(dp), intent(in), optional :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in), optional :: G
        real(dp), intent(in), optional :: k_att(:), alpha_att, velocity
        type(Result)                  :: r

        integer :: alloc_stat
        integer :: nspm
        real(dp), allocatable :: tmp(:)

        ! Basic properties
        me%x = x
        me%y = y
        me%compartment = trim(adjustl(compartment))
        me%volume = volume
        me%T_water = T_water
        me%contaminant => contaminant_in

        ! Defaults
        me%G         = 0.0
        me%alpha_att = 0.0_dp
        me%velocity  = 0.0_dp
        if (present(G))         me%G         = real(G, kind=kind(me%G))
        if (present(alpha_att)) me%alpha_att = alpha_att
        if (present(velocity))  me%velocity  = velocity

        ! Clean pre-existing allocations (defensive)
        if (allocated(me%C_spm))        deallocate(me%C_spm)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        if (allocated(me%k_att))        deallocate(me%k_att)

        ! ---- Allocate & set C_spm ------------------------------------------------
        if (present(C_spm) .and. size(C_spm) > 0) then
            allocate(me%C_spm(size(C_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=901, message="Failed to allocate C_spm"))
                return
            end if
            me%C_spm = C_spm
        else
            ! Use configured number of SPM size classes (fall back to 1 if needed)
            nspm = max(1, C%nSizeClassesSpm)
            allocate(me%C_spm(nspm), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=901, message="Failed to allocate C_spm"))
                return
            end if
            me%C_spm = 0.0_dp
        end if

        ! ---- Allocate & set W_settle_spm -----------------------------------------
        if (present(W_settle_spm) .and. size(W_settle_spm) > 0) then
            allocate(me%W_settle_spm(size(W_settle_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=902, message="Failed to allocate W_settle_spm"))
                return
            end if
            me%W_settle_spm = W_settle_spm
        else
            allocate(me%W_settle_spm(size(me%C_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=902, message="Failed to allocate W_settle_spm"))
                return
            end if
            me%W_settle_spm = 0.0_dp
        end if

        ! Keep SPM arrays consistent in length (resize W_settle_spm if needed)
        if (size(me%W_settle_spm) /= size(me%C_spm)) then
            allocate(tmp(size(me%C_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=904, message="Failed to reallocate W_settle_spm to match C_spm length"))
                return
            end if
            tmp = 0.0_dp
            tmp(1:min(size(tmp), size(me%W_settle_spm))) = &
                me%W_settle_spm(1:min(size(tmp), size(me%W_settle_spm)))
            call move_alloc(tmp, me%W_settle_spm)
        end if

        ! ---- Optional k_att ------------------------------------------------------
        if (present(k_att)) then
            allocate(me%k_att(size(k_att)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=903, message="Failed to allocate k_att"))
                return
            end if
            me%k_att = k_att
        end if

        ! Pull any grid-based inputs (if the reactor needs them)
        call r%addErrors(.errors. me%parseInputData())
    end function


    function updateReactor(me, j_contaminant_in, dt) result(r)
        class(Reactor), intent(inout) :: me
        type(Contaminant), intent(in), optional :: j_contaminant_in
        real(dp), intent(in) :: dt
        type(Result) :: r

        ! Add inflows if provided
        if (present(j_contaminant_in)) then
            call me%contaminant%add(j_contaminant_in)
        end if

        ! Update based on compartment
        select case (me%compartment)
            case ('water', 'estuary')
                ! Calculate k_att for water/estuary if not already allocated
                if (.not. allocated(me%k_att)) then
                    allocate(me%k_att(C%nContaminantSizeClasses))
                    me%k_att = me%contaminant%calculateAttachmentRate(me%T_water, DATASET%soilDefaultPorosity, &
                                                                    DATASET%spmSizeClasses(1), me%velocity)
                    me%alpha_att = merge(DATASET%riverAttachmentEfficiency, DATASET%estuaryAttachmentEfficiency, &
                                        me%compartment == 'water')
                end if
                call r%addErrors(.errors. me%contaminant%update(dt, me%T_water, me%C_spm, me%W_settle_spm, &
                                                                real(me%G, dp), me%volume, me%compartment, me%k_att, me%alpha_att))
            case ('sediment')
                call r%addErrors(.errors. me%contaminant%update(dt, me%T_water, me%C_spm, me%W_settle_spm, &
                                                                real(me%G, dp), me%volume, 'sediment'))
            case ('soil')
                call r%addErrors(.errors. me%contaminant%update(dt, me%T_water, me%C_spm, me%W_settle_spm, &
                                                                real(me%G, dp), me%volume, 'soil', me%k_att, me%alpha_att))
            case default
                call r%addError(ErrorInstance(code=900, message="Invalid compartment: " // trim(me%compartment)))
        end select
    end function

    subroutine finaliseReactor(me)
        class(Reactor), intent(inout) :: me
        if (associated(me%contaminant)) then
            call me%contaminant%finalise()
            me%contaminant => null()
        end if
        if (allocated(me%C_spm)) deallocate(me%C_spm)
        if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
        if (allocated(me%k_att)) deallocate(me%k_att)
    end subroutine

    function parseInputDataReactor(me) result(r)
        class(Reactor), intent(inout) :: me
        type(Result) :: r
        ! Placeholder for additional data parsing if needed
    end function
end module
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
        integer, intent(in) :: x, y
        character(len=*), intent(in) :: compartment
        type(Contaminant), target, intent(in) :: contaminant_in
        real(dp), intent(in) :: volume
        real(dp), intent(in) :: T_water 
        real(dp), intent(in), optional :: C_spm(:), W_settle_spm(:)
        real(dp), intent(in), optional :: G  
        real(dp), intent(in), optional :: k_att(:), alpha_att
        real(dp), intent(in), optional :: velocity  
        type(Result) :: r
        integer :: alloc_stat

        me%x = x
        me%y = y
        me%compartment = compartment
        me%contaminant => contaminant_in
        me%volume = max(volume, 0.0_dp)
        me%T_water = T_water
        me%velocity = merge(velocity, 0.0_dp, present(velocity))

        ! Initialize Contaminant object
        select case (compartment)
            case ('water', 'estuary', 'sediment')
                call r%addErrors(.errors. me%contaminant%create_from_data( &
                    DATASET%nc, compartment, DATASET%contaminantDensity, &
                    DATASET%soilConstantAttachmentEfficiency, DATASET%riverAttachmentEfficiency, &
                    DATASET%estuaryAttachmentEfficiency, DATASET%contaminant_k_diss_pristine, &
                    DATASET%contaminant_k_diss_transformed, DATASET%contaminant_k_transform_pristine, &
                    T_water))
            case ('soil')
                call r%addErrors(.errors. me%contaminant%create_from_data( &
                    DATASET%nc, 'soil', DATASET%contaminantDensity, &
                    DATASET%soilConstantAttachmentEfficiency, DATASET%riverAttachmentEfficiency, &
                    DATASET%estuaryAttachmentEfficiency, DATASET%contaminant_k_diss_pristine, &
                    DATASET%contaminant_k_diss_transformed, DATASET%contaminant_k_transform_pristine, &
                    T_water))
            case default
                call r%addError(ErrorInstance(code=900, message="Invalid compartment: " // trim(compartment)))
                return
        end select

        ! Allocate and assign optional parameters
        if (present(C_spm)) then
            allocate(me%C_spm(size(C_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=901, message="Failed to allocate C_spm"))
                return
            end if
            me%C_spm = C_spm
        end if
        if (present(W_settle_spm)) then
            allocate(me%W_settle_spm(size(W_settle_spm)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=901, message="Failed to allocate W_settle_spm"))
                return
            end if
            me%W_settle_spm = W_settle_spm
        end if
        if (present(G)) me%G = G
        if (present(k_att)) then
            allocate(me%k_att(size(k_att)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call r%addError(ErrorInstance(code=901, message="Failed to allocate k_att"))
                return
            end if
            me%k_att = k_att
        end if
        if (present(alpha_att)) me%alpha_att = alpha_att

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
                if (.not. (allocated(me%C_spm) .and. allocated(me%W_settle_spm) .and. me%G >= 0.0_dp)) then
                    call r%addError(ErrorInstance(code=900, message="Required parameters for water/estuary not provided"))
                    return
                end if
                ! Calculate k_att for water/estuary
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
                if (.not. (allocated(me%C_spm) .and. allocated(me%W_settle_spm) .and. me%G >= 0.0_dp)) then
                    call r%addError(ErrorInstance(code=900, message="Required parameters for sediment not provided"))
                    return
                end if
                call r%addErrors(.errors. me%contaminant%update(dt, me%T_water, me%C_spm, me%W_settle_spm, &
                                                                real(me%G, dp), me%volume, 'sediment'))
            case ('soil')
                if (.not. (allocated(me%k_att) .and. me%alpha_att > 0.0_dp)) then
                    call r%addError(ErrorInstance(code=900, message="k_att and alpha_att required for soil"))
                    return
                end if
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
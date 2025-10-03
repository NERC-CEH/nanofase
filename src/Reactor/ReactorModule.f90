module ReactorModule
    use ContaminantModule
    use GlobalsModule
    use ResultModule
    use AbstractReactorModule
    use DataInputModule, only: DATASET
    use LoggerModule,   only: LOGR
    use ErrorInstanceModule
    use UtilModule, only: ref
    implicit none

    type, public, extends(AbstractReactor) :: Reactor
        character(len=100)    :: compartment
        real(dp)              :: T_water
        real(dp), allocatable :: C_spm(:)
        real(dp), allocatable :: W_settle_spm(:)
        real(dp)              :: G
        real(dp), allocatable :: k_att(:)
        real(dp)              :: alpha_att
        real(dp)              :: velocity
    contains
        procedure :: create       => createReactor
        procedure :: update       => updateReactor
        procedure :: finalise     => finaliseReactor
        procedure :: parseInputData => parseInputDataReactor
    end type

contains

! Matches abstract interface exactly
function createReactor(me, x, y, compartment, contaminant_in, volume, T_water, &
                       C_spm, W_settle_spm, G, k_att, alpha_att, velocity) result(r)
    class(Reactor), intent(inout)                   :: me
    integer, intent(in)                             :: x, y
    character(len=*), intent(in)                    :: compartment
    type(Contaminant), target, intent(in)           :: contaminant_in
    real(dp), intent(in)                            :: volume
    real(dp), intent(in)                            :: T_water
    real(dp), intent(in), optional                  :: C_spm(:), W_settle_spm(:)
    real(dp), intent(in), optional                  :: G
    real(dp), intent(in), optional                  :: k_att(:), alpha_att
    real(dp), intent(in), optional                  :: velocity
    type(Result)                                     :: r

    integer :: alloc_stat
    integer :: nspm

    ! basics
    me%x = x; me%y = y
    me%ref        = trim(ref('Reactor', x, y, 0))
    me%compartment = trim(compartment)
    me%volume      = volume
    me%T_water     = T_water

    ! associate to live contaminant (caller-owned)
    if (associated(me%contaminant)) nullify(me%contaminant)
    me%contaminant => contaminant_in

    ! defaults
    me%G         = 0.0_dp
    me%alpha_att = 0.0_dp
    me%velocity  = 0.0_dp
    if (present(G))         me%G         = G
    if (present(alpha_att)) me%alpha_att = alpha_att
    if (present(velocity))  me%velocity  = velocity

    ! (re)allocate arrays
    if (allocated(me%C_spm))        deallocate(me%C_spm)
    if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
    if (allocated(me%k_att))        deallocate(me%k_att)

    if (present(C_spm)) then
        allocate(me%C_spm(size(C_spm)), stat=alloc_stat); if (alloc_stat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate C_spm")); return
        end if
        me%C_spm = C_spm
    else
        nspm = max(1, C%nSizeClassesSpm)
        allocate(me%C_spm(nspm), stat=alloc_stat); if (alloc_stat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate C_spm")); return
        end if
        me%C_spm = 0.0_dp
    end if

    if (present(W_settle_spm)) then
        allocate(me%W_settle_spm(size(W_settle_spm)), stat=alloc_stat); if (alloc_stat /= 0) then
            call r%addError(ErrorInstance(code=902, message="Failed to allocate W_settle_spm")); return
        end if
        me%W_settle_spm = W_settle_spm
    else
        allocate(me%W_settle_spm(size(me%C_spm)), stat=alloc_stat); if (alloc_stat /= 0) then
            call r%addError(ErrorInstance(code=902, message="Failed to allocate W_settle_spm")); return
        end if
        me%W_settle_spm = 0.0_dp
    end if

    if (present(k_att)) then
        allocate(me%k_att(size(k_att)), stat=alloc_stat); if (alloc_stat /= 0) then
            call r%addError(ErrorInstance(code=903, message="Failed to allocate k_att")); return
        end if
        me%k_att = k_att
    end if
end function createReactor


function updateReactor(me, j_contaminant_in, dt) result(r)
    class(Reactor), intent(inout)           :: me
    type(Contaminant), intent(in), optional :: j_contaminant_in
    real(dp), intent(in)                    :: dt
    type(Result)                            :: r
    type(ErrorInstance)                     :: err(1)
    real(dp), allocatable                   :: local_k_att(:)
    real(dp)                                :: d_grain_eff

    if (.not. associated(me%contaminant)) then
         err(1) = ErrorInstance(code=905, message="Reactor's contaminant pointer is not associated.")
         call r%addErrors(err)
         return
    end if

    select case (trim(me%compartment))
    case ('water','estuary')
        ! If k_att is provided externally, use it directly; otherwise build a temporary fallback.
        if (allocated(me%k_att)) then
            call r%addErrors(.errors. me%contaminant%update( &
                dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume,  &
                me%compartment, me%k_att, me%alpha_att))
        else
            allocate(local_k_att(C%contaminantDim(1)))
            if (allocated(DATASET%spmSizeClasses) .and. size(DATASET%spmSizeClasses) > 0) then
                d_grain_eff = DATASET%spmSizeClasses(1)
            else
                d_grain_eff = C%d_spm(1)    ! representative SPM diameter
            end if
            local_k_att = me%contaminant%calculateAttachmentRate( &
                me%T_water, DATASET%soilDefaultPorosity, d_grain_eff, me%velocity)

            call r%addErrors(.errors. me%contaminant%update( &
                dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume,  &
                me%compartment, local_k_att, me%alpha_att))

            deallocate(local_k_att)
        end if

    case ('sediment')
        call r%addErrors(.errors. me%contaminant%update( &
            dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume, 'sediment'))

    case ('soil')
        call r%addErrors(.errors. me%contaminant%update( &
            dt, me%T_water, me%C_spm, me%W_settle_spm, me%G, me%volume, 'soil', me%k_att, me%alpha_att))

    case default
        err(1) = ErrorInstance(code=900, message="Invalid compartment in Reactor: " // trim(me%compartment))
        call r%addErrors(err)
    end select
end function updateReactor



subroutine finaliseReactor(me)
    class(Reactor), intent(inout) :: me
    if (associated(me%contaminant)) nullify(me%contaminant)
    if (allocated(me%C_spm))        deallocate(me%C_spm)
    if (allocated(me%W_settle_spm)) deallocate(me%W_settle_spm)
    if (allocated(me%k_att))        deallocate(me%k_att)
end subroutine finaliseReactor


function parseInputDataReactor(me) result(r)
    class(Reactor), intent(inout) :: me
    type(Result) :: r
    ! hook for future per-cell reactor inputs
end function parseInputDataReactor

end module

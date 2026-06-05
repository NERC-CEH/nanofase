module PFASEBoundaryFluxModule
    use GlobalsModule, only: dp
    implicit none
    type, public :: PFASEBoundaryFlux
        real(dp), allocatable :: to_groundwater(:)
        real(dp), allocatable :: to_downstream(:)
        real(dp), allocatable :: to_sediment(:)
        real(dp), allocatable :: to_atmosphere(:)
        real(dp), allocatable :: to_foam(:)
        real(dp), allocatable :: to_biota(:)
    contains
        procedure :: create => createFlux
        procedure :: empty => emptyFlux
        procedure :: add => addFlux
        procedure :: finalise => finaliseFlux
    end type
contains
    subroutine createFlux(this, n)
        class(PFASEBoundaryFlux), intent(inout) :: this
        integer, intent(in) :: n
        call this%finalise()
        allocate(this%to_groundwater(n), this%to_downstream(n), this%to_sediment(n), &
                 this%to_atmosphere(n), this%to_foam(n), this%to_biota(n))
        call this%empty()
    end subroutine createFlux

    subroutine emptyFlux(this)
        class(PFASEBoundaryFlux), intent(inout) :: this
        if (allocated(this%to_groundwater)) this%to_groundwater = 0.0_dp
        if (allocated(this%to_downstream))  this%to_downstream  = 0.0_dp
        if (allocated(this%to_sediment))    this%to_sediment    = 0.0_dp
        if (allocated(this%to_atmosphere))  this%to_atmosphere  = 0.0_dp
        if (allocated(this%to_foam))        this%to_foam        = 0.0_dp
        if (allocated(this%to_biota))       this%to_biota       = 0.0_dp
    end subroutine emptyFlux

    subroutine addFlux(this, other)
        class(PFASEBoundaryFlux), intent(inout) :: this
        type(PFASEBoundaryFlux), intent(in) :: other
        if (allocated(this%to_groundwater) .and. allocated(other%to_groundwater)) this%to_groundwater = this%to_groundwater + other%to_groundwater
        if (allocated(this%to_downstream)  .and. allocated(other%to_downstream))  this%to_downstream  = this%to_downstream  + other%to_downstream
        if (allocated(this%to_sediment)    .and. allocated(other%to_sediment))    this%to_sediment    = this%to_sediment    + other%to_sediment
        if (allocated(this%to_atmosphere)  .and. allocated(other%to_atmosphere))  this%to_atmosphere  = this%to_atmosphere  + other%to_atmosphere
        if (allocated(this%to_foam)        .and. allocated(other%to_foam))        this%to_foam        = this%to_foam        + other%to_foam
        if (allocated(this%to_biota)       .and. allocated(other%to_biota))       this%to_biota       = this%to_biota       + other%to_biota
    end subroutine addFlux

    subroutine finaliseFlux(this)
        class(PFASEBoundaryFlux), intent(inout) :: this
        if (allocated(this%to_groundwater)) deallocate(this%to_groundwater)
        if (allocated(this%to_downstream))  deallocate(this%to_downstream)
        if (allocated(this%to_sediment))    deallocate(this%to_sediment)
        if (allocated(this%to_atmosphere))  deallocate(this%to_atmosphere)
        if (allocated(this%to_foam))        deallocate(this%to_foam)
        if (allocated(this%to_biota))       deallocate(this%to_biota)
    end subroutine finaliseFlux
end module 

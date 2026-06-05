module PFASEProcessModule
    !! Generic PFAS mass-transfer operators.
    use GlobalsModule, only: dp, C
    use PFASEConstantsModule
    implicit none
contains
    pure function pfase_clamp01(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = max(0.0_dp, min(1.0_dp, x))
    end function pfase_clamp01

    subroutine pfase_remove_by_fraction(pool, fraction, removed)
        real(dp), intent(inout) :: pool(:)
        real(dp), intent(in)    :: fraction
        real(dp), intent(out)   :: removed(:)
        real(dp) :: f
        f = pfase_clamp01(fraction)
        removed = min(max(0.0_dp, pool), max(0.0_dp, pool) * f)
        pool = max(0.0_dp, pool - removed)
    end subroutine pfase_remove_by_fraction

    subroutine pfase_equilibrium_partition(c, volume_water, mass_solid, kd, phase)
        !! Equilibrium linear partitioning between AQ and one sorbed phase.
        !! Kd units: m3 kg-1 when volume_water is m3 and mass_solid is kg.
        real(dp), intent(inout) :: c(:,:,:)
        real(dp), intent(in)    :: volume_water, mass_solid
        real(dp), intent(in)    :: kd(:)
        integer, intent(in)     :: phase
        integer :: i, f
        real(dp) :: denom, mtot, aq_new
        if (volume_water <= C%epsilon .or. mass_solid <= 0.0_dp) return
        if (phase < 1 .or. phase > size(c,3) .or. phase == PFAS_AQ) return
        do i = 1, min(size(c,1), size(kd))
            denom = volume_water + max(0.0_dp, kd(i)) * mass_solid
            if (denom <= C%epsilon) cycle
            do f = 1, size(c,2)
                mtot = max(0.0_dp, c(i,f,PFAS_AQ) + c(i,f,phase))
                aq_new = mtot * volume_water / denom
                c(i,f,PFAS_AQ) = aq_new
                c(i,f,phase) = mtot - aq_new
            end do
        end do
    end subroutine pfase_equilibrium_partition

    subroutine pfase_kinetic_exchange(c, dt, k_ads, k_des, capacity, phase)
        real(dp), intent(inout) :: c(:,:,:)
        real(dp), intent(in) :: dt, k_ads(:), k_des(:), capacity(:)
        integer, intent(in) :: phase
        integer :: i, f, n
        real(dp) :: ads, des, cap_remaining
        if (dt <= 0.0_dp) return
        if (phase < 1 .or. phase > size(c,3) .or. phase == PFAS_AQ) return
        n = min(size(c,1), size(k_ads), size(k_des), size(capacity))
        do i = 1, n
            do f = 1, size(c,2)
                cap_remaining = max(0.0_dp, capacity(i) - c(i,f,phase))
                ads = min(c(i,f,PFAS_AQ), max(0.0_dp, k_ads(i)) * dt * c(i,f,PFAS_AQ) * max(0.0_dp, cap_remaining))
                des = min(c(i,f,phase),  max(0.0_dp, k_des(i)) * dt * c(i,f,phase))
                c(i,f,PFAS_AQ) = max(0.0_dp, c(i,f,PFAS_AQ) - ads + des)
                c(i,f,phase)   = max(0.0_dp, c(i,f,phase)   + ads - des)
            end do
        end do
    end subroutine pfase_kinetic_exchange

    subroutine pfase_transform_network(c, dt, k_react, reaction_yield)
        !! First-order precursor/intermediate/terminal network.
        !! k_react(i,j) = rate from species i to species j [s-1].
        !! Competing outgoing reactions are handled as one total loss with branching.
        real(dp), intent(inout) :: c(:,:,:)
        real(dp), intent(in) :: dt, k_react(:,:), reaction_yield(:,:)

        integer :: i, j, f, p, ns
        real(dp), allocatable :: delta(:,:,:)
        real(dp) :: k_total, frac_loss, branch, loss

        if (dt <= 0.0_dp) return

        ns = min(size(c,1), size(k_react,1), size(k_react,2), &
                size(reaction_yield,1), size(reaction_yield,2))

        allocate(delta(size(c,1), size(c,2), size(c,3)))
        delta = 0.0_dp

        do i = 1, ns
            k_total = 0.0_dp
            do j = 1, ns
                if (j /= i) k_total = k_total + max(0.0_dp, k_react(i,j))
            end do

            if (k_total <= 0.0_dp) cycle

            frac_loss = pfase_clamp01(1.0_dp - exp(-k_total * dt))

            do f = 1, size(c,2)
                do p = 1, size(c,3)
                    loss = min(max(0.0_dp, c(i,f,p)), max(0.0_dp, c(i,f,p)) * frac_loss)
                    delta(i,f,p) = delta(i,f,p) - loss

                    do j = 1, ns
                        if (j == i) cycle
                        if (k_react(i,j) <= 0.0_dp) cycle
                        branch = max(0.0_dp, k_react(i,j)) / k_total
                        delta(j,f,p) = delta(j,f,p) + loss * branch * max(0.0_dp, reaction_yield(i,j))
                    end do
                end do
            end do
        end do

        c = max(0.0_dp, c + delta)
        deallocate(delta)
    end subroutine pfase_transform_network
end module

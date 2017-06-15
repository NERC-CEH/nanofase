! 
program main
    use classBedSedimentLayer1
    implicit none

    type(objBedSedimentLayer1) :: bs1
    type(objBedSedimentLayer1) :: bs2
    integer :: i

    ! Try adding two Biota objects, one of each type (1 and 2)
    call bs1%create( &
        ldepth=1.0, &
        lpdens=1.0, &
        lporosity=1.0, &
        lnbiota=2, &                ! Just a thought: lnbiota is redundant really, we can just use size(ltbiota)
        ltbiota=[1,2], &
        lnreactor=3, &
        ltreactor=[1,2,1] &
    )
    
    write(*, '(a)') "Bed sediment with two Biota objects:"
    ! Print out the names of the Biota objects ('Biota 1' and 'Biota 2')
    do i=1, size(bs1%colBiota)
        write(*,'(a)') trim(bs1%colBiota(i)%item%name)
    end do

    ! Do the same but add a few more Biota objects (1, 2, 2, 1, 2)
    call bs2%create( &
        ldepth=1.0, &
        lpdens=1.0, &
        lporosity=1.0, &
        lnbiota=5, &
        ltbiota=[1,2,2,1,2], &
        lnreactor=3, &
        ltreactor=[1,2,1] &
    )
    
    write(*, '(a)') "Bed sediment with five Biota objects:"
    ! Print out the names of the Biota objects ('Biota 1' and 'Biota 2')
    do i=1, size(bs2%colBiota)
        write(*,'(a)') trim(bs2%colBiota(i)%item%name)
    end do
end program
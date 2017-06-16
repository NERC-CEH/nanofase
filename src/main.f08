! Test implementation of the bed sediment and biota classes.
program main
    use classBedSedimentLayer1
    implicit none

    type(objBedSedimentLayer1) :: bs1           ! Bed sediment layer
    type(objBedSedimentLayer1) :: bs2           ! Another bed sediment layer
    integer :: i                                ! Loop iterator

    ! Initialise the first bed sediment layer with two Biota objects,
    ! one of each type (1 and 2).
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
    ! Print out the names of the Biota objects ('Biota 1' and 'Biota 2' - hard coded
    ! in classBiotaX for testing purposes.)
    do i=1, size(bs1%colBiota)
        write(*,'(a)') trim(bs1%colBiota(i)%item%name)
    end do

    ! Do the same with the second bed sediment layer, but add a few more Biota objects
    ! of types 1, 2, 2, 1, 2.
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
    ! Print out the names of the Biota objects
    do i=1, size(bs2%colBiota)
        write(*,'(a)') trim(bs2%colBiota(i)%item%name)
    end do
    
end program
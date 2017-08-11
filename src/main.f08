! Test implementation of the bed sediment and biota classes.
program main
    use Globals                                 ! For error handling. ErrorCriteria object is ERROR_HANDLER
    use classBedSedimentLayer1
    use classRiverReach1
    implicit none

    type(objBedSedimentLayer1) :: bs1           ! Bed sediment layer
    type(objBedSedimentLayer1) :: bs2           ! Another bed sediment layer
    type(objBedSedimentLayer1) :: bs3
    type(RiverReach1) :: rr
    type(Result) :: r
    integer :: i                                ! Loop iterator

    ! Initialise the error handler with custom error in Globals module
    call GLOBALS_INIT()

    ! Initialise the first bed sediment layer with two Biota objects,
    ! one of each type (1 and 2).
    call bs1%create("Bed Sediment Layer A", &
        ldepth=1.0, &
        lpdens=1.0, &
        lporosity=1.0, &
        ltbiota=[1,2], &
        ltreactor=[1,2,1] &
    )
    write(*, '(a)') "Bed sediment with two Biota objects and three Reactor objects:"
    ! Print out the names of the Biota objects ('Biota 1' and 'Biota 2' - hard coded
    ! in classBiotaX for testing purposes.)
    write (*,'(a)') trim(bs1%name)
    do i=1, size(bs1%colBiota)
        write(*,'(a)') trim(bs1%colBiota(i)%item%name)
    end do
    do i=1, size(bs1%colReactor)
        write(*,'(a)') trim(bs1%colReactor(i)%item%name)
    end do

    ! Do the same with the second bed sediment layer, but add a few more Biota objects
    ! of types 1, 2, 2, 1, 2.
    call bs2%create("Bed Sediment Layer B", &
        ldepth=1.0, &
        lpdens=1.0, &
        lporosity=1.0, &
        ltbiota=[1,2,2,1,2], &
        ltreactor=[1,2] &
    )
    write(*, '(a)') "Bed sediment with five Biota objects and two Reactor objects:"
    ! Print out the names of the Biota and Reactor objects
    write (*,'(a)') trim(bs2%name)
    do i=1, size(bs2%colBiota)
        write(*,'(a)') trim(bs2%colBiota(i)%item%name)
    end do
    do i=1, size(bs2%colReactor)
        write(*,'(a)') trim(bs2%colReactor(i)%item%name)
    end do

    ! Finally, let's try one that's going to fail, by specifying an index for ltbiota
    ! for which a specific Biota object doesn't exist.
    call bs3%create("Bed Sediment Layer C", &
        ldepth=1.0, &
        lpdens=1.0, &
        lporosity=1.0, &
        ltbiota=[1,2], &
        ltreactor=[1,2] &
    )

    ! Try to create a RiverReach
    r = rr%create()
    call ERROR_HANDLER%trigger(errors = .errors. r)
    print *, "River width: ", rr%W
    print *, "River depth: ", rr%D
    print *, "Settling velocities: ", rr%W_s
    print *, "Settling rates: ", rr%k_settle

end program

program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach1
    use classEnvironment1
    implicit none

    real :: start, finish                                               ! Simulation start and finish times
    type(Result) :: r                                                   ! Result object
    integer :: x,y,s,t                                                  ! Loop iterator
    type(Environment1) :: env                                           ! Environment object

    call GLOBALS_INIT()                                                 ! Set up global vars and constants
    open(unit=2,file=C%outputFile)                                      ! Open the output data file
    open(unit=3,file='data/output_erosion.csv')                         ! Open the output data file

    call cpu_time(start)                                                ! Simulation start time

    r = env%create()                                                    ! Create the environment

    do t = 1, C%nTimeSteps
        r = env%update(t)
        ! TODO: Do something with Result object
        do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
            do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
                if (.not. env%colGridCells(x,y)%item%isEmpty) then
                     write(3,*) t, ", ", x, ", ", y, ", ", &
                         env%colGridCells(x,y)%item%erodedSediment(1)
                    do s = 1, size(env%colGridCells(x,y)%item%colSubRivers) ! Loop through the SubRivers
                        ! Write to the data file
                        write(2,*) t, ", ", x, &
                             ", ", y, ", ", s, ", " &
                            , env%colGridCells(x,y)%item%colSubRivers(s)%item%m_spm(1), ", " &
                            , env%colGridCells(x,y)%item%colSubRivers(s)%item%m_spm(2), ", " &
                            , env%colGridCells(x,y)%item%colSubRivers(s)%item%m_spm(3), ", " &
                            , env%colGridCells(x,y)%item%colSubRivers(s)%item%m_spm(4), ", " &
                            , env%colGridCells(x,y)%item%colSubRivers(s)%item%m_spm(5)
                    end do
                end if
            end do
        end do
    end do

    close(2)                                                                ! Close the output file
    call cpu_time(finish)                                                   ! Simulation finish time
    print *, 'Time taken to simulate and write data (s): ', finish-start    ! How long did it take?

end program

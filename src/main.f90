program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach1
    use classEnvironment1
    implicit none

    real :: start, finish                                               ! Simulation start and finish times
    type(Result) :: r                                                   ! Result object
    integer :: x,y,rr,t                                                 ! Loop iterator
    real(dp) :: m_spm(5)
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
                    do rr = 1, env%colGridCells(x,y)%item%nRiverReaches ! Loop through the RiverReaches
                        m_spm = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_spm
                        ! Write to the data file
                        write(2,'(i4,A,i2,A,i2,A,i2,A,F12.3,A,F12.3,A,F12.3,a,f12.3,a,f12.3)') t, ", ", x, ", ", y, ", ", rr, ", ", &
                            m_spm(1), ", ", m_spm(2), ", ", m_spm(3), ", ", m_spm(4), ", ", m_spm(5)
                    end do
                end if
            end do
        end do
    end do

    close(2)                                                                ! Close the output file
    call cpu_time(finish)                                                   ! Simulation finish time
    print *, 'Time taken to simulate and write data (s): ', finish-start    ! How long did it take?

end program

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
    real(dp) :: m_np(5, 4, 7)
    real(dp) :: m_np_hetero(5,5)
    real(dp) :: m_np_free

    call GLOBALS_INIT()                                                 ! Set up global vars and constants
    open(unit=2,file=C%outputFile)                                      ! Open the output data file
    open(unit=3,file='data/output_erosion.csv')                         ! Open the output data file
    open(unit=4,file='data/output_hetero_vs_free.csv')                         ! Open the output data file
    open(unit=5,file='data/output_total_m_np.csv')                         ! Open the output data file

    call cpu_time(start)                                                ! Simulation start time

    r = env%create()                                                    ! Create the environment

    do t = 1, C%nTimeSteps
        m_np_free = 0
        m_np_hetero = 0
        r = env%update(t)
        ! TODO: Do something with Result object
        do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
            do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
                if (.not. env%colGridCells(x,y)%item%isEmpty) then
                    write(3,*) t, ", ", x, ", ", y, ", ", &
                         sum(env%colGridCells(x,y)%item%erodedSediment)
                    do rr = 1, env%colGridCells(x,y)%item%nRiverReaches ! Loop through the RiverReaches
                        m_spm = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_spm
                        m_np = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out
                        ! Write to the data file
                        write(2,'(i4,A,i2,A,i2,A,i2,A,F12.3,A,F12.3,A,F12.3,a,f12.3,a,f12.3)') t, ", ", x, ", ", y, ", ", rr, ", ", &
                            m_spm(1), ", ", m_spm(2), ", ", m_spm(3), ", ", m_spm(4), ", ", m_spm(5)
                        
                        write(5, *) t, ",", x, ",", y, ",", rr, ",", trim(str(sum(m_np)))
                        
                        m_np_hetero = m_np_hetero + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,3:) + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out(:,1,3:)
                        m_np_free = m_np_free + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,1)) + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out(:,1,1))
                    end do
                end if
            end do
        end do
        write(4, '(i4,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6)') t, ",", m_np_free, ",", sum(m_np_hetero), ",", sum(m_np_hetero(:,1)), ",", sum(m_np_hetero(:,2)), ",", &
            sum(m_np_hetero(:,3)), ",", sum(m_np_hetero(:,4)), ",", sum(m_np_hetero(:,5))
    end do

    close(2)                                                                ! Close the output file
    call cpu_time(finish)                                                   ! Simulation finish time
    print *, 'Time taken to simulate and write data (s): ', finish-start    ! How long did it take?

end program

! Test implementation of the bed sediment and biota classes.
program main
    use Globals                                 ! For error handling. ErrorCriteria object is ERROR_HANDLER
    use UtilModule
    use classBedSedimentLayer1
    use classRiverReach1
    use classEnvironment1
    implicit none

    real :: start, finish                                               ! Simulation start and finish times
    type(Result) :: r                                                   ! Result object
    integer :: x,y,s,t,i                                                ! Loop iterator
    type(Environment1) :: env                                           ! Environment object

    call GLOBALS_INIT()                                                 ! Set up global vars and constants
    open(unit=2,file='output.txt')                                      ! Open the output data file
    open(unit=3,file='output_hydrograph.txt')
    open(unit=4,file='output_spm.txt')

    call cpu_time(start)                                                ! Simulation start time

    r = env%create()                                                    ! Create the environment
    do t=1, 8760
        r = env%update(t)                                               ! Run the simulation for 1 year
        do x = 1, size(env%colGridCells, 1)                             ! Loop through the rows
            do y = 1, size(env%colGridCells, 2)                         ! Loop through the columns
                if (.not. env%colGridCells(x,y)%item%isEmpty) then
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
        write(3,*) t, ", ", env%colGridCells(1,1)%item%colSubRivers(1)%item%colReaches(1)%item%Qrunoff
        write(4,*) t, ", ", env%colGridCells(1,1)%item%colSubRivers(1)%item%colReaches(1)%item%m_spmTimeSeries(1,t)
    end do
    ! t doesn't do anything at the moment, but might be useful in the future for getting time-dependent
    ! data from input (e.g., met or runoff data).

    close(2)                                                                ! Close the output file
    call cpu_time(finish)                                                   ! Simulation finish time
    print *, 'Time taken to simulate and write data (s): ', finish-start    ! How long did it take?

end program

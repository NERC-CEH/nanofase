program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach1
    use classEnvironment1
    use classDataInterfacer, only: DATA
    use classLogger, only: LOG
    use datetime_module
    implicit none

    real :: start, finish                                               ! Simulation start and finish times
    type(Result) :: r                                                   ! Result object
    integer :: x, y, rr, t, i, s                                           ! Loop iterators
    real(dp) :: m_spm(5)
    type(Environment1) :: env                                           ! Environment object
    real(dp) :: m_np(5, 4, 7)
    real(dp) :: m_np_l1(5, 4, 7)
    real(dp) :: m_np_l2(5, 4, 7)
    real(dp) :: m_np_l3(5, 4, 7)
    real(dp) :: m_np_l4(5, 4, 7)
    real(dp) :: m_np_eroded(5, 4, 7)
    real(dp) :: m_np_buried(5, 4, 7)
    real(dp) :: m_np_in(5, 4, 7)
    real(dp) :: C_np(5, 4, 7)
    real(dp) :: npDep(5, 4, 7)
    real(dp) :: m_np_hetero(5, 5)
    real(dp) :: m_np_free
    real(dp) :: bedSedimentMass
    real(dp) :: npRunoff
    type(datetime) :: currentDate
    real(dp) :: riverVolume
    real(dp) :: Q_out
    real(dp) :: npPointSource

    ! Set up global vars and constants, and initialise data interfacer.
    ! These vars are available globally
    call GLOBALS_INIT()                                                 ! Set up global vars and constants
    call LOG%init( &
        logToFile=.true., &
        logToConsole=.true., &
        logFilePath=C%logFilePath &
    )
    call LOG%toConsole("--------------------------------")
    call LOG%toConsole(" Welcome to the NanoFASE model! ")
    call LOG%toConsole("--------------------------------\n")
    call DATA%init(C%inputFile)

    open(unit=2, file=trim(C%outputPath) // C%outputFile)                                      ! Open the output data file
    open(unit=3, file=trim(C%outputPath) // 'output_erosion.csv')
    open(unit=4, file=trim(C%outputPath) // 'output_hetero_vs_free.csv')
    open(unit=5, file=trim(C%outputPath) // 'output_soil.csv')
    write(2, '(A,A)') "t,x,y,rr,total_m_np_1,total_m_np_2,total_m_np_3,total_m_np_4,", &
        "total_m_np_5,total_C_np,total_np_dep,total_np_runoff,total_spm,river_volume,river_flow,total_np_pointsource"
    write(5, '(A,A)') "t,x,y,m_np_l1_free,m_np_l2_free,m_np_l3_free,m_np_l4_free,", &
        "m_np_l1_att,m_np_l2_att,m_np_l3_att,m_np_l4_att,m_np_eroded,m_np_buried,m_np_in"

    call cpu_time(start)                                                ! Simulation start time
    r = env%create()                                                    ! Create the environment

    do t = 1, C%nTimeSteps
        m_np_free = 0
        m_np_hetero = 0
        r = env%update(t)
        call LOG%toFile(errors=.errors.r)                               ! Output any errors to the log file
        call ERROR_HANDLER%trigger(errors=.errors.r)                    ! Then trigger them

        ! TODO: Do something with Result object
        do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
            do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
                if (.not. env%colGridCells(x,y)%item%isEmpty) then
                    write(3,*) t, ", ", x, ", ", y, ", ", &
                         sum(env%colGridCells(x,y)%item%erodedSediment)

                    ! RiverReachs
                    do rr = 1, env%colGridCells(x,y)%item%nRiverReaches
                        m_spm = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_spm
                        m_np = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out
                        if (.not. isZero(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume)) then
                            C_np = m_np/env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume
                        else
                            C_np = 0
                        end if
                        npDep = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_dep
                        if (C%includeBedSediment) then
                            bedSedimentMass = .dp. env%colGridCells(x,y)%item%colRiverReaches(rr)%item%bedSediment%Mf_bed_all()
                        else
                            bedSedimentMass = 0    
                        end if
                        npRunoff = sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_runoff)
                        riverVolume = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume
                        Q_out = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%Q_out/C%timeStep     ! Converted from m3/timestep to m3/s
                        npPointSource = 0
                        if (env%colGridCells(x,y)%item%colRiverReaches(rr)%item%hasPointSource) then
                            do s = 1, size(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%pointSources)
                                npPointSource = npPointSource + &
                                    sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%pointSources(s)%j_np_pointSource)
                            end do
                        end if

                        ! Write to the data file
                        write(2, '(i4,A,i2,A,i2,A,i2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)') &
                            t, ",", &
                            x, ",", &
                            y, ",", &
                            rr, ",", &
                            trim(str(sum(m_np(1,:,:)))), ",", &
                            trim(str(sum(m_np(2,:,:)))), ",", &
                            trim(str(sum(m_np(3,:,:)))), ",", &
                            trim(str(sum(m_np(4,:,:)))), ",", &
                            trim(str(sum(m_np(5,:,:)))), ",", &
                            trim(str(sum(C_np))), ",", &
                            trim(str(sum(npDep))), ",", &
                            trim(str(npRunoff)), ",", &
                            trim(str(sum(m_spm))), ",", &
                            trim(str(riverVolume)), ",", &
                            trim(str(Q_out)), ",", &
                            trim(str(npPointSource))
                        
                        m_np_hetero = m_np_hetero + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,3:) + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out(:,1,3:)
                        m_np_free = m_np_free + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,1)) + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_out(:,1,1))
                    end do

                    m_np_l1 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%m_np
                    m_np_l2 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(2)%item%m_np
                    m_np_l3 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(3)%item%m_np
                    m_np_l4 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(4)%item%m_np
                    m_np_eroded = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%m_np_eroded
                    m_np_buried = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%m_np_buried
                    m_np_in = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%m_np_in
                    write(5,*) t, ", ", x, ", ", y, ", ", &
                        sum(m_np_l1(:,1,1)), ", ", sum(m_np_l2(:,1,1)), ", ", sum(m_np_l3(:,1,1)), ", ", &
                        sum(m_np_l4(:,1,1)), ", ", sum(m_np_l1(:,1,2)), ", ", sum(m_np_l2(:,1,2)), ", ", &
                        sum(m_np_l3(:,1,2)), ", ", sum(m_np_l4(:,1,2)), ", ", sum(m_np_eroded(:,1,2)), ", ", &
                        sum(m_np_buried), ", ", sum(m_np_in)
                end if
            end do
        end do
        write(4, '(i4,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6,A,F12.6)') t, ",", m_np_free, ",", &
            sum(m_np_hetero), ",", sum(m_np_hetero(:,1)), ",", sum(m_np_hetero(:,2)), ",", &
            sum(m_np_hetero(:,3)), ",", sum(m_np_hetero(:,4)), ",", sum(m_np_hetero(:,5))
    end do

    close(2)                                                                ! Close the output file
    call cpu_time(finish)                                                   ! Simulation finish time
    print *, 'Time taken to simulate and write data (s): ', finish-start    ! How long did it take?

end program

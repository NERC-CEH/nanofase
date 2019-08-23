program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach
    use classEstuaryReach
    use classEnvironment1
    use classDataInterfacer, only: DATA
    use classDatabase, only: DATASET
    use classLogger, only: LOG, timestamp
    use datetime_module
    use omp_lib
    implicit none

    real :: start, finish, wallStart, wallFinish                    ! Simulation start and finish times
    type(Result) :: r                                               ! Result object
    integer :: x, y, rr, t, i, s, j                                 ! Loop iterators
    real(dp) :: m_spm(5)
    real(dp) :: C_spm(5)
    type(Environment1) :: env                                       ! Environment object
    real(dp), allocatable :: m_np(:,:,:)
    real(dp), allocatable :: m_np_l1(:,:,:)
    real(dp), allocatable :: m_np_l2(:,:,:)
    real(dp), allocatable :: m_np_l3(:,:,:)
    real(dp), allocatable :: m_np_l4(:,:,:)
    real(dp), allocatable :: m_np_eroded(:,:,:)
    real(dp), allocatable :: m_np_buried(:,:,:)
    real(dp), allocatable :: m_np_in(:,:,:)
    real(dp), allocatable :: C_np(:,:,:)
    real(dp) :: C_np_biota, C_np_biota_noStoredFraction
    real(dp), allocatable :: npDep(:,:,:)
    real(dp) :: m_np_hetero(5, 5)
    real(dp) :: m_np_free
    real(dp) :: bedSedimentMass
    real(dp) :: npRunoff
    type(datetime) :: currentDate
    real(dp) :: riverVolume
    real(dp) :: Q_out, np_out
    real(dp) :: npPointSource
    character(len=3) :: reachType
    real(dp) :: reachDepth
    integer :: nDisp
    real(dp) :: total_m_np
    real(dp) :: total_C_np
    real(dp) :: bulkDensity

    call cpu_time(start)                                                ! Simulation start time
    !wallStart = omp_get_wtime()

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

    ! Open the output files to print to
    open(unit=2, file=trim(C%outputPath) // C%outputFile)
    open(unit=3, file=trim(C%outputPath) // 'output_erosion.csv')
    open(unit=5, file=trim(C%outputPath) // 'output_soil.csv')
    if (C%calibrationRun) then
        open(unit=7, file=trim(C%outputPath) // 'output_calibration.csv')
        write(7, *) "t,site_code,site_type,x,y,r,reach_volume(m3),reach_flow(m3/s),reach_depth(m),", &
                    "reach_type,total_m_spm(kg),total_C_spm(g/l)"
    end if
    write(2, '(A,A,A,A)', advance='no') "t,x,y,rr,total_m_np,total_C_np,m_transformed,C_transformed,", &
        "m_dissolved,C_dissolved,total_np_dep,total_np_runoff,total_m_spm,total_C_spm,river_volume,reach_depth,river_flow,", &
        "total_np_pointsource,C_np_biota,C_np_biota_noStoredFraction,reach_type,total_np_outflow"
    write(2, '(A,A)') ""
    write(5, '(A,A)') "t,x,y,total_m_np,total_C_np,bulk_density,m_np_l1_free,m_np_l2_free,m_np_l3_free,", &
        "m_np_l1_att,m_np_l2_att,m_np_l3_att,m_np_eroded,m_np_buried,m_np_in,C_np_biota,C_np_biota_noStoredFraction"

    call DATA%init(C%inputFile)                                         ! Initialise the data interfacer TODO to be deprecated
    call DATASET%init(C%flatInputFile, C%constantsFile)                 ! Initialise the flat dataset - this closes the input data file as well
    r = env%create()                                                    ! Create the environment
    call DATA%close()                                                   ! We should be done with the data input now, so close the file

    allocate(m_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_l1(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_l2(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_l3(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_l4(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_eroded(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_buried(C%npDim(1), C%npDim(2), C%npDim(3)), &
        m_np_in(C%npDim(1), C%npDim(2), C%npDim(3)), &
        C_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
        npDep(C%npDim(1), C%npDim(2), C%npDim(3)) &
    )

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
                    do rr = 1, env%colGridCells(x,y)%item%nReaches
                        m_spm = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_spm
                        C_spm = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%C_spm
                        m_np = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np
                        if (.not. isZero(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume)) then
                            C_np = m_np/env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume
                        else
                            C_np = 0
                        end if
                        npDep = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_deposit()
                        if (C%includeBedSediment) then
                            bedSedimentMass = .dp. env%colGridCells(x,y)%item%colRiverReaches(rr)%item%bedSediment%Mf_bed_all()
                        else
                            bedSedimentMass = 0    
                        end if
                        npRunoff = sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_runoff())
                        riverVolume = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%volume
                        reachDepth = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%depth
                        Q_out = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%Q(1)/C%timeStep     ! Converted from m3/timestep to m3/s
                        np_out = sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_outflow())

                        ! What type of reach is this?
                        select type (reach => env%colGridCells(x,y)%item%colRiverReaches(rr)%item)
                            type is (RiverReach)
                                reachType = 'riv'
                            type is (EstuaryReach)
                                reachType = 'est'
                            class default
                                reachType = 'non'
                        end select

                        npPointSource = sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np_pointsource())
                        C_np_biota = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%biota%C_np
                        C_np_biota_noStoredFraction &
                            = env%colGridCells(x,y)%item%colRiverReaches(rr)%item%biota%C_np_noStoredFraction
                        ! Write to the data file
                        write(2, '(i4,A,i2,A,i2,A,i2,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)') &
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
                            trim(str(sum(C_spm))), ",", &
                            trim(str(riverVolume)), ",", &
                            trim(str(reachDepth)), ",", &
                            trim(str(Q_out)), ",", &
                            trim(str(npPointSource)), ",", &
                            trim(str(C_np_biota)), ",", &
                            trim(str(C_np_biota_noStoredFraction)), ",", &
                            trim(reachType), ",", &
                            trim(str(np_out))
                       
                        m_np_hetero = m_np_hetero + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,3:) + &
                            env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np(1,:,1,3:)
                        m_np_free = m_np_free + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%m_np(:,1,1)) + &
                            sum(env%colGridCells(x,y)%item%colRiverReaches(rr)%item%j_np(1,:,1,1))

                        if (C%calibrationRun) then
                            if (env%colGridCells(x,y)%item%colRiverReaches(rr)%item%calibrationSiteRef == C%startSite) then
                                write(7, *) t, ",", C%startSite, ",", "start_site,", x, ",", y, ",", rr, ",", &
                                            trim(str(riverVolume)), ",", trim(str(Q_out)), ",", trim(str(reachDepth)), ",", &
                                            trim(reachType), ",", trim(str(sum(m_spm))), ",", trim(str(sum(C_spm)))
                            else if (env%colGridCells(x,y)%item%colRiverReaches(rr)%item%calibrationSiteRef == C%endSite) then
                                write(7, *) t, ",", C%endSite, ",", "end_site,", x, ",", y, ",", rr, ",", &
                                            trim(str(riverVolume)), ",", trim(str(Q_out)), ",", trim(str(reachDepth)), ",", &
                                            trim(reachType), ",", trim(str(sum(m_spm))), ",", trim(str(sum(C_spm)))
                            else
                                do i = 1, size(C%otherSites)
                                    if (env%colGridCells(x,y)%item%colRiverReaches(rr)%item%calibrationSiteRef &
                                        == C%otherSites(i)) then
                                        write(7, *) t, ",", C%otherSites(i), ",", "other_site,", x, ",", y, ",", rr, ",", &
                                            trim(str(riverVolume)), ",", trim(str(Q_out)), ",", trim(str(reachDepth)), ",", &
                                            trim(reachType), ",", trim(str(sum(m_spm))), ",", trim(str(sum(C_spm)))
                                    end if
                                end do
                            end if
                        end if

                    end do
        
                    m_np_l1 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%m_np
                    m_np_l2 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(2)%item%m_np
                    m_np_l3 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(3)%item%m_np
                    ! m_np_l4 = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(4)%item%m_np
                    m_np_eroded = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%m_np_eroded
                    m_np_buried = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%m_np_buried
                    m_np_in = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%m_np_in
                    C_np_biota = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%biota%C_np
                    C_np_biota_noStoredFraction &
                        = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%biota%C_np_noStoredFraction
                    total_m_np = sum(m_np_l1) + sum(m_np_l2) + sum(m_np_l3)
                    if (env%colGridCells(x,y)%item%colSoilProfiles(1)%item%bulkDensity < 0) then
                        bulkDensity = 1220
                    else
                        bulkDensity = env%colGridCells(x,y)%item%colSoilProfiles(1)%item%bulkDensity
                    end if
                    total_C_np = total_m_np / (bulkDensity * 0.4 * 5000 * 5000)
                    write(5,*) t, ", ", x, ", ", y, ", ", &
                        total_m_np, ", ", total_C_np, ", ", bulkDensity, ", ", &
                        sum(m_np_l1(:,1,1)), ", ", sum(m_np_l2(:,1,1)), ", ", sum(m_np_l3(:,1,1)), ", ", &
                        sum(m_np_l1(:,1,2)), ", ", sum(m_np_l2(:,1,2)), ", ", &
                        sum(m_np_l3(:,1,2)), ", ", sum(m_np_eroded(:,1,2)), ", ", &
                        sum(m_np_buried), ", ", sum(m_np_in), ", ", C_np_biota, ", ", C_np_biota_noStoredFraction

                end if
            end do
        end do
    end do

    close(2)                                                                ! Close the output file
    
    ! Timings
    call cpu_time(finish)
    !wallFinish = omp_get_wtime()
    print *, 'CPU time taken to simulate and write data (s): ', finish - start
    !print *, 'Wall time taken to simulate and write data (s):', wallFinish - wallStart

end program

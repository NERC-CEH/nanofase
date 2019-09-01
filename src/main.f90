program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach
    use classEstuaryReach
    use classEnvironment1
    use classDataInterfacer, only: DATA
    use classDatabase, only: DATASET
    use classLogger, only: LOGR, timestamp
    use datetime_module
    use omp_lib
    implicit none

    real :: start, finish, wallStart, wallFinish                    ! Simulation start and finish times
    type(Result) :: r                                               ! Result object
    integer :: x, y, rr, t, i, s, j, b, yr, k                       ! Loop iterators
    integer :: tPreviousBatch = 0
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
    real(dp) :: m_transformed, C_transformed, m_dissolved, C_dissolved
    real(dp) :: C_np_l1, C_np_l2, C_np_l3
    real(dp) :: C_transformed_l1, C_transformed_l2, C_transformed_l3
    real(dp) :: C_dissolved_l1, C_dissolved_l2, C_dissolved_l3
    real(dp), allocatable :: npDep(:,:,:)
    real(dp) :: m_np_hetero(1, 5)
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
    real(dp) :: volume

    call cpu_time(start)                                                ! Simulation start time
    !wallStart = omp_get_wtime()

    ! Set up global vars and constants, and initialise data interfacer.
    ! These vars are available globally
    call GLOBALS_INIT()                                                 ! Set up global vars and constants
    call LOGR%init( &
        logToFile=.true., &
        logToConsole=.true., &
        logFilePath=C%logFilePath &
    )
    call LOGR%toConsole("--------------------------------")
    call LOGR%toConsole(" Welcome to the NanoFASE model! ")
    call LOGR%toConsole("--------------------------------\n")

    ! Open the output files to print to
    open(unit=2, file=trim(C%outputPath) // C%outputFile)
    open(unit=3, file=trim(C%outputPath) // 'output_spm.csv')
    open(unit=5, file=trim(C%outputPath) // 'output_soil.csv')
    open(unit=8, file=trim(C%outputPath) // 'output_soil_biota.csv')
    open(unit=9, file=trim(C%outputPath) // 'output_water_biota.csv')
    if (C%calibrationRun) then
        open(unit=7, file=trim(C%outputPath) // 'output_calibration.csv')
        write(7, *) "t,site_code,site_type,x,y,r,reach_volume(m3),reach_flow(m3/s),reach_depth(m),", &
                    "reach_type,total_m_spm(kg),total_C_spm(g/l)"
    end if
    write(2, *) "t,x,y,rr,reach_type,m_np,C_np,m_transformed,C_transformed,", &
        "m_dissolved,C_dissolved,m_np_dep,m_transformed_dep,m_spm,C_spm,reach_volume,reach_depth,reach_flow,", &
        "m_np_outflow,m_transformed_outflow,m_dissolved_outflow,k_settle,k_resus"
    write(3, *) "t,x,y,rr,m_spm,j_spm_runoff,j_spm_outflow,j_spm_deposit,reach_type"
    write(5, *) "t,x,y,total_m_np,total_C_np,total_C_transformed,total_C_dissolved,bulk_density,", &
        "m_np_l1_free,m_np_l2_free,m_np_l3_free,m_np_l1_att,m_np_l2_att,m_np_l3_att,", &
        "C_np_l1,C_np_l2,C_np_l3,C_transformed_l1,C_transformed_l2,C_transformed_l3,C_dissolved_l1, ", &
        "C_dissolved_l2,C_dissolved_l3,m_np_eroded,m_np_buried,m_np_in"
    write(8, *) "t,x,y,b,name,C_active_l1,C_stored_l1,C_active_l2,C_stored_l2,C_active_l3,C_stored_l3"
    write(9, *) "t,x,y,rr,b,name,compartment,C_active,C_stored"

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

    ! call GLOBALS_READ_NEW_BATCH(trim(C%batchConfigFiles(k)))

    call DATA%init(C%inputFile)                                         ! Initialise the data interfacer TODO to be deprecated
    call DATASET%init(C%flatInputFile, C%constantsFile)                 ! Initialise the flat dataset - this closes the input data file as well

    r = env%create()                                                    ! Create the environment
    call DATA%close()                                                   ! We should be done with the data input now, so close the file


    do k = 1, C%nBatches
        ! If we're not on the first batch, we need to update the data for this batch
        ! TODO at the moment the first config file specified in the batch config file doesn't have any effect
        ! update this to be more logical (and less likely to break, e.g. audit grid dimensions etc are the same
        ! between data files)
        if (k > 1) then
            call DATASET%update(C%batchConfigFiles(k))
            call env%parseNewBatchData()
        end if

        do yr = 1, 1
            do t = 1, C%nTimeSteps
                m_np_free = 0
                m_np_hetero = 0
                r = env%update(t)
                call LOGR%toFile(errors=.errors.r)                               ! Output any errors to the log file
                call ERROR_HANDLER%trigger(errors=.errors.r)                    ! Then trigger them

                ! TODO: Do something with Result object
                do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
                    do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
                        if (.not. env%colGridCells(x,y)%item%isEmpty) then

                           ! RiverReachs
                            do rr = 1, env%colGridCells(x,y)%item%nReaches

                                associate(reach => env%colGridCells(x,y)%item%colRiverReaches(rr)%item)
                                    ! What reach type is this?
                                    select type (reach)
                                        type is (RiverReach)
                                            reachType = 'riv'
                                        type is (EstuaryReach)
                                            reachType = 'est'
                                    end select
                                    ! Water output file
                                    write(2, *) t + tPreviousBatch, ",", x, ",", y, ",", rr, ",", reachType, ",", &
                                        trim(str(sum(reach%m_np))), ",", &
                                        trim(str(sum(reach%C_np))), ",", &
                                        trim(str(sum(reach%m_transformed))), ",", &
                                        trim(str(sum(reach%C_transformed))), ",", &
                                        trim(str(reach%m_dissolved)), ",", &
                                        trim(str(reach%C_dissolved)), ",", &
                                        trim(str(sum(reach%j_np_deposit()))), ",", &
                                        trim(str(sum(reach%j_transformed_deposit()))), ",", &
                                        trim(str(sum(reach%m_spm))), ",", &
                                        trim(str(sum(reach%C_spm))), ",", &
                                        trim(str(reach%volume)), ",", &
                                        trim(str(reach%depth)), ",", &
                                        trim(str(reach%Q(1)/C%timeStep)), ",", &
                                        trim(str(sum(reach%j_np_outflow()))), ",", &
                                        trim(str(sum(reach%j_transformed_outflow()))), ",", &
                                        trim(str(reach%j_dissolved_outflow())), ",", &
                                        trim(str(sum(reach%k_settle))), ",", &
                                        trim(str(sum(reach%k_resus)))
                                    ! SPM output file
                                    write(3,*) t + tPreviousBatch, ",", x, ",", y, ",", rr, ",", &
                                        trim(str(sum(reach%m_spm))), ",", &
                                        trim(str(sum(reach%j_spm_runoff()))), ",", &
                                        trim(str(sum(reach%j_spm_outflow()))), ",", &
                                        trim(str(sum(reach%j_spm_deposit()))), ",", &
                                        trim(reachType)
                                    ! Biota
                                    do b = 1, reach%nBiota
                                        write(9, *) t + tPreviousBatch, ",", x, ",", y, ",", rr, ",", b, ",", &
                                            trim(reach%biota(b)%name), ",", &
                                            reachType, ",", &
                                            reach%biota(b)%C_active, ",", &
                                            reach%biota(b)%C_stored
                                    end do

                                    if (C%calibrationRun) then
                                        if (reach%calibrationSiteRef == C%startSite) then
                                            write(7, *) t, ",", C%startSite, ",", "start_site,", x, ",", y, ",", rr, ",", &
                                                        trim(str(reach%volume)), ",", trim(str(reach%Q(1)/C%timeStep)), ",", &
                                                        trim(str(reach%depth)), ",", trim(reachType), ",", &
                                                        trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                        else if (reach%calibrationSiteRef == C%endSite) then
                                            write(7, *) t, ",", C%endSite, ",", "end_site,", x, ",", y, ",", rr, ",", &
                                                        trim(str(reach%volume)), ",", trim(str(reach%Q(1)/C%timeStep)), ",", &
                                                        trim(str(reach%depth)), ",", trim(reachType), ",", &
                                                        trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                        else
                                            do i = 1, size(C%otherSites)
                                                if (reach%calibrationSiteRef &
                                                    == C%otherSites(i)) then
                                                    write(7, *) t, ",", C%otherSites(i), ",", "other_site,", x, ",", y, ",", &
                                                        rr, ",", &
                                                        trim(str(reach%volume)), ",", trim(str(reach%Q(1)/C%timeStep)), ",", &
                                                        trim(str(reach%depth)), ",", trim(reachType), ",", &
                                                        trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                                end if
                                            end do
                                        end if
                                    end if
                                end associate
                            end do

                            associate (profile => env%colGridCells(x,y)%item%colSoilProfiles(1)%item)
                                if (profile%bulkDensity < 0) then
                                    bulkDensity = 1220
                                else
                                    bulkDensity = profile%bulkDensity
                                end if
                                m_np_l1 = profile%colSoilLayers(1)%item%m_np
                                m_np_l2 = profile%colSoilLayers(2)%item%m_np
                                m_np_l3 = profile%colSoilLayers(3)%item%m_np
                                C_np_l1 = sum(m_np_l1) / (bulkDensity * profile%colSoilLayers(1)%item%volume)
                                C_np_l2 = sum(m_np_l2) / (bulkDensity * profile%colSoilLayers(2)%item%volume)
                                C_np_l3 = sum(m_np_l3) / (bulkDensity * profile%colSoilLayers(3)%item%volume)
                                C_transformed_l1 = sum(profile%colSoilLayers(1)%item%m_transformed) &
                                    / (bulkDensity * profile%colSoilLayers(1)%item%volume)
                                C_transformed_l2 = sum(profile%colSoilLayers(2)%item%m_transformed) &
                                    / (bulkDensity * profile%colSoilLayers(2)%item%volume)
                                C_transformed_l3 = sum(profile%colSoilLayers(3)%item%m_transformed) &
                                    / (bulkDensity * profile%colSoilLayers(3)%item%volume)
                                C_dissolved_l1 = profile%colSoilLayers(1)%item%m_dissolved / profile%colSoilLayers(1)%item%volume
                                C_dissolved_l2 = profile%colSoilLayers(2)%item%m_dissolved / profile%colSoilLayers(2)%item%volume
                                C_dissolved_l3 = profile%colSoilLayers(3)%item%m_dissolved / profile%colSoilLayers(3)%item%volume
                                m_np_eroded = profile%colSoilLayers(1)%item%m_np_eroded
                                m_np_buried = profile%m_np_buried
                                m_np_in = profile%m_np_in
                                total_m_np = sum(m_np_l1) + sum(m_np_l2) + sum(m_np_l3)
                                total_C_np = total_m_np / (bulkDensity * 0.4 * 5000 * 5000)
                            end associate

                            write(5,*) t + tPreviousBatch, ", ", x, ", ", y, ", ", &
                                total_m_np, ", ", total_C_np, ", ", &
                                C_transformed_l1 + C_transformed_l2 + C_transformed_l3, ",", &
                                C_dissolved_l1 + C_dissolved_l2 + C_dissolved_l3, ",", bulkDensity, ",", &
                                sum(m_np_l1(:,1,1)), ", ", sum(m_np_l2(:,1,1)), ", ", sum(m_np_l3(:,1,1)), ", ", &
                                sum(m_np_l1(:,1,2)), ", ", sum(m_np_l2(:,1,2)), ", ", &
                                sum(m_np_l3(:,1,2)), ", ", &
                                C_np_l1, ",", &
                                C_np_l2, ",", &
                                C_np_l3, ",", &
                                C_transformed_l1, ",", &
                                C_transformed_l2, ",", &
                                C_transformed_l3, ",", &
                                C_dissolved_l1, ",", &
                                C_dissolved_l2, ",", &
                                C_dissolved_l3, ",", &
                                sum(m_np_eroded(:,1,2)), ", ", &
                                sum(m_np_buried), ", ", sum(m_np_in)

                            do b = 1, env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%nBiota
                                associate(profile => env%colGridCells(x,y)%item%colSoilProfiles(1)%item)
                                    write(8, *) t + tPreviousBatch, ",", x, ",", y, ",", b, ",", &
                                        trim(profile%colSoilLayers(1)%item%biota(b)%name), ",", &
                                        profile%colSoilLayers(1)%item%biota(b)%C_active, ",", &
                                        profile%colSoilLayers(1)%item%biota(b)%C_stored, ",", &
                                        profile%colSoilLayers(2)%item%biota(b)%C_active, ",", &
                                        profile%colSoilLayers(2)%item%biota(b)%C_stored, ",", &
                                        profile%colSoilLayers(3)%item%biota(b)%C_active, ",", &
                                        profile%colSoilLayers(3)%item%biota(b)%C_stored
                                end associate
                            end do
                        end if
                    end do
                end do
            end do
            tPreviousBatch = tPreviousBatch + t
        end do
    end do

    close(2)                                                                ! Close the output file
    
    ! Timings
    call cpu_time(finish)
    !wallFinish = omp_get_wtime()
    print *, 'CPU time taken to simulate and write data (s): ', finish - start
    !print *, 'Wall time taken to simulate and write data (s):', wallFinish - wallStart

end program
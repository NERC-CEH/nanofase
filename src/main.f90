program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach
    use classEstuaryReach
    use classEnvironment1
    use classDatabase, only: DATASET
    use DataOutputModule
    use classLogger, only: LOGR, timestamp
    use datetime_module
    use omp_lib
    implicit none

    real :: start, finish           ! Simulation start and finish times
    type(Environment1) :: env       ! Environment object
    type(Result) :: r               ! Result object
    type(DataOutput) :: output      ! Data output class
    integer :: x, y, t, k           ! Loop iterators
    integer :: tPreviousBatch = 0   ! Timestep at end of previous batch

    ! real(dp) :: C_spm(5)
    ! real(dp) :: m_spm(5)
    ! real(dp), allocatable :: m_np(:,:,:)
    ! real(dp), allocatable :: m_np_l1(:,:,:)
    ! real(dp), allocatable :: m_np_l2(:,:,:)
    ! real(dp), allocatable :: m_np_l3(:,:,:)
    ! real(dp), allocatable :: m_np_l4(:,:,:)
    ! real(dp), allocatable :: m_np_eroded(:,:,:)
    ! real(dp), allocatable :: m_np_buried(:,:,:)
    ! real(dp), allocatable :: m_np_in(:,:,:)
    ! real(dp), allocatable :: C_np(:,:,:)
    ! real(dp) :: C_np_biota, C_np_biota_noStoredFraction
    ! real(dp) :: m_transformed, C_transformed, m_dissolved, C_dissolved
    ! real(dp) :: C_np_l1, C_np_l2, C_np_l3, C_np_l4
    ! real(dp) :: C_transformed_l1, C_transformed_l2, C_transformed_l3
    ! real(dp) :: C_dissolved_l1, C_dissolved_l2, C_dissolved_l3
    ! real(dp), allocatable :: npDep(:,:,:)
    ! real(dp) :: m_np_hetero(1, 5)
    ! real(dp) :: m_np_free
    ! real(dp) :: bedSedimentMass
    ! real(dp) :: npRunoff
    ! type(datetime) :: currentDate
    ! real(dp) :: riverVolume
    ! real(dp) :: Q_out, np_out
    ! real(dp) :: npPointSource
    ! character(len=3) :: reachType
    ! real(dp) :: reachDepth
    ! integer :: nDisp
    ! real(dp) :: total_m_np
    ! real(dp) :: total_C_np
    ! real(dp) :: total_C_np_byMass
    ! real(dp) :: bulkDensity
    ! real(dp) :: volume
    ! real(dp) :: meanDepth

    ! Get the CPU time at the start of the model run
    call cpu_time(start) 

    ! Set up global vars/constants and initialise the logger
    call GLOBALS_INIT()
    call LOGR%init( &
        logToFile=.true., &
        logToConsole=.true., &
        logFilePath=C%logFilePath &
    )
    call LOGR%toConsole("--------------------------------")
    call LOGR%toConsole(" Welcome to the NanoFASE model! ")
    call LOGR%toConsole("--------------------------------\n")

    ! Pull in the input data, create the output and set up the output data files
    call DATASET%init(C%inputFile, C%constantsFile)
    r = env%create()
    call LOGR%toFile(errors=.errors.r)
    call ERROR_HANDLER%trigger(errors=.errors.r)
    call output%init(env)

    ! TODO move all this to a data output module
    ! Open the output files to print to
    ! open(unit=8, file=trim(C%outputPath) // 'output_soil_biota.csv')
    ! open(unit=9, file=trim(C%outputPath) // 'output_water_biota.csv')
    ! if (C%calibrationRun) then
    !     open(unit=7, file=trim(C%outputPath) // 'output_calibration.csv')
    !     write(7, *) "t,site_code,site_type,x,y,easts,norths,r,reach_volume(m3),reach_flow(m3/s),reach_depth(m),", &
    !                 "reach_type,total_m_spm(kg),total_C_spm(g/l)"
    ! end if
    ! write(8, *) "t,x,y,easts,norths,b,name,C_active_l1,C_stored_l1,C_active_l2,C_stored_l2,C_active_l3,C_stored_l3"
    ! write(9, *) "t,x,y,easts,norths,rr,b,name,compartment,C_active,C_stored"


    ! Loop through grid cell and pull out static data to save to output.
    ! TODO potentially create "geographical scenario" dataset for this
    ! do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
    !     do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
    !         if (.not. env%colGridCells(x,y)%item%isEmpty) then
    !             do rr = 1, env%colGridCells(x,y)%item%nReaches
    !                 associate(reach => env%colGridCells(x,y)%item%colRiverReaches(rr)%item)
    !                     select type (reach)
    !                         type is (EstuaryReach)
    !                             reachType = 'est'
    !                             meanDepth = reach%meanDepth
    !                         type is (RiverReach)
    !                             reachType = 'riv'
    !                     end select
    !                     if (trim(reachType) == 'est') then
    !                         print *, "yep"
    !                         write(12, *) x, ",", y, ",", int(DATASET%x(x)), ",", int(DATASET%y(y)), ",", rr, ",", &
    !                             reach%width, ",", &
    !                             meanDepth, ",", &
    !                             reach%streamOrder, ",", reach%f_m
    !                     end if
    !                 end associate
    !             end do
    !         end if
    !     end do
    ! end do

    ! error stop

    do k = 1, C%nChunks
        ! If we're not on the first batch, we need to update the data for this batch
        ! TODO at the moment the first config file specified in the batch config file doesn't have any effect
        ! update this to be more logical (and less likely to break, e.g. audit grid dimensions etc are the same
        ! between data files)
        if (k > 1) then
            call DATASET%update(k)
            call env%parseNewBatchData()
        end if

        do t = 1, C%nTimeSteps
            r = env%update(t)
            call LOGR%toFile(errors=.errors.r)                              ! Output any errors to the log file
            call ERROR_HANDLER%trigger(errors=.errors.r)                    ! Then trigger them
            call output%update(t + tPreviousBatch)

            ! TODO: Do something with Result object
            ! do y = 1, size(env%colGridCells, 2)                             ! Loop through the rows
            !     do x = 1, size(env%colGridCells, 1)                         ! Loop through the columns
            !         if (.not. env%colGridCells(x,y)%item%isEmpty) then

            !            ! RiverReachs
            !             do rr = 1, env%colGridCells(x,y)%item%nReaches

            !                 associate(reach => env%colGridCells(x,y)%item%colRiverReaches(rr)%item)
            !                     ! What reach type is this?
            !                     select type (reach)
            !                         type is (RiverReach)
            !                             reachType = 'riv'
            !                         type is (EstuaryReach)
            !                             reachType = 'est'
            !                     end select
                                ! Biota
                                ! do b = 1, reach%nBiota
                                !     write(9, *) t + tPreviousBatch, ",", x, ",", y, ",", &
                                !         DATASET%x(x), ",", DATASET%y(y), ",", rr, ",", b, ",", &
                                !         trim(reach%biota(b)%name), ",", &
                                !         reachType, ",", &
                                !         reach%biota(b)%C_active, ",", &
                                !         reach%biota(b)%C_stored
                                ! end do

                                ! if (C%calibrationRun) then
                                !     if (reach%calibrationSiteRef == C%startSite) then
                                !         write(7, *) t, ",", C%startSite, ",", "start_site,", x, ",", y, ",", &
                                !                     DATASET%x(x), ",", DATASET%y(y), ",", &
                                !                     rr, ",", trim(str(reach%volume)), ",", trim(str(reach%Q(1)/C%timeStep)), &
                                !                     ",", trim(str(reach%depth)), ",", trim(reachType), ",", &
                                !                     trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                !     else if (reach%calibrationSiteRef == C%endSite) then
                                !         write(7, *) t, ",", C%endSite, ",", "end_site,", x, ",", y, ",", &
                                !                     DATASET%x(x), ",", DATASET%y(y), ",", &
                                !                     rr, ",", trim(str(reach%volume)), ",", &
                                !                     trim(str(reach%Q(1)/C%timeStep)), ",", &
                                !                     trim(str(reach%depth)), ",", trim(reachType), ",", &
                                !                     trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                !     else
                                !         do i = 1, size(C%otherSites)
                                !             if (reach%calibrationSiteRef &
                                !                 == C%otherSites(i)) then
                                !                 write(7, *) t, ",", C%otherSites(i), ",", "other_site,", x, ",", y, ",", &
                                !                     DATASET%x(x), ",", DATASET%y(y), ",", rr, ",", trim(str(reach%volume)), &
                                !                     ",", trim(str(reach%Q(1)/C%timeStep)), ",", &
                                !                     trim(str(reach%depth)), ",", trim(reachType), ",", &
                                !                     trim(str(sum(reach%m_spm))), ",", trim(str(sum(reach%C_spm)))
                                !             end if
                                !         end do
                                !     end if
                                ! end if
                        !     end associate
                        ! end do

                        ! do b = 1, env%colGridCells(x,y)%item%colSoilProfiles(1)%item%colSoilLayers(1)%item%nBiota
                        !     associate(profile => env%colGridCells(x,y)%item%colSoilProfiles(1)%item)
                        !         write(8, *) t + tPreviousBatch, ",", x, ",", y, ",", &
                        !             DATASET%x(x), ",", DATASET%y(y), ",", b, ",", &
                        !             trim(profile%colSoilLayers(1)%item%biota(b)%name), ",", &
                        !             profile%colSoilLayers(1)%item%biota(b)%C_active, ",", &
                        !             profile%colSoilLayers(1)%item%biota(b)%C_stored, ",", &
                        !             profile%colSoilLayers(2)%item%biota(b)%C_active, ",", &
                        !             profile%colSoilLayers(2)%item%biota(b)%C_stored, ",", &
                        !             profile%colSoilLayers(3)%item%biota(b)%C_active, ",", &
                        !             profile%colSoilLayers(3)%item%biota(b)%C_stored
                        !     end associate
                        ! end do
            !         end if
            !     end do
            ! end do
        end do
        tPreviousBatch = tPreviousBatch + t
    end do

    ! Write the simulation summary to file and close output data files
    call output%finalise()
    
    ! Timings
    call cpu_time(finish)
    print *, 'CPU time taken to simulate and write data (s): ', finish - start

end program
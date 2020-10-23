program main
    use Globals
    use UtilModule
    use ResultModule
    use classRiverReach
    use classEstuaryReach
    use classEnvironment1
    use classDatabase, only: DATASET
    use CheckpointModule, only: Checkpoint
    use DataOutputModule
    use classLogger, only: LOGR
    use DefaultsModule, only: iouLog
    use datetime_module
    implicit none

    real                :: start, finish                    ! Simulation start and finish times
    type(Environment1)  :: env                              ! Environment object
    type(Result)        :: r                                ! Result object
    type(DataOutput)    :: output                           ! Data output class
    type(Checkpoint)    :: checkpt                          ! Checkpoint module
    integer             :: t, k                             ! Loop iterators
    integer             :: tPreviousChunk = 0               ! Timestep at end of previous batch
    integer             :: i                                ! If running to steady state, this is the iterator
    logical             :: steadyStateReached = .false.     ! Has steady state been reached?
    real(dp)            :: delta_max                        ! Maximum difference between the same size classes on separate runs
    character(len=10)   :: iterationStr                     ! What steady state iteration are we on

    ! Get the CPU time at the start of the model run
    call cpu_time(start)

    ! Set up global vars/constants and initialise the logger
    call GLOBALS_INIT()
    ! Initialise the logger
    call LOGR%init( &
        logToFile=C%writeToLog, &
        logToConsole=.true., &
        logFilePath=C%logFilePath, &
        fileUnit=iouLog &
    )
    call LOGR%toConsole("--------------------------------")
    call LOGR%toConsole(" Welcome to the NanoFASE model! ")
    call LOGR%toConsole("--------------------------------\n")

    ! Pull in the input data, create the output and set up the output data files
    call DATASET%init(C%inputFile, C%constantsFile)

    ! Create the Environment object and deal with any errors that arise
    r = env%create()
    call LOGR%toFile(errors=.errors.r)
    call ERROR_HANDLER%trigger(errors=.errors.r)

    ! Initialise the data output module
    call output%init(env)
    if (C%runToSteadyState .and. trim(C%steadyStateMode) == 'sediment_size_distribution') then
        call output%initSedimentSizeDistribution()
        call LOGR%toFile("Running in steady state mode " // trim(C%steadyStateMode))
        call LOGR%toConsole("\x1B[94mRunning in steady state mode '" // trim(C%steadyStateMode) // "'\x1B[39m")
    end if
    
    ! Set up checkpointing and check if we're meant to be reinstating a checkpoint now
    call checkpt%init(env, C%checkpointFile)
    if (C%reinstateCheckpoint) then
        call checkpt%reinstate(preserve_timestep=C%preserveTimestep)
    end if

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

    ! Loop until steady state is reached, if we're in run to steady state mode. Otherwise, we'll
    ! trip out of this loop after the first iteration
    i = 1
    do while (.not. steadyStateReached)

        ! If we're running to steady state, log some info about it
        if (C%runToSteadyState) then
            call LOGR%toFile("Model iteration #" // trim(str(i)))
            call LOGR%toConsole("\x1B[94mModel iteration #" // trim(str(i)) // "\x1B[0m")
        end if

        ! Loop through each chunk in the batch run. There will only be one if this isn't a batch run
        do k = 1, C%nChunks
            ! If we're not on the first chunk, we need to update the data for this chunk, otherwise the
            ! correct data will already be in the DATASET object
            if (k > 1) then
                call DATASET%update(k)
                call env%parseNewBatchData()
            end if
            ! Loop through the timestep in this batch. The model itself is intentionally agnostic to
            ! the fact we might be in a batch run and so the timestep within the batch, t, can be used
            do t = 1, C%nTimeSteps
                ! Update the environment for this timestep, which in turn updates all compartments
                call env%update(t)
                ! Check for any errors returned from updated, and log/trigger them
                call LOGR%toFile(errors=.errors.r)
                call ERROR_HANDLER%trigger(errors=.errors.r)
                ! Update the output files. Passing the "correct" timestep, taking into account previous
                ! chunks, is important otherwise timestep indices will be incorrect in the data
                call output%update(t + tPreviousChunk)

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
                                    !     write(9, *) t + tPreviousChunk, ",", x, ",", y, ",", &
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
                            !         write(8, *) t + tPreviousChunk, ",", x, ",", y, ",", &
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
            ! Keep a tally of what actual timestep we're on across the batch run
            tPreviousChunk = tPreviousChunk + t - 1
        end do

        ! If we're meant to be running to steady state, then we now need to check whether we've reached it
        if (C%runToSteadyState) then
            ! Sediment size distribution mode
            if (trim(C%steadyStateMode) == 'sediment_size_distribution') then
                ! Update the sediment size distribution file, and also retrieve the difference
                ! between model iterations
                delta_max = output%updateSedimentSizeDistribution(i)
                ! Check if we've met the criteria set in config to have reached steady state
                if (delta_max <= C%steadyStateDelta) then
                    steadyStateReached = .true.
                    if (i == 1) then
                        iterationStr = "iteration"
                    else
                        iterationStr = "iterations"
                    end if
                    call LOGR%toFile("Steady state reached after " // trim(str(i)) // " " // iterationStr)
                    call LOGR%toConsole("\x1B[94mSteady state reached after " // trim(str(i)) // " " // iterationStr // "\x1B[0m")
                end if
                ! Increment the iterator to the next model run
                i = i + 1
            end if
        else
            ! If we're not meant to be looping until steady state, then stop the loop now
            steadyStateReached = .true.
        end if

    end do

    ! Have we been asked to create a checkpoint?
    if (C%saveCheckpoint) then
        call checkpt%save(tPreviousChunk)
    end if

    ! Write the simulation summary to file and close output data files. Pass the steady state
    ! iterator in to give number of iterations until steady state
    call output%finalise(i-1)
    
    ! Timings
    call cpu_time(finish)
    print *, 'CPU time taken to run simulation (s): ', finish - start

end program
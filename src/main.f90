!-------------------------------------------------------------------------------!
!> NanoFASE model                                                               !
!> --------------                                                               !
!> Nanomaterial Fate And Speciation in the Environment                          !
!>                                                                              !
!> Authors: Sam Harrison (sharrison@ceh.ac.uk)                                  !
!>          Stephen Lofts                                                       !
!>          Virginie Keller                                                     !
!>          Michael Hutchins                                                    !
!>          Richard Williams                                                    !
!> Institute: UK Centre for Ecology & Hydrology                                 !
!> Repository: https://github.com/nerc-ceh/nanofase                             !
!> Documentation: *                                                             !
!> Changelog: https://github.com/NERC-CEH/nanofase/blob/develop/CHANGELOG.md    !
!> License: BSD 3-Clause,                                                       !
!>          https://github.com/NERC-CEH/nanofase/blob/develop/LICENSE           !
!-------------------------------------------------------------------------------!
program main
    use Globals
    use UtilModule
    use ResultModule
    use RiverReachModule
    use EstuaryReachModule
    use EnvironmentModule
    use DataInputModule, only: DATASET
    use CheckpointModule, only: Checkpoint
    use DataOutputModule
    use LoggerModule, only: LOGR
    use DefaultsModule, only: iouLog
    use datetime_module
    implicit none

    real                :: start, finish                        !! Simulation start and finish CPU times
    integer(i64)        :: start_wall, finish_wall, clock_rate  !! Simulation start and finish wall times, and clock rate
    type(Environment)   :: env                                  !! Environment object
    type(Result)        :: rslt                                 !! Result object
    type(DataOutput)    :: output                               !! Data output class
    type(Checkpoint)    :: checkpt                              !! Checkpoint module
    integer             :: t, k                                 !! Loop iterators
    integer             :: tPreviousChunk = 0                   !! Timestep at end of previous batch
    integer             :: i                                    !! If running to steady state, this is the iterator
    logical             :: steadyStateReached = .false.         !! Has steady state been reached?
    real(dp)            :: delta_max                            !! Maximum difference between the same size classes on separate runs

    ! Get the CPU and wall time at the start of the model run
    call cpu_time(start)
    call system_clock(start_wall, clock_rate)

    ! Set up global vars/constants and initialise the logger
    call GLOBALS_INIT()
    ! Initialise the logger
    call LOGR%init( &
        logToFile=C%writeToLog, &
        logToConsole=.true., &
        logFilePath=C%logFilePath, &
        fileUnit=iouLog &
    )

    ! Welcome, good to have you here!
    call printWelcome()

    ! Load the input data
    call DATASET%init(C%inputFile, C%constantsFile)

    ! Create the Environment object and deal with any errors that arise
    rslt = env%create()
    call LOGR%toFile(errors=.errors.rslt)
    call ERROR_HANDLER%trigger(errors=.errors.rslt)

    ! Initialise the data output module, check if we're running to steady state
    call output%init(env)
    if (C%runToSteadyState .and. trim(C%steadyStateMode) == 'sediment_size_distribution') then
        call output%initSedimentSizeDistribution()
        call LOGR%add("Running in steady state mode " // trim(C%steadyStateMode), COLOR_BLUE)
    end if
    
    ! Set up checkpointing and check if we're meant to be reinstating a checkpoint now
    call checkpt%init(env, C%checkpointFile)
    if (C%reinstateCheckpoint) then
        call checkpt%reinstate(preserve_timestep=C%preserveTimestep)
    end if

    ! Check if we've been asked to run a warm up period, which runs the first N timesteps' worth
    ! of data, excluding NM inputs, through the model, where N is specified by C%warmUpPeriod
    if (C%warmUpPeriod > 0) then
        ! Log some info about it
        call LOGR%add("Running for warm up period of " // trim(str(C%warmUpPeriod)) // " time steps", COLOR_BLUE)
        ! Run the model with the warmUp flag
        do t = 1, C%warmUpPeriod
            call env%update(t, t, .true.)
        end do
        ! Log that we're finished warming up and on to the real model
        call LOGR%add("Finished warm up period", COLOR_BLUE)
        ! Have we been asked to create a checkpoint after the warm up period?
        if (C%saveCheckpointAfterWarmUp) then
            call checkpt%save(0)
        end if
    end if

    ! Loop until steady state is reached, if we're in run to steady state mode. Otherwise, we'll
    ! trip out of this loop after the first iteration
    i = 1
    do while (.not. steadyStateReached)
        ! If we're running to steady state, log some info about it
        if (C%runToSteadyState) then
            call LOGR%add("Model iteration #" // trim(str(i)), "lightblue")
        end if

        ! Loop through each chunk in the batch run. There will only be one if this isn't a batch run
        do k = 1, C%nChunks
            ! If we're not on the first chunk, we need to update the data for this chunk, otherwise the
            ! correct data will already be in the DATASET object
            if (k > 1) then
                call DATASET%update(k)
                call env%parseNewBatchData()
                call output%newChunk(k)
            end if
            ! Loop through the timestep in this batch. The model itself is intentionally agnostic to
            ! the fact we might be in a batch run and so the timestep within the chunk, t, can be used
            do t = 1, C%nTimeSteps
                ! Update the environment for this timestep, which in turn updates all compartments
                call env%update(t, t + tPreviousChunk, .false.)
                ! Check for any errors returned from updated, and log/trigger them
                call LOGR%toFile(errors=.errors.rslt)
                call ERROR_HANDLER%trigger(errors=.errors.rslt)
                ! Update the output files. Passing the "correct" timestep, taking into account previous
                ! chunks, is important otherwise timestep indices will be incorrect in the data
                call output%update(t + tPreviousChunk, t)
            end do
            ! Finalise the output for this chunk, which amounts to writing to the NetCDF
            ! if in 'end' mode (otherwise this subroutine doesn't do anything)
            call output%finaliseChunk(tPreviousChunk + 1, k == C%nChunks)
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
                    call LOGR%add("Steady state reached after " // trim(str(i)) // " " // &
                                  trim(pluralize("iteration", i)), COLOR_BLUE)
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

    ! Write the simulation summary to file, close output data files and report that it was a successful
    ! model run. Pass the steady state iterator in to give number of iterations until steady state
    call output%finalise(i-1)
    call LOGR%add("Model run completeled successfully", COLOR_GREEN)
    
    ! Timings
    call cpu_time(finish)
    call system_clock(finish_wall, clock_rate)
    call LOGR%add("CPU time taken to run simulation (s): " // trim(str(finish - start)), COLOR_YELLOW)
    call LOGR%add("Wall time taken to run simulation (s): " // &
                  trim(str(real(finish_wall - start_wall) / real(clock_rate))), COLOR_YELLOW)

end program
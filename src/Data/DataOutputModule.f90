module DataOutputModule
    use Globals, only: C
    use classDatabase, only: DATASET
    use spcEnvironment
    use classEnvironment1
    use UtilModule
    use datetime_module
    implicit none

    type, public :: DataOutput
        character(len=256) :: outputPath                    !! Path to the output directory
        type(EnvironmentPointer) :: env                     !! Pointer to the environment, to retrieve state variables
      contains
        procedure, public :: init => initDataOutput
        procedure, public :: update => updateDataOutput
        procedure, public :: finalise => finaliseDataOutput
        procedure, private :: writeHeaders => writeHeadersDataOutput
    end type

  contains
    
    subroutine initDataOutput(me, env)
        class(DataOutput)           :: me
        type(Environment1), target  :: env
        
        ! Point the Environment object to that passed in
        me%env%item => env

        ! Open the files to write to
        open(unit=100, file=trim(C%outputPath) // 'simulation_summary.md')
        if (C%writeCSV) then
            open(unit=101, file=trim(C%outputPath) // 'n_output_water.csv')
            open(unit=102, file=trim(C%outputPath) // 'n_output_sediment.csv')
            open(unit=103, file=trim(C%outputPath) // 'n_output_soil.csv')
            open(unit=104, file=trim(C%outputPath) // 'n_output_soil_biota.csv')
            open(unit=105, file=trim(C%outputPath) // 'n_output_water_biota.csv')
        end if

        ! Write the headers for the files
        call me%writeHeaders()
    end subroutine

    subroutine updateDataOutput(me)
        class(DataOutput) :: me
        ! Save the output...
    end subroutine

    subroutine finaliseDataOutput(me)
        class(DataOutput) :: me
        ! Close the files
        ! write(100, *) "\n## PECs"
        ! write(100, *) "- Soil, final timestep:"
        ! write(100, *) "  - Spatial median: " // me%env%item%
    end subroutine

    subroutine writeHeadersDataOutput(me)
        class(DataOutput)   :: me
        type(datetime)      :: simDatetime
        type(datetime)      :: modelEndDate

        ! Parse some datetimes
        simDatetime = simDatetime%now()
        modelEndDate = C%startDate + timedelta(C%nTimeSteps - 1)

        ! Summary file headers
        write(100, '(a)') "# NanoFASE model simulation summary"
        write(100, *) "- Description: " // trim(C%runDescription)
        write(100, *) "- Simulation datetime: " // simDatetime%isoformat()
        write(100, *) "- Is batch run? " // str(C%isBatchRun)
        write(100, *) "- Number of batches: " // str(C%nBatches)

        write(100, *) "\n## Temporal domain"
        write(100, *) "- Start date: " // C%startDate%strftime('%Y-%m-%d')
        write(100, *) "- End date: " // modelEndDate%strftime('%Y-%m-%d')
        write(100, *) "- Timestep length: " // trim(str(C%timeStep)) // " s"
        write(100, *) "- Number of timesteps: " // trim(str(C%nTimeSteps))
        
        write(100, *) "\n## Spatial domain"
        write(100, *) "- Grid resolution: " // trim(str(DATASET%gridRes(1))) // ", " // trim(str(DATASET%gridRes(2))) 
        write(100, *) "- Grid bounds: " // trim(str(DATASET%gridBounds(1))) // ", " // trim(str(DATASET%gridBounds(2))) // &
            ", " // trim(str(DATASET%gridBounds(3))) // ", " // trim(str(DATASET%gridBounds(4)))
        write(100, *) "- Grid shape: " // trim(str(DATASET%gridShape(1))) // ", " // trim(str(DATASET%gridShape(2)))
        write(100, *) "- Number of non-empty grid cells: " // trim(str(me%env%item%nGridCells))

    end subroutine

end module
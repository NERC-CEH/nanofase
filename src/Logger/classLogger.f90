module classLogger
    use UtilModule
    use ErrorInstanceModule
    use Globals
    implicit none

    !> The `Logger` object is responsible to outputting messages
    !! to a log file and the console.
    type, public :: Logger
        logical :: logToFile = .false.                      !> Should the log be output to a file?
        logical :: logToConsole = .true.                    !> Should the log be output to the console?
        character(len=256), private :: logFilePath          !> The path to the log file
        character(len=19), private :: logFileName           !> The name of the log file (the timestamp when init() called)
        integer :: fileUnit = 42                            !> File unit for the log file

      contains
        procedure :: init => initLogger
        procedure :: add => addLogger
        procedure :: toFile => toFileLogger
        procedure :: toConsole => toConsoleLogger
    end type

    type(Logger) :: LOG         !> Globally-available LOG object

  contains

    !> Initialise the Logger with any specified options
    subroutine initLogger(me, logToFile, logFilePath, fileUnit, logToConsole)
        class(Logger)                   :: me
        logical, optional               :: logToFile        !> Should the log be output to a file?
        character(*), optional          :: logFilePath      !> Where should the log file be placed? Defaults to ./log/YYYY-MM-DD_HH:MM:SS
        integer, optional               :: fileUnit         !> What unit should the log file be opened with? Defaults to 42.
        logical, optional               :: logToConsole     !> Should the log be output to the console?
        
        ! Should we be logging to a file? Defaults to false
        if (present(logToFile)) me%logToFile = logToFile
        if (me%logToFile) then
            ! Set the log file name to the current timestamp
            me%logFileName = timestamp()
            ! Set the file path to the log file, or default to log/
            if (present(logFilePath)) then
                me%logFilePath = trim(logFilePath)
            else
                me%logFilePath = "log/"
            end if
            ! Set the file unit, if it has been providedx
            if (present(fileUnit)) me%fileUnit = fileUnit
            ! Open the log file
            open(unit=me%fileUnit, file=trim(me%logFilePath) // trim(me%logFileName), status="new")
        end if

        ! Should we be logging to the console? Defaults to true
        if (present(logToConsole)) me%logToConsole = logToConsole
    end subroutine

    !> Log to file (if enabled) and console
    subroutine addLogger(me, message)
        class(Logger)       :: me
        character(*)        :: message
        if (me%logToConsole) print *, message
        if (me%logToFile) write(me%fileUnit, '(A)') timestamp() // ": " // message
    end subroutine

    !> Log just to file. Takes either a message or any array of ErrorInstanaces.
    !! The ErrorInstanaces are ignored if a message is supplied.
    subroutine toFileLogger(me, message, errors)
        class(Logger)                   :: me           !! This Logger instance
        character(*), optional          :: message      !! The message to log
        type(ErrorInstance), optional   :: errors(:)    !! Error messages to log
        integer                         :: i, j
        character(len=9)                :: messagePrefix
        character(len=256)              :: outputMessage
        character(len=512)              :: traceMessage
        if (present(message)) then
            if (me%logToFile) write(me%fileUnit, '(A)') timestamp() // ": " // message
        else if (present(errors)) then
            if (me%logToFile) then
                do i=1, size(errors)
                    if (errors(i)%isError()) then
                        if (errors(i)%isCritical) then
                            messagePrefix = "Error:"
                        else
                            messagePrefix = "Warning:"
                        end if
                        outputMessage = trim(messagePrefix) // " " // &
                                        trim(errors(i)%message)

                        if (size(errors(i)%trace)>0 .and. allocated(errors(i)%trace)) then
                            traceMessage = "Trace: "
                            do j=1, size(errors(i)%trace)
                                traceMessage = trim(traceMessage) // " " // trim(errors(i)%trace(j))
                                if (j<size(errors(i)%trace)) traceMessage = " " // trim(traceMessage) // " >"
                            end do
                            outputMessage = trim(outputMessage) // trim(traceMessage) // "."
                        end if
                        write(me%fileUnit, '(A)') timestamp() // ": " // trim(outputMessage)
                    end if
                end do
            end if
        end if
    end subroutine

    subroutine toConsoleLogger(me, message)
        class(Logger) :: me
        character(*)        :: message
        if (me%logToConsole) print *, message
    end subroutine

    function timestamp()
        character(len=19)   :: timestamp
        integer             :: values(8)
        call date_and_time(values=values)
        write(timestamp, "(I4.4, A, I2.2, A, I2.2, A, I2.2, A, I2.2, A, I2.2)") &
            values(1), "-", values(2), "-", values(3), "_", values(5), ".", values(6), ".", values(7)
    end function

end module
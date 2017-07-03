module Globals
    use ErrorCriteriaModule
    use ErrorInstanceModule
    implicit none

    type(ErrorCriteria) :: ERROR_HANDLER

  contains

    !> Initialise global variables. For the moment, just error
    !! handling, but concievably could deal with constants, files
    !! and other setup tasks in the future.
    subroutine GLOBALS_INIT()
        ! Add custom errors to the error handler
        call ERROR_HANDLER%init(errors=[ &
            ErrorInstance(code=999,message="Invalid biota index provided when creating bed sediment layer."), &
            ErrorInstance(code=998,message="Invalid reactor index provided when creating bed sediment layer."), &
            ErrorInstance(code=997,message="Invalid bed sediment layer index provided when creating bed sediment."), &
            ErrorInstance(code=996,message="Invalid number of bed sediment layers provided. Must be greater than zero.") &
        ])
    end subroutine
end module
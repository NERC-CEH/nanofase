!> Module for interacting with the input data.
!! TODO: Re-write to not rely on mo_netcdf to speed things up
module classDataInterfacer
    use ResultModule
    use ErrorInstanceModule
    use UtilModule
    use Globals
    use mo_netcdf
    use classLogger, only: LOG
    implicit none
    
    type, public :: DataInterfacer
        character(len=256) :: inputFilePath
        type(NcDataset) :: inputDataset
        class(NcGroup), allocatable :: grp

      contains
        procedure, public :: init => initDataInterfacer
        procedure, public :: close => closeDataInterfacer
        procedure, public :: setGroup => setGroupDataInterfacer
        
        ! Methods to get variables of different types/ranks. All hopes of DRY coding out of the window...
        procedure, private :: getVariableInt0D
        procedure, private :: getVariableInt1D
        procedure, private :: getVariableInt2D
        procedure, private :: getVariableInt3D
        procedure, private :: getVariableInt4D
        procedure, private :: getVariableInt1DSpreadDefault
        procedure, private :: getVariableInt2DSpreadDefault
        procedure, private :: getVariableInt3DSpreadDefault
        procedure, private :: getVariableInt4DSpreadDefault
        procedure, private :: getVariableReal0D
        procedure, private :: getVariableReal1D
        procedure, private :: getVariableReal2D
        procedure, private :: getVariableReal3D
        procedure, private :: getVariableReal4D
        procedure, private :: getVariableReal1DSpreadDefault
        procedure, private :: getVariableReal2DSpreadDefault
        procedure, private :: getVariableReal3DSpreadDefault
        procedure, private :: getVariableReal4DSpreadDefault
        procedure, private :: getVariableDp0D
        procedure, private :: getVariableDp1D
        procedure, private :: getVariableDp2D
        procedure, private :: getVariableDp3D
        procedure, private :: getVariableDp4D
        procedure, private :: getVariableDp1DSpreadDefault
        procedure, private :: getVariableDp2DSpreadDefault
        procedure, private :: getVariableDp3DSpreadDefault
        procedure, private :: getVariableDp4DSpreadDefault
        ! Combine those methods so they can all be accessed by me%get
        generic, public :: get => getVariableInt0D, &
            getVariableInt1D, &
            getVariableInt2D, &
            getVariableInt3D, &
            getVariableInt4D, &
            getVariableInt1DSpreadDefault, &
            getVariableInt2DSpreadDefault, &
            getVariableInt3DSpreadDefault, &
            getVariableInt4DSpreadDefault, &
            getVariableReal0D, &
            getVariableReal1D, &
            getVariableReal2D, &
            getVariableReal3D, &
            getVariableReal4D, &
            getVariableReal1DSpreadDefault, &
            getVariableReal2DSpreadDefault, &
            getVariableReal3DSpreadDefault, &
            getVariableReal4DSpreadDefault, &
            getVariableDp0D, &
            getVariableDp1D, &
            getVariableDp2D, &
            getVariableDp3D, &
            getVariableDp4D, &
            getVariableDp1DSpreadDefault, &
            getVariableDp2DSpreadDefault, &
            getVariableDp3DSpreadDefault, &
            getVariableDp4DSpreadDefault

      end type
      
      type(DataInterfacer) :: DATA

  contains
    !> Initialise the `DataInterfacer`
    subroutine initDataInterfacer(me, inputFilePath)
        class(DataInterfacer) :: me                     !! This DataInterfacer object
        character(len=*) :: inputFilePath               !! Path to the data file

        me%inputFilePath = inputFilePath                ! Store the file path to the input dataset
        me%inputDataset = NcDataset(me%inputFilePath, "r")   ! Open dataset as read-only
        me%grp = me%inputDataset                        ! Initialise the current group as the input dataset
        call LOG%add("Initialising DataInterfacer: success")
    end subroutine

    !> Close the NetCDF file
    subroutine closeDataInterfacer(me)
        class(DataInterfacer) :: me
        call me%inputDataset%close()
    end subroutine
    
    !> Set the current group to retrieve variables from.
    !! This saves multiple queries to find the group if we're
    !! parsing a lot of variables from the same group
    function setGroupDataInterfacer(me, grp) result(r)
        class(DataInterfacer) :: me
        character(len=*) :: grp(:)        ! Array of group names for nested hierarchies
        integer :: i
        type(Result) :: r
        ! Check if the group exists
        do i = 1, size(grp)
            if (i == 1) then
                if (me%inputDataset%hasGroup(trim(grp(1)))) then
                    me%grp = me%inputDataset%getGroup(trim(grp(1)))
                else
                    call r%addError(ErrorInstance( &
                        message="NetCDF group not found in input file: " // trim(grp(1)) // ".")) 
                end if
            else
                if (me%grp%hasGroup(trim(grp(i)))) then
                    me%grp = me%grp%getGroup(trim(grp(i)))
                else
                    call r%addError(ErrorInstance( &
                        message="NetCDF group not found in input file: " // trim(grp(i)) // ".")) 
                end if
            end if
        end do
    end function
    
!---------!
!-- GET --!
!---------!
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 0D integer.
    !! TODO Error code should be 201, unless we get rid of error codes
    function getVariableInt0D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)               :: me
        character(len=*)                    :: varName
        integer, intent(out)                :: var
        integer, optional                   :: default
        character(*), optional              :: defaultMessage       !! Override the "Defaulting to..." message
        logical, optional                   :: warnIfDefaulting     !! Should a warning be issued if var defaults? Defaults to false
        logical, optional                   :: silentlyFail         !! If the variable isn't found, do absolutely nothing (default is overriden)
        type(NcVariable)                    :: ncVar
        character(256)                      :: message
        type(Result)                        :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'. "
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = message // adjustl(trim(defaultMessage))
                else
                    message = message // "Defaulting to " // trim(str(default)) // "."
                end if
                ! Trigger a warning, if asked to
                if (present(warnIfDefaulting) .and. warnIfDefaulting) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D integer.
    function getVariableInt1D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:)
        integer, optional           :: default(:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 2D integer.
    function getVariableInt2D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:)
        integer, optional           :: default(:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 2D integer.
    function getVariableInt3D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:,:)
        integer, optional           :: default(:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 4D integer.
    function getVariableInt4D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:,:,:)
        integer, optional           :: default(:,:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D integer.
    function getVariableInt1DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:)
        integer                     :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D integer.
    function getVariableInt2DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:)
        integer                     :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D integer.
    function getVariableInt3DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:,:)
        integer                     :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D integer.
    function getVariableInt4DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        integer, allocatable        :: var(:,:,:,:)
        integer                     :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 0D real.
    function getVariableReal0D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, intent(out)           :: var
        real, optional              :: default
        character(*), optional      :: defaultMessage               ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'. "
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = message // adjustl(trim(defaultMessage))
                else
                    message = message // "Defaulting to " // trim(str(default)) // "."
                end if
                ! Trigger a warning, if asked to
                if (present(warnIfDefaulting) .and. warnIfDefaulting) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D real.
    function getVariableReal1D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:)
        real, optional              :: default(:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 2D real.
    function getVariableReal2D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:)
        real, optional              :: default(:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 3D real.
    function getVariableReal3D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:,:)
        real, optional              :: default(:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 4D real.
    function getVariableReal4D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:,:,:)
        real, optional              :: default(:,:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D real.
    function getVariableReal1DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:)
        real                        :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D real.
    function getVariableReal2DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:)
        real                        :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D real.
    function getVariableReal3DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:,:)
        real                        :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D real.
    function getVariableReal4DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real, allocatable           :: var(:,:,:,:)
        real                        :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 0D double precision real.
    function getVariableDp0D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), intent(out)       :: var
        real(dp), optional          :: default
        character(*), optional      :: defaultMessage               ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'. "
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = message // adjustl(trim(defaultMessage))
                else
                    message = message // "Defaulting to " // trim(str(default)) // "."
                end if
                ! Trigger a warning, if asked to
                if (present(warnIfDefaulting) .and. warnIfDefaulting) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp1D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:)
        real(dp), optional          :: default(:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 2D double precision real.
    function getVariableDp2D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:)
        real(dp), optional          :: default(:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 3D double precision real.
    function getVariableDp3D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:,:)
        real(dp), optional          :: default(:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp4D(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:,:,:)
        real(dp), optional          :: default(:,:,:,:)
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            if (present(default)) then
                var = default                           ! Default to whatever is provided
                if (present(defaultMessage)) then
                    message = trim(message) // " " // adjustl(trim(defaultMessage))
                else
                    message = trim(message) // " Defaulting to specified default value."
                end if
                ! Trigger a warning, if asked to
                if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                    call r%addError(ErrorInstance(message=message, isCritical = .false.))
                end if
            else
                call r%addError(ErrorInstance(message=message))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp1DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:)
        real(dp)                    :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp2DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:)
        real(dp)                    :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp3DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:,:)
        real(dp)                    :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function
    
    !> Get variable `varName` from current group (use `setGroup` to set current group),
    !! when variable is a 1D double precision real.
    function getVariableDp4DSpreadDefault(me, varName, var, default, defaultMessage, warnIfDefaulting, silentlyFail) result(r)
        class(DataInterfacer)       :: me
        character(len=*)            :: varName
        real(dp), allocatable       :: var(:,:,:,:)
        real(dp)                    :: default
        character(*), optional      :: defaultMessage           ! Override the "Defaulting to..." message
        logical, optional           :: warnIfDefaulting
        logical, optional           :: silentlyFail             !! If the variable isn't found, no absolutely nothing (default is overriden)
        type(NcVariable)            :: ncVar
        character(256)              :: message
        type(Result)                :: r
        ! Check if the variable exists, then get it if so.
        if (me%grp%hasVariable(varName)) then
            ncVar = me%grp%getVariable(varName)     ! Get the variable asked for
            call ncVar%getData(var)                 ! Place it in the var provided
        else if (present(silentlyFail) .and. silentlyFail) then
            return
        else
            message = "Variable '" // trim(varName) // "' not found in group '" // trim(me%grp%getName()) // "'."
            var = default                           ! Default to whatever is provided
            if (present(defaultMessage)) then
                message = trim(message) // " " // adjustl(trim(defaultMessage))
            else
                message = trim(message) // " Defaulting to specified default value."
            end if
            ! Trigger a warning, if asked to
            if ((present(warnIfDefaulting) .and. warnIfDefaulting) .or. (.not. present(warnIfDefaulting))) then
                call r%addError(ErrorInstance(message=message, isCritical = .false.))
            end if
        end if
    end function

end module
module classSubRiver1
                                                                    ! SubRiver1 subclass
                                                                    ! implements spcSubRiver superclass
                                                                    ! of a SubRiver class
                                                                    ! a SubRiver class acts as a container for a collection of RiverReach objects which collectively define a
                                                                    ! contiguous stretch of flowing waters within each grid cell
                                                                    ! IMPORTED MODULES
                                                                    ! Description
                                                                    ! -----------
    use Globals                                                     ! global declarations
    use UtilModule                                                  ! useful utilities
    use netcdf                                                      ! input/output handling
    use mo_netcdf                                                   ! input/output handling
    use ResultModule                                                ! error handling classes, required for
    use ErrorInstanceModule                                         ! generation of trace error messages
    use spcSubRiver                                                 ! Module containing SubRiver abstract interface
    use classRiverReach1
    implicit none                                                   ! force declaration of all variables
    type, extends(SubRiver), public :: SubRiver1                    ! type declaration for subclass

      contains
                                                                    ! METHODS
                                                                    ! Description
                                                                    ! -----------
        procedure, public :: create => createSubRiver1              ! create the SubRiver1 object. Exposed name: create
        procedure, public :: destroy => destroySubRiver1            ! remove the SubRiver1 object and all contained objects. Exposed name: destroy
        procedure, public :: update => updateSubRiver1            ! route water and suspended solids through the SubRiver. Exposed name: routing
        procedure, public :: finaliseUpdate => finaliseUpdateSubRiver1 ! Finalise the routing by setting temp outflows to actual outflows
                                                                    ! Description
                                                                    ! -----------
        procedure, private :: auditrefs                             ! internal property function: sense check the inflow and outflow GridCell references
        ! THIS PROCEDURE IS NOT DEFINED IN THE ABSTRACT CLASS AS IT IS PRIVATE. CAN IT STILL BE INHERITED?
    end type

    !> Interface so that we can create new SubRivers by `sr = SubRiver1()`
    interface SubRiver1
        module procedure newSubRiver1
    end interface
  contains
    !> Return a newly-created SubRiver1 object. This is bound to SubRiver1 interface
    !! TODO: Do something with result object
    function newSubRiver1(x, y, s, length, QrunoffTimeSeries) result(me)
        type(SubRiver1) :: me                                       !! The new SubRiver to return
        integer :: x, y, s                                          !! Location of the SubRiver
        real(dp) :: length                                          !! Length of the SubRiver (without meandering)
        real(dp), allocatable :: QrunoffTimeSeries(:)               !! Any initial runoff from the hydrological model
        type(Result) :: r                                           !! Result object
        ! Create the new SubRiver
        r = me%create(x, y, s, length, QrunoffTimeSeries)
    end function

    function createSubRiver1(me, x, y, s, length, QrunoffTimeSeries) result(r)! create the SubRiver object by reading data in from file
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(integer), intent(in) :: x                              ! the row number of the enclosing GridCell
        type(integer), intent(in) :: y                              ! the column number of the enclosing GridCell
        type(integer), intent(in) :: s                              ! reference SubRiver number
        real(dp) :: length                                          ! The length of the SubRiver (without meandering)
        real(dp), allocatable :: QrunoffTimeSeries(:)               ! Initial runoff from the hydrological model
        type(Result) :: r                                           ! the result object
        real(dp), allocatable :: riverReachRunoffTimeSeries(:)      ! Runoff for each RiverReach
        type(NcDataset) :: NC                                       ! NetCDF dataset
        type(NcVariable) :: var                                     ! NetCDF variable
        type(NcGroup) :: grp                                        ! NetCDF group
        type(NcGroup) :: subRiverGrp                                ! NetCDF group specifically for this SubRiver
        type(integer) :: i, t                                       ! loop counter
        type(character(len=100)) :: sr1                             ! string to dynamically compile and hold group names (must be specific length)
        type(character(len=100)) :: sr2                             ! string to dynamically compile and hold group names
        type(RiverReach1), allocatable :: r1                        ! private RiverReach1 type, used for dynamic assignment
        character(len=5) :: charMaxRiverReaches                     ! character string to store max number of RiverReaches allowed in
        ! Function purpose
        ! -------------------------------------------------------------
        ! parameterise a SubRiver object in a specified grid cell,
        ! including the creation of RiverReach objects
        !
        ! Function inputs
        ! -------------------------------------------------------------
        ! x  : x reference to the enclosing grid cell
        ! y  : y reference to the enclosing grid cell
        ! s  : reference SubRiver number
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------
        ! Fully specified SubRiver object, comprising:
        ! Collection of RiverReach objects: me%colReaches.
        ! Set of up to three inflow references inflowRefs(:),
        ! comprising Grid x and y references and  SubRiver number
        ! reference, or null if SubRiver is a headwater.

        allocate(me%spmOut(C%nSizeClassesSPM), &                    ! Initialise SPM arrays to size of size classes
            me%spmIn(C%nSizeClassesSpm), &
            me%tmpSpmOut(C%nSizeClassesSpm), &
            me%tmpm_spm(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            stat=me%allst)             
        me%ref = trim(ref("SubRiver", x, y, s))                     ! Create SubRiver reference name, SubRiver_x_y_s
        me%length = length                                          ! Set the length
        allocate(me%QrunoffTimeSeries, source=QrunoffTimeSeries)    ! Set the runoff
        me%spmOut = 0                                               ! Initialise SPM to zero
        me%spmIn = 0

        ! Get and validate the input data
        ! TODO: Auditing of input data is a bit of a mess at the moment, but I'm
        ! leaving as-is until data input is moved to some kind of Database object.
        ! Then we can pre-validate data files before running the model (if we want to).
        nc = NcDataset(C%inputFile, "r")                            ! Open dataset as read-only
        grp = nc%getGroup("Environment")
        grp = grp%getGroup("GridCell_" // trim(str(x)) // "_" // trim(str(y)))
        subRiverGrp = grp%getGroup(me%ref)                          ! point to the SubRiver group
        var = subRiverGrp%getVariable("nInflows")                   ! point to the variable nInflows: the number of inflows
        call var%getData(me%nInflows)                               ! pull data into variable: number of inflows
        call ERROR_HANDLER%trigger( &                               ! Check nInflows is 1, 2 or 3
            error = ERROR_HANDLER%limit( &                          ! TODO: Also check nInflows equals number of inflow_x groups (or just get rid of nInflows)
                me%nInflows, 0, 3, &
                "Number of RiverReach inflows must be between 0 and 3.", &
                "Creating " // trim(me%ref) &
        ))
        allocate(me%inflowRefs(me%nInflows), stat=me%allst)         ! allocate required space to hold the inflow references for this SubRiver
        allocate(me%inflows(me%nInflows), stat=me%allst)            ! likewise for the array of inflow pointers
        var = subRiverGrp%getVariable("reachTypes")                 ! point to the array ReachTypes: the type identifiers for the RiverReach objects in this SubRiver
        call var%getData(me%reachTypes)                             ! pull the Reach type references into SubRiver object
        me%nReaches = size(me%reachTypes)                           ! get the number of reaches from the ReachType array size
        call ERROR_HANDLER%trigger( &                               ! Check nReaches is >0 but <maxRiverReaches (specified in config file(?))
            error = ERROR_HANDLER%limit( &                          ! TODO: Get number of SubRivers from outflow GridCell to check SubRiver number isn't greater
                me%nReaches, 0, C%maxRiverReaches, &
                "Number of RiverReaches must be positive but less than " &
                    // trim(str(C%maxRiverReaches)), &
                "Creating " // trim(me%ref) &
        ))

        if (me%nInflows > 0) then
            do i = 1, me%nInflows                                   ! loop to read the Inflow references for this SubRiver
                sr1 = "inflow_" // trim(str(i))                     ! create character variable 'Inflow1', 'Inflow2' etc.
                if (.not. subRiverGrp%hasGroup(sr1)) then           ! Check if inflow_i group exists
                    call ERROR_HANDLER%trigger(error=ErrorInstance( &     ! We need to immediatedly trigger error if not, as otherwise
                        code = 202, &                               ! NetCDF library will trigger a less useful one instead
                        message = "Group " // trim(sr1) // " representing inflow not found in data file. " // &
                                    "Make sure nInflows matches number of inflow groups.", &
                        trace = ["Creating " // trim(me%ref)] &
                    ))
                end if
                grp = subRiverGrp%getGroup(sr1)                     ! point to the Inflow1, Inflow2 etc. group
                var = grp%getVariable("gridX")                      ! point to the variable defining the row of the grid cell
                call var%getData(me%inflowRefs(i)%GridX)            ! pull GridX reference into SubRiver object
                                                                    ! AUDIT GridX here - must be >0 and <= the highest grid cell number
                call r%addError( &                                  ! Check GridX is >0. Need to think about where highest grid cell number is specified.
                    ERROR_HANDLER%positive( &
                        value = me%inflowRefs(i)%GridX, &
                        message = "Inflow grid cell row number x must be positive." &
                    ) &
                )
                var = grp%getVariable("gridY")                      ! point to the variable defining the column of the grid cell
                call var%getData(me%inflowRefs(i)%GridY)            ! pull GridY reference into SubRiver object
                                                                    ! AUDIT GridY here - must be >0 and <= the highest grid cell number
                call r%addError( &                                  ! Check GridY is >0. Need to think about where highest grid cell number is specified.
                    ERROR_HANDLER%positive( &
                        value = me%inflowRefs(i)%GridY, &
                        message = "Inflow grid cell column number y must be positive." &
                    ) &
                )
                var = grp%getVariable("subRiver")                   ! point to the variable defining the SubRiver acting as an input
                call var%getData(me%inflowRefs(i)%SubRiver)         ! pull SubRiver reference into SubRiver object
                                                                    ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers
                call r%addError( &                                  ! Check SubRiver number is >0. If it's a headwater, then won't nInflows = 0 (and thus we'll never enter this loop)?
                    ERROR_HANDLER%positive( &                       ! TODO: Get number of SubRivers from inflow GridCell to check SubRiver number isn't greater
                        value = me%inflowRefs(i)%SubRiver, &
                        message = "Inflow SubRiver number must be positive." &
                    ) &
                )
                call r%addToTrace("Processing " // sr1)             ! Add this inflow to the error trace
                ! I've assumed here that this is the only way to read in single elements of a user-defined type, i.e. by listing each as a separate variable.
                ! But can a single user-defined type (i.e. GridX, GridY and SubRiver) be listed as a single object in the .json file and
                ! read in as a single variable? Then "nInflows" could be listed in the .json file as a dimension, in the way that "ReachTypes" is, and the number of inflows
                ! inferred from the size of the inflowRefs(:) array after the inflow references have been read in.
            end do
        end if
                                                                    ! AUDIT size(ReachTypes)=nReaches here
                                                                    ! ^ nReaches is set as size(ReachTypes) now - is there any reason not to do this?
        allocate(me%colReaches(1:me%nReaches), stat=me%allst)       ! Set colReaches to be of size nReaches
        allocate(riverReachRunoffTimeSeries(size(me%QrunoffTimeSeries)))
        do t = 1, size(me%QrunoffTimeSeries)                        ! Split the runoff between the reaches
            if (me%QrunoffTimeSeries(t) > 0) then
                riverReachRunoffTimeSeries(t) = me%QrunoffTimeSeries(t)/me%nReaches
            else
                riverReachRunoffTimeSeries(t) = 0
            end if
        end do
        do i = 1, me%nReaches                                       ! loop through each RiverReach in each SubRiver to create the reaches
            select case (me%reachTypes(i))                          ! look at the type identifier for the yth RiverReach
                case (1)
                    allocate(r1, stat=me%allst)                     ! RiverReach1 type - create the object
                    r = r1%create(x,y,s,i,me%length/me%nReaches,riverReachRunoffTimeSeries)    ! call the RiverReach1 constructor
                    call move_alloc(r1, me%colReaches(i)%item)      ! move the RiverReach1 object to the yth element of the colReaches collection
                case default
                    ! Add error if RiverReach type index is invalid
                    call r%addError(ErrorInstance( &
                        code = 901, &
                        message = "Invalid RiverReach type index (" // trim(str(me%reachTypes(i))) // ") provided.", &
                        trace = ["Creating " // trim(ref("RiverReach", x, y, s, i))] &
                    ))
            end select
        end do
        call r%addToTrace("Creating " // trim(me%ref))
    end function
    
    function destroySubRiver1(me) result(r)
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(Result) :: r                                           ! the Result object
        type(integer) :: i                                          ! loop counter
        do i = 1, me%nReaches                                       ! loop through each RiverReach
            r = me%colReaches(i)%item%destroy()                     ! call destroy routine in the SubRiver object
        end do
        ! TODO: Something here to compile all returned Result objects into one?
    end function

    ! TODO: Sort out object storage of spmIn and Q_in
    function updateSubRiver1(me, t) result(r)                         ! routes inflow(s) through the specified SubRiver
        class(SubRiver1) :: me                                      ! the SubRiver instance
        integer :: t                                                ! What time step are we on?
        type(Result) :: r                                           ! the Result object
        type(Result) :: reachR                                      ! Result object for each reach
        real(dp) :: Qin(me%nReaches + 1)                            ! The inflow, final element for outflow [m3/timestep]
        real(dp) :: spmIn(me%nReaches + 1, C%nSizeClassesSpm)       ! The SPM inflow per size class, final element for outflow [kg/timestep]
        type(integer) :: i                                          ! loop counter
        real(dp) :: tmpSpmIn(C%nSizeClassesSpm)                     ! Temporary variable to pass as argument, avoiding array temporary warning (https://stackoverflow.com/questions/28859524/fortran-runtime-warning-temporary-array)
        ! Function purpose
        ! -------------------------------------------------------------
        ! route water and suspended material from the upstream
        ! SubRiver(s), and from overland flow and erosion, through
        ! this SubRiver
        !
        ! Function inputs
        ! -------------------------------------------------------------
        ! Function uses inflows(:), which point to inflow SubRivers,
        ! to interrogate other SubRivers for their discharge and suspended material
        ! fluxes.
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------
        ! Qout : outflow discharge (m3)
        ! spmOut(:) : outflow SPM fluxes (kg)
        ! These variables are stored at object level for interrogation
        ! by the downstream SubRiver

        Qin = 0                                                 ! Initialise Q and SPM to zero, before we add the inflows and sources
        spmIn = 0
        me%tmpm_spm = 0                                         ! m_spm is obtained from summing across RiverReaches, so set to zero before
                                                                ! starting the summation. Reason for temporary var detailed below.
        ! Routing procedure:
        !   - Loop through inflows and sum Q and SPM
        !   - Pass the inflows to the first RiverReach, which internally calculates an
        !     outflow (and updates volume, densities, etc), which is then passed to the
        !     next RiverReach, and so on.
        !   - Outflow from final RiverReach used to set *temporary* outflow Q and SPM variables.
        !     These are temporary so that they don't affect inflow to other SubRivers until all
        !     SubRiver calculations are complete, after which a procedure in GridCell's update()
        !     method stores them in me%Qout and me%spmOut.
        do i = 1, me%nInflows                                       ! Loop through the inflows to retrieve and sum discharges
            Qin(1) = Qin(1) + me%inflows(i)%item%getQOut()          ! Pull in discharge from upstream SubRiver to first RiverReach
            ! SubRiver%Qout isn't set until all SubRivers have been routed, thus ensuring this is Qout for the correct timestep
            spmIn(1,:) = spmIn(1,:) + me%inflows(i)%item%getSpmOut() ! pull in SPM fluxes from upstream SubRiver
        end do

        do i = 1, me%nReaches                                       ! main routing loop
            ! Main simulation call to the RiverReach, which recalculates dimensions
            ! and outflows based on the inflow Q and SPM
            tmpSpmIn = spmIn(i,:)                                   ! Temporary array to avoid warning when using assumed shape as argument
            reachR = me%colReaches(i)%item%update(Qin(i), tmpSpmIn, t)
            call r%addErrors(.errors. reachR)                       ! Add any error that occured to the Result object to return
            Qin(i+1) = me%colReaches(i)%item%getQOut()              ! Set the next reach's inflows from this reach's outflow
            spmIn(i+1,:) = me%colReaches(i)%item%getSpmOut()
            me%tmpm_spm = me%tmpm_spm + me%colReaches(i)%item%m_spm ! Sum the SPM mass across the reaches to get total SPM mass for the SubRiver
        end do

        ! Temporary storage for QOut and spmOut, until all SubRivers have been routed and we can
        ! be sure that updating Qout won't result in the wrong timestep's Qout being used as Qin
        ! to a downstream SubRiver
        me%tmpQOut = Qin(me%nReaches + 1)                          ! store the final outflow volume [m3]
        me%tmpSpmOut = spmIn(me%nReaches + 1, :)                   ! output SPM flux (kg) of size class 'n' for this displacement

        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function
    
    !> Set the outflow and SPM mass from the temporary variables that were set by the
    !! routing procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another SubRiver whilst the SubRivers
    !! are looped through.
    function finaliseUpdateSubRiver1(me) result(r)
        class(SubRiver1) :: me                                      !! This SubRiver1 instace
        type(Result) :: r                                           !! The Result object
        me%Qout = me%tmpQout
        me%spmOut = me%tmpSpmOut
        me%m_spm = me%tmpm_spm
    end function

    ! ******************************************************
    function auditrefs(me) result(r)
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(Result) :: r                                           ! the result object
        ! the purpose of this function is to sense check the inflow and outflow references, i.e. do they form
        ! robust, consistent links to adjacent grid cells?
        ! but perhaps this could be a function within the Environment module that audits all links at the same time
        ! on startup? i.e. call Audit(<GridCell>) for all GridCells?
    end function
end module
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
        procedure, public :: routing => routingSubRiver1            ! route water and suspended solids through the SubRiver. Exposed name: routing
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
    !! and is not type-bound.
    !! TODO: Do something with result object
    function newSubRiver1(x, y, s, length) result(me)
        type(SubRiver1) :: me                                       !! The new SubRiver to return
        integer :: x, y, s                                          !! Location of the SubRiver
        real(dp) :: length                                          !! Length of the SubRiver (without meandering)
        type(Result) :: r                                           !! Result object
        ! Create the new SubRiver
        r = me%create(x, y, s, length)
    end function

    function createSubRiver1(me, x, y, s, length) result(r)         ! create the SubRiver object by reading data in from file
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(integer), intent(in) :: x                              ! the row number of the enclosing GridCell
        type(integer), intent(in) :: y                              ! the column number of the enclosing GridCell
        type(integer), intent(in) :: s                              ! reference SubRiver number
        real(dp) :: length                                          ! The length of the SubRiver (without meandering)
        type(Result) :: r                                           ! the result object
        type(NcDataset) :: NC                                       ! NetCDF dataset
        type(NcVariable) :: var                                     ! NetCDF variable
        type(NcGroup) :: grp                                        ! NetCDF group
        type(NcGroup) :: subRiverGrp                                ! NetCDF group specifically for this SubRiver
        type(integer) :: i                                          ! loop counter
        type(character(len=100)) :: sr1                             ! string to dynamically compile and hold group names (must be specific length)
        type(character(len=100)) :: sr2                             ! string to dynamically compile and hold group names
        type(RiverReach1), allocatable :: r1                        ! private RiverReach1 type, used for dynamic assignment
        type(ErrorInstance), allocatable :: errors(:)
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
        allocate(errors(0))                                         ! No errors to begin with
        me%length = length                                          ! Set the length
                                                                    ! NO NEED TO AUDIT SC - WILL ALREADY HAVE BEEN done
        nc = NcDataset(C%inputFile, "r")                            ! Open dataset as read-only
        sr1 = trim(str(x)) // "_"
        sr1 = trim(sr1) // trim(str(y))
        sr1 = "SubRiver_" // trim(sr1)
        sr2 = "_" // trim(str(s))
        sr1 = trim(sr1) // trim(sr2)                                ! dynamically create reference group name for subriver
                                                                    ! Format is SubRiver_x_y_s
        me%ref = sr1                                                ! Store this in instance variable (useful for printing errors later)
        grp = nc%getGroup("Environment")
        grp = grp%getGroup("GridCell_" // trim(str(x)) // "_" // trim(str(y)))
        subRiverGrp = grp%getGroup(me%ref)                          ! point to the SubRiver group
        var = subRiverGrp%getVariable("nInflows")                   ! point to the variable nInflow: the number of inflows
        call var%getData(me%nInflows)                               ! pull data into variable: number of inflows
                                                                    ! AUDITING CODE HERE - RETURN ERROR IF nInflows IS NOT =1, 2 OR 3
                                                                    ! TODO: Also check nInflows equals number of inflow_x groups (or just get rid of nInflows)
        allocate(me%inflowRefs(me%nInflows), stat=me%allst)         ! allocate required space to hold the inflow references for this SubRiver
        allocate(me%inflows(me%nInflows), stat=me%allst)            ! likewise for the array of inflow pointers
        var = subRiverGrp%getVariable("reachTypes")                 ! point to the array ReachTypes: the type identifiers for the RiverReach objects in this SubRiver
        call var%getData(me%reachTypes)                             ! pull the Reach type references into SubRiver object
        me%nReaches = size(me%reachTypes)                           ! get the number of reaches from the ReachType array size
        call r%addError( &                                          ! Check nReaches is >0 but <maxRiverReaches (specified in config file(?))
            ERROR_HANDLER%limit( &                                  ! TODO: Get number of SubRivers from outflow GridCell to check SubRiver number isn't greater
              value = me%nReaches, &
              lbound = 0, &
              ubound = C%maxRiverReaches, &
              message = "Number of RiverReaches must be positive but less than " &
                // adjustl(trim(str(C%maxRiverReaches))) &
            ) &
        )

        if (me%nInflows > 0) then
            do i = 1, me%nInflows                                   ! loop to read the Inflow references for this SubRiver
                sr1 = "inflow_" // trim(str(i))                     ! create character variable 'Inflow1', 'Inflow2' etc.
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
        do i = 1, me%nReaches                                       ! loop through each RiverReach in each SubRiver to create the reaches
            select case (me%reachTypes(i))                          ! look at the type identifier for the yth RiverReach
                case (1)
                    allocate(r1, stat=me%allst)                     ! RiverReach1 type - create the object
                    r = r1%create(x,y,s,i,me%length/me%nReaches)    ! call the RiverReach1 constructor
                    call move_alloc(r1, me%colReaches(i)%item)      ! move the RiverReach1 object to the yth element of the colReaches collection
                case default
                    ! not a valid RiverReach type - must cause an error
            end select
        end do
        call r%addToTrace("Creating " // me%ref)
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
    function routingSubRiver1(me) result(r)                         ! routes inflow(s) through the specified SubRiver
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(Result) :: r                                           ! the Result object
        type(real(dp)), allocatable :: Qin(:)                       ! the inflow, each element representing a RiverReach (m3)
        ! Placed SPMin and nInflows in spcSubRiver type declaration to fit in with me%SPMin, me%nInflow calls below
        ! type(real(dp)), allocatable :: SPMin(:)                   ! array of SPM masses, one per size class, in inflow (kg)
        ! type(integer) :: nInflows                                 ! the number of inflows
        type(integer) :: x                                          ! the row number of a GridCell
        type(integer) :: y                                          ! the column number of a GridCell
        type(integer) :: SR                                         ! SubRiver number reference
        type(integer) :: ndisp                                      ! displacement counter
        type(real(dp)) :: dQ                                        ! inflow volume (m3) per displacement
        type(real(dp)) :: rQ                                        ! reach capacity (m3) per timestep
        type(real(dp)) :: dSPM(C%nSizeClassesSPM)                   ! inflow SPM per size class (kg) per displacement
        type(integer) :: i, j, n                                    ! loop counters
        real(dp), allocatable :: rArray(:)                          ! Temporary variable to store type(Result) data arrays in
        ! Function purpose
        ! -------------------------------------------------------------
        ! route water and suspended material from the upstream
        ! SubRiver(s), and from overland flow and erosion, through
        ! this SubRiver
        !
        ! Function inputs
        ! -------------------------------------------------------------
        ! function uses inflowRefs(:) to interrogate other SubRivers
        ! for their discharge and suspended material fluxes.
        ! Does this by referencing Subrivers as
        ! Environment%GridCell(x, y)%SubRiver(z) for a SubRiver
        ! outside the GridCell, and
        ! GridCell(x, y)%Subriver(z) for a Subriver in this GridCell.
        ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
        !
        ! Function outputs/outcomes
        ! -------------------------------------------------------------
        ! Qout : outflow discharge (m3)
        ! Sout(:) : outflow SPM fluxes (kg)
        ! These variables are stored at object level for interrogation
        ! by the downstream SubRiver

        me%nInflows = size(me%inflows)                              ! get the number of inflows to be processed
        allocate(me%Qin(me%nReaches + 1), stat=me%allst)            ! initialise Qin - extra element holds final discharge
        allocate(me%SPMin(me%nReaches + 1, C%nSizeClassesSPM), me%spmOut(C%nSizeClassesSPM), stat=me%allst)    ! initialise SPMin - extra element holds final discharge
        me%Qin(1) = 0                                               ! Initalise Q and SPM to zero, before we add the inflows and sources
        me%spmIn(1,:) = 0
        do i = 1, me%nInflows                                       ! loop through the inflows to retrieve and sum discharges
            me%Qin(1) = me%Qin(1) + me%inflows(i)%item%getQOut()    ! pull in discharge from upstream SubRiver to first RiverReach
            do n = 1, C%nSizeClassesSPM                             ! loop through all SPM size classes
                me%spmIn(1, n) = me%spmIn(1, n) + &                 ! pull in SPM fluxes from upstream SubRiver
                    me%inflows(i)%item%getSpmOut(n)
            end do
        end do                                                      ! loop to sum all discharges and SPM fluxes
        do i = 1, me%nReaches                                       ! main routing loop
            r = me%colReaches(i)%item%updateDimensions(me%Qin(i))   ! call function in RiverReach to set up dimensions of
                                                                    ! the reach for this timestep. Qin(1) set above, Qin(>1)
                                                                    ! set by the previous loop iteration. This also sets me%Q_in for the reach.
            rQ = me%colReaches(i)%item%getVolume()                  ! get volumetric capacity of the reach (m3) for this timestep
            ! If the inflow Qin is larger than the reach's capacity rQ, then we need to split the inflow
            ! up into separate displacements and simulate the reach's routing for each of these displacements
            ! individually
            ! ***
            ! SH: As we're calculating the volume from Qin (see calculateWidth, calculateDepth and calculateVolume
            ! in classRiverReach.f08), I think that always contrains the volume to be larger than
            ! Qin - i.e., Qin forces the river to be large enough to encompass the inflow. Thus, is this approach needed?
            ! ***
            ! ndisp = 0                                               ! count number of displacements required
            ! do while (dQ > Qin(i))                                  ! loop until the inflow volume is less than the number of displacements
            !     dQ = rQ / (ndisp + 1)                               ! compute input volume as a function of the no. of displacements
            !     ndisp = ndisp + 1                                   ! increment the number of displacements and repeat
            ! end do
            ! SH: Shouldn't the above be:
            ndisp = 1
            dQ = me%Qin(i) / ndisp
            do while (dQ > rQ)
                dQ = me%Qin(i) / ndisp                              ! Split Qin into the number of displacements,
                ndisp = ndisp + 1                                   ! or keep it the same if Qin is never > rQ
            end do
            me%Qin(i + 1) = 0                                       ! initialise discharge summation for next RiverReach
            do n = 1, C%nSizeClassesSPM                             ! compute inflow SPM fluxes for this displacement
                dSPM(n) = me%spmIn(i, n) / ndisp                    ! SPM flux (kg) of size class 'n' for this displacement
                me%spmIn(i + 1, n) = 0                              ! initialise SPM flux summation for next RiverReach
            end do
            do j = 1, ndisp                                         ! route water and SPM on each displacement
                ! Main simulation call for the RiverReach (e.g., will get rid of stuff by settling, abstraction).
                ! nDisp needs to be passed to simulate to calculate the SPM density.
                ! Returned dQ and dSPM will then be put into Qin(i+1) and SPMin(i+1,:) and summed across each
                ! loop iteration. Then, on the next iteration of the main loop (i, through the RiverReaches), Qin(i+1)
                ! becomes Qin(i), the input volume to the next RiverReach. Similarly for each SPM size class.
                ! On the final iteration, we exit the loop with the final outflow and SPM fluxes in Qin(nReaches + 1)
                ! and SPMin(nReaches + 1, :)
                rArray = .dp. me%colReaches(i)%item%simulate(dQ, dSPM, ndisp) ! simulate() returns an array:
                dQ = rArray(1)                                      ! First element is outflow dQ
                dSPM = rArray(2:)                                   ! Second element is outflow dSPM
                ! SH: I changed simulate() to return variables rather than altering the dQ and dSPM passed to it
                me%Qin(i + 1) = me%Qin(i + 1) + dQ                  ! sum the outflow discharge on each displacement
                do n = 1, C%nSizeClassesSPM
                    me%spmIn(i + 1, n) = me%spmIn(i + 1, n) + dSPM(n) ! sum the outflow SPM fluxes on each displacement
                end do
            end do
        end do
        me%QOut = me%Qin(me%nReaches + 1)                           ! store the final outflow volume (m3)
        do n = 1, C%nSizeClassesSPM                                 ! compute inflow SPM fluxes for this displacement
            me%spmOut(n) = me%spmIn(me%nReaches + 1, n)             ! output SPM flux (kg) of size class 'n' for this displacement
        end do

        ! *********************************************** !
        ! Hack to set an outflow from the first grid cell !
        ! *********************************************** !
        if (me%ref == "SubRiver_1_1_1") then
            me%QOut = 10*C%timeStep                             ! Q = 10 m3/s = 864000 m3/day
            me%spmOut = 0.01_dp*me%Qout                         ! 0.01 kg/m3 advecting at 10 m3/s = 8640 kg/day
        end if
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

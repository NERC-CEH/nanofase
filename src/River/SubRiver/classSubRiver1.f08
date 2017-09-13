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
        procedure, public :: finaliseRouting => finaliseRoutingSubRiver1 ! Finalise the routing by setting temp outflows to actual outflows
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
    function newSubRiver1(x, y, s, length, Qrunoff) result(me)
        type(SubRiver1) :: me                                       !! The new SubRiver to return
        integer :: x, y, s                                          !! Location of the SubRiver
        real(dp) :: length                                          !! Length of the SubRiver (without meandering)
        real(dp) :: Qrunoff                                         !! Any initial runoff from the hydrological model
        type(Result) :: r                                           !! Result object
        ! Create the new SubRiver
        r = me%create(x, y, s, length, Qrunoff)
    end function

    function createSubRiver1(me, x, y, s, length, Qrunoff) result(r)! create the SubRiver object by reading data in from file
        class(SubRiver1) :: me                                      ! the SubRiver instance
        type(integer), intent(in) :: x                              ! the row number of the enclosing GridCell
        type(integer), intent(in) :: y                              ! the column number of the enclosing GridCell
        type(integer), intent(in) :: s                              ! reference SubRiver number
        real(dp) :: length                                          ! The length of the SubRiver (without meandering)
        real(dp) :: Qrunoff                                         ! Initial runoff from the hydrological model
        type(Result) :: r                                           ! the result object
        real(dp) :: riverReachRunoff                                ! Runoff for each RiverReach
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
        allocate(me%spmOut(C%nSizeClassesSPM), &                    ! Initialise SPM arrays to size of size classes
            me%spmIn(C%nSizeClassesSpm), &
            me%tmpSpmOut(C%nSizeClassesSpm), &
            me%tmpm_spm(C%nSizeClassesSpm), &
            me%m_spm(C%nSizeClassesSpm), &
            stat=me%allst)             
        me%length = length                                          ! Set the length
        me%Qrunoff = Qrunoff                                        ! Set the runoff
        me%spmOut = 0                                               ! Initialise SPM to zero
        me%spmIn = 0
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
        if (me%Qrunoff > 0) then                                    ! Split the runoff between the reaches
            riverReachRunoff = me%Qrunoff/me%nReaches
        else
            riverReachRunoff = 0
        end if
        do i = 1, me%nReaches                                       ! loop through each RiverReach in each SubRiver to create the reaches
            select case (me%reachTypes(i))                          ! look at the type identifier for the yth RiverReach
                case (1)
                    allocate(r1, stat=me%allst)                     ! RiverReach1 type - create the object
                    r = r1%create(x,y,s,i,me%length/me%nReaches,riverReachRunoff)    ! call the RiverReach1 constructor
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
        real(dp) :: Qin(me%nReaches + 1)                            ! The inflow, final element for outflow [m3/timestep]
        real(dp) :: spmIn(me%nReaches + 1, C%nSizeClassesSpm)       ! The SPM inflow per size class, final element for outflow [kg/timestep]
        ! type(integer) :: ndisp                                      ! displacement counter
        ! type(real(dp)) :: dQ                                        ! inflow volume (m3) per displacement
        ! type(real(dp)) :: rQ                                        ! reach capacity (m3) per timestep
        ! type(real(dp)) :: dSPM(C%nSizeClassesSpm)                   ! inflow SPM per size class (kg) per displacement
        type(integer) :: i                                          ! loop counter
        real(dp) :: tmpSpmIn(C%nSizeClassesSpm)                     ! Temporary variable to pass as argument, avoiding array temporary warning (https://stackoverflow.com/questions/28859524/fortran-runtime-warning-temporary-array)
        ! real(dp), allocatable :: rArray(:)                          ! Temporary variable to store type(Result) data arrays in
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

        Qin = 0                                                 ! Initialise Q and SPM to zero, before we add the inflows and sources
        spmIn = 0
        me%tmpm_spm = 0                                         ! m_spm is obtained from summing across RiverReaches, so set to zero before
                                                                ! starting the summation
        ! Routing procedure:
        !   - Loop through inflows and sum Q and SPM to store in me%Qin and me%spmIn
        !   - Pass the inflows to the first RiverReach, which internally calculates an
        !     outflow (and updates volume, densities, etc), which is then passed to the
        !     next RiverReach, and so on.
        !   - Outflow from final RiverReach used to set *temporary* outflow Q and SPM variables.
        !     These are temporary so that they don't affect inflow to other SubRivers until all
        !     SubRiver calculation are complete, after which a procedure in GridCell's update()
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
            r = me%colReaches(i)%item%update(Qin(i), tmpSpmIn)
            Qin(i+1) = me%colReaches(i)%item%getQOut()              ! Set the next reach's inflows from this reach's outflow
            spmIn(i+1,:) = me%colReaches(i)%item%getSpmOut()
            me%tmpm_spm = me%tmpm_spm + me%colReaches(i)%item%m_spm ! Sum the SPM mass across the reaches to get total SPM mass for the SubRiver

            ! r = me%colReaches(i)%item%updateDimensions(Qin(i))   ! call function in RiverReach to set up dimensions of
                                                                    ! the reach for this timestep. Qin(1) set above, Qin(>1)
                                                                    ! set by the previous loop iteration. This also sets me%Q_in for the reach.
            ! rQ = me%colReaches(i)%item%getVolume()                  ! get volumetric capacity of the reach (m3) for this timestep
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
            ! ndisp = 1
            ! dQ = Qin(i) / ndisp
            ! do while (dQ > rQ)
            !     dQ = Qin(i) / ndisp                              ! Split Qin into the number of displacements,
            !     ndisp = ndisp + 1                                   ! or keep it the same if Qin is never > rQ
            ! end do
            ! Qin(i + 1) = 0                                       ! initialise discharge summation for next RiverReach
            ! do n = 1, C%nSizeClassesSPM                             ! compute inflow SPM fluxes for this displacement
            !     dSPM(n) = spmIn(i, n) / ndisp                    ! SPM flux (kg) of size class 'n' for this displacement
            !     spmIn(i + 1, n) = 0                              ! initialise SPM flux summation for next RiverReach
            ! end do
            ! do j = 1, ndisp                                         ! route water and SPM on each displacement
            !     ! Main simulation call for the RiverReach (e.g., will get rid of stuff by settling, abstraction).
            !     ! nDisp needs to be passed to simulate to calculate the SPM density.
            !     ! Returned dQ and dSPM will then be put into Qin(i+1) and SPMin(i+1,:) and summed across each
            !     ! loop iteration. Then, on the next iteration of the main loop (i, through the RiverReaches), Qin(i+1)
            !     ! becomes Qin(i), the input volume to the next RiverReach. Similarly for each SPM size class.
            !     ! On the final iteration, we exit the loop with the final outflow and SPM fluxes in Qin(nReaches + 1)
            !     ! and SPMin(nReaches + 1, :)
            !     rArray = .dp. me%colReaches(i)%item%simulate(dQ, dSPM, ndisp) ! simulate() returns an array:
            !     dQ = rArray(1)                                      ! First element is outflow dQ
            !     dSPM = rArray(2:)                                   ! Second element is outflow dSPM
            !     ! SH: I changed simulate() to return variables rather than altering the dQ and dSPM passed to it
            !     Qin(i + 1) = Qin(i + 1) + dQ                  ! sum the outflow discharge on each displacement
            !     do n = 1, C%nSizeClassesSPM
            !         spmIn(i + 1, n) = spmIn(i + 1, n) + dSPM(n) ! sum the outflow SPM fluxes on each displacement
            !     end do
            ! end do
        end do

        ! Temporary storage for QOut and spmOut, until all SubRivers have been routed and we can
        ! be sure that updating Qout won't result in the wrong timestep's Qout being used as Qin
        ! to a downstream SubRiver
        me%tmpQOut = Qin(me%nReaches + 1)                          ! store the final outflow volume [m3]
        me%tmpSpmOut = spmIn(me%nReaches + 1, :)                   ! output SPM flux (kg) of size class 'n' for this displacement
    end function
    
    !> Set the outflow and SPM mass from the temporary variables that were set by the
    !! routing procedure. This step is kept separate from the routing so that the
    !! wrong outflow isn't used as an inflow for another SubRiver whilst the SubRivers
    !! are looped through.
    function finaliseRoutingSubRiver1(me) result(r)
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

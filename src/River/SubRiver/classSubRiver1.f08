module classSubRiver1
                                                                     ! SubRiver1 subclass
                                                                     ! implements spcSubRiver superclass
                                                                     ! of a SubRiver class
                                                                     ! a SubRiver class acts as a container for a collection of RiverReach objects which collectively define a
                                                                     ! contiguous stretch of flowing waters within each grid cell
                                                                     ! IMPORTED MODULES
                                                                     ! Description
                                                                     ! -----------
  use Globals                                                        ! global declarations
  use UtilModule
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  ! ENVIRONMENT
  ! use classEnvironment1                                              ! Environment gives ability to get flows, SPM content etc from other grid cells
  use spcSubRiver                                                    ! Module containing SubRiver abstract interface
  use classRiverReach1
  implicit none                                                      ! force declaration of all variables
  type, extends(SubRiver), public :: SubRiver1                        ! type declaration for subclass

    contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public :: create => createSubRiver1                   ! create the SubRiver1 object. Exposed name: create
    procedure, public :: destroy => destroySubRiver1                 ! remove the SubRiver1 object and all contained objects. Exposed name: destroy
    procedure, public :: routing => routingSubRiver1                 ! route water and suspended solids through the SubRiver. Exposed name: routing
                                                                     ! Description
                                                                     ! -----------
    procedure, private :: auditrefs                                  ! internal property function: sense check the inflow and outflow GridCell references

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
    function newSubRiver1(x, y, s) result(me)
      type(SubRiver1) :: me                         !! The new GridCell to return
      integer :: x, y, s                            !! Location of the GridCell
      type(Result) :: r                             !! Result object
      ! Create the new SubRiver
      r = me%create(x, y, s)
    end function

    function createSubRiver1(Me, Gx, Gy, SRr) result(r)              ! create the SubRiver object by reading data in from file
      class(SubRiver1) :: Me                                         ! the SubRiver instance
      type(integer), intent(in) :: Gx                                ! the row number of the enclosing GridCell
      type(integer), intent(in) :: Gy                                ! the column number of the enclosing GridCell
      ! Not needed as number of SPM size classes set in Globals:
      ! type(integer), intent(in) :: SC                              ! the number of SPM size classes
      type(integer), intent(in) :: SRr                               ! reference SubRiver number
      type(Result) :: r                                              ! the result object
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
      type(NcGroup) :: subRiverGrp                                   ! NetCDF group specifically for this SubRiver
      type(integer) :: x                                             ! loop counter
      type(character(len=100)) :: sr1                                ! string to dynamically compile and hold group names (must be specific length)
      type(character(len=100)) :: sr2                                ! string to dynamically compile and hold group names
      ! Moved to spcSubRiver type declaration to fit in with me%reachTypes used below:
      ! type(integer), allocatable :: ReachTypes(:)                  ! integer array of Reach type identifiers
      type(RiverReach1), allocatable :: r1                           ! private RiverReach1 type, used for dynamic assignment
      type(ErrorInstance), allocatable :: errors(:)
      character(len=5) :: charMaxRiverReaches                        ! character string to store max number of RiverReaches allowed in
      ! Function purpose
      ! -------------------------------------------------------------
      ! parameterise a SubRiver object in a specified grid cell,
      ! including the creation of RiverReach objects
      !
      ! Function inputs
      ! -------------------------------------------------------------
      ! Gx  : x reference to the enclosing grid cell
      ! Gy  : y reference to the enclosing grid cell
      ! SC  : number of suspended matter size classess
      ! SRr :
      !
      ! Function outputs/outcomes
      ! -------------------------------------------------------------
      ! Fully specified SubRiver object, comprising:
      ! Collection of RiverReach objects: Me%colReaches.
      ! Set of up to three inflow references inflowRefs(:),
      ! comprising Grid x and y references and  SubRiver number
      ! reference, or null if SubRiver is a headwater.
      ! An outflow reference, comprising Grid x and y references
      ! and SubRiver number reference.
                                                                     ! NO NEED TO AUDIT SC - WILL ALREADY HAVE BEEN done
      nc = NcDataset(C%inputFile, "r")                               ! Open dataset as read-only
      sr1 = trim(str(Gx)) // "_"
      sr1 = trim(sr1) // trim(str(Gy))
      sr1 = "SubRiver_" // trim(sr1)
      sr2 = "_" // trim(str(SRr))
      sr1 = trim(sr1) // trim(sr2)                                   ! dynamically create reference group name for subriver
                                                                     ! Format is SubRiver_Gx_Gy_SRr
      me%ref = sr1                                                   ! Store this in instance variable (useful for printing errors later)
      grp = nc%getGroup("Environment")
      grp = grp%getGroup("GridCell_" // trim(str(Gx)) // "_" // trim(str(Gy)))
      subRiverGrp = grp%getGroup(me%ref)                             ! point to the SubRiver group
      var = subRiverGrp%getVariable("nInflows")                      ! point to the variable nInflow: the number of inflows
      call var%getData(Me%nInflows)                                  ! pull data into variable: number of inflows
                                                                     ! AUDITING CODE HERE - RETURN ERROR IF nInflows IS NOT =1, 2 OR 3
                                                                     ! TODO: Also check nInflows equals number of inflow_x groups (or just get rid of nInflows)
      allocate(Me%inflowRefs(Me%nInflows), stat=Me%allst)            ! allocate required space to hold the inflow references for this SubRiver
      allocate(me%inflows(me%nInflows), stat=me%allst)               ! likewise for the array of inflow pointers
      var = subRiverGrp%getVariable("reachTypes")                    ! point to the array ReachTypes: the type identifiers for the RiverReach objects in this SubRiver
      call var%getData(Me%reachTypes)                                ! pull the Reach type references into SubRiver object
      ! CORRECT? getData will pull a multidimensional variable into an array and dynamically allocate the array size?
      ! if this is the case, how are the array elements numbered? I would like a consistent approach to element numbering,
      ! always starting with 1.
      Me%nReaches = size(Me%reachTypes)                              ! get the number of reaches from the ReachType array size
      call r%addError( &                                             ! Check nReaches is >0 but <maxRiverReaches (specified in config file(?))
        ERROR_HANDLER%limit( &                                       ! TODO: Get number of SubRivers from outflow GridCell to check SubRiver number isn't greater
          value = Me%nReaches, &
          lbound = 0, &
          ubound = C%maxRiverReaches, &
          message = "Number of RiverReaches must be positive but less than " &
            // adjustl(trim(str(C%maxRiverReaches))) &
        ) &
      )

      if (me%nInflows > 0) then
        do x = 1, me%nInflows                                          ! loop to read the Inflow references for this SubRiver
          ! read(x,*) sr1                                                ! read loop counter into character variable 'sr1'
                                                                       ! CORRECT SYNTAX?
          sr1 = "inflow_" // trim(str(x))                              ! create character variable 'Inflow1', 'Inflow2' etc.
          grp = subRiverGrp%getGroup(sr1)                              ! point to the Inflow1, Inflow2 etc. group
          var = grp%getVariable("gridX")                               ! point to the variable defining the row of the grid cell
          call var%getData(Me%inflowRefs(x)%GridX)                     ! pull GridX reference into SubRiver object
                                                                       ! AUDIT GridX here - must be >0 and <= the highest grid cell number
          call r%addError( &                                           ! Check GridX is >0. Need to think about where highest grid cell number is specified.
            ERROR_HANDLER%positive( &
              value = Me%inflowRefs(x)%GridX, &
              message = "Inflow grid cell row number x must be positive." &
            ) &
          )
          var = grp%getVariable("gridY")                               ! point to the variable defining the column of the grid cell
          call var%getData(Me%inflowRefs(x)%GridY)                     ! pull GridY reference into SubRiver object
                                                                       ! AUDIT GridY here - must be >0 and <= the highest grid cell number
          call r%addError( &                                           ! Check GridY is >0. Need to think about where highest grid cell number is specified.
            ERROR_HANDLER%positive( &
              value = Me%inflowRefs(x)%GridY, &
              message = "Inflow grid cell column number y must be positive." &
            ) &
          )
          var = grp%getVariable("subRiver")                            ! point to the variable defining the SubRiver acting as an input
          call var%getData(Me%inflowRefs(x)%SubRiver)                  ! pull SubRiver reference into SubRiver object
                                                                       ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers
          call r%addError( &                                           ! Check SubRiver number is >0. If it's a headwater, then won't nInflows = 0 (and thus we'll never enter this loop)?
            ERROR_HANDLER%positive( &                                  ! TODO: Get number of SubRivers from inflow GridCell to check SubRiver number isn't greater
              value = Me%inflowRefs(x)%SubRiver, &
              message = "Inflow SubRiver number must be positive." &
            ) &
          )
          call r%addToTrace("Processing " // sr1)                       ! Add this inflow to the error trace
        ! I've assumed here that this is the only way to read in single elements of a user-defined type, i.e. by listing each as a separate variable.
        ! But can a single user-defined type (i.e. GridX, GridY and SubRiver) be listed as a single object in the .json file and
        ! read in as a single variable? Then "nInflows" could be listed in the .json file as a dimension, in the way that "ReachTypes" is, and the number of inflows
        ! inferred from the size of the inflowRefs(:) array after the inflow references have been read in.
        end do
      end if
                                                                     ! AUDIT size(ReachTypes)=nReaches here
                                                                     ! ^ nReaches is set as size(ReachTypes) now - is there any reason not to do this?
      allocate(Me%colReaches(1:Me%nReaches), stat=Me%allst)          ! Set colReaches to be of size nReaches
      do x = 1, Me%nReaches                                          ! loop through each RiverReach in each SubRiver to create the reaches
        select case (Me%reachTypes(x))                               ! look at the type identifier for the yth RiverReach
          case (1)
            allocate(r1, stat=Me%allst)                              ! RiverReach1 type - create the object
            r = r1%create()                                          ! call the RiverReach1 constructor
            call move_alloc(r1, Me%colReaches(x)%item)               ! move the RiverReach1 object to the yth element of the colReaches collection
          case default
              ! not a valid RiverReach type - must cause an error
        end select
      end do

      call r%addToTrace("Creating " // me%ref)
    end function
    function destroySubRiver1(Me) result(r)
      class(SubRiver1) :: Me                                         ! the SubRiver instance
      type(Result) :: r                                              ! the result object
      type(integer) :: x                                             ! loop counter
      do x = 1, Me%nReaches                                          ! loop through each RiverReach
        r = Me%colReaches(x)%item%destroy()                          ! call destroy routine in the SubRiver object
      end do

    ! Something here to compile all returned Result objects into one?

    end function
    function routingSubRiver1(Me) result(r)                          ! routes inflow(s) through the specified SubRiver
      class(SubRiver1) :: Me                                         ! the SubRiver instance
      type(Result) :: r                                              ! the result object
      type(real(dp)), allocatable :: Qin(:)                          ! the inflow (m3)
      ! Placed SPMin and nInflows in spcSubRiver type declaration to fit in with me%SPMin, me%nInflow calls below
      ! type(real(dp)), allocatable :: SPMin(:)                        ! array of SPM masses, one per size class, in inflow (kg)
      ! type(integer) :: nInflows                                      ! the number of inflows
      type(integer) :: Gx                                            ! the row number of a GridCell
      type(integer) :: Gy                                            ! the column number of a GridCell
      type(integer) :: SR                                            ! SubRiver number reference
      type(integer) :: ndisp                                         ! displacement counter
      type(real(dp)) :: dQ                                           ! inflow volume (m3) per displacement
      type(real(dp)) :: rQ                                           ! reach capacity (m3) per timestep
      type(real(dp)), allocatable :: dSPM(:)                         ! inflow SPM per size class (kg) per displacement
      type(integer) :: x, y, z                                       ! loop counters
      ! type(Environment1) :: enviro                                    ! temporary dummy to get compiling whilst we think about where to get Q and SPM from other cells from
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
      ! Environment%GridCell(Gx, Gy)%SubRiver(z) for a SubRiver
      ! outside the GridCell, and
      ! GridCell(Gx, Gy)%Subriver(z) for a Subriver in this GridCell.
      ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
      !
      ! Function outputs/outcomes
      ! -------------------------------------------------------------
      ! Qout : outflow discharge (m3)
      ! Sout(:) : outflow SPM fluxes (kg)
      ! These variables are stored at object level for interrogation
      ! by the downstream SubRiver
      me%nInflows = size(me%inflowRefs)                              ! get the number of inflows to be processed
      allocate(Qin(1:me%nReaches + 1), stat=Me%allst)                ! initialise Qin - extra element holds final discharge
      allocate(Me%SPMin(1:me%nReaches + 1, 1:C%nSizeClassesSPM), stat=Me%allst)    ! initialise SPMin - extra element holds final discharge
      do x = 1, Me%nInflows                                          ! loop through the inflows to retrieve and sum discharges
        Gx = Me%inflowRefs(x)%GridX                                  ! x reference of GridCell supplying inflow
        Gy = Me%inflowRefs(x)%GridY                                  ! y reference of GridCell supplying inflow
        SR = Me%inflowRefs(x)%SubRiver                               ! SubRiver of GridCell supplying inflow
        ! ENVIRONMENT
        ! Qin(1) = Qin(1) + .dp. enviro%getQ(Gx, Gy, SR)               ! pull in discharge from upstream SubRiver
        ! GetQ method of Environment object retrieves the outflow Q from subriver SR of grid cell (Gx, Gy)
        ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
        ! ARE THERE OTHER WAYS OF DOING THIS, e.g. PASSING REFERENCES TO THE SUBRIVER OBJECTS
        ! THAT PROVIDE THE INFLOW FROM THE CALLING ROUTINE (WHICH WILL BE IN THE GRIDCELL OBJECT)
        ! INTO THIS METHOD?
        do y = 1, C%nSizeClassesSPM                                   ! loop through all SPM size classes
          ! ENVIRONMENT
          ! Me%spmIn(1, y) = Me%spmIn(1, y) + &                         ! only for the first RiverReach,
          !       .dp. enviro%getSpm(Gx, Gy, SR, y)                     ! pull in SPM fluxes from upstream SubRiver
                ! GetSPM method of Environment object retrieves the SPM flux in size class y from subriver SR of grid cell (Gx, Gy)
                ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
                ! ARE THERE OTHER WAYS OF DOING THIS, e.g. PASSING REFERENCES TO THE SUBRIVER OBJECTS
                ! THAT PROVIDE THE INFLOW FROM THE CALLING ROUTINE (WHICH WILL BE IN THE GRIDCELL OBJECT)
                ! INTO THIS METHOD?
        end do
      end do                                                         ! loop to sum all discharges and SPM fluxes
      do x = 1, Me%nReaches                                          ! main routing loop
        ! Renamed to "initDimension" so we can keep "get" functions only as
        ! ones that return state/class variables. E.g., once initDimensions() has
        ! been called, you can `getVolume()` to get the width that initDimensions()
        ! has calculated.
        r = Me%colReaches(x)%item%initDimensions(Qin(x))            ! call function in RiverReach to set up dimensions of
                                                                     ! the reach for this timestep
        rQ = Me%colReaches(x)%item%getVolume()                       ! get volumetric capacity of the reach (m3) for this timestep
        ! spcRiverReach NEEDS TO EXPOSE A VOLUME PROPERTY. THIS PROPERTY WILL ALSO NEED TO BE DEFINED FOR THE RiverReach OBJECT'S
        ! INTERNAL ROUTING COMPUTATIONS. IT NEEDS TO BE CALCULATED FOR EACH TIMESTEP.
        ndisp = 0                                                    ! count number of displacements required
        do while (dQ > Qin(x))                                       ! loop until the inflow volume is less than the number of displacements
          dQ = rQ / (ndisp + 1)                                      ! compute input volume as a function of the no. of displacements
          ndisp = ndisp + 1                                          ! increment the number of displacements and repeat
        end do
        Qin(x + 1) = 0                                               ! initialise discharge summation for next RiverReach
        do y = 1, C%nSizeClassesSPM                                  ! compute inflow SPM fluxes for this displacement
          dSPM(y) = me%spmIn(x, y) / ndisp                           ! SPM flux (kg) of size class 'y' for this displacement
          me%spmIn(x + 1, y) = 0                                     ! initialise SPM flux summation for next RiverReach
        end do
        do y = 1, ndisp                                              ! route water and SPM on each displacement
          r = Me%colReaches(x)%item%simulate(dQ, dSPM)               ! main simulation call for the RiverReach
          Qin(x + 1) = Qin(x + 1) + dQ                               ! sum the outflow discharge on each displacement
          do z = 1, C%nSizeClassesSPM
            me%SPMin(x + 1, z) = me%SPMin(x + 1, z) + dSPM(z)         ! sum the outflow SPM fluxes on each displacement
          end do
          ! FUNCTION HERE TO SEND dQ, dSPM(:) to the RiverReach and return volumes and SPM classes (in the same variables) to be put into
          ! Qin(x+1) and SPMin(x+1, ) by summing across each loop iteration
          ! then on the next iteration of the main loop (x), Qin(x+1) becomes Qin(x), the input volume to the next RiverReach
          ! similarly for each SPM size class.
          ! On the final iteration, we exit the loop with the final outflow and SPM fluxes in Qin(nReaches + 1) and SPMin(nReaches + 1, [1,2,...])
        end do
      end do
      Me%Qout = Qin(Me%nReaches + 1)                                 ! store the final outflow volume (m3)
      do y = 1, C%nSizeClassesSPM                                    ! compute inflow SPM fluxes for this displacement
        Me%SPMout(y) = me%spmIn(Me%nReaches + 1, y)                  ! output SPM flux (kg) of size class 'y' for this displacement
      end do
    end function
    ! ******************************************************
    function auditrefs(Me) result(r)
      class(SubRiver1) :: Me                                           ! the SubRiver instance
      type(Result) :: r                                              ! the result object
      ! the purpose of this function is to sense check the inflow and outflow references, i.e. do they form
      ! robust, consistent links to adjacent grid cells?
      ! but perhaps this could be a function within the Environment module that audits all links at the same time
      ! on startup? i.e. call Audit(<GridCell>) for all GridCells?
    end function
end module

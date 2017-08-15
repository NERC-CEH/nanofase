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
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  implicit none                                                      ! force declaration of all variables
  type, extends(SubRiver) public :: SubRiver1                        ! type declaration for subclass

    contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public :: Create => createSubRiver1                   ! create the SubRiver1 object. Exposed name: create
    procedure, public :: Destroy => destroySubRiver1                 ! remove the SubRiver1 object and all contained objects. Exposed name: destroy
    procedure, public :: Routing => routingSubRiver1                 ! route water and suspended solids through the SubRiver. Exposed name: routing
                                                                     ! Description
                                                                     ! -----------
    procedure, private :: auditrefs                                  ! internal property function: sense check the inflow and outflow GridCell references

    ! THIS PROCEDURE IS NOT DEFINED IN THE ABSTRACT CLASS AS IT IS PRIVATE. CAN IT STILL BE INHERITED?

  end type
contains
    function createSubRiver1(Me, Gx, Gy, SC, SRr) result(r)          ! create the SubRiver object by reading data in from file
      class(SubRiver) :: Me                                          ! the SubRiver instance
      type(integer), intent(in) :: Gx                                ! the row number of the enclosing GridCell
      type(integer), intent(in) :: Gy                                ! the column number of the enclosing GridCell
      type(integer), intent(in) :: SC                                ! the number of SPM size classes
      type(integer), intent(in) :: SRr                               ! reference SubRiver number
      type(Result) :: r                                              ! the result object
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
      type(integer) :: x                                             ! loop counter
      type(integer) :: y                                             ! loop counter
      type(character(len=*)) :: sr1                                  ! string to dynamically compile and hold group names
      type(character(len=*)) :: sr2                                  ! string to dynamically compile and hold group names
      type(integer) :: nInflows                                      ! number of inflows for each SubRiver
      type(integer), allocatable :: ReachTypes(:)                    ! integer array of Reach type identifiers
      type(RiverReach1), allocatable :: r1                           ! private RiverReach1 type, used for dynamic assignment
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
      ! Set of up to three inflow references inflow_ref(:),
      ! comprising Grid x and y references and  SubRiver number
      ! reference, or null if SubRiver is a headwater.
      ! An outflow reference, comprising Grid x and y references
      ! and SubRiver number reference.
      Me%nSPMSC = SC                                                 ! copy in the number of SPM size classes
                                                                     ! NO NEED TO AUDIT SC - WILL ALREADY HAVE BEEN done
      nc = NcDataset(C%InputFile, "r")                               ! Open dataset as read-only
      read(Gx,*) sr1
      read(Gy,*) sr2
      ! CORRECT SYNTAX?
      sr1 = sr1 // "_"
      sr1 = sr1 // sr2
      sr1 = "SubRiver_" // sr1
      read(SRr,*) sr2
      sr2 = "_" // sr2
      sr1 = sr1 // sr2                                               ! dynamically create reference group name for subriver
                                                                     ! Format is SubRiver_Gx_Gy_SRr
      grp = nc%getGroup(sr1)                                         ! point to the SubRiver group
      var = grp%getVariable("nInflows")                              ! point to the variable nInflow: the number of inflows
      call var%getData(Me%nInflows)                                  ! pull data into variable: number of inflows
                                                                     ! IS THIS CORRECT SYNTAX, OR SHOULD IT BE var%getData(nInflows)
                                                                     ! AUDITING CODE HERE - RETURN ERROR IF nInflows IS NOT =1, 2 OR 3
      allocate(Me%inflow_ref(1:Me%nInflows), stat=Me%allst)          ! allocate required space to hold the inflow references for this SubRiver
      var = grp%getVariable("ReachTypes")                            ! point to the array ReachTypes: the type identifiers for the RiverReach objects in this SubRiver
      call var%getData(Me%ReachTypes)                                ! pull the Reach type references into SubRiver object
      ! CORRECT? getData will pull a multidimensional variable into an array and dynamically allocate the array size?
      ! if this is the case, how are the array elements numbered? I would like a consistent approach to element numbering,
      ! always starting with 1.
      Me%nReaches = size(Me%ReachTypes)                              ! get the number of reaches from the ReachType array size
      do x = 1, nInflows                                             ! loop to read the Inflow references for this SubRiver
        read(x,*) sr1                                                ! read loop counter into character variable 'sr1'
                                                                     ! CORRECT SYNTAX?
        sr1 = "Inflow" // sr1                                        ! create character variable 'Inflow1', 'Inflow2' etc.
        grp = nc%getGroup(sr1)                                       ! point to the Inflow1, Inflow2 etc. group
        var = grp%getVariable("GridX")                               ! point to the variable defining the row of the grid cell
        call var%getData(Me%inflow_ref(x)%GridX)                     ! pull GridX reference into SubRiver object
                                                                     ! AUDIT GridX here - must be >0 and <= the highest grid cell number
        var = grp%getVariable("GridY")                               ! point to the variable defining the column of the grid cell
        call var%getData(Me%inflow_ref(x)%GridY)                     ! pull GridY reference into SubRiver object
                                                                     ! AUDIT GridY here - must be >0 and <= the highest grid cell number
        var = grp%getVariable("SubRiver")                            ! point to the variable defining the SubRiver acting as an input
        call var%getData(Me%inflow_ref(x)%SubRiver)                  ! pull SubRiver reference into SubRiver object
                                                                     ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers
      ! I've assumed here that this is the only way to read in single elements of a user-defined type, i.e. by listing each as a separate variable.
      ! But can a single user-defined type (i.e. GridX, GridY and SubRiver) be listed as a single object in the .json file and
      ! read in as a single variable? Then "nInflows" could be listed in the .json file as a dimension, in the way that "ReachTypes" is, and the number of inflows
      ! inferred from the size of the inflow_ref(:) array after the inflow references have been read in.
      end do
      grp = nc%getGroup("Outflow")                                   ! point to the Outflow group
      var = grp%getVariable("GridX")                                 ! point to the variable defining the row of the grid cell
      call var%getData(Me%outflow_ref%GridX)                         ! pull GridX reference into SubRiver object
                                                                     ! AUDIT GridX here - must be >0 and <= the highest grid cell number
      var = grp%getVariable("GridY")                                 ! point to the variable defining the column of the grid cell
      call var%getData(Me%outflow_ref%GridY)                         ! pull GridY reference into SubRiver object
                                                                     ! AUDIT GridY here - must be <= the highest grid cell number
      var = grp%getVariable("SubRiver")                              ! point to the variable defining the SubRiver acting as an input
      call var%getData(Me%outflow_ref%SubRiver)                      ! pull SubRiver reference into SubRiver object
                                                                     ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers
      var = grp%getVariable("nReaches")                              ! point to the variable nReaches: the number of reaches in this SubRiver
      call var%getData(Me%nReaches)                                  ! pull nReaches reference into SubRiver object
                                                                     ! AUDIT nReaches here: must be > 0, possibly warn if greater than a stipulated maximum
                                                                     ! AUDIT size(ReachTypes)=nReaches here
      allocate(Me%colReaches(1:Me%nReaches), stat=Me%allst)          ! Set colReaches to be of size nReaches
      do x = 1, Me(x)%nReaches                                       ! loop through each RiverReach in each SubRiver to create the reaches
        select case Me%ReachTypes(x)                                 ! look at the type identifier for the yth RiverReach
          case (1)
            allocate(r1, stat=Me%allst)                              ! RiverReach1 type - create the object
            call r1%create()                                         ! call the RiverReach1 constructor
            call move_alloc(r1, Me%colReaches(x)%item)               ! move the RiverReach1 object to the yth element of the colReaches collection
          case default
              ! not a valid RiverReach type - must cause an error
        end select
      end do
    end do
    end function
    function destroySubRiver1(Me) result(r)
      class(SubRiver) :: Me                                          ! the SubRiver instance
      type(Result) :: r                                              ! the result object
      type(integer) :: x                                             ! loop counter
      do x = 1, Me%nReaches                                          ! loop through each RiverReach
        r = Me%colReaches(x)%item%destroy                            ! call destroy routine in the SubRiver object
      end do

    ! Something here to compile all returned Result objects into one?

    end function
    function routingSubRiver1(Me) result(r)                          ! routes inflow(s) through the specified SubRiver
      class(River) :: Me                                             ! the SubRiver instance
      type(Result) :: r                                              ! the result object
      type(real(dp)), allocatable :: Qin(:)                          ! the inflow (m3)
      type(real(dp)), allocatable :: SPMin(:)                        ! array of SPM masses, one per size class, in inflow (kg)
      type(integer) :: nInflows                                      ! the number of inflows
      type(integer) :: Gx                                            ! the row number of a GridCell
      type(integer) :: Gy                                            ! the column number of a GridCell
      type(integer) :: SR                                            ! SubRiver number reference
      type(integer) :: ndisp                                         ! displacement counter
      type(real(dp)) :: dQ                                           ! inflow volume (m3) per displacement
      type(real(dp)) :: rQ                                           ! reach capacity (m3) per timestep
      type(real(dp), allocatable) :: dSPM(:)                         ! inflow SPM per size class (kg) per displacement
      type(integer) :: x                                             ! loop counter
      type(integer) :: y                                             ! loop counter
      ! Function purpose
      ! -------------------------------------------------------------
      ! route water and suspended material from the upstream
      ! SubRiver(s), and from overland flow and erosion, through
      ! this SubRiver
      !
      ! Function inputs
      ! -------------------------------------------------------------
      ! function uses inflow_ref(:) to interrogate other SubRivers
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
      nInflows = size(inflow_ref)                                    ! get the number of inflows to be processed
      allocate(Qin(1:nReaches + 1), stat=Me%allst)                   ! initialise Qin - extra element holds final discharge
      allocate(SPMin(1:nReaches + 1, 1:nSPMSC), stat=Me%allst)       ! initialise SPMin - extra element holds final discharge
      do x = 1, Me%nInflows                                          ! loop through the inflows to retrieve and sum discharges
        Gx = Me%inflow_ref(x)%GridX                                  ! x reference of GridCell supplying inflow
        Gy = Me%inflow_ref(x)%GridY                                  ! y reference of GridCell supplying inflow
        SR = Me%inflow_ref(x)%SubRiver                               ! SubRiver of GridCell supplying inflow
        Qin(1) = Qin(1) + Environment%GetQ(Gx, Gy, SR)               ! pull in discharge from upstream SubRiver
        ! GetQ method of Environment object retrieves the outflow Q from subriver SR of grid cell (Gx, Gy)
        ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
        ! ARE THERE OTHER WAYS OF DOING THIS, e.g. PASSING REFERENCES TO THE SUBRIVER OBJECTS
        ! THAT PROVIDE THE INFLOW FROM THE CALLING ROUTINE (WHICH WILL BE IN THE GRIDCELL OBJECT)
        ! INTO THIS METHOD?
        do y = 1, Me%nSPMSC                                          ! loop through all SPM size classes
          Me%SPMin(1, y) = Me%SPMin(1, y) + &
                Environment%GetSPM(Gx, Gy, SR, y)                    ! pull in SPM fluxes from upstream SubRiver
                ! GetSPM method of Environment object retrieves the SPM flux in size class y from subriver SR of grid cell (Gx, Gy)
                ! HOW DO WE ENSURE THAT WE CAN REFERENCE THE ENVIRONMENT OBJECT FROM HERE???
                ! ARE THERE OTHER WAYS OF DOING THIS, e.g. PASSING REFERENCES TO THE SUBRIVER OBJECTS
                ! THAT PROVIDE THE INFLOW FROM THE CALLING ROUTINE (WHICH WILL BE IN THE GRIDCELL OBJECT)
                ! INTO THIS METHOD?
        end do
      end do                                                         ! loop to sum all discharges and SPM fluxes
      do x = 1, Me%nReaches                                          ! main routing loop
        call Me%colReaches(x)%GetDimensions(Qin(x))                  ! call function in RiverReach to set up dimensions of
                                                                     ! the reach for this timestep
        rQ = Me%colReaches(x)%Vol                                    ! get volumetric capacity of the reach (m3) for this timestep
        ! spcRiverReach NEEDS TO EXPOSE A VOLUME PROPERTY. THIS PROPERTY WILL ALSO NEED TO BE DEFINED FOR THE RiverReach OBJECT'S
        ! INTERNAL ROUTING COMPUTATIONS. IT NEEDS TO BE CALCULATED FOR EACH TIMESTEP.
        ndisp = 0                                                    ! count number of displacements required
        do while dQ > Qin(x)                                         ! loop until the inflow volume is less than the number of displacements
          dQ = rQ / (ndisp + 1)                                      ! compute input volume as a function of the no. of displacements
          ndisp = ndisp + 1                                          ! increment the number of displacements and repeat
        end do
        Qin(x + 1) = 0                                               ! initialise discharge summation for next RiverReach
        do y = 1, Me%nSPMSC                                          ! compute inflow SPM fluxes for this displacement
          dSPM(y) = SPMin(x, y) / ndisp                              ! SPM flux (kg) of size class 'y' for this displacement
          SPMin(x + 1, y) = 0                                        ! initialise SPM flux summation for next RiverReach
        end do
        do y = 1, ndisp                                              ! route water and SPM on each displacement
          call Me%colReaches(x)Simulate(dQ, dSPM)                    ! main simulation call for the RiverReach
          Qin(x + 1) = Qin(x + 1) + dQ                               ! sum the outflow discharge on each displacement
          do z = 1, Me%nSPMSC
            SPMin(x + 1, z) = SPMin(x + 1, z) + dSPM(z)              ! sum the outflow SPM fluxes on each displacement
          end do
          ! FUNCTION HERE TO SEND dQ, dSPM(:) to the RiverReach and return volumes and SPM classes (in the same variables) to be put into
          ! Qin(x+1) and SPMin(x+1, ) by summing across each loop iteration
          ! then on the next iteration of the main loop (x), Qin(x+1) becomes Qin(x), the input volume to the next RiverReach
          ! similarly for each SPM size class.
          ! On the final iteration, we exit the loop with the final outflow and SPM fluxes in Qin(nReaches + 1) and SPMin(nReaches + 1, [1,2,...])
        end do
      end do
      Me%Qout = Qin(Me%nReaches + 1)                                 ! store the final outflow volume (m3)
      do y = 1, Me%nSPMSC                                            ! compute inflow SPM fluxes for this displacement
        Me%SPMout(y) = SPMin(Me%nReaches + 1, y)                     ! output SPM flux (kg) of size class 'y' for this displacement
      end do
    end function
    ! ******************************************************
    function auditrefs(Me) result(r)
      class(River) :: Me                                             ! the River instance
      type(Result) :: r                                              ! the result object
      ! the purpose of this function is to sense check the inflow and outflow references, i.e. do they form
      ! robust, consistent links to adjacent grid cells?
      ! but perhaps this could be a function within the Environment module that audits all links at the same time
      ! on startup? i.e. call Audit(<GridCell>) for all GridCells?
    end function
end module

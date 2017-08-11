module classRiver1
                                                                     ! River1 subclasses
                                                                     ! implements spcRiver superclass
                                                                     ! of a River class
                                                                     ! a River class acts as a container for a collection of RiverReach objects which collectively define the
                                                                     ! flowing waters within each grid cell
                                                                     ! RiverReach objects are grouped into SubRiver UDTs
                                                                     ! the RiverReach class routes, water, suspended sediments (and ultimately nanoparticles) through the flowing waters within
                                                                     ! the grid cell
                                                                     ! IMPORTED MODULES
                                                                     ! Description
                                                                     ! -----------
  use Globals                                                        ! global declarations
  use netcdf                                                         ! input/output handling
  use mo_netcdf                                                      ! input/output handling
  use ResultModule                                                   ! error handling classes, required for
  use ErrorInstanceModule                                            ! generation of trace error messages
  implicit none                                                      ! force declaration of all variables
  type, extends(sprRiver) public :: River1                           ! type declaration for subclass
    contains
                                                                     ! METHODS
                                                                     ! Description
                                                                     ! -----------
    procedure, public :: create => createRiver1                      ! create the River object1. Exposed name: create
    procedure, public :: destroy => destroyRiver1                    ! remove the River1 object and all contained objects. Exposed name: destroy
                                                                     ! PRIVATE ROUTINES
                                                                     ! Description
                                                                     ! -----------
    procedure, private :: auditrefs                                  ! internal property function: sense check the inflow and outflow GridCell references

    ! THIS PROCEDURE IS NOT DEFINED IN THE ABSTRACT CLASS AS IT IS PRIVATE. CAN IT STILL BE INHERITED?

  end type
contains
    function createRiver1(Me, R, C) result(r)                        ! create the River object by reading data in from file
      class(River) :: Me                                             ! the River instance
      type(integer), intent(in) :: R                                 ! the row number of the enclosing GridCell
      type(integer), intent(in) :: C                                 ! the column number of the enclosing GridCell
      type(NcDataset) :: NC                                          ! NetCDF dataset
      type(NcVariable) :: var                                        ! NetCDF variable
      type(NcGroup) :: grp                                           ! NetCDF group
      type(Result) :: r                                              ! the result object
      type(integer) :: x                                             ! loop counter
      type(integer) :: y                                             ! loop counter
      type(character(len=*)) :: srs                                  ! string to dynamically hold group names for SubRiver data in input file
      type(integer) :: nInflows                                      ! number of inflows for each SubRiver

      ! AUDIT R here: must be >0, <= maximum grid size

      Me%GridRow = R

      ! AUDIT C here: must be >0, <= maximum grid size

      Me%GridCol = C

      nc = NcDataset(C%InputFile, "r")                               ! Open dataset as read-only
      grp = nc%getGroup("River")                                     ! point to the River group
      var = grp%getVariable("nSubRivers")                            ! point to the variable nSubRivers
      call var%getData(Me%nSubRivers)                                ! pull variable into variable: number of SubRivers

      ! AUDITING CODE HERE - RETURN ERROR IF nSubRivers IS NOT =1, 2 OR 3

      allocate(Me%objSubRiver(1:nSubRivers))                         ! allocate space for the SubRiver objects
      do x=1, nSubRivers                                             ! loop through each SubRiver to pull in data
        read(x,*) srs                                                ! read loop counter into character variable 'srs'

        ! CORRECT SYNTAX?

        srs = "SubRiver" // srs                                      ! create character var 'SubRiver1', 'SubRiver2' etc.
        grp = nc%getGroup(srs)                                       ! point to each SubRiver group in turn
        var = grp%getVariable("nInflows")                            ! point to the variable nInflow: the number of inflows
        call var%getData(Me%nInflows)                                ! pull data into variable: number of inflows

        ! IS THIS CORRECT SYNTAX, OR SHOULD IT BE var%getData(nInflows)
        ! AUDITING CODE HERE - RETURN ERROR IF nInflows IS NOT =1, 2 OR 3

        allocate(Me%objSubRiver(x)%inflow_ref(1:Me%nInflows))        ! allocate required space to hold the inflow references for this SubRiver
        do y=1, nInflows                                             ! loop to read the Inflow references for this SubRiver
          read(y,*) srs                                              ! read loop counter into character variable 'srs'

          ! CORRECT SYNTAX?

          srs = "Inflow" // srs                                      ! create character variable 'Inflow1', 'Inflow2' etc.
          grp = nc%getGroup(srs)                                     ! point to the Inflow1, Inflow 2 etc. group
          var = grp%getVariable("GridRow")                           ! point to the variable defining the row of the grid cell
          call var%getData(Me%objSubRiver(x)%inflow_ref(y)%GridRow)  ! pull GridRow reference into SubRiver object

          ! AUDIT GridRow here - must be either null (-999, indicating reference is to this GridCell), or >0 and <= the highest grid cell number

          var = grp%getVariable("GridCol")                           ! point to the variable defining the column of the grid cell
          call var%getData(Me%objSubRiver(x)%inflow_ref(y)%GridCol)  ! pull GridCol reference into SubRiver object

          ! AUDIT GridCol here - must be either null (-999, indicating reference is to this GridCell), or >0 and <= the highest grid cell number

          var = grp%getVariable("SubRiver")                          ! point to the variable defining the SubRiver acting as an input
          call var%getData(Me%objSubRiver(x)%inflow_ref(y)%SubRiver) ! pull SubRiver reference into SubRiver object

          ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers

        end do
        grp = nc%getGroup("Outflow")                                 ! point to the Outflow group
        var = grp%getVariable("GridRow")                             ! point to the variable defining the row of the grid cell
        call var%getData(Me%objSubRiver(x)%outflow_ref%GridRow)      ! pull GridRow reference into SubRiver object

        ! AUDIT GridRow here - must be >0 and <= the highest grid cell number

        var = grp%getVariable("GridCol")                             ! point to the variable defining the column of the grid cell
        call var%getData(Me%objSubRiver(x)%outflow_ref%GridCol)      ! pull GridCol reference into SubRiver object

        ! AUDIT GridCol here - must be <= the highest grid cell number

        var = grp%getVariable("SubRiver")                            ! point to the variable defining the SubRiver acting as an input
        call var%getData(Me%objSubRiver(x)%outflow_ref%SubRiver)     ! pull SubRiver reference into SubRiver object

        ! AUDIT SubRiver here - must be either null (-999, indicating SubRiver is a headwater), or >0 and <= nSubRivers

        var = grp%getVariable("nReaches")                            ! point to the variable nReaches: the number of reaches in this SubRiver
        call var%getData(Me%objSubRiver(x)%nReaches)                 ! pull nReaches reference into SubRiver object

        ! AUDIT nReaches here: must be > 0, possibly warn if greater than a stipulated maximum

        allocate Me%objSubRiver%Reach(1:nReaches)                    ! allocate the array of RiverReachElements
        do y=1, Me%objSubRiver(x)%nReaches                           ! loop through each RiverReach in each SubRiver to create the reaches
            r = Me%objSubRiver(x)%Reach(y)%Create                    ! create the reach

        ! THIS REQUIRES AMENDING TO ALLOW FOR CREATION OF REACHES OF ANY POSSIBLE TYPE

        end do
      end do
    end function
    function destroyRiver(Me) result(r)
      class(River) :: Me                                             ! the River instance
      type(Result) :: r                                              ! the result object
      type(integer) :: x                                             ! loop counter
      type(integer) :: y                                             ! loop counter
      do x=1, Me%nSubRivers                                          ! loop through each SubRiver
        do y=1, Me%objSubRiver(x)%nReaches                           ! loop through each Reach in the SubRiver
          r = Me%objSubRiver%Reach(y)%item%destroy                   ! call destroy routine in the Reach object
        end do
      end do

    ! Something here to compile all returned Result objects into one?

    end function
    function Routing(Me) result(r)                                   ! routes inflow(s) through the river system
      class(River) :: Me                                             ! the River instance
      type(Result) :: r                                              ! the result object
      ! 1.  For each SubRiver that receives inflow & SPM from upstream, retrieve that inflow & SPM
      !     NOTE must establish what the units of the inflow will be. I would like volume,
      !     and also, for flexibility, timestep.
      ! 2.  Retrieve the surface runoff & erosion. Again, must establish what the units are.
      ! 3.  Subdivide the surface runoff & erosion among the subrivers and their reaches. A method to do this needs to be agreed.
      !     This should probably be done via a private function that returns the surface runoff for a reach, given
      !     a set of relevant parameters.
      ! 4.  For each subriver
      !     a.  For each reach in turn
      !       I.  feed in the inflow and surface runoff & erosion/SPM
      !      II.  call RiverReach to compute sediment dynamics etc., return outflow & SPM
      !     III.  feed outflow & SPM into next reach, unless is lowest reach
      !     b.  Store outflow & SPM from lowest reach to feed into next reach
      !     c.  Repeat a. and b. until all subrivers are done, store outflow & SPM at grid exit point
      !     NOTE for parallelisation of this process, which will be central to parallelisation of the catchment
      !     as a whole, should we actually have a formal SubRiver object (not just a user-defined type)?
      !     My feeling is that this extra structuring would make it clearer where parallelisation needs to be invoked
    end function
    function auditrefs(Me) result(r)
      class(River) :: Me                                             ! the River instance
      type(Result) :: r                                              ! the result object
      ! the purpose of this function is to sense check the inflow and outflow references, i.e. do they form
      ! proper, consistent links to adjacent grid cells?
      ! but perhaps this could be a function within the Environment module that audits all links at the same time
      ! on startup? i.e. call Audit(<GridCell>) for all GridCells?
    end function
end module

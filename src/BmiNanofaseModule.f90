module BmiNanofaseModule
    use bmif_2_0
    use GlobalsModule
    use UtilModule
    use ResultModule
    use RiverReachModule
    use EstuaryReachModule
    use EnvironmentModule, only: Environment
    use DataInputModule, only: DATASET
    use CheckpointModule, only: Checkpoint
    use DataOutputModule
    use LoggerModule, only: LOGR
    use DefaultsModule, only: iouLog
    use datetime_module      
    implicit none

    type, extends(bmi) :: BmiNanofase
        type(Environment)   :: env              !! Object representing the environment
        type(DataOutput)    :: output           !! Data output from the model
        type(Checkpoint)    :: checkpt          !! Object dealing with checkpointing
        integer             :: t = 1            !! Current model timestep index
        double precision    :: start_datenum    !! Model start date as seconds since 0 CE
        double precision    :: end_datenum      !! Model end date as seconds since 0 CE

      contains
        ! Initialize, run, finalize (IRF)
        procedure :: initialize => initialize
        procedure :: update => update
        procedure :: update_until => update_until
        procedure :: finalize => finalize

        ! Exchange items
        procedure :: get_component_name => get_component_name
        procedure :: get_input_item_count => get_input_item_count
        procedure :: get_output_item_count => get_output_item_count
        procedure :: get_input_var_names => get_input_var_names
        procedure :: get_output_var_names => get_output_var_names

        ! Variable information
        procedure :: get_var_grid => get_var_grid
        procedure :: get_var_type => get_var_type
        procedure :: get_var_units => get_var_units
        procedure :: get_var_itemsize => get_var_itemsize
        procedure :: get_var_nbytes => get_var_nbytes
        procedure :: get_var_location => get_var_location

        ! Time information
        procedure :: get_current_time => get_current_time
        procedure :: get_start_time => get_start_time
        procedure :: get_end_time => get_end_time
        procedure :: get_time_units => get_time_units
        procedure :: get_time_step => get_time_step

        ! Getters, by type
        procedure :: get_value_int => get_value_int
        procedure :: get_value_float => get_value_float
        procedure :: get_value_double => get_value_double
        procedure :: get_value_ptr_int => get_value_ptr_int
        procedure :: get_value_ptr_float => get_value_ptr_float
        procedure :: get_value_ptr_double => get_value_ptr_double
        procedure :: get_value_at_indices_int => get_value_at_indices_int
        procedure :: get_value_at_indices_float => get_value_at_indices_float
        procedure :: get_value_at_indices_double => get_value_at_indices_double

        ! Setters, by type
        procedure :: set_value_int => set_value_int
        procedure :: set_value_float => set_value_float
        procedure :: set_value_double => set_value_double
        procedure :: set_value_at_indices_int => set_value_at_indices_int
        procedure :: set_value_at_indices_float => set_value_at_indices_float
        procedure :: set_value_at_indices_double => set_value_at_indices_double

        ! Grid information
        procedure :: get_grid_rank => get_grid_rank
        procedure :: get_grid_size => get_grid_size
        procedure :: get_grid_type => get_grid_type

        ! Uniform rectilinear
        procedure :: get_grid_shape => get_grid_shape
        procedure :: get_grid_spacing => get_grid_spacing
        procedure :: get_grid_origin => get_grid_origin

        ! Non-uniform rectilinear, curvilinear
        procedure :: get_grid_x => get_grid_x
        procedure :: get_grid_y => get_grid_y
        procedure :: get_grid_z => get_grid_z

        ! Unstructured
        procedure :: get_grid_node_count => get_grid_node_count
        procedure :: get_grid_edge_count => get_grid_edge_count
        procedure :: get_grid_face_count => get_grid_face_count
        procedure :: get_grid_edge_nodes => get_grid_edge_nodes
        procedure :: get_grid_face_edges => get_grid_face_edges
        procedure :: get_grid_face_nodes => get_grid_face_nodes
        procedure :: get_grid_nodes_per_face => get_grid_nodes_per_face
    end type BmiNanofase

    character(len=BMI_MAX_COMPONENT_NAME), target :: component_name = "FASE"

  contains

    !> Perform startup tasks for the model, including running the
    !! warmup period (if specified)
    function initialize(this, config_file) result(bmi_status)
        class(BmiNanofase), intent(out) :: this             ! This instance
        character(len=*), intent(in)    :: config_file      ! Path to the model config file
        integer                         :: bmi_status       ! Check if initialisation was successful
        type(Result)                    :: rslt             ! Result object for internal error handling
        integer                         :: t                ! Time iterator

        ! Set up global vars/constants and initialise the logger
        call GLOBALS_INIT(config_file)
        call LOGR%init( &
            logToFile=C%writeToLog, &
            logToConsole=.true., &
            logFilePath=C%logFilePath, &
            fileUnit=iouLog &
        )

        ! Print the welcome
        call printWelcome()

        ! Load the input data
        call DATASET%init(C%inputFile, C%constantsFile)

        ! Create the Environment object and deal with any errors that arise
        rslt = this%env%create()
        call LOGR%toFile(errors=.errors.rslt)
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
    
        ! Initialise the data output module, check if we're running to steady state
        call this%output%init(this%env)
        if (C%runToSteadyState .and. trim(C%steadyStateMode) == 'sediment_size_distribution') then
            call this%output%initSedimentSizeDistribution()
            call LOGR%add("Running in steady state mode " // trim(C%steadyStateMode), COLOR_BLUE)
        end if
        
        ! Set up checkpointing and check if we're meant to be reinstating a checkpoint now
        call this%checkpt%init(this%env, C%checkpointFile)
        if (C%reinstateCheckpoint) then
            call this%checkpt%reinstate(preserve_timestep=C%preserveTimestep)
        end if
        
        ! Check if we've been asked to run a warm up period, which runs the first N timesteps' worth
        ! of data (excluding NM inputs) through the model, where N is specified by C%warmUpPeriod
        if (C%warmUpPeriod > 0) then
            ! Log some info about it
            call LOGR%add("Running for warm up period of " // trim(str(C%warmUpPeriod)) // " time steps", COLOR_BLUE)
            ! Run the model with the warmUp flag
            do t = 1, C%warmUpPeriod
                call this%env%update(t, t, .true.)
            end do
            ! Log that we're finished warming up and on to the real model
            call LOGR%add("Finished warm up period", COLOR_BLUE)
            ! Have we been asked to create a checkpoint after the warm up period?
            if (C%saveCheckpointAfterWarmUp) then
                call this%checkpt%save(0)
            end if
        end if

        ! Turn the model start and end dates into reals
        this%start_datenum = date2num(C%startDate)
        this%end_datenum = date2num(C%endDate)

        ! If we got this far without errors, the init must have been successful
        bmi_status = BMI_SUCCESS
    end function

    !> Advance the model one time step. For the moment, the BMI is only
    !! capable of controlling "standard" model runs that aren't batch runs
    !! and aren't to steady state. The BMI user is responsible for
    !! implementing batch and steady state runs. This might change.
    function update(this) result(bmi_status)
        class(BmiNanofase), intent(inout) :: this   !! This instance
        integer :: bmi_status                       !! BMI return status

        ! Make sure we haven't gone beyond the number of timesteps
        ! available in the model data
        if (this%t > C%nTimeSteps) then
            bmi_status = BMI_FAILURE
            return
        end if
        
        ! Update the environment for this timestep
        call this%env%update(t=this%t, &
                             tInBatch=this%t, &
                             isWarmUp=.false.)
        ! Update the output files and timestep
        call this%output%update(this%t, this%t)
        this%t = this%t + 1

        ! If we got this far, the update must have been successful
        bmi_status = BMI_SUCCESS
    end function

    !> Advance the model until the given time, from the current timestep
    !! (i.e. not necessarily from the start of the model run)
    function update_until(this, time) result(bmi_status)
        class(BmiNanofase), intent(inout)   :: this         !! This instance
        double precision, intent(in)        :: time         !! The time to update until is seconds since 0 CE
        integer                             :: bmi_status   !! BMI return status
        type(datetime)                      :: dtime        ! Datetime representation of `time`
        type(timedelta)                     :: delta        ! Timedelta between `time` and model start time
        integer                             :: i_time       ! Integer timestep representation of `time`
        integer                             :: t            ! Time loop iterator

        ! Convert the time from seconds since 0 CE to model timestep index
        dtime = num2date(time)
        delta = dtime - C%startDate
        i_time = floor(delta%total_seconds()) / C%timeStep + 1

        ! Check the timestep index is available in the model and not
        ! before the current timestep
        if (i_time < 1 .or. i_time > C%nTimeSteps .or. i_time < this%t) then
            bmi_status = BMI_FAILURE
            return
        end if

        ! Loop from the current timestep to the final timestep
        do t = this%t, i_time
            bmi_status = this%update()
        end do

        bmi_status = BMI_SUCCESS
    end function

    ! Perform teardown tasks for the model.
    function finalize(this) result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        integer :: bmi_status

        ! Finalise the output for this chunk, which amounts to writing to the NetCDF
        ! if in 'end' mode (otherwise this subroutine doesn't do anything). Because
        ! this BMI doesn't support batch runs yet, this is the one and only chunk
        call this%output%finaliseChunk(tStart=1, isFinalChunk=.true.)

        ! Have we been asked to create a checkpoint?
        if (C%saveCheckpoint) then
            call this%checkpt%save(this%t - 1)
        end if

        ! Write the simulation summary to file, close output data files and report that it was a successful
        ! model run. Pass the steady state iterator in to give number of iterations until steady state
        call this%output%finalise(0)
        call LOGR%add("Model run completeled successfully", COLOR_GREEN)

        ! If we got this far then it must have been a success
        bmi_status = BMI_SUCCESS
    end function finalize

    !> Get the name of the model
    function get_component_name(this, name) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), pointer, intent(out) :: name
        integer :: bmi_status
        name => component_name
        bmi_status = BMI_SUCCESS
    end function get_component_name

    ! Count a model's input variables.
    function get_input_item_count(this, count) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status
    end function get_input_item_count

    ! Count a model's output variables.
    function get_output_item_count(this, count) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status
    end function get_output_item_count

    ! List a model's input variables.
    function get_input_var_names(this, names) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), pointer, intent(out) :: names(:)
        integer :: bmi_status
    end function get_input_var_names

    ! List a model's output variables.
    function get_output_var_names(this, names) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), pointer, intent(out) :: names(:)
        integer :: bmi_status
    end function get_output_var_names

    ! Get the grid identifier for the given variable.
    function get_var_grid(this, name, grid) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out) :: grid
        integer :: bmi_status
    end function get_var_grid

    ! Get the data type of the given variable as a string.
    function get_var_type(this, name, type) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: type
        integer :: bmi_status
    end function get_var_type

    ! Get the units of the given variable.
    function get_var_units(this, name, units) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: units
        integer :: bmi_status
    end function get_var_units

    ! Get memory use per array element, in bytes.
    function get_var_itemsize(this, name, size) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out) :: size
        integer :: bmi_status
    end function get_var_itemsize

    ! Get size of the given variable, in bytes.
    function get_var_nbytes(this, name, nbytes) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out) :: nbytes
        integer :: bmi_status
    end function get_var_nbytes

    ! Describe where a variable is located: node, edge, or face.
    function get_var_location(this, name, location) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: location
        integer :: bmi_status
    end function get_var_location

    !> Current time of the model
    function get_current_time(this, time) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        double precision, intent(out) :: time
        integer :: bmi_status

        ! Convert the timestep index to the time since epoch

    end function

    ! Start time of the model.
    function get_start_time(this, time) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        double precision, intent(out) :: time
        integer :: bmi_status
    end function get_start_time

    ! End time of the model.
    function get_end_time(this, time) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        double precision, intent(out) :: time
        integer :: bmi_status
    end function get_end_time

    ! Time units of the model.
    function get_time_units(this, units) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(out) :: units
        integer :: bmi_status
    end function get_time_units

    ! Time step of the model.
    function get_time_step(this, time_step) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        double precision, intent(out) :: time_step
        integer :: bmi_status
    end function get_time_step

    ! Get a copy of values (flattened!) of the given integer variable.
    function get_value_int(this, name, dest) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(inout) :: dest(:)
        integer :: bmi_status
    end function get_value_int

    ! Get a copy of values (flattened!) of the given real variable.
    function get_value_float(this, name, dest) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        real, intent(inout) :: dest(:)
        integer :: bmi_status
    end function get_value_float

    ! Get a copy of values (flattened!) of the given double variable.
    function get_value_double(this, name, dest) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, intent(inout) :: dest(:)
        integer :: bmi_status
    end function get_value_double

    ! Get a reference to the given integer variable.
    function get_value_ptr_int(this, name, dest_ptr) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, pointer, intent(inout) :: dest_ptr(:)
        integer :: bmi_status
    end function get_value_ptr_int

    ! Get a reference to the given real variable.
    function get_value_ptr_float(this, name, dest_ptr) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        real, pointer, intent(inout) :: dest_ptr(:)
        integer :: bmi_status
    end function get_value_ptr_float

    ! Get a reference to the given double variable.
    function get_value_ptr_double(this, name, dest_ptr) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, pointer, intent(inout) :: dest_ptr(:)
        integer :: bmi_status
    end function get_value_ptr_double

    ! Get integer values at particular (one-dimensional) indices.
    function get_value_at_indices_int(this, name, dest, inds) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(inout) :: dest(:)
        integer, intent(in) :: inds(:)
        integer :: bmi_status
    end function get_value_at_indices_int

    ! Get real values at particular (one-dimensional) indices.
    function get_value_at_indices_float(this, name, dest, inds) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        real, intent(inout) :: dest(:)
        integer, intent(in) :: inds(:)
        integer :: bmi_status
    end function get_value_at_indices_float

    ! Get double values at particular (one-dimensional) indices.
    function get_value_at_indices_double(this, name, dest, inds) &
        result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        character(len=*), intent(in) :: name
        double precision, intent(inout) :: dest(:)
        integer, intent(in) :: inds(:)
        integer :: bmi_status
    end function get_value_at_indices_double

    ! Set new values for an integer model variable.
    function set_value_int(this, name, src) result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_int

    ! Set new values for a real model variable.
    function set_value_float(this, name, src) result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        real, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_float

    ! Set new values for a double model variable.
    function set_value_double(this, name, src) result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        double precision, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_double

    ! Set integer values at particular (one-dimensional) indices.
    function set_value_at_indices_int(this, name, inds, src) &
        result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: inds(:)
        integer, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_at_indices_int

    ! Set real values at particular (one-dimensional) indices.
    function set_value_at_indices_float(this, name, inds, src) &
        result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: inds(:)
        real, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_at_indices_float

    ! Set double values at particular (one-dimensional) indices.
    function set_value_at_indices_double(this, name, inds, src) &
        result(bmi_status)
        class(BmiNanofase), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: inds(:)
        double precision, intent(in) :: src(:)
        integer :: bmi_status
    end function set_value_at_indices_double

    ! Get number of dimensions of the computational grid.
    function get_grid_rank(this, grid, rank) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, intent(out) :: rank
        integer :: bmi_status
    end function get_grid_rank

    ! Get the total number of elements in the computational grid.
    function get_grid_size(this, grid, size) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, intent(out) :: size
        integer :: bmi_status
    end function get_grid_size

    ! Get the grid type as a string.
    function get_grid_type(this, grid, type) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        character(len=*), intent(out) :: type
        integer :: bmi_status
    end function get_grid_type

    ! Get the dimensions of the computational grid.
    function get_grid_shape(this, grid, shape) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, dimension(:), intent(out) :: shape
        integer :: bmi_status
    end function get_grid_shape

    ! Get distance between nodes of the computational grid.
    function get_grid_spacing(this, grid, spacing) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        double precision, dimension(:), intent(out) :: spacing
        integer :: bmi_status
    end function get_grid_spacing

    ! Get coordinates of the origin of the computational grid.
    function get_grid_origin(this, grid, origin) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        double precision, dimension(:), intent(out) :: origin
        integer :: bmi_status
    end function get_grid_origin

    ! Get the x-coordinates of the nodes of a computational grid.
    function get_grid_x(this, grid, x) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        double precision, dimension(:), intent(out) :: x
        integer :: bmi_status
    end function get_grid_x

    ! Get the y-coordinates of the nodes of a computational grid.
    function get_grid_y(this, grid, y) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        double precision, dimension(:), intent(out) :: y
        integer :: bmi_status
    end function get_grid_y

    ! Get the z-coordinates of the nodes of a computational grid.
    function get_grid_z(this, grid, z) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        double precision, dimension(:), intent(out) :: z
        integer :: bmi_status
    end function get_grid_z

    ! Get the number of nodes in an unstructured grid.
    function get_grid_node_count(this, grid, count) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, intent(out) :: count
        integer :: bmi_status
    end function get_grid_node_count

    ! Get the number of edges in an unstructured grid.
    function get_grid_edge_count(this, grid, count) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, intent(out) :: count
        integer :: bmi_status
    end function get_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    function get_grid_face_count(this, grid, count) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, intent(out) :: count
        integer :: bmi_status
    end function get_grid_face_count

    ! Get the edge-node connectivity.
    function get_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, dimension(:), intent(out) :: edge_nodes
        integer :: bmi_status
    end function get_grid_edge_nodes

    ! Get the face-edge connectivity.
    function get_grid_face_edges(this, grid, face_edges) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, dimension(:), intent(out) :: face_edges
        integer :: bmi_status
    end function get_grid_face_edges

    ! Get the face-node connectivity.
    function get_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, dimension(:), intent(out) :: face_nodes
        integer :: bmi_status
    end function get_grid_face_nodes

    ! Get the number of nodes for each face.
    function get_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
        class(BmiNanofase), intent(in) :: this
        integer, intent(in) :: grid
        integer, dimension(:), intent(out) :: nodes_per_face
        integer :: bmi_status
    end function get_grid_nodes_per_face

end module
module classModel
    implicit none

    !> Class that controls the time-stepping of the model, and
    !! will implement the [BMI](http://bmi-spec.readthedocs.io/en/latest/bmi.spec.html)
    !! if used as a component in the [CSDMS](http://csdms.colorado.edu).
    !! This is with a view to aiding integration with different models in the
    !! future, possibly within the scope of the Landscape Modelling Platform.
    !! Following the BMI seems a good starting point to ensuring it will be
    !! easy for us to make any necessary changes in the future.
    type, public :: Model
        character(len=8) :: component_name = "nanofase"
        character(len=19) :: grid_type = "uniform_rectilinear"
        integer :: grid_rank = 2

      contains
        ! Model control functions
        procedure :: initialize
        procedure :: update
        procedure :: finalize

        ! Model information functions
        procedure :: get_input_var_names
        procedure :: get_output_var_names
        procedure :: get_input_var_name_count
        procedure :: get_output_var_name_count
        procedure :: get_component_name
        procedure :: get_attribute

        ! Time functions
        procedure :: get_time_step
        procedure :: get_time_unit
        procedure :: get_start_time
        procedure :: get_current_time
        procedure :: get_end_time

        ! Variable information functions
        procedure :: get_var_grid
        procedure :: get_var_units
        procedure :: get_var_type
        procedure :: get_var_itemsize
        procedure :: get_var_nbytes

        ! Variable getters and setter
        procedure :: get_value
        procedure :: set_value

        ! Model grids
        procedure :: get_grid_type
        procedure :: get_grid_rank
        procedure :: get_grid_size
        procedure :: get_grid_shape
        procedure :: get_grid_origin
        procedure :: get_grid_spacing
    end type

  contains

!-------------------!
!-- Model control --!
!-------------------!

    !> Initialize the model with a given config file.
    subroutine initialize(me, configFile)
        class(Model) :: me                  ! This Model instance.
        character(len=*) :: configFile      ! Path to config file.
        ! Logic here
    end subroutine

    !> The operations to be performed at each time step.
    subroutine update(me)
        class(Model) :: me                  ! This Model instance.
        ! Logic here
    end subroutine

    !> Finalize the model.
    subroutine finalize(me)
        class(Model) :: me                  ! This Model instance.
        ! Logic here
    end subroutine

!-----------------------!
!-- Model information --!
!-----------------------!

    !> Return a list of the input variables as names that follow the
    !! Standard Names convention.
    function get_input_var_names(me) result(input_var_names)
        class(Model) :: me
        character(len=*) :: input_var_names(:)
        ! Logic here
    end function

    !> Return a list of the output variables as names that follow the
    !! Standard Names convention.
    function get_output_var_names(me) result(output_var_names)
        class(Model) :: me
        character(len=*) :: output_var_names(:)
        ! Logic here
    end function

    !> Return the name of the model.
    function get_component_name(me) result(component_name)
        class(Model) :: me
        character(len=8) :: component_name
        component_name = me%component_name          ! Should we get this from config file instead?
    end function

    !> Return a given attribute value from an attribute name.
    function get_attribute(me, att_name) result(att_value)
        class(Model) :: me
        character(len=*) :: att_name
        character(len=*) :: att_value
        ! Get attribute value from name here
    end function

!----------!
!-- Time --!
!----------!

    !> Return the time step that the model operates on
    function get_time_step(me) result(time_step)
        class(Model) :: me
        real(dp) :: time_step
        ! Get time step - maybe from config file?
    end function

    !> Return the units used for start and end times
    function get_time_units(me) result(time_unit)
        class(Model) :: me
        character(len=4) :: time_unit
        time_unit = "days"
        ! Maybe get this from config file too
    end function

    !> Return the time that the model starts
    function get_start_time(me) result(start_time)
        class(Model) :: me
        real(dp) :: start_time
        ! Logic here
    end function

    !> Return the time of the current model step
    function get_current_time(me) result(current_time)
        class(Model) :: me
        real(dp) :: current_time
        ! Logic here
    end function

    !> Return the time that the model ends
    function get_end_time(me) result(end_time)
        class(Model) :: me
        real(dp) :: end_time
        ! Logic here
    end function

!--------------------------!
!-- Variable information --!
!--------------------------!

    !> Get the ID of the grid used by the variable with
    !! the specified name
    function get_var_grid(me, name) result(var_grid)
        class(Model) :: me
        character(len=*) :: name
        integer :: var_grid
        ! Logic here
    end function

    !> Get the units of the specified variable
    function get_var_units(me, name) result(var_units)
        class(Model) :: me
        character(len=*) :: name
        character(len=*) :: var_units
        ! Logic here
    end function

    !> Get the data type of the variable, following the numpy
    !! dtype convention
    function get_var_type(me, name) result(var_type)
        class(Model) :: me
        character(len=*) :: name
        character(len=*) :: var_type
        ! Logic here
    end function

    !> Get the size, in bytes, of each item of the variable. E.g.,
    !! if the data for a variable are stored as 64-bit integers,
    !! this would return 8.
    function get_var_itemsize(me, name) result(var_itemsize)
        class(Model) :: me
        character(len=*) :: name
        integer :: var_itemsize
        ! Logic here
    end function

    !> Return the total amount of memory used to store the entire
    !! array of data for the specified variable. That is, the number
    !! of items multiplied by the size of each item.
    function get_var_nbytes(me, name) result(nbytes)
        class(Model) :: me
        character(len=*) :: name
        integer :: nbytes
        ! Logic here
    end function

!----------------------------------!
!-- Variable getters and setters --!
!----------------------------------!

    !> Get a variable from the model's state for the current time step,
    !! copying it to the buffer
    subroutine get_value(me, name, buffer)
        class(Model) :: me
        character(len=*) :: name
        class(*) :: buffer(:)
        ! Logic here
    end subroutine

    !> Set a variable in the model's state for the current time step.
    subroutine set_value(me, name, value)
        class(Model) :: me
        character(len=*) :: name
        class(*) :: value(:)
        ! Logic here
    end subroutine

!----------------!
!-- Model grid --!
!----------------!

    !> Get the type of the grid as a string. BMI supports uniform_rectilinear,
    !! rectilinear, structured_quadrilateral and unstructured.
    function get_grid_type(me, id) result(grid_type)
        class(Model) :: me
        integer :: id
        character(len=19) :: grid_type
        grid_type = me%grid_type
    end function

    !> Get the number of dimensions (rank) of a grid.
    function get_grid_rank(me, id) result(grid_rank)
        class(Model) :: me
        integer :: id
        integer :: grid_rank
        grid_rank = me%grid_rank
    end function

    !> The grid size is the number of elements the grid has.
    function get_grid_size(me, id) result(grid_size)
        class(Model) :: me
        integer :: id
        integer :: grid_size
        ! Logic here
    end function

    !> The grid shape is an array of the number of rows and columns
    !! (for a 2D grid).
    function get_grid_shape(me, id) result(grid_shape)
        class(Model) :: me
        integer :: id
        integer :: grid_shape(me%grid_rank)
        ! Logic here
    end function

    !> The location of the lower-left corner of the grid
    function get_grid_origin(me, id) result(grid_origin)
        class(Model) :: me
        integer :: id
        real(dp) :: grid_origin(me%grid_rank)
        ! Logic here
    end function

    function get_grid_spacing(me, id) result(grid_spacing)
        class(Model) :: me
        integer :: id
        integer :: grid_spacing(me%grid_rank)
        ! Logic here
    end function

end module
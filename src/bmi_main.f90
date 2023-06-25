program bmi_main
    use GlobalsModule
    use BmiNanofaseModule
    use datetime_module
    implicit none
    
    type(BmiNanofase)   :: bminf
    integer             :: status
    character(len=256)  :: config_file_path
    character(len=256), pointer :: model_name
    
    ! Get the path to the config file
    call get_command_argument(1, config_file_path)    

    ! Initialise the model and check for errors
    status = bminf%initialize(config_file_path)
    if (status == BMI_FAILURE) then
        print *, "Error initialising the model"
        error stop
    end if

    ! Run the model until the final timestep 
    status = bminf%update_until(date2num(C%endDate))
    if (status == BMI_FAILURE) then
        print *, "Error running the model"
        error stop
    end if

    ! Finalise the model, which takes care of saving a
    ! checkpoint (if requested) and finalising output data
    status = bminf%finalize()

end program
program bmi_main
    use GlobalsModule
    use BmiNanofaseModule
    use datetime_module
    implicit none
    
    type(BmiNanofase) :: bminf
    integer :: status
    character(len=256) :: config_file_path
    integer :: t
    
    call get_command_argument(1, config_file_path)    

    status = bminf%initialize(config_file_path)

    ! do t = 1, 5
    !     status = bminf%update()
    ! end do

    status = bminf%update_until(date2num(C%endDate))
    print *, status

    status = bminf%finalize()
    print *, "Hello!"
end program
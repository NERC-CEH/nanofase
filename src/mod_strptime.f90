module mod_strptime
    use datetime_module
    implicit none 

    contains
    
    function f_strptime(str) result(dt)
        character(len=10),intent(in) :: str     !! time string, format yyyy-mm-dd
        type(datetime) :: dt
        integer :: year, month, day
        
        read(str(1:4),*) year
        read(str(6:7),*) month
        read(str(9:10),*) day
        dt = datetime(year, month, day)
    end function
    
end module
program sandbox
    use classWaterBody1
    use ResultModule
    use Globals
    implicit none
    
    type(WaterBody1) :: wb
    type(Result) :: rslt
    
    call GLOBALS_INIT()
    
    rslt = wb%create(1, 1, 1, [0.0_dp], [0.0_dp], 1000.0_dp)
    rslt = wb%update(1, 0.0_dp, 0.0_dp)
    
end program
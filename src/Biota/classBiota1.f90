module classBiota1                                                      !! class definition for Biota1
    use spcBiota                                                        ! use spcBiota interface
    use ResultModule, only: Result
    use Globals
    implicit none                                                       ! force declaration of all variables
    type, public, extends(Biota) :: Biota1                              !! type declaration for class - extends interface
        contains
            procedure :: create => createBiota1
            procedure :: update => updateBiota1
            procedure :: parseInputData => parseInputDataBiota1
    end type
    contains
        function createBiota1(me) result(rslt)
            class(Biota1) :: me                                      !! This Biota1 instance
            type(Result) :: rslt
            ! Set defaults and ref
            me%ref = "Biota"
            me%C_np = 0.0_dp
            me%C_np_noStoredFraction = 0.0_dp
            ! Get data from input file
            call rslt%addErrors(.errors. me%parseInputData())
            call rslt%addToTrace('Creating Biota')                 ! Add this procedure to the trace
            ! call LOGR%toFile(errors=.errors. rslt)
        end function

        function updateBiota1(me, t, C_np_matrices) result(rslt)
            class(Biota1) :: me                 !! This Biota1 instance
            integer :: t                        !! The current time step
            real(dp) :: C_np_matrices(2)        !! Concentration of nanomaterial in all matrices (soil/water) and diet
            type(Result) :: rslt                !! The Result object to return errors in
            integer :: tDays

            ! TODO currently assuming no initial concentration

            tDays = t*C%timeStep/86400                      ! This time step in days
            if (tDays .le. me%eliminationPhaseStart) then   ! If we're in the uptake phase
                me%C_np = me%storedFraction * (C_np_matrices(1) * me%k_uptake(1) + C_np_matrices(2) &
                    * me%k_uptake(2)) * tDays + (1 - me%storedFraction) * (C_np_matrices(1) * me%k_uptake(1) &
                    + C_np_matrices(2) * me%k_uptake(2)) / (me%k_elim + me%k_growth) &
                    * (1 - exp(-(me%k_elim + me%k_growth) * tDays))
                me%C_np_noStoredFraction = (C_np_matrices(1) * me%k_uptake(1) &
                    + C_np_matrices(2) * me%k_uptake(2)) / (me%k_elim + me%k_growth) &
                    * (1 - exp(-(me%k_elim + me%k_growth) * tDays))
            else                                            ! Else we must be in the elimination phase
                me%C_np = me%storedFraction * (C_np_matrices(1) * me%k_uptake(1) + C_np_matrices(2) &
                    * me%k_uptake(2)) * (me%eliminationPhaseStart - 1) + (1 - me%storedFraction) * (C_np_matrices(1) &
                    * me%k_uptake(1) + C_np_matrices(2) * me%k_uptake(2)) / (me%k_elim + me%k_growth) &
                    * (1 - exp(-(me%k_elim + me%k_growth) * (me%eliminationPhaseStart - 1))) &
                    * exp(-(me%k_elim + me%k_growth) * (tDays - me%eliminationPhaseStart - 1))
                me%C_np_noStoredFraction = (C_np_matrices(1) &
                    * me%k_uptake(1) + C_np_matrices(2) * me%k_uptake(2)) / (me%k_elim + me%k_growth) &
                    * (1 - exp(-(me%k_elim + me%k_growth) * (me%eliminationPhaseStart - 1))) &
                    * exp(-(me%k_elim + me%k_growth) * (tDays - me%eliminationPhaseStart - 1))
            end if
        end function

        function parseInputDataBiota1(me) result(rslt)
            class(Biota1) :: me
            type(Result) :: rslt

            ! HACK get these from input data file
            me%k_uptake(1) = 0.05               ! Uptake from matrix (soil/water) [/day]
            me%k_uptake(2) = 0.05               ! Update from diet [/day]
            me%k_elim = 0.5                     ! [/day]
            me%k_growth = 0.1                   ! [/day]
            me%eliminationPhaseStart = 366      ! [day]
            me%C_np_init = 0                    ! Initial NM conc [kg/day]
            me%storedFraction = 0.5             ! [-]
        end function
end module

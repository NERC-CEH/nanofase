module AbstractReactorModule
    use GlobalsModule, only: dp
    use ContaminantModule, only: Contaminant
    implicit none

    type, abstract, public :: AbstractReactor
        character(len=100) :: ref
        integer :: x, y
        character(len=32) :: compartment
        type(Contaminant), pointer :: contaminant => null()
        real(dp) :: volume
    contains
        procedure(createAbstractReactor), deferred :: create
        procedure(updateAbstractReactor), deferred :: update
        procedure(finaliseAbstractReactor), deferred :: finalise
        procedure(parseInputDataAbstractReactor), deferred :: parseInputData
    end type

    abstract interface

        function createAbstractReactor(me, x, y, compartment, contaminant_in, volume, T_water, &
                                       C_spm, W_settle_spm, G, k_att, alpha_att, velocity) result(r)
            use GlobalsModule, only: dp
            use ResultModule, only: Result
            use ContaminantModule, only: Contaminant
            import AbstractReactor

            class(AbstractReactor), intent(inout) :: me
            integer, intent(in) :: x, y
            character(len=*), intent(in) :: compartment
            type(Contaminant), target, intent(inout) :: contaminant_in
            real(dp), intent(in) :: volume
            real(dp), intent(in) :: T_water
            real(dp), intent(in), optional :: C_spm(:), W_settle_spm(:)
            real(dp), intent(in), optional :: G
            real(dp), intent(in), optional :: k_att(:), alpha_att
            real(dp), intent(in), optional :: velocity
            type(Result) :: r
        end function

        function updateAbstractReactor(me, j_contaminant_in, dt) result(r)
            use GlobalsModule, only: dp
            use ContaminantModule, only: Contaminant
            use ResultModule, only: Result
            import AbstractReactor

            class(AbstractReactor), intent(inout) :: me
            type(Contaminant), intent(in), optional :: j_contaminant_in
            real(dp), intent(in) :: dt
            type(Result) :: r
        end function

        subroutine finaliseAbstractReactor(me)
            import AbstractReactor
            class(AbstractReactor), intent(inout) :: me
        end subroutine

        function parseInputDataAbstractReactor(me) result(r)
            use ResultModule, only: Result
            import AbstractReactor

            class(AbstractReactor), intent(inout) :: me
            type(Result) :: r
        end function

    end interface
end module 
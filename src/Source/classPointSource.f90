module classPointSource
    use Globals
    use ResultModule
    implicit none
    private

    type, public :: PointSource
        private

      contains
        ! Proceudres
        procedure :: destroy => destroyPointSource
    end type

  contains
    function destroyPointSource(me) result(r)
        class(PointSource) :: me
        type(Result) :: r
        ! Destroy
    end function

end module
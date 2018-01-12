module classDiffuseSource
    use Globals
    use ResultModule
    implicit none
    private

    type, public :: DiffuseSource
        private

      contains
        ! Proceudres
        procedure :: destroy => destroyDiffuseSource
    end type

  contains
    function destroyDiffuseSource(me) result(r)
        class(DiffuseSource) :: me
        type(Result) :: r
        ! Destroy
    end function

end module
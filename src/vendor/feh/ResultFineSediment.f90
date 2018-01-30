!> Custom extension of Fortran Error Handler's ResultModule to provide operators
!! for FineSediment data, as a scalar or array of rank 1 or 2.
module ResultFineSedimentModule
    use ErrorInstanceModule
    use ResultModule
    use classFineSediment1
    implicit none
    private

    !> Result object with operator for FineSediment scalar data
    type, public, extends(Result0D) :: ResultFineSediment0D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment0D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

    !> Result object with operator for data as 1D FineSediment array
    type, public, extends(Result1D) :: ResultFineSediment1D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment1D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

    !> Result object with operator for data as 2D FineSediment array
    type, public, extends(Result2D) :: ResultFineSediment2D
      contains
        procedure, public :: getDataAsFineSediment => getDataAsFineSediment2D
        generic, public :: operator(.finesediment.) => getDataAsFineSediment
    end type

  contains

    !> Attempt to return the data as a scalar FineSediment1 object
    function getDataAsFineSediment0D(this) result(data)
        class(ResultFineSediment0D), intent(in) :: this     !! This Result object
        type(FineSediment1)                     :: data     !! The data as a FineSediment1 object
        select type (d => this%data)
            type is (FineSediment1)
                data = d
            class default
                error stop "Error trying to return 0D data as FineSediment1. Are you sure the data is of type FineSediment1?"
        end select
    end function

    !> Attempt to return the data as a 1D FineSediment1 object array
    function getDataAsFineSediment1D(this) result(data)
        class(ResultFineSediment1D), intent(in) :: this                     !! This Result object
        type(FineSediment1)                     :: data(size(this%data))    !! The data as a 1D FineSediment1 array
        select type (d => this%data)
            type is (FineSediment1)
                data = d
            class default
                error stop "Error trying to return 1D data as FineSediment1. Are you sure the data is of type FineSediment1?"
        end select
    end function

    !> Attempt to return the data as a 2D FineSediment1 object array
    function getDataAsFineSediment2D(this) result(data)
        class(ResultFineSediment2D), intent(in) :: this                         !! This Result object
        type(FineSediment1)                     :: data(size(this%data,1), &
                                                        size(this%data,2))      !! The data as a 1D FineSediment1 array
        select type (d => this%data)
            type is (FineSediment1)
                data = d
            class default
                error stop "Error trying to return 2D data as FineSediment1. Are you sure the data is of type FineSediment1?"
        end select
    end function
    
end module
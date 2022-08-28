!> Module for class definition for class representing an area of a crop.
!! TODO: What crop info do we need from demands data? 
module CropModule
    use GlobalsModule
    implicit none
    private
    
    character(len=100) :: nameLookup(601:647) = [character(len=16)::'wheat','maize','rice','barley','rye','millet', &
        'sorghum','soybeans', 'sunflower','potatoes','sugarcane','sugar_beets','pulses','citrus','date_palm','cotton', &
        'others_perennial','grass','others_annual','wheat','others_annual','others_annual','others_annual','others_annual', &
        'wheat','maize','rice','barley','rye','millet', &
        'sorghum','soybeans','sunflower','potatoes','sugar_beets','rapeseed_canola','groundnut_peanut', &
        'pulses','grapes','cotton','others_annual','wheat','rye','others_annual','others_annual', &
        'others_annual','others_annual' &
    ]

    !> The `Crop` class represents an area of a crop of a
    !! specific type and planting month
    type, public :: Crop
        real(dp)            :: area
        integer             :: typeInt
        character(len=100)  :: name
        integer             :: plantingMonth

      contains
        procedure, private :: create => createCrop
    end type
    
    interface Crop
        procedure init
    end interface
    
  contains
    
    !> Interface to return a new `Crop` object
    function init(typeInt, area, plantingMonth)
        type(Crop)      :: init
        integer         :: typeInt
        real(dp)        :: area
        integer         :: plantingMonth
        call init%create(typeInt, area, plantingMonth)
    end function
    
    !> Intialise this `Crop` object with a type, area and planting month
    subroutine createCrop(me, typeInt, area, plantingMonth)
        class(Crop)     :: me
        integer         :: typeInt
        real(dp)        :: area
        integer         :: plantingMonth
        me%area = area
        me%typeInt = typeInt
        me%plantingMonth = plantingMonth
        me%name = nameLookup(me%typeInt)
    end subroutine

end module
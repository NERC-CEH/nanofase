module classDatabase
    use mo_netcdf
    use Globals
    use classLogger, only: LOG
    use UtilModule
    implicit none

    type, public :: Database
        type(NcDataset)     :: nc
        ! Routing variables
        integer, allocatable :: outflow(:,:,:)
        integer, allocatable :: inflows(:,:,:,:)
        logical, allocatable :: isHeadwater(:,:)
        integer, allocatable :: nWaterbodies(:,:)
        ! Spatiotemporal variables
        real, allocatable :: runoff(:,:,:)
        real, allocatable :: precip(:,:,:)
        real, allocatable :: evap(:,:,:)
        ! Spatial variables
        real, allocatable :: soilBulkDensity(:,:)
        real, allocatable :: soilWaterContentFieldCapacity(:,:)
        real, allocatable :: soilWaterContentSaturation(:,:)
        real, allocatable :: soilHydraulicConductivity(:,:)
        real, allocatable :: soilTextureClayContent(:,:)
        real, allocatable :: soilTextureSandContent(:,:)
        real, allocatable :: soilTextureSiltContent(:,:)
        real, allocatable :: soilTextureCoarseFragContent(:,:)
        ! Spatial 1D variables
        real, allocatable :: landUse(:,:,:)
        ! Constants
      contains
        procedure, public :: init => initDatabase
    end type

    type(Database) :: DATASET

  contains

    subroutine initDatabase(me, inputFile)
        class(Database)     :: me
        type(NcVariable)    :: var
        character(len=*)    :: inputFile
        integer             :: x,y
        integer, allocatable :: isHeadwaterInt(:,:)     ! Temporary variable to store int before convert to bool

        ! Open the dataset
        me%nc = NcDataset(inputFile, 'r')

        ! Variable units: These will already have been converted to the correct
        ! units for use in the model by nanofase-data (the input data compilation
        ! script). Hence, no maths need be done on variables here to convert and
        ! thus no FPEs will occur from the masked (_FillValue) values - the model will
        ! check the relevant variables for these *when they are used*.

        ! ROUTING VARIABLES
        var = me%nc%getVariable('outflow')
        call var%getData(me%outflow)
        var = me%nc%getVariable('inflows')
        call var%getData(me%inflows)
        var = me%nc%getVariable('is_headwater')
        call var%getData(isHeadwaterInt)
        me%isHeadwater = lgcl(isHeadwaterInt)       ! Convert int to logical
        var = me%nc%getVariable('n_waterbodies')
        call var%getData(me%nWaterbodies)

        ! SPATIOTEMPORAL VARIABLES
        ! Runoff        [m/timestep]
        var = me%nc%getVariable('runoff')
        call var%getData(me%runoff)
        ! Precip        [m/timestep]
        var = me%nc%getVariable('precip')
        call var%getData(me%precip)
        ! Evap
        ! TODO actually get some data for this
        if (me%nc%hasVariable('evap')) then
            var = me%nc%getVariable('evap')
            call var%getData(me%evap)
        else
            allocate(me%evap(44, 27, 365)) ! HACK
            me%evap = 0.0
        end if

        ! SPATIAL VARIABLES
        ! Soil bulk density                          [kg/m3]
        var = me%nc%getVariable('soil_bulk_density')
        call var%getData(me%soilBulkDensity)
        ! Soil water content at field capacity      [cm3/cm3]
        var = me%nc%getVariable('soil_water_content_field_capacity')
        call var%getData(me%soilWaterContentFieldCapacity)
        ! Soil water content at saturation          [cm3/cm3]
        var = me%nc%getVariable('soil_water_content_saturation')
        call var%getData(me%soilWaterContentSaturation)
        ! Soil hydraulic conductivity               [m/s]
        var = me%nc%getVariable('soil_hydraulic_conductivity')
        call var%getData(me%soilHydraulicConductivity)
        ! Soil texture                              [%]
        var = me%nc%getVariable('soil_texture_clay_content')
        call var%getData(me%soilTextureClayContent)
        var = me%nc%getVariable('soil_texture_sand_content')
        call var%getData(me%soilTextureSandContent)
        var = me%nc%getVariable('soil_texture_silt_content')
        call var%getData(me%soilTextureSiltContent)
        var = me%nc%getVariable('soil_texture_coarse_frag_content')
        call var%getData(me%soilTextureCoarseFragContent)

        ! SPATIAL 1D VARIABLES
        ! Land use                                  [-]
        var = me%nc%getVariable('land_use')
        call var%getData(me%landUse)

        ! Close the dataset
        call me%nc%close()

        call LOG%add("Initialising database: success")
    end subroutine

end module
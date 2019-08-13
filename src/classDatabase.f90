module classDatabase
    use mo_netcdf
    use netcdf
    use Globals
    use classLogger, only: LOG
    use UtilModule
    implicit none

    type, public :: Database
        type(NcDataset)     :: nc
        ! Constants
        real, allocatable :: defaultNMSizeDistribution(:)    ! Default distribution to use to split NM across size classes
        real, allocatable :: nmSizeClasses(:)   ! Diameter of each NM size class [m]
        ! Grid and coordinate variables
        integer, allocatable :: gridShape(:)    ! Number of grid cells along each grid axis [-]
        real, allocatable :: gridRes(:)         ! Resolution of grid cells [m]
        real, allocatable :: gridBounds(:)      ! Bounding box of grid, indexed as left, bottom, right, top [m]
        logical, allocatable :: gridMask(:,:)   ! Logical mask for extent of grid [-]
        real, allocatable :: x(:)               ! Centre of cell [m]
        real, allocatable :: x_l(:)             ! Left side of cell [m]
        real, allocatable :: y(:)               ! Centre of cell [m]
        real, allocatable :: y_u(:)             ! Upper side of cell [m]
        integer, allocatable :: t(:)            ! Seconds since start date [s]
        integer :: nTimesteps                   ! Number of timesteps for the model run [-]
        ! Routing variables
        integer, allocatable :: outflow(:,:,:)
        integer, allocatable :: inflows(:,:,:,:)
        logical, allocatable :: isHeadwater(:,:)
        integer, allocatable :: nWaterbodies(:,:)
        logical, allocatable :: isEstuary(:,:)
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
        ! Emissions - areal
        real(dp), allocatable :: emissionsArealSoilPristine(:,:)
        real(dp), allocatable :: emissionsArealSoilTransformed(:,:)
        real(dp), allocatable :: emissionsArealSoilDissolved(:,:)
        real(dp), allocatable :: emissionsArealWaterPristine(:,:)
        real(dp), allocatable :: emissionsArealWaterTransformed(:,:)
        real(dp), allocatable :: emissionsArealWaterDissolved(:,:)
        ! Emissions - atmospheric depo
        real(dp), allocatable :: emissionsAtmosphericDryDepoPristine(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoTransformed(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoDissolved(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoPristine(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoTransformed(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoDissolved(:,:,:)
        ! Emisions - point
        real(dp), allocatable :: emissionsPointWaterPristine(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterTransformed(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterDissolved(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterCoords(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterBodyPristine(:,:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterBodyTransformed(:,:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterBodyDissolved(:,:,:,:,:)
        integer, allocatable :: nPointSources(:,:)
        ! Spatial 1D variables
        real, allocatable :: landUse(:,:,:)
        ! Constants
      contains
        procedure, public :: init => initDatabase
        procedure, private :: parseConstants => parseConstantsDatabase
        procedure, private :: mask => maskDatabase
        procedure, public :: inModelDomain => inModelDomainDatabase
        procedure, private :: calculateNPointSources => calculateNPointSourcesDatabase
        procedure, public :: coordsToCellIndex => coordsToCellIndexDatabase
        procedure, public :: coordsToFractionalCellIndex => coordsToFractionalCellIndexDatabase
    end type

    type(Database) :: DATASET

  contains

    subroutine initDatabase(me, inputFile, constantsFile)
        class(Database)     :: me
        type(NcVariable)    :: var
        character(len=*)    :: inputFile
        character(len=*)    :: constantsFile
        integer, allocatable :: isHeadwaterInt(:,:)     ! Temporary variable to store int before convert to bool
        integer, allocatable :: isEstuaryInt(:,:)
        type(NcDimension)   :: p_dim
        integer             :: maxPointSources
        
        ! Open the dataset and constants NML file
        me%nc = NcDataset(inputFile, 'r')
        call me%parseConstants(constantsFile)
        
        ! Variable units: These will already have been converted to the correct
        ! units for use in the model by nanofase-data (the input data compilation
        ! script). Hence, no maths need be done on variables here to convert and
        ! thus no FPEs will occur from the masked (_FillValue) values - the model will
        ! check the relevant variables for these *when they are used*.

        ! GRID AND COORDINATE VARIABLES
        var = me%nc%getVariable('grid_shape')
        call var%getData(me%gridShape)
        var = me%nc%getVariable('grid_res')
        call var%getData(me%gridRes)
        var = me%nc%getVariable('grid_bounds')
        call var%getData(me%gridBounds)
        var = me%nc%getVariable('x')
        call var%getData(me%x)
        allocate(me%x_l(me%gridShape(1)))
        me%x_l = me%x - 0.5 * me%gridRes(1)
        var = me%nc%getVariable('y')
        call var%getData(me%y)
        allocate(me%y_u(me%gridShape(2)))
        me%y_u = me%y + 0.5 * me%gridRes(2)
        var = me%nc%getVariable('t')
        call var%getData(me%t)
        me%nTimesteps = size(me%t)

        ! ROUTING VARIABLES
        var = me%nc%getVariable('outflow')
        call var%getData(me%outflow)
        var = me%nc%getVariable('inflows')
        call var%getData(me%inflows)
        var = me%nc%getVariable('is_headwater')
        call var%getData(isHeadwaterInt)
        me%isHeadwater = ulgcl(isHeadwaterInt)      ! Convert uint1 to logical
        var = me%nc%getVariable('n_waterbodies')
        call var%getData(me%nWaterbodies)
        var = me%nc%getVariable('is_estuary')
        call var%getData(isEstuaryInt)
        me%isEstuary = ulgcl(isEstuaryInt)          ! Convert uint1 to logical

        ! Use the nWaterbodies array to set the grid mask
        allocate(me%gridMask(me%gridShape(1), me%gridShape(2)))
        me%gridMask = me%mask(me%nWaterbodies)

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
            allocate(me%evap(me%gridShape(1), me%gridShape(2), C%nTimesteps)) ! HACK
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
        ! Emissions - areal                         [kg/m2/timestep]
        ! Soil
        if (me%nc%hasVariable('emissions_areal_soil_pristine')) then
            var = me%nc%getVariable('emissions_areal_soil_pristine')
            call var%getData(me%emissionsArealSoilPristine)
        else
            allocate(me%emissionsArealSoilPristine(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealSoilPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_areal_soil_transformed')) then
            var = me%nc%getVariable('emissions_areal_soil_transformed')
            call var%getData(me%emissionsArealSoilTransformed)
        else
            allocate(me%emissionsArealSoilTransformed(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealSoilTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_areal_soil_dissolved')) then
            var = me%nc%getVariable('emissions_areal_soil_dissolved')
            call var%getData(me%emissionsArealSoilDissolved)
        else
            allocate(me%emissionsArealSoilDissolved(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealSoilDissolved = nf90_fill_double
        end if
        ! Water
        if (me%nc%hasVariable('emissions_areal_water_pristine')) then
            var = me%nc%getVariable('emissions_areal_water_pristine')
            call var%getData(me%emissionsArealWaterPristine)
        else
            allocate(me%emissionsArealWaterPristine(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealWaterPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_areal_water_transformed')) then
            var = me%nc%getVariable('emissions_areal_water_transformed')
            call var%getData(me%emissionsArealWaterTransformed)
        else
            allocate(me%emissionsArealWaterTransformed(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealWaterTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_areal_water_dissolved')) then
            var = me%nc%getVariable('emissions_areal_water_dissolved')
            call var%getData(me%emissionsArealWaterDissolved)
        else
            allocate(me%emissionsArealWaterDissolved(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealWaterDissolved = nf90_fill_double
        end if

        ! Emissions - atmospheric                   [kg/m2/timestep]
        if (me%nc%hasVariable('emissions_atmospheric_drydepo_pristine')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_pristine')
            call var%getData(me%emissionsAtmosphericDryDepoPristine)
        else
            allocate(me%emissionsAtmosphericDryDepoPristine(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericDryDepoPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_atmospheric_drydepo_transformed')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_transformed')
            call var%getData(me%emissionsAtmosphericDryDepoTransformed)
        else
            allocate(me%emissionsAtmosphericDryDepoTransformed(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericDryDepoTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_atmospheric_drydepo_dissolved')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_dissolved')
            call var%getData(me%emissionsAtmosphericDryDepoDissolved)
        else
            allocate(me%emissionsAtmosphericDryDepoDissolved(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericDryDepoDissolved = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_pristine')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_pristine')
            call var%getData(me%emissionsAtmosphericWetDepoPristine)
        else
            allocate(me%emissionsAtmosphericWetDepoPristine(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericWetDepoPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_transformed')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_transformed')
            call var%getData(me%emissionsAtmosphericWetDepoTransformed)
        else
            allocate(me%emissionsAtmosphericWetDepoTransformed(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericWetDepoTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_dissolved')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_dissolved')
            call var%getData(me%emissionsAtmosphericWetDepoDissolved)
        else
            allocate(me%emissionsAtmosphericWetDepoDissolved(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericWetDepoDissolved = nf90_fill_double
        end if
        ! Emissions - point (all water)                     [kg/timestep]
        p_dim = me%nc%getDimension('p')
        maxPointSources = p_dim%getLength()
        if (me%nc%hasVariable('emissions_point_water_pristine')) then
            var = me%nc%getVariable('emissions_point_water_pristine')
            call var%getData(me%emissionsPointWaterPristine)
        else
            allocate(me%emissionsPointWaterPristine(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_point_water_transformed')) then
            var = me%nc%getVariable('emissions_point_water_transformed')
            call var%getData(me%emissionsPointWaterTransformed)
        else
            allocate(me%emissionsPointWaterTransformed(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_point_water_dissolved')) then
            var = me%nc%getVariable('emissions_point_water_dissolved')
            call var%getData(me%emissionsPointWaterDissolved)
        else
            allocate(me%emissionsPointWaterDissolved(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterDissolved = nf90_fill_double
        end if
        ! Emissions - point coordinates
        if (me%nc%hasVariable('emissions_point_water_pristine_coords')) then
            var = me%nc%getVariable('emissions_point_water_pristine_coords')
            call var%getData(me%emissionsPointWaterCoords)
        else
            allocate(me%emissionsPointWaterCoords(me%gridShape(1), me%gridShape(2), maxPointSources, 2))
            me%emissionsPointWaterCoords = nf90_fill_double
        end if

        ! Calculate the number of point source per cell
        call me%calculateNPointSources(maxPointSources)

        ! SPATIAL 1D VARIABLES
        ! Land use                                  [-]
        var = me%nc%getVariable('land_use')
        call var%getData(me%landUse)

        ! Close the dataset
        call me%nc%close()

        call LOG%add("Initialising database: success")
    end subroutine

    subroutine parseConstantsDatabase(me, constantsFile)
        class(Database)         :: me
        character(len=*)        :: constantsFile
        integer :: n_default_nm_size_distribution, n_nm_size_classes
        integer, allocatable :: default_nm_size_distribution(:)
        real, allocatable :: nm_size_classes(:)
        namelist /allocatable_array_sizes/ n_default_nm_size_distribution, n_nm_size_classes
        namelist /size_classes/ default_nm_size_distribution, nm_size_classes
        ! Open and read the NML file
        open(11, file=constantsFile, status="old")
        read(11, nml=allocatable_array_sizes)
        ! Allocate the appropriate
        allocate(default_nm_size_distribution(n_default_nm_size_distribution))
        allocate(nm_size_classes(n_nm_size_classes))
        read(11, nml=size_classes)
        close(11)
        ! Save these to class variables
        me%defaultNMSizeDistribution = default_nm_size_distribution / 100.0
        me%nmSizeClasses = nm_size_classes
    end subroutine

    elemental function maskDatabase(me, int) result(mask)
        class(Database), intent(in) :: me
        integer, intent(in) :: int
        logical :: mask
        if (int == nf90_fill_int2) then
            mask = .true.
        else
            mask = .false.
        end if
    end function

    !> Calculate the number of point sources per grid cell
    subroutine calculateNPointSourcesDatabase(me, maxPointSources)
        class(Database) :: me
        integer         :: maxPointSources
        integer :: i, j, k, n
        allocate(me%nPointSources(me%gridShape(1), me%gridShape(2)))
        do j = 1, me%gridShape(2)
            do i = 1, me%gridShape(1)
                n = 0
                do k = 1, maxPointSources
                    if (me%emissionsPointWaterCoords(i, j, k, 1) /= nf90_fill_double) then
                        n = n + 1
                    end if
                end do
                me%nPointSources(i, j) = n
            end do
        end do
    end subroutine

    function inModelDomainDatabase(me, x, y) result(inModelDomain)
        class(Database), intent(in) :: me
        integer, intent(in) :: x, y
        logical :: inModelDomain
        logical :: xInDomain
        logical :: yInDomain

        xInDomain = x .ge. 1 .and. x .le. me%gridShape(1)
        yInDomain = y .ge. 1 .and. y .le. me%gridShape(2)

        if (xInDomain .and. yInDomain) then
            if (.not. me%gridMask(x, y)) then
                inModelDomain = .true.
            else
                inModelDomain = .false.
            end if
        else
            inModelDomain = .false.
        end if
    end function

    function coordsToCellIndexDatabase(me, easts, norths) result(indicies)
        class(Database) :: me
        real :: easts
        real :: norths
        integer :: indicies(2)
        integer :: x, y
        x = (int(easts) - mod(int(easts), int(me%gridRes(1))) - int(me%gridBounds(1)))/int(me%gridRes(1)) + 1
        y = (int(me%gridBounds(4)) - (int(norths) - mod(int(norths), int(me%gridRes(2)))))/int(me%gridRes(2))
        indicies = [x, y]
    end function

    function coordsToFractionalCellIndexDatabase(me, easts, norths) result(fracIndicies)
        class(Database) :: me
        real :: easts
        real :: norths
        real :: fracIndicies(2)
        integer :: indicies(2)
        indicies = me%coordsToCellIndex(easts, norths)
        fracIndicies(1) = indicies(1) + mod(easts, me%gridRes(1)) / me%gridRes(1) 
        fracIndicies(2) = indicies(2) + 1 - mod(norths, me%gridRes(2)) / me%gridRes(2)
    end function

end module
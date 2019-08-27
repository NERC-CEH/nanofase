module classDatabase
    use mo_netcdf
    use netcdf
    use Globals
    use ResultModule, only: Result
    use ErrorInstanceModule, only: ErrorInstance
    use classLogger, only: LOGR
    use UtilModule
    implicit none

    type, public :: Database
        type(NcDataset)     :: nc
        ! Constants
        real, allocatable :: defaultNMSizeDistribution(:)   ! Default distribution to split NM across size classes
        real, allocatable :: defaultSpmSizeDistribution(:)  ! Default distribution to split SPM across size classes
        real, allocatable :: nmSizeClasses(:)   ! Diameter of each NM size class [m]
        real, allocatable :: spmSizeClasses(:)  ! Diameter of each SPM size class [m]
        real, allocatable :: defaultMatrixEmbeddedDistributionToSpm(:)  ! Default distribution to proportion matrix-embedded releases to SPM size classes
        integer :: nSizeClassesSpm              ! Number of SPM size classes
        integer :: nSizeClassesNM               ! Number of NM size classes
        real(dp) :: soilDarcyVelocity           ! Darcy velocity in soil [m/s]
        real(dp) :: soilDefault_alpha_att       ! Default attachment efficiency [-]
        real(dp) :: soilDefaultPorosity         ! Default porosity [-]  ! TODO deprecate this in favour of spatially resolved porosity
        real(dp) :: soilHamakerConstant         ! Hamaker constant for soil [J]
        real(dp) :: soilParticleDensity         ! Particle density of soil [kg m-3]
        real :: soilDefaultAttachmentEfficiency ! Attachment efficiency to soil matrix [-]
        integer :: earthwormDensityArable           ! Earthworm density arable [individuals/m2]
        integer :: earthwormDensityConiferous       ! Earthworm density coniferous [individuals/m2]
        integer :: earthwormDensityDeciduous        ! Earthworm density deciduous [individuals/m2]
        integer :: earthwormDensityGrassland        ! Earthworm density grassland [individuals/m2]
        integer :: earthwormDensityHeathland        ! Earthworm density heathland [individuals/m2]
        integer :: earthwormDensityUrbanCapped      ! Earthworm density urban_capped [individuals/m2]
        integer :: earthwormDensityUrbanGardens     ! Earthworm density urban_gardens [individuals/m2]
        integer :: earthwormDensityUrbanParks       ! Earthworm density urban_parks [individuals/m2]
        real, allocatable :: earthwormVerticalDistribution(:)   ! Vertical distribution of earthworms
        ! Biota
        character(len=100), allocatable :: biotaName(:)
        character(len=100), allocatable :: biotaCompartment(:)
        real(dp), allocatable :: biotaInitial_C_org(:)
        real(dp), allocatable :: biota_k_growth(:)
        real(dp), allocatable :: biota_k_death(:)
        real(dp), allocatable :: biota_k_uptake_np(:)
        real(dp), allocatable :: biota_k_elim_np(:)
        real(dp), allocatable :: biota_k_uptake_transformed(:)
        real(dp), allocatable :: biota_k_elim_transformed(:)
        real(dp), allocatable :: biota_k_uptake_dissolved(:)
        real(dp), allocatable :: biota_k_elim_dissolved(:)
        real, allocatable :: biotaStoredFraction(:)
        character(len=17), allocatable :: biotaUptakeFromForm(:)
        integer, allocatable :: biotaHarvestInMonth(:)
        logical :: hasBiota = .false.
        integer :: nBiota = 0
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
        real, allocatable :: soilAttachmentEfficiency(:,:)
        real, allocatable :: soilAttachmentRate(:,:)
        ! Emissions - areal
        real(dp), allocatable :: emissionsArealSoilPristine(:,:)
        real(dp), allocatable :: emissionsArealSoilMatrixEmbedded(:,:)
        real(dp), allocatable :: emissionsArealSoilTransformed(:,:)
        real(dp), allocatable :: emissionsArealSoilDissolved(:,:)
        real(dp), allocatable :: emissionsArealWaterPristine(:,:)
        real(dp), allocatable :: emissionsArealWaterMatrixEmbedded(:,:)
        real(dp), allocatable :: emissionsArealWaterTransformed(:,:)
        real(dp), allocatable :: emissionsArealWaterDissolved(:,:)
        ! Emissions - atmospheric depo
        real(dp), allocatable :: emissionsAtmosphericDryDepoPristine(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoMatrixEmbedded(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoTransformed(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoDissolved(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoPristine(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoMatrixEmbedded(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoTransformed(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoDissolved(:,:,:)
        ! Emisions - point
        real(dp), allocatable :: emissionsPointWaterPristine(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterMatrixEmbedded(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterTransformed(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterDissolved(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterCoords(:,:,:,:)
        integer, allocatable :: nPointSources(:,:)
        ! Spatial 1D variables
        real, allocatable :: landUse(:,:,:)
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
        type(Result)        :: rslt
        integer, allocatable :: isHeadwaterInt(:,:)     ! Temporary variable to store int before convert to bool
        integer, allocatable :: isEstuaryInt(:,:)
        type(NcDimension)   :: p_dim
        integer             :: maxPointSources
        

        print *, "before constants"
        ! Open the dataset and constants NML file
        me%nc = NcDataset(inputFile, 'r')
        call me%parseConstants(constantsFile)
        print *, "after constants"
        
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
        ! Soil attachment efficienecy/rate
        ! Try and get attachment rate. This is used preferentially by soil profile
        if (me%nc%hasVariable('soil_attachment_rate')) then
            var = me%nc%getVariable('soil_attachment_rate')
            call var%getData(me%soilAttachmentRate)
        else
            allocate(me%soilAttachmentRate(me%gridShape(1), me%gridShape(2)))
            me%soilAttachmentRate = nf90_fill_real
        end if
        ! Try and get attachment efficiency. This is used to calculate rate
        ! if rate not present. Defaults to to value given in constants file
        if (me%nc%hasVariable('soil_attachment_efficiency')) then
            var = me%nc%getVariable('soil_attachment_efficiency')
            call var%getData(me%soilAttachmentEfficiency)
        else
            allocate(me%soilAttachmentEfficiency(me%gridShape(1), me%gridShape(2)))
            me%soilAttachmentEfficiency = me%soilDefaultAttachmentEfficiency
        end if
        ! Emissions - areal                         [kg/m2/timestep]
        ! Soil
        if (me%nc%hasVariable('emissions_areal_soil_pristine')) then
            var = me%nc%getVariable('emissions_areal_soil_pristine')
            call var%getData(me%emissionsArealSoilPristine)
        else
            allocate(me%emissionsArealSoilPristine(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealSoilPristine = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_areal_soil_matrixembedded')) then
            var = me%nc%getVariable('emissions_areal_soil_matrixembedded')
            call var%getData(me%emissionsArealSoilMatrixEmbedded)
        else
            allocate(me%emissionsArealSoilMatrixEmbedded(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealSoilMatrixEmbedded = nf90_fill_double
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
        if (me%nc%hasVariable('emissions_areal_water_matrixembedded')) then
            var = me%nc%getVariable('emissions_areal_water_matrixembedded')
            call var%getData(me%emissionsArealWaterMatrixEmbedded)
        else
            allocate(me%emissionsArealWaterMatrixEmbedded(me%gridShape(1), me%gridShape(2)))
            me%emissionsArealWaterMatrixEmbedded = nf90_fill_double
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
        if (me%nc%hasVariable('emissions_atmospheric_drydepo_matrixembedded')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_matrixembedded')
            call var%getData(me%emissionsAtmosphericDryDepoMatrixEmbedded)
        else
            allocate(me%emissionsAtmosphericDryDepoMatrixEmbedded(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericDryDepoMatrixEmbedded = nf90_fill_double
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
        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_matrixembedded')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_matrixembedded')
            call var%getData(me%emissionsAtmosphericWetDepoMatrixEmbedded)
        else
            allocate(me%emissionsAtmosphericWetDepoMatrixEmbedded(me%gridShape(1), me%gridShape(2), me%nTimesteps))
            me%emissionsAtmosphericWetDepoMatrixEmbedded = nf90_fill_double
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
        ! Pristine
        if (me%nc%hasVariable('emissions_point_water_pristine')) then
            var = me%nc%getVariable('emissions_point_water_pristine')
            call var%getData(me%emissionsPointWaterPristine)
            ! Get point source coords
            if (me%nc%hasVariable('emissions_point_water_pristine_coords')) then
                var = me%nc%getVariable('emissions_point_water_pristine_coords')
                call var%getData(me%emissionsPointWaterCoords)
            end if
        else
            allocate(me%emissionsPointWaterPristine(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterPristine = nf90_fill_double
        end if
        ! Matrix-embedded
        if (me%nc%hasVariable('emissions_point_water_matrixembedded')) then
            var = me%nc%getVariable('emissions_point_water_matrixembedded')
            call var%getData(me%emissionsPointWaterMatrixEmbedded)
            ! Get point source coords
            if (me%nc%hasVariable('emissions_point_water_matrixembedded_coords')) then
                var = me%nc%getVariable('emissions_point_water_matrixembedded_coords')
                call var%getData(me%emissionsPointWaterCoords)
            end if
        else
            allocate(me%emissionsPointWaterMatrixEmbedded(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterMatrixEmbedded = nf90_fill_double
        end if
        ! Transformed
        if (me%nc%hasVariable('emissions_point_water_transformed')) then
            var = me%nc%getVariable('emissions_point_water_transformed')
            call var%getData(me%emissionsPointWaterTransformed)
            ! Get point source coords
            if (me%nc%hasVariable('emissions_point_water_transformed_coords')) then
                var = me%nc%getVariable('emissions_point_water_transformed_coords')
                call var%getData(me%emissionsPointWaterCoords)
            end if
        else
            allocate(me%emissionsPointWaterTransformed(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterTransformed = nf90_fill_double
        end if
        if (me%nc%hasVariable('emissions_point_water_dissolved')) then
            var = me%nc%getVariable('emissions_point_water_dissolved')
            call var%getData(me%emissionsPointWaterDissolved)
            ! Get point source coords
            if (me%nc%hasVariable('emissions_point_water_dissolved_coords')) then
                var = me%nc%getVariable('emissions_point_water_dissolved_coords')
                call var%getData(me%emissionsPointWaterCoords)
            end if
        else
            allocate(me%emissionsPointWaterDissolved(me%gridShape(1), me%gridShape(2), me%nTimesteps, maxPointSources))
            me%emissionsPointWaterDissolved = nf90_fill_double
        end if
        ! Emissions - point coordinates. We only need to use one form's point coords (they should
        ! all be the same), so we'll go through them all until we hit one that exists (in case
        ! we're only inputting a certain form)
        if ((maxPointSources > 0) .and. (.not. allocated(me%emissionsPointWaterCoords))) then
            call ERROR_HANDLER%trigger(error=ErrorInstance( &
                message="Unable to find coordinates for point sources in input data. Check point sources " // &
                        "have coordinates sidecar variables." &
            ))
        end if

        ! Calculate the number of point source per cell
        call me%calculateNPointSources(maxPointSources)

        ! SPATIAL 1D VARIABLES
        ! Land use                                  [-]
        var = me%nc%getVariable('land_use')
        call var%getData(me%landUse)

        ! Close the dataset
        call me%nc%close()

        call rslt%addToTrace('Initialising database')
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
        call LOGR%add("Initialising database: success")
    end subroutine

    subroutine parseConstantsDatabase(me, constantsFile)
        class(Database)         :: me
        character(len=*)        :: constantsFile
        integer                 :: nmlIOStat
        integer :: n
        integer :: n_default_nm_size_distribution, n_nm_size_classes, n_default_spm_size_distribution, &
            n_spm_size_classes, n_default_matrixembedded_distribution_to_spm, n_vertical_distribution, &
            n_initial_c_org, n_k_death, n_k_elim_np, n_k_growth, n_k_uptake_np, n_name, n_stored_fraction, &
            n_k_uptake_transformed, n_k_elim_transformed, n_biota, n_compartment, &
            n_k_uptake_dissolved, n_k_elim_dissolved, n_uptake_from_form, n_harvest_in_month
        integer :: arable, coniferous, deciduous, grassland, heathland, urban_capped, urban_gardens, urban_parks
        integer, allocatable :: default_nm_size_distribution(:), default_spm_size_distribution(:), &
            default_matrixembedded_distribution_to_spm(:), vertical_distribution(:), harvest_in_month(:)
        real, allocatable :: nm_size_classes(:), spm_size_classes(:), stored_fraction(:)
        real :: darcy_velocity, default_attachment_efficiency, default_porosity, particle_density
        real(dp) :: hamaker_constant
        real(dp), allocatable :: initial_C_org(:), k_growth(:), k_death(:), k_elim_np(:), k_uptake_np(:), &
            k_elim_transformed(:), k_uptake_transformed(:), k_uptake_dissolved(:), &
            k_elim_dissolved(:)
        character(len=100), allocatable :: name(:), compartment(:)
        character(len=17), allocatable :: uptake_from_form(:)
        namelist /allocatable_array_sizes/ n_default_nm_size_distribution, n_nm_size_classes, &
            n_default_spm_size_distribution, n_spm_size_classes, n_default_matrixembedded_distribution_to_spm, &
            n_vertical_distribution, n_initial_c_org, n_k_death, n_k_growth, n_name, n_stored_fraction, &
            n_k_uptake_np, n_k_elim_np, n_k_uptake_transformed, n_k_elim_transformed, &
            n_compartment, n_k_uptake_dissolved, n_k_elim_dissolved, &
            n_uptake_from_form, n_harvest_in_month
        namelist /n_biota_grp/ n_biota
        namelist /biota/ initial_C_org, k_death, k_growth, k_elim_np, k_uptake_np, name, &
            k_elim_transformed, k_uptake_transformed, stored_fraction, compartment, &
            k_uptake_dissolved, k_elim_dissolved, uptake_from_form, harvest_in_month
        namelist /earthworm_densities/ arable, coniferous, deciduous, grassland, heathland, urban_capped, urban_gardens, &
            urban_parks, vertical_distribution
        namelist /size_classes/ default_nm_size_distribution, nm_size_classes, default_spm_size_distribution, &
            spm_size_classes, default_matrixembedded_distribution_to_spm
        namelist /soil/ darcy_velocity, default_attachment_efficiency, default_porosity, hamaker_constant, particle_density

        print *, "start"

        ! Open and read the NML file
        open(11, file=constantsFile, status="old")
        read(11, nml=allocatable_array_sizes)
        rewind(11)

        print *, "alloc read done"

        ! Allocate the appropriate
        allocate(default_nm_size_distribution(n_default_nm_size_distribution), &
            nm_size_classes(n_nm_size_classes), &
            default_spm_size_distribution(n_default_spm_size_distribution), &
            spm_size_classes(n_spm_size_classes), &
            default_matrixembedded_distribution_to_spm(n_default_matrixembedded_distribution_to_spm), &
            vertical_distribution(n_vertical_distribution))

        print *, "mem allocated"

        read(11, nml=n_biota_grp, iostat=nmlIOStat)
        rewind(11)
        if (nmlIOStat .ge. 0) then
            allocate(initial_C_org(n_biota), k_death(n_biota), k_elim_np(n_biota), &
                k_uptake_np(n_biota), k_growth(n_biota), name(n_biota), &
                stored_fraction(n_biota), k_uptake_transformed(n_biota), &
                k_elim_transformed(n_biota), compartment(n_biota), &
                k_uptake_dissolved(n_biota), k_elim_dissolved(n_biota), &
                uptake_from_form(n_biota), harvest_in_month(n_biota))
            read(11, nml=biota)
            rewind(11)
            me%hasBiota = .true.
        end if

        print *, "nml stat checked, second mem alloc done"
        read(11, nml=earthworm_densities)
        rewind(11)
        read(11, nml=size_classes)
        rewind(11)
        read(11, nml=soil)
        close(11)

        ! Save these to class variables
        me%defaultNMSizeDistribution = default_nm_size_distribution / 100.0
        me%defaultSpmSizeDistribution = default_spm_size_distribution / 100.0
        me%nmSizeClasses = nm_size_classes
        me%spmSizeClasses = spm_size_classes
        me%defaultMatrixEmbeddedDistributionToSpm = default_matrixembedded_distribution_to_spm / 100.0
        me%nSizeClassesSpm = n_spm_size_classes
        me%nSizeClassesNM = n_nm_size_classes
        me%soilDarcyVelocity = darcy_velocity       ! TODO maybe calculate from water flow, though it doesn't massively affect alpha_att calc
        me%soilDefaultAttachmentEfficiency = default_attachment_efficiency
        me%soilDefaultPorosity = default_porosity
        me%soilHamakerConstant = hamaker_constant
        me%soilParticleDensity = particle_density

        print *, "soil done, earthworm next"
        ! Earthworm densities
        me%earthwormDensityArable = arable
        me%earthwormDensityConiferous = coniferous
        me%earthwormDensityDeciduous = deciduous
        me%earthwormDensityGrassland = grassland
        me%earthwormDensityHeathland = heathland
        me%earthwormDensityUrbanCapped = urban_capped
        me%earthwormDensityUrbanGardens = urban_gardens
        me%earthwormDensityUrbanParks = urban_parks

        print *, "before vert dist"
        me%earthwormVerticalDistribution = vertical_distribution / 100.0

        print *, "before biota"

        ! Biota
        if (me%hasBiota) then
            me%biotaName = name
            me%biotaInitial_C_org = initial_C_org
            me%biota_k_growth = k_growth
            me%biota_k_death = k_death
            me%biota_k_uptake_np = k_uptake_np
            me%biota_k_elim_np = k_elim_np
            me%biota_k_uptake_transformed = k_uptake_transformed
            me%biota_k_elim_transformed = k_elim_transformed
            me%biota_k_uptake_dissolved = k_uptake_dissolved
            me%biota_k_elim_dissolved = k_elim_dissolved
            me%biotaStoredFraction = stored_fraction
            me%nBiota = n_biota
            me%biotaCompartment = compartment
            me%biotaUptakeFromForm = uptake_from_form
            me%biotaHarvestInMonth = harvest_in_month
        end if

        print *, "before globals stuff"

        ! HACK to override Globals values, need to unify all this
        C%nSizeClassesSpm = me%nSizeClassesSPM
        C%nSizeClassesNP = me%nSizeClassesNM
        deallocate(C%d_np, C%d_spm, C%d_spm_low, C%d_spm_upp)
        C%d_np = me%nmSizeClasses
        C%d_spm = me%spmSizeClasses

        print *, "globals dealloc done"

        ! Set the number of size classes
        C%nSizeClassesSpm = size(C%d_spm)
        C%nSizeClassesNP = size(C%d_np)
        C%nFracCompsSpm = size(C%rho_spm)
        allocate(C%d_spm_low(C%nSizeClassesSpm))
        allocate(C%d_spm_upp(C%nSizeClassesSpm))

        print *, "globals realloc done"
        ! Set the upper and lower bounds of each size class, if treated as a distribution
        do n = 1, C%nSizeClassesSpm
            ! Set the upper and lower limit of the size class's distributions
            if (n == C%nSizeClassesSpm) then
                C%d_spm_upp(n) = 1                                              ! failsafe overall upper size limit
            else
                C%d_spm_upp(n) = C%d_spm(n+1) - (C%d_spm(n+1)-C%d_spm(n))/2     ! Halfway between d_1 and d_2
            end if                
        end do
        do n = 1, C%nSizeClassesSpm
            if (n == 1) then
                C%d_spm_low(n) = 0                                              ! Particles can be any size below d_upp,1
            else
                C%d_spm_low(n) = C%d_spm_upp(n-1)                               ! lower size boundary equals upper size boundary of lower size class
            end if
        end do        

        ! Array to store default NM and ionic array dimensions. NM:
        !   1: NP size class
        !   2: form (core, shell, coating, corona)
        !   3: state (free, bound, heteroaggregated)
        ! Ionic: Form (free ion, solution, adsorbed)
        C%npDim = [C%nSizeClassesNP, 4, C%nSizeClassesSpm + 2]
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
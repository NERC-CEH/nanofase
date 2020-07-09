!> The classDatabase module contains the Database type, which is responsible for
!! data input to the model, as well as a variable (DATASET) of type(Database), which
!! can be imported into other modules, thus making the data parsed by the Database type
!! accessible throughout the model.
module classDatabase
    use mo_netcdf
    use netcdf
    use DefaultsModule
    use Globals
    use ResultModule, only: Result
    use ErrorInstanceModule, only: ErrorInstance
    use classLogger, only: LOGR
    use UtilModule
    implicit none

    !> The Database type is responsible for data input to the model. It parses data
    !! from the NetCDF and constant namelist files.
    type, public :: Database
        type(NcDataset)     :: nc                               ! The NetCDF dataset
        ! CONSTANTS
        ! Nanomaterial
        real :: nmDensity                                       ! Density of the nanomaterial [kg/m3]
        real, allocatable :: nmSizeClasses(:)                   ! Diameter of each NM size class [m]
        real, allocatable :: defaultNMSizeDistribution(:)       ! Default distribution to split NM across size classes
        integer :: nSizeClassesNM                               ! Number of NM size classes
        ! Sediment
        real, allocatable :: defaultSpmSizeDistribution(:)      ! Default distribution to split SPM across size classes
        real, allocatable :: spmSizeClasses(:)                  ! Diameter of each SPM size class [m]
        real, allocatable :: defaultMatrixEmbeddedDistributionToSpm(:)  ! Default distribution to proportion matrix-embedded releases to SPM size classes
        integer :: nSizeClassesSpm                              ! Number of SPM size classes
        ! Soil
        real(dp) :: soilDarcyVelocity                           ! Darcy velocity in soil [m/s]
        real(dp) :: soilDefaultPorosity                         ! Default porosity [-]  ! TODO deprecate this in favour of spatially resolved porosity
        real(dp) :: soilHamakerConstant                         ! Hamaker constant for soil [J]
        real(dp) :: soilParticleDensity                         ! Particle density of soil [kg m-3]
        real(dp) :: soilErosivity_a1                            ! Erosivity a1 parameter [-]
        real(dp) :: soilErosivity_a2                            ! Erosivity a2 parameter [-]
        real(dp) :: soilErosivity_a3                            ! Erosivity a3 parameter [-]
        real(dp) :: soilErosivity_b                             ! Erosivity b parameter [-]
        real :: soilConstantAttachmentEfficiency                ! Attachment efficiency to soil matrix [-]
        ! Earthworms
        integer :: earthwormDensityArable                       ! Earthworm density arable [individuals/m2]
        integer :: earthwormDensityConiferous                   ! Earthworm density coniferous [individuals/m2]
        integer :: earthwormDensityDeciduous                    ! Earthworm density deciduous [individuals/m2]
        integer :: earthwormDensityGrassland                    ! Earthworm density grassland [individuals/m2]
        integer :: earthwormDensityHeathland                    ! Earthworm density heathland [individuals/m2]
        integer :: earthwormDensityUrbanCapped                  ! Earthworm density urban_capped [individuals/m2]
        integer :: earthwormDensityUrbanGardens                 ! Earthworm density urban_gardens [individuals/m2]
        integer :: earthwormDensityUrbanParks                   ! Earthworm density urban_parks [individuals/m2]
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
        ! Water
        real :: riverMeanderingFactor               ! Meandering factor for rivers (not estuaries) [-]
        real(dp) :: waterResuspensionAlpha          ! Resuspension parameter alpha
        real(dp) :: waterResuspensionBeta           ! Resuspension parameter beta
        real(dp) :: waterResuspensionAlphaEstuary   ! Resuspension parameter alpha for estuary
        real(dp) :: waterResuspensionBetaEstuary    ! Resuspension parameter beta for estuary
        real(dp) :: water_k_diss_pristine           ! Dissolution rate constant for pristine NM [/s]
        real(dp) :: water_k_diss_transformed        ! Dissolution rate constant for transformed NM [/s]
        real(dp) :: water_k_transform_pristine      ! Transformation rate constant for pristine NM [/s]
        real :: waterTemperature                    ! Average water temperature TODO use temporally varying water temp [deg C]
        real(dp) :: riverAttachmentEfficiency       ! Attachment efficiency for NM to SPM in rivers [-]
        ! Estuary
        real(dp) :: estuaryAttachmentEfficiency     ! Attachment efficiency for NM to SPM in estuaries [-]
        real :: estuaryTidalM2                      ! Estuary tidal harmonics parameter M2
        real :: estuaryTidalS2                      ! Estuary tidal harmonics parameter S2
        real :: estuaryMeanDepthExpA                ! Estuary mean depth exponential parameter A
        real :: estuaryMeanDepthExpB                ! Estuary mean depth exponential parameter B
        real :: estuaryWidthExpA                    ! Estuary width exponential parameter A
        real :: estuaryWidthExpB                    ! Estuary width exponential parameter B
        real :: estuaryMeanderingFactor             ! Estuary meandering factor, used to calculate distance to mouth [-]
        real :: estuaryMouthCoords(2)               ! Coordinates of the estuary mouth, used to calculate distance to mouth
        ! Sediment
        real, allocatable :: sedimentPorosity(:)    ! Porosity of the bed sediment layers [-]
        real(dp), allocatable :: sedimentInitialMass(:) ! Initial mass of each sediment size class [kg/m2]
        real, allocatable :: sedimentFractionalComposition(:) ! Distribution of sediment amongst fractional compositions [-]
        ! Grid and coordinate variables
        integer, allocatable :: gridShape(:)        ! Number of grid cells along each grid axis [-]
        real, allocatable :: gridRes(:)             ! Resolution of grid cells [m]
        real, allocatable :: gridBounds(:)          ! Bounding box of grid, indexed as left, bottom, right, top [m]
        logical, allocatable :: gridMask(:,:)       ! Logical mask for extent of grid [-]
        real, allocatable :: x(:)                   ! Centre of cell [m]
        real, allocatable :: x_l(:)                 ! Left side of cell [m]
        real, allocatable :: y(:)                   ! Centre of cell [m]
        real, allocatable :: y_u(:)                 ! Upper side of cell [m]
        integer, allocatable :: t(:)                ! Seconds since start date [s]
        integer :: nTimesteps                       ! Number of timesteps for the model run [-]
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
        real(dp), allocatable :: soilUsleCFactor(:,:)
        real(dp), allocatable :: soilUslePFactor(:,:)
        real(dp), allocatable :: soilUsleLSFactor(:,:)
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
        procedure, public :: update => updateDatabase
        procedure, public :: readBatchVariables => readBatchVariablesDatabase
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
        
        ! Open the dataset and parse constants NML file
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

        ! Read the variables that can be updated on each batch (i.e. not geographical)
        call me%readBatchVariables()

        ! Close the dataset
        call me%nc%close()

        call rslt%addToTrace('Initialising database')
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
        call LOGR%add("Initialising database: success")
    end subroutine

    !> Update the database based on data for a new chunk
    subroutine updateDatabase(me, k)
        class(Database) :: me                           !! This Database instance
        integer         :: k                            !! The index of this chunk, used to access correct config options

        ! Get the config options for this chunk
        C%inputFile = C%batchInputFiles(k)
        C%constantsFile = C%batchConstantFiles(k)
        C%nTimeSteps = C%batchNTimesteps(k)
        C%startDate = C%batchStartDates(k)

        ! Read in the new constants file
        call me%parseConstants(C%constantsFile)

        ! Open the new dataset
        me%nc = NcDataset(C%inputFile, 'r')
        ! Deallocate the previous chunk's variables
        deallocate(me%soilAttachmentRate)
        deallocate(me%soilAttachmentEfficiency)
        deallocate(me%emissionsArealSoilPristine)
        deallocate(me%emissionsArealSoilMatrixEmbedded)
        deallocate(me%emissionsArealSoilTransformed)
        deallocate(me%emissionsArealSoilDissolved)
        deallocate(me%emissionsArealWaterPristine)
        deallocate(me%emissionsArealWaterMatrixEmbedded)
        deallocate(me%emissionsArealWaterTransformed)
        deallocate(me%emissionsArealWaterDissolved)
        deallocate(me%emissionsAtmosphericDryDepoPristine)
        deallocate(me%emissionsAtmosphericDryDepoMatrixEmbedded)
        deallocate(me%emissionsAtmosphericDryDepoTransformed)
        deallocate(me%emissionsAtmosphericDryDepoDissolved)
        deallocate(me%emissionsAtmosphericWetDepoPristine)
        deallocate(me%emissionsAtmosphericWetDepoMatrixEmbedded)
        deallocate(me%emissionsAtmosphericWetDepoTransformed)
        deallocate(me%emissionsAtmosphericWetDepoDissolved)
        deallocate(me%emissionsPointWaterPristine)
        deallocate(me%emissionsPointWaterMatrixEmbedded)
        deallocate(me%emissionsPointWaterTransformed)
        deallocate(me%emissionsPointWaterDissolved)
        ! Read this chunk's variables
        call me%readBatchVariables()
        ! Close the dataset
        call me%nc%close()
    end subroutine

    !> Read variables in for the new chunk as part of a batch run
    subroutine readBatchVariablesDatabase(me)
        class(Database)     :: me
        type(NcVariable)    :: var
        type(NcDimension)   :: p_dim
        integer             :: maxPointSources

        ! Spatial data (grid setup, rivers etc) will stay the same between chunks,
        ! but the number of timesteps might not, so let's change that
        var = me%nc%getVariable('t')
        call var%getData(me%t)
        me%nTimesteps = size(me%t)

        ! SPATIOTEMPORAL VARIABLES
        ! If number of timesteps in this chunk is different, the getData() method
        ! will take care of reallocating the variable to the correct length. But
        ! we need to be careful to reallocate variables we don't get by getData
        ! (i.e. ones that aren't present in the data file)
        
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
            if (allocated(me%evap)) deallocate(me%evap)
            allocate(me%evap(me%gridShape(1), me%gridShape(2), C%nTimesteps))
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
        var = me%nc%getVariable('soil_usle_c_factor')
        call var%getData(me%soilUsleCFactor)
        var = me%nc%getVariable('soil_usle_ls_factor')
        call var%getData(me%soilUsleLSFactor)
        var = me%nc%getVariable('soil_usle_p_factor')
        call var%getData(me%soilUslePFactor)
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
            me%soilAttachmentEfficiency = me%soilConstantAttachmentEfficiency
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
    end subroutine

    subroutine parseConstantsDatabase(me, constantsFile)
        class(Database)         :: me                   !! This Database instance
        character(len=*)        :: constantsFile        !! The constants file path
        integer                 :: nmlIOStat            ! IO status for NML file
        integer :: n_default_nm_size_distribution, n_default_spm_size_distribution, &
            n_default_matrixembedded_distribution_to_spm, n_vertical_distribution, &
            n_initial_c_org, n_k_death, n_k_elim_np, n_k_growth, n_k_uptake_np, n_name, n_stored_fraction, &
            n_k_uptake_transformed, n_k_elim_transformed, n_biota, n_compartment, &
            n_k_uptake_dissolved, n_k_elim_dissolved, n_uptake_from_form, n_harvest_in_month, &
            n_porosity, n_initial_mass, &
            n_fractional_composition_distribution, n_estuary_mouth_coords, &
            arable, coniferous, deciduous, grassland, heathland, urban_capped, urban_gardens, urban_parks
        real :: estuary_mouth_coords(2)
        integer, allocatable :: default_nm_size_distribution(:), default_spm_size_distribution(:), &
            default_matrixembedded_distribution_to_spm(:), vertical_distribution(:), harvest_in_month(:)
        real, allocatable :: stored_fraction(:), &
            porosity(:), fractional_composition_distribution(:)
        real :: darcy_velocity, default_porosity, particle_density, &
            estuary_tidal_S2, estuary_mean_depth_expA, estuary_mean_depth_expB, estuary_width_expA, &
            estuary_width_expB, estuary_tidal_M2, estuary_meandering_factor, nm_density, river_meandering_factor, &
            water_temperature
        real(dp) :: hamaker_constant, resuspension_alpha, resuspension_beta, &
            resuspension_alpha_estuary, resuspension_beta_estuary, k_diss_pristine, k_diss_transformed, &
            k_transform_pristine, erosivity_a1, erosivity_a2, erosivity_a3, erosivity_b, &
            river_attachment_efficiency, estuary_attachment_efficiency, soil_attachment_efficiency
        real(dp), allocatable :: initial_C_org(:), k_growth(:), k_death(:), k_elim_np(:), k_uptake_np(:), &
            k_elim_transformed(:), k_uptake_transformed(:), k_uptake_dissolved(:), &
            k_elim_dissolved(:), initial_mass(:)
        character(len=100), allocatable :: name(:), compartment(:)
        character(len=17), allocatable :: uptake_from_form(:)

        ! Define the namelists and their variables
        namelist /allocatable_array_sizes/ n_default_nm_size_distribution, &
            n_default_spm_size_distribution, n_default_matrixembedded_distribution_to_spm, &
            n_vertical_distribution, n_initial_c_org, n_k_death, n_k_growth, n_name, n_stored_fraction, &
            n_k_uptake_np, n_k_elim_np, n_k_uptake_transformed, n_k_elim_transformed, &
            n_compartment, n_k_uptake_dissolved, n_k_elim_dissolved, &
            n_uptake_from_form, n_harvest_in_month, n_porosity, &
            n_initial_mass, n_fractional_composition_distribution, n_estuary_mouth_coords
        namelist /nanomaterial/ nm_density, default_nm_size_distribution
        namelist /n_biota_grp/ n_biota
        namelist /biota/ initial_C_org, k_death, k_growth, k_elim_np, k_uptake_np, name, &
            k_elim_transformed, k_uptake_transformed, stored_fraction, compartment, &
            k_uptake_dissolved, k_elim_dissolved, uptake_from_form, harvest_in_month
        namelist /earthworm_densities/ arable, coniferous, deciduous, grassland, heathland, urban_capped, urban_gardens, &
            urban_parks, vertical_distribution
        namelist /soil/ darcy_velocity, default_porosity, hamaker_constant, particle_density, &
            erosivity_a1, erosivity_a2, erosivity_a3, erosivity_b, soil_attachment_efficiency
        namelist /water/ resuspension_alpha, resuspension_beta, resuspension_alpha_estuary, resuspension_beta_estuary, &
            k_diss_pristine, k_diss_transformed, k_transform_pristine, estuary_tidal_m2, estuary_tidal_s2, estuary_mouth_coords, &
            estuary_mean_depth_expa, estuary_mean_depth_expb, estuary_width_expa, estuary_width_expb, estuary_meandering_factor, &
            river_meandering_factor, water_temperature, river_attachment_efficiency, estuary_attachment_efficiency
        namelist /sediment/ porosity, initial_mass, fractional_composition_distribution, &
            default_spm_size_distribution, default_matrixembedded_distribution_to_spm

        ! Open and read the NML file
        open(ioUnitConstants, file=constantsFile, status="old")
        read(ioUnitConstants, nml=allocatable_array_sizes)
        rewind(ioUnitConstants)

        ! Allocate the appropriate variable dimensions
        allocate(default_nm_size_distribution(n_default_nm_size_distribution), &
            default_spm_size_distribution(n_default_spm_size_distribution), &
            default_matrixembedded_distribution_to_spm(n_default_matrixembedded_distribution_to_spm), &
            vertical_distribution(n_vertical_distribution), &
            porosity(n_porosity), &
            initial_mass(n_initial_mass), &
            fractional_composition_distribution(n_fractional_composition_distribution) &
        )
        ! Allocate the class variables, first checking they're not already allocated (e.g. from a previous batch)
        if (.not. allocated(me%defaultNMSizeDistribution)) then
            allocate(me%defaultNMSizeDistribution(n_default_nm_size_distribution))
        end if
        if (.not. allocated(me%defaultSpmSizeDistribution)) then
            allocate(me%defaultSpmSizeDistribution(n_default_spm_size_distribution))
        end if
        if (.not. allocated(me%defaultMatrixEmbeddedDistributionToSpm)) then
            allocate(me%defaultMatrixEmbeddedDistributionToSpm(n_default_matrixembedded_distribution_to_spm))
        end if
        if (.not. allocated(me%earthwormVerticalDistribution)) then
            allocate(me%earthwormVerticalDistribution(n_vertical_distribution))
        end if
        if (.not. allocated(me%sedimentPorosity)) then
            allocate(me%sedimentPorosity(n_porosity))
        end if
        if (.not. allocated(me%sedimentInitialMass)) then
            allocate(me%sedimentInitialMass(n_initial_mass))
        end if
        if (.not. allocated(me%sedimentFractionalComposition)) then
            allocate(me%sedimentFractionalComposition(n_fractional_composition_distribution))
        end if

        ! Defaults, if the variable doesn't exist in namelist
        ! TODO move to separate defaults.nml file, or similar
        resuspension_alpha_estuary = 0.0_dp
        resuspension_beta_estuary = 0.0_dp
        soil_attachment_efficiency = defaultSoilAttachmentEfficiency
        k_diss_pristine = default_k_diss_pristine
        k_diss_transformed = default_k_diss_transformed
        k_transform_pristine = default_k_transform_pristine
        estuary_meandering_factor = defaultEstuaryMeanderingFactor
        river_meandering_factor = defaultRiverMeanderingFactor
        porosity = 0.0

        ! Read in the namelists
        read(ioUnitConstants, nml=n_biota_grp, iostat=nmlIOStat); rewind(ioUnitConstants)
        ! Only read in the biota group if there is one
        if (nmlIOStat .ge. 0) then
            allocate(initial_C_org(n_biota), k_death(n_biota), k_elim_np(n_biota), &
                k_uptake_np(n_biota), k_growth(n_biota), name(n_biota), &
                stored_fraction(n_biota), k_uptake_transformed(n_biota), &
                k_elim_transformed(n_biota), compartment(n_biota), &
                k_uptake_dissolved(n_biota), k_elim_dissolved(n_biota), &
                uptake_from_form(n_biota), harvest_in_month(n_biota))
            read(ioUnitConstants, nml=biota); rewind(ioUnitConstants)
            me%hasBiota = .true.
        end if
        read(ioUnitConstants, nml=nanomaterial); rewind(ioUnitConstants)
        read(ioUnitConstants, nml=earthworm_densities); rewind(ioUnitConstants)
        read(ioUnitConstants, nml=soil); rewind(ioUnitConstants)
        read(ioUnitConstants, nml=water); rewind(ioUnitConstants)
        read(ioUnitConstants, nml=sediment); rewind(ioUnitConstants)
        close(ioUnitConstants)

        ! Save these to class variables
        me%nmDensity = nm_density
        me%defaultNMSizeDistribution = default_nm_size_distribution / 100.0
        me%defaultSpmSizeDistribution = default_spm_size_distribution / 100.0
        me%defaultMatrixEmbeddedDistributionToSpm = default_matrixembedded_distribution_to_spm / 100.0
        me%soilDarcyVelocity = darcy_velocity       ! TODO maybe calculate from water flow, though it doesn't massively affect alpha_att calc
        me%soilDefaultPorosity = default_porosity
        me%soilHamakerConstant = hamaker_constant
        me%soilParticleDensity = particle_density
        me%soilConstantAttachmentEfficiency = soil_attachment_efficiency
        me%soilErosivity_a1 = erosivity_a1
        me%soilErosivity_a2 = erosivity_a2
        me%soilErosivity_a3 = erosivity_a3
        me%soilErosivity_b = erosivity_b
        ! Earthworm densities
        me%earthwormDensityArable = arable
        me%earthwormDensityConiferous = coniferous
        me%earthwormDensityDeciduous = deciduous
        me%earthwormDensityGrassland = grassland
        me%earthwormDensityHeathland = heathland
        me%earthwormDensityUrbanCapped = urban_capped
        me%earthwormDensityUrbanGardens = urban_gardens
        me%earthwormDensityUrbanParks = urban_parks
        me%earthwormVerticalDistribution = vertical_distribution / 100.0
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
        ! Water
        me%riverMeanderingFactor = river_meandering_factor
        me%waterResuspensionAlpha = resuspension_alpha
        me%waterResuspensionBeta = resuspension_beta
        me%water_k_diss_pristine = k_diss_pristine
        me%water_k_diss_transformed = k_diss_transformed
        me%water_k_transform_pristine = k_transform_pristine
        ! Check if estuary params have been provided, otherwise default to freshwater
        if (resuspension_alpha_estuary /= 0.0_dp) then
            me%waterResuspensionAlphaEstuary = resuspension_alpha_estuary
        else
            me%waterResuspensionAlphaEstuary = me%waterResuspensionAlpha
        end if
        if (resuspension_beta_estuary /= 0.0_dp) then
            me%waterResuspensionBetaEstuary = resuspension_beta_estuary
        else
            me%waterResuspensionBetaEstuary = me%waterResuspensionBeta
        end if
        me%waterTemperature = water_temperature
        me%riverAttachmentEfficiency = river_attachment_efficiency
        ! Estuary
        me%estuaryAttachmentEfficiency = estuary_attachment_efficiency
        me%estuaryTidalM2 = estuary_tidal_M2
        me%estuaryTidalS2 = estuary_tidal_S2
        me%estuaryMeanDepthExpA = estuary_mean_depth_expA
        me%estuaryMeanDepthExpB = estuary_mean_depth_expB
        me%estuaryWidthExpA = estuary_width_expA
        me%estuaryWidthExpB = estuary_width_expB
        me%estuaryMeanderingFactor = estuary_meandering_factor
        me%estuaryMouthCoords = estuary_mouth_coords
        ! Sediment
        me%sedimentInitialMass = initial_mass
        me%sedimentPorosity = porosity
        me%sedimentFractionalComposition = fractional_composition_distribution
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
        if (.not. allocated(me%nPointSources)) then
            allocate(me%nPointSources(me%gridShape(1), me%gridShape(2)))
        end if
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

    !> Check whether a set of coordinates (x,y) is in the model domain
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

    !> Convert easting and northing coordinates to cell indicies
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

    !> Convert easting and northing coordinates to fractional cell indicies,
    !! i.e. as a fraction of the position of the coords within the cell
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


! AUDITING TO DO:
!   array size checks, particularly variables for size classes of NM and SPM
!   sedimentPorosity: 0 <= x <= 1
!   water content at saturation is greater than water content at field capacity
!   are batch runs contiguous (dates follow on from each other for different chunks)
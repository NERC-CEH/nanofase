!> The DataInputModule contains the Database type, which is responsible for
!! data input to the model, as well as a variable (DATASET) of type(Database), which
!! can be imported into other modules, thus making the data parsed by the Database type
!! accessible throughout the model.
module DataInputModule
    use mo_netcdf
    use DefaultsModule
    use GlobalsModule, only: dp, C, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
    use ResultModule, only: Result
    use ErrorInstanceModule, only: ErrorInstance
    use LoggerModule, only: LOGR
    use UtilModule
    implicit none

    !> The Database type is responsible for data input to the model. It parses data
    !! from the NetCDF and constant namelist files.
    type, public :: Database
        type(NcDataset)     :: nc                               ! The NetCDF dataset

        ! CONSTANTS
        ! ---------
        ! Contaminant
        real(dp)          :: contaminantDensity                    ! Density of the contaminant [kg/m3]
        real(dp), allocatable :: contaminantSizeClasses(:)             ! Diameter of each contaminant size class [m]
        real, allocatable :: defaultDistributionContaminant(:) ! Default distribution to split contaminant across size classes
        real, allocatable :: defaultContaminantFormDistribution(:)
        integer           :: nContaminantSizeClasses               ! Number of contaminant size classes
        ! Sediment
        real, allocatable :: defaultSpmSizeDistribution(:)      ! Default distribution to split SPM across size classes
        real(dp), allocatable :: spmDensityBySizeClass(:)           ! Density of sediment in each size class [kg/m3]
        real(dp), allocatable :: spmSizeClasses(:)                  ! Diameter of each SPM size class [m]
        real, allocatable :: defaultMatrixEmbeddedDistributionToSpm(:)  ! Default distribution to proportion matrix-embedded releases to SPM size classes
        integer :: nSizeClassesSpm                              ! Number of SPM size classes
        real(dp) :: sedimentEnrichment_k                        ! Clay enrichment scaling factor
        real(dp) :: sedimentEnrichment_a                        ! Clay enrichment skew factor
        ! Soil
        real(dp) :: soilDarcyVelocity                           ! Darcy velocity in soil [m/s]
        real(dp) :: soilDefaultPorosity                         ! Default porosity [-]
        real(dp) :: soilHamakerConstant                         ! Hamaker constant for soil [J]
        real(dp) :: soilParticleDensity                         ! Particle density of soil [kg m-3]
        real(dp) :: soilErosivity_a1                            ! Erosivity a1 parameter [-]
        real(dp) :: soilErosivity_a2                            ! Erosivity a2 parameter [-]
        real(dp) :: soilErosivity_a3                            ! Erosivity a3 parameter [-]
        real(dp) :: soilErosivity_b                             ! Erosivity b parameter [-]
        real(dp) :: soilConstantAttachmentEfficiency            ! Attachment efficiency to soil matrix [-]
        real(dp) :: sedimentTransport_aConstant                 ! Sediment transport capacity a parameter (scaling factor) [kg/m2/km2]
        real(dp) :: sedimentTransport_bConstant                 ! Sediment transport capacity b parameter (overland flow threshold) [m2/s]
        real(dp) :: sedimentTransport_cConstant                 ! Sediment transport capacity c parameter (non-linear coefficient) [-]
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
        real(dp), allocatable :: biota_k_uptake_contaminant(:,:) ! Uptake rates for contaminant forms [nBiota, nContaminantForms]
        real(dp), allocatable :: biota_k_elim_contaminant(:,:)   ! Elimination rates for contaminant forms [nBiota, nContaminantForms]
        real(dp), allocatable :: biota_k_uptake_dissolved(:)     ! Uptake rate for dissolved contaminant
        real(dp), allocatable :: biota_k_elim_dissolved(:)       ! Elimination rate for dissolved contaminant
        real, allocatable :: biotaStoredFraction(:)
        character(len=17), allocatable :: biotaUptakeFromForm(:)
        integer, allocatable :: biotaHarvestInMonth(:)
        logical :: hasBiota = .false.
        integer :: nBiota = 0
        ! Water
        real     :: riverMeanderingFactor           ! Meandering factor for rivers (not estuaries) [-]
        real(dp) :: waterResuspensionAlpha          ! Resuspension parameter alpha
        real(dp) :: waterResuspensionBeta           ! Resuspension parameter beta
        real(dp) :: waterResuspensionAlphaEstuary   ! Resuspension parameter alpha for estuary
        real(dp) :: waterResuspensionBetaEstuary    ! Resuspension parameter beta for estuary
        real(dp) :: depositionAlphaConstant         ! Deposition parameter alpha - constant if spatial variable not supplied
        real(dp) :: depositionBetaConstant          ! Deposition parameter beta - constant if spatial variable not supplied
        real(dp) :: bankErosionAlphaConstant        ! Bank erosion parameter alpha - constant if spatial variable not supplied
        real(dp) :: bankErosionBetaConstant         ! Bank erosion parameter beta - constant if spatial variable not supplied
        real(dp) :: contaminant_k_diss_pristine     ! Dissolution rate constant for pristine contaminant [/s]
        real(dp) :: contaminant_k_diss_transformed  ! Dissolution rate constant for transformed contaminant [/s]
        real(dp) :: contaminant_k_transform_pristine ! Transformation rate constant for pristine contaminant [/s]
        real(dp) :: shearRate                       ! Shear rate [/s]
        real(dp) :: waterTemperature(366)           ! Temporally varying water temperature [deg C]
        real(dp) :: riverAttachmentEfficiency
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

        ! NETCDF VARIABLES
        ! ----------------
        ! Grid and coordinate variables
        integer, allocatable :: gridShape(:)        ! Number of grid cells along each grid axis [-]
        real, allocatable :: gridRes(:)             ! Resolution of grid cells [m]
        real, allocatable :: gridBounds(:)          ! Bounding box of grid, indexed as left, bottom, right, top [m]
        logical, allocatable :: gridMask(:,:)       ! Logical mask for extent of grid [-]
        character(len=2000) :: crsWKT               ! The well-known text of the CRS, to be saved to output data
        integer :: epsgCode                         ! The EPSG code of the CRS
        real, allocatable :: x(:)                   ! Centre of cell [m]
        real, allocatable :: x_l(:)                 ! Left side of cell [m]
        real, allocatable :: y(:)                   ! Centre of cell [m]
        real, allocatable :: y_u(:)                 ! Upper side of cell [m]
        integer, allocatable :: t(:)                ! Seconds since start date [s]
        integer :: nTimesteps                       ! Number of timesteps for the model run [-]
        integer, allocatable :: dem(:,:)            ! Digital elevation model [dm asl]
        ! Simulation mask
        logical, allocatable :: simulationMask(:,:) ! Boolean mask to apply to the simulation
        integer :: nNonMaskedCells                  ! Number of non-masked grid cells
        ! Routing variables
        integer, allocatable :: outflow(:,:,:)
        integer, allocatable :: inflows(:,:,:,:)
        logical, allocatable :: isHeadwater(:,:)
        integer, allocatable :: nWaterbodies(:,:)
        logical, allocatable :: isEstuary(:,:)
        integer :: maxNWaterbodies                  ! Maximum number of waterbodies per cell in the model domain
        ! Spatiotemporal variables
        real, allocatable :: runoff(:,:,:)
        real, allocatable :: quickflow(:,:,:)
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
        real(dp), allocatable :: resuspensionAlpha(:,:)
        real(dp), allocatable :: resuspensionBeta(:,:)
        real(dp), allocatable :: depositionAlpha(:,:)
        real(dp), allocatable :: depositionBeta(:,:)
        real(dp), allocatable :: bankErosionAlpha(:,:)
        real(dp), allocatable :: bankErosionBeta(:,:)
        real(dp), allocatable :: sedimentTransport_a(:,:)                       ! Sediment transport capacity a parameter (scaling factor) [kg/m2/km2]
        real(dp), allocatable :: sedimentTransport_b(:,:)                       ! Sediment transport capacity b parameter (overland flow threshold) [m2/s]
        real(dp), allocatable :: sedimentTransport_c(:,:)                       ! Sediment transport capacity c parameter (non-linear coefficient) [-]
        ! Initial concentrations
        real(dp), allocatable :: initialContaminantConcsSoil(:,:,:,:,:)
        real(dp), allocatable :: initialContaminantConcsWater(:,:,:,:,:)
        real(dp), allocatable :: initialContaminantConcsSediment(:,:,:,:,:)
        real(dp), allocatable :: initialDissolvedConcsSoil(:,:)
        real(dp), allocatable :: initialDissolvedConcsWater(:,:)
        real(dp), allocatable :: initialDissolvedConcsSediment(:,:)
        ! Emissions - areal
        real(dp), allocatable :: emissionsArealSoilContaminant(:,:,:,:,:)
        real(dp), allocatable :: emissionsArealWaterContaminant(:,:,:,:,:)
        real(dp), allocatable :: emissionsArealSoilDissolvedContaminant(:,:)
        real(dp), allocatable :: emissionsArealWaterDissolvedContaminant(:,:)
        ! Emissions - atmospheric depo
        real(dp), allocatable :: emissionsAtmosphericDryDepoContaminant(:,:,:,:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoContaminant(:,:,:,:,:,:)
        real(dp), allocatable :: emissionsAtmosphericDryDepoDissolvedContaminant(:,:,:)
        real(dp), allocatable :: emissionsAtmosphericWetDepoDissolvedContaminant(:,:,:)
        ! Emissions - point
        real(dp), allocatable :: emissionsPointWaterCoords(:,:,:,:)
        real(dp), allocatable :: emissionsPointWaterContaminant(:,:,:,:,:,:,:)  ! (x, y, t, p, size, form, state)
        real(dp), allocatable :: emissionsPointWaterDissolvedContaminant(:,:,:) 
        integer, allocatable :: nPointSources(:,:)
        integer :: maxPointSources                                              ! Maximum number of point sources in a cell in the whole environment
        ! Spatial 1D variables
        real, allocatable :: landUse(:,:,:)
      contains
        procedure, public   :: init => initDatabase
        procedure, public   :: update => updateDatabase
        procedure, public   :: readBatchVariables => readBatchVariablesDatabase
        procedure, private  :: parseConstants => parseConstantsDatabase
        procedure, private  :: mask => maskDatabase
        procedure, public   :: inModelDomain => inModelDomainDatabase
        procedure, private  :: calculateNPointSources => calculateNPointSourcesDatabase
        procedure, private  :: calculateMeanderingFactorFromCellSize => calculateMeanderingFactorFromCellSizeDatabase
        procedure, public   :: coordsToCellIndex => coordsToCellIndexDatabase
        procedure, public   :: coordsToFractionalCellIndex => coordsToFractionalCellIndexDatabase
        procedure, private  :: calculateWaterTemperatureTimeSeries => calculateWaterTemperatureTimeSeriesWaterBody
        ! Auditing
        procedure, private :: audit => auditDatabase
    end type

    type(Database) :: DATASET

  contains

    !> Initialise the database, by opening the NetCDF and constants files, and parsing
    !! their deposition
    subroutine initDatabase(me, inputFile, constantsFile)
        class(Database)     :: me
        type(NcDataset)     :: nc_simulationMask
        type(NcVariable)    :: var
        character(len=*)    :: inputFile
        character(len=*)    :: constantsFile
        type(Result)        :: rslt
        integer, allocatable :: isHeadwaterInt(:,:)     ! Temporary variable to store int before convert to bool
        integer, allocatable :: isEstuaryInt(:,:)
        integer, allocatable :: simulationMask(:,:)
        
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
        var = me%nc%getVariable('crs')
        call var%getAttribute('crs_wkt', me%crsWKT)

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
        me%maxNWaterbodies = maxval(me%nWaterbodies)
        ! If we're meant to be including the estuary, then get the is_estuary variable
        if (C%includeEstuary) then
            var = me%nc%getVariable('is_estuary')
            call var%getData(isEstuaryInt)
            me%isEstuary = ulgcl(isEstuaryInt)          ! Convert uint1 to logical
        else
            allocate(me%isEstuary(me%gridShape(1), me%gridShape(2)))
            me%isEstuary = .false.
        end if

        ! Use the nWaterbodies array to set the grid mask
        allocate(me%gridMask(me%gridShape(1), me%gridShape(2)))
        me%gridMask = me%mask(me%nWaterbodies)

        ! Meandering factors are set using grid resolution, if not present in constants,
        ! so they must be set after grid resolution pulled for NetCDF file (here), as
        ! opposed to in the constants parsing routine
        if (isZero(me%riverMeanderingFactor)) then
            me%riverMeanderingFactor = me%calculateMeanderingFactorFromCellSize()
        end if
        if (isZero(me%estuaryMeanderingFactor)) then
            me%estuaryMeanderingFactor = me%calculateMeanderingFactorFromCellSize()
        end if

        ! Read the variables that can be updated on each batch (i.e. not geographical)
        call me%readBatchVariables()

        ! Close the dataset
        call me%nc%close()

        ! Has a simulation mask been provided?
        if (C%hasSimulationMask) then
            nc_simulationMask = NcDataset(C%simulationMaskPath, 'r')
            var = nc_simulationMask%getVariable('simulation_mask')
            call var%getData(simulationMask)
            me%simulationMask = ulgcl(simulationMask)
            me%nNonMaskedCells = count(me%simulationMask)
        else
            allocate(me%simulationMask(me%gridShape(1), me%gridShape(2)))
            me%simulationMask = .true.
            me%nNonMaskedCells = count(.not. me%gridMask)
        end if

        ! Do the auditing
        call rslt%addErrors(.errors. me%audit())

        call rslt%addToTrace('Initialising database')
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
        call LOGR%toFile("Initialising database: success")
        call LOGR%toConsole("Initialising database: "//COLOR_GREEN//"success"//COLOR_RESET)
    end subroutine

    !> Update the database based on data for a new chunk (k), or for the only chunk if this
    !! isn't a batch run.
    subroutine updateDatabase(me, k)
        class(Database) :: me               !! This Database instance
        integer         :: k                !! The index of this chunk, used to access correct config options

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
        if (allocated(me%t)) deallocate(me%t)
        if (allocated(me%soilAttachmentRate)) deallocate(me%soilAttachmentRate)
        if (allocated(me%soilAttachmentEfficiency)) deallocate(me%soilAttachmentEfficiency)
        if (allocated(me%emissionsArealSoilContaminant)) deallocate(me%emissionsArealSoilContaminant)
        if (allocated(me%emissionsArealWaterContaminant)) deallocate(me%emissionsArealWaterContaminant)
        if (allocated(me%emissionsAtmosphericDryDepoContaminant)) deallocate(me%emissionsAtmosphericDryDepoContaminant)
        if (allocated(me%emissionsAtmosphericWetDepoContaminant)) deallocate(me%emissionsAtmosphericWetDepoContaminant)
        if (allocated(me%emissionsPointWaterContaminant)) deallocate(me%emissionsPointWaterContaminant)
        if (allocated(me%emissionsPointWaterCoords)) deallocate(me%emissionsPointWaterCoords)
        if (allocated(me%resuspensionAlpha)) deallocate(me%resuspensionAlpha)
        if (allocated(me%resuspensionBeta)) deallocate(me%resuspensionBeta)
        if (allocated(me%depositionAlpha)) deallocate(me%depositionAlpha)
        if (allocated(me%depositionBeta)) deallocate(me%depositionBeta)
        if (allocated(me%bankErosionAlpha)) deallocate(me%bankErosionAlpha)
        if (allocated(me%bankErosionBeta)) deallocate(me%bankErosionBeta)
        if (allocated(me%sedimentTransport_a)) deallocate(me%sedimentTransport_a)
        if (allocated(me%sedimentTransport_b)) deallocate(me%sedimentTransport_b)
        if (allocated(me%sedimentTransport_c)) deallocate(me%sedimentTransport_c)
        if (allocated(me%initialContaminantConcsSoil)) deallocate(me%initialContaminantConcsSoil)
        if (allocated(me%initialContaminantConcsWater)) deallocate(me%initialContaminantConcsWater)
        if (allocated(me%initialContaminantConcsSediment)) deallocate(me%initialContaminantConcsSediment)
        if (allocated(me%initialDissolvedConcsSoil)) deallocate(me%initialDissolvedConcsSoil)
        if (allocated(me%initialDissolvedConcsWater)) deallocate(me%initialDissolvedConcsWater)
        if (allocated(me%initialDissolvedConcsSediment)) deallocate(me%initialDissolvedConcsSediment)
        if (allocated(me%emissionsArealSoilDissolvedContaminant)) deallocate(me%emissionsArealSoilDissolvedContaminant)
        if (allocated(me%emissionsArealWaterDissolvedContaminant)) deallocate(me%emissionsArealWaterDissolvedContaminant)
        if (allocated(me%emissionsAtmosphericDryDepoDissolvedContaminant)) &
            deallocate(me%emissionsAtmosphericDryDepoDissolvedContaminant)
        if (allocated(me%emissionsAtmosphericWetDepoDissolvedContaminant)) &
            deallocate(me%emissionsAtmosphericWetDepoDissolvedContaminant)
        if (allocated(me%emissionsPointWaterDissolvedContaminant)) &
            deallocate(me%emissionsPointWaterDissolvedContaminant)

        ! Read this chunk's variables
        call me%readBatchVariables()
        
        ! Close the dataset
        call me%nc%close()
    end subroutine

    !> Read variables in for the new chunk as part of a batch run
    subroutine readBatchVariablesDatabase(me)
        class(Database)     :: me               ! This Database instance
        type(NcVariable)    :: var              ! NetCDF variable
        type(NcDimension)   :: p_dim            ! NetCDF dimensions for point sources
        integer             :: x, y, n, s, f, st, p  ! Grid cell and dimension iterators
        integer             :: alloc_stat       ! Allocation status
        character(len=256)  :: varname          ! Variable name for logging
        real(dp), allocatable :: temp_array(:,:,:)  ! Temporary array for reading 3D NetCDF data
        real(dp), allocatable :: temp_array_4d(:,:,:,:)  ! Temporary array for reading 4D NetCDF data

        ! Allocate emissions arrays
        allocate( &
            me%emissionsArealSoilContaminant(me%gridShape(1), me%gridShape(2), &
                C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
            me%emissionsArealWaterContaminant(me%gridShape(1), me%gridShape(2), &
                C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
            me%emissionsAtmosphericDryDepoContaminant(me%gridShape(1), me%gridShape(2), &
                C%nTimesteps, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
            me%emissionsAtmosphericWetDepoContaminant(me%gridShape(1), me%gridShape(2), &
                C%nTimesteps, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
            me%emissionsPointWaterContaminant(me%gridShape(1), me%gridShape(2), C%nTimesteps, &
                me%maxPointSources, C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
            stat=alloc_stat)
        if (alloc_stat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(message="Emission allocation failed"))
        end if

        ! Initialize with zeros
        me%emissionsArealSoilContaminant = 0.0_dp
        me%emissionsArealWaterContaminant = 0.0_dp
        me%emissionsAtmosphericDryDepoContaminant = 0.0_dp
        me%emissionsAtmosphericWetDepoContaminant = 0.0_dp
        me%emissionsPointWaterContaminant = 0.0_dp

        ! Soil emissions
        varname = "emissions_areal_soil_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do st = 1, C%contaminantDim(3)
                call var%getData(temp_array_4d)
                me%emissionsArealSoilContaminant(:,:,:,:,st) = temp_array_4d
            end do
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing "//trim(varname)//"; trying old variable names", &
                isCritical=.false.)])
            if (me%nc%hasVariable('emissions_areal_soil_pristine')) then
                var = me%nc%getVariable('emissions_areal_soil_pristine')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealSoilContaminant(:,:,1,1,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealSoilContaminant(:,:,n,1,FREE_CONTAMINANT) = &
                        me%emissionsArealSoilContaminant(:,:,1,1,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_areal_soil_matrixembedded')) then
                var = me%nc%getVariable('emissions_areal_soil_matrixembedded')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealSoilContaminant(:,:,1,2,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealSoilContaminant(:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsArealSoilContaminant(:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_areal_soil_transformed')) then
                var = me%nc%getVariable('emissions_areal_soil_transformed')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealSoilContaminant(:,:,1,2,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealSoilContaminant(:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsArealSoilContaminant(:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
        end if

        varname = "emissions_areal_soil_dissolved_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%emissionsArealSoilDissolvedContaminant = temp_array(:,:,1)
            deallocate(temp_array)
        else
            allocate(me%emissionsArealSoilDissolvedContaminant(me%gridShape(1), me%gridShape(2)))
            if (me%nc%hasVariable('emissions_areal_soil_dissolved')) then
                var = me%nc%getVariable('emissions_areal_soil_dissolved')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealSoilDissolvedContaminant = temp_array(:,:,1)
                deallocate(temp_array)
                call LOGR%toFile(errors=[ErrorInstance( &
                    message="Using legacy variable 'emissions_areal_soil_dissolved'", &
                    isCritical=.false.)])
            else
                me%emissionsArealSoilDissolvedContaminant = 0.0_dp
            end if
        end if

        varname = "emissions_areal_water_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do st = 1, C%contaminantDim(3)
                call var%getData(temp_array_4d)
                me%emissionsArealWaterContaminant(:,:,:,:,st) = temp_array_4d
            end do
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing "//trim(varname)//"; trying old variable names", &
                isCritical=.false.)])
            if (me%nc%hasVariable('emissions_areal_water_pristine')) then
                var = me%nc%getVariable('emissions_areal_water_pristine')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealWaterContaminant(:,:,1,1,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealWaterContaminant(:,:,n,1,FREE_CONTAMINANT) = &
                        me%emissionsArealWaterContaminant(:,:,1,1,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_areal_water_matrixembedded')) then
                var = me%nc%getVariable('emissions_areal_water_matrixembedded')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealWaterContaminant(:,:,1,2,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealWaterContaminant(:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsArealWaterContaminant(:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_areal_water_transformed')) then
                var = me%nc%getVariable('emissions_areal_water_transformed')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealWaterContaminant(:,:,1,2,FREE_CONTAMINANT) = temp_array(:,:,1)
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsArealWaterContaminant(:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsArealWaterContaminant(:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
        end if

        varname = "emissions_areal_water_dissolved_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%emissionsArealWaterDissolvedContaminant = temp_array(:,:,1)
            deallocate(temp_array)
        else
            allocate(me%emissionsArealWaterDissolvedContaminant(me%gridShape(1), me%gridShape(2)))
            if (me%nc%hasVariable('emissions_areal_water_dissolved')) then
                var = me%nc%getVariable('emissions_areal_water_dissolved')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsArealWaterDissolvedContaminant = temp_array(:,:,1)
                deallocate(temp_array)
                call LOGR%toFile(errors=[ErrorInstance( &
                    message="Using legacy variable 'emissions_areal_water_dissolved'", &
                    isCritical=.false.)])
            else
                me%emissionsArealWaterDissolvedContaminant = 0.0_dp
            end if
        end if

        varname = "emissions_atmospheric_drydepo_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%nTimesteps, C%contaminantDim(1)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do f = 1, C%contaminantDim(2)
                do st = 1, C%contaminantDim(3)
                    call var%getData(temp_array_4d)
                    me%emissionsAtmosphericDryDepoContaminant(:,:,:,:,f,st) = temp_array_4d
                end do
            end do
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing "//trim(varname)//"; trying old variable names", &
                isCritical=.false.)])
            if (me%nc%hasVariable('emissions_atmospheric_drydepo_pristine')) then
                var = me%nc%getVariable('emissions_atmospheric_drydepo_pristine')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,1,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,1,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,1,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_atmospheric_drydepo_matrixembedded')) then
                var = me%nc%getVariable('emissions_atmospheric_drydepo_matrixembedded')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_atmospheric_drydepo_transformed')) then
                var = me%nc%getVariable('emissions_atmospheric_drydepo_transformed')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
        end if

        varname = "emissions_atmospheric_drydepo_dissolved_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%emissionsAtmosphericDryDepoDissolvedContaminant = temp_array
            deallocate(temp_array)
        else
            allocate(me%emissionsAtmosphericDryDepoDissolvedContaminant( &
                me%gridShape(1), me%gridShape(2), C%nTimesteps))
            if (me%nc%hasVariable('emissions_atmospheric_drydepo_dissolved')) then
                var = me%nc%getVariable('emissions_atmospheric_drydepo_dissolved')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericDryDepoDissolvedContaminant = temp_array
                deallocate(temp_array)
                call LOGR%toFile(errors=[ErrorInstance( &
                    message="Using legacy variable 'emissions_atmospheric_drydepo_dissolved'", &
                    isCritical=.false.)])
            else
                me%emissionsAtmosphericDryDepoDissolvedContaminant = 0.0_dp
            end if
        end if

        varname = "emissions_atmospheric_wetdepo_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%nTimesteps, C%contaminantDim(1)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do f = 1, C%contaminantDim(2)
                do st = 1, C%contaminantDim(3)
                    call var%getData(temp_array_4d)
                    me%emissionsAtmosphericWetDepoContaminant(:,:,:,:,f,st) = temp_array_4d
                end do
            end do
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing "//trim(varname)//"; trying old variable names", &
                isCritical=.false.)])
            if (me%nc%hasVariable('emissions_atmospheric_wetdepo_pristine')) then
                var = me%nc%getVariable('emissions_atmospheric_wetdepo_pristine')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,1,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,1,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,1,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_atmospheric_wetdepo_matrixembedded')) then
                var = me%nc%getVariable('emissions_atmospheric_wetdepo_matrixembedded')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_atmospheric_wetdepo_transformed')) then
                var = me%nc%getVariable('emissions_atmospheric_wetdepo_transformed')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) = temp_array
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
        end if

        varname = "emissions_atmospheric_wetdepo_dissolved_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%emissionsAtmosphericWetDepoDissolvedContaminant = temp_array
            deallocate(temp_array)
        else
            allocate(me%emissionsAtmosphericWetDepoDissolvedContaminant( &
                me%gridShape(1), me%gridShape(2), C%nTimesteps))
            if (me%nc%hasVariable('emissions_atmospheric_wetdepo_dissolved')) then
                var = me%nc%getVariable('emissions_atmospheric_wetdepo_dissolved')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsAtmosphericWetDepoDissolvedContaminant = temp_array
                deallocate(temp_array)
                call LOGR%toFile(errors=[ErrorInstance( &
                    message="Using legacy variable 'emissions_atmospheric_wetdepo_dissolved'", &
                    isCritical=.false.)])
            else
                me%emissionsAtmosphericWetDepoDissolvedContaminant = 0.0_dp
            end if
        end if

        if (me%nc%hasDimension('p')) then
            p_dim = me%nc%getDimension('p')
            me%maxPointSources = p_dim%getLength()
        else
            me%maxPointSources = 0
        end if
        allocate(me%emissionsPointWaterContaminant(me%gridShape(1), me%gridShape(2), C%nTimesteps, me%maxPointSources, &
                                                C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)), &
                stat=alloc_stat)
        if (alloc_stat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(message="Point source emission allocation failed"))
        end if
        me%emissionsPointWaterContaminant = 0.0_dp

        varname = "emissions_point_water_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%nTimesteps, me%maxPointSources), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do s = 1, C%contaminantDim(1)
                do f = 1, C%contaminantDim(2)
                    do st = 1, C%contaminantDim(3)
                        call var%getData(temp_array_4d)
                        me%emissionsPointWaterContaminant(:,:,:,:,s,f,st) = temp_array_4d
                    end do
                end do
            end do
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing "//trim(varname)//"; trying old variable names", &
                isCritical=.false.)])
            if (me%nc%hasVariable('emissions_point_water_pristine')) then
                var = me%nc%getVariable('emissions_point_water_pristine')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                do p = 1, me%maxPointSources
                    me%emissionsPointWaterContaminant(:,:,:,p,1,1,FREE_CONTAMINANT) = temp_array
                end do
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsPointWaterContaminant(:,:,:,:,n,1,FREE_CONTAMINANT) = &
                        me%emissionsPointWaterContaminant(:,:,:,:,1,1,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_point_water_matrixembedded')) then
                var = me%nc%getVariable('emissions_point_water_matrixembedded')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                do p = 1, me%maxPointSources
                    me%emissionsPointWaterContaminant(:,:,:,p,1,2,FREE_CONTAMINANT) = temp_array
                end do
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsPointWaterContaminant(:,:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsPointWaterContaminant(:,:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
            if (me%nc%hasVariable('emissions_point_water_transformed')) then
                var = me%nc%getVariable('emissions_point_water_transformed')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                do p = 1, me%maxPointSources
                    me%emissionsPointWaterContaminant(:,:,:,p,1,2,FREE_CONTAMINANT) = temp_array
                end do
                deallocate(temp_array)
                do n = 2, C%nContaminantSizeClasses
                    me%emissionsPointWaterContaminant(:,:,:,:,n,2,FREE_CONTAMINANT) = &
                        me%emissionsPointWaterContaminant(:,:,:,:,1,2,FREE_CONTAMINANT) * &
                        me%defaultDistributionContaminant(n)
                end do
            end if
        end if

        varname = "emissions_point_water_dissolved_contaminant"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%emissionsPointWaterDissolvedContaminant = temp_array
            deallocate(temp_array)
        else
            allocate(me%emissionsPointWaterDissolvedContaminant( &
                me%gridShape(1), me%gridShape(2), C%nTimesteps))
            if (me%nc%hasVariable('emissions_point_water_dissolved')) then
                var = me%nc%getVariable('emissions_point_water_dissolved')
                allocate(temp_array(me%gridShape(1), me%gridShape(2), C%nTimesteps), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
                end if
                call var%getData(temp_array)
                me%emissionsPointWaterDissolvedContaminant = temp_array
                deallocate(temp_array)
                call LOGR%toFile(errors=[ErrorInstance( &
                    message="Using legacy variable 'emissions_point_water_dissolved'", &
                    isCritical=.false.)])
            else
                me%emissionsPointWaterDissolvedContaminant = 0.0_dp
            end if
        end if

        varname = "emissions_point_water_contaminant_coords"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), me%maxPointSources, 2), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            call var%getData(temp_array_4d)
            me%emissionsPointWaterCoords = temp_array_4d
            deallocate(temp_array_4d)
        else
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing emissions_point_water_contaminant_coords", &
                isCritical=.true.)])
            me%maxPointSources = 0
        end if

        ! Initial concentrations
        varname = "initial_contaminant_concentrations_soil"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do st = 1, C%contaminantDim(3)
                call var%getData(temp_array_4d)
                me%initialContaminantConcsSoil(:,:,:,:,st) = temp_array_4d
            end do
            deallocate(temp_array_4d)
        else
            allocate(me%initialContaminantConcsSoil( &
                me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
            me%initialContaminantConcsSoil = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_contaminant_concentrations_soil; using zero", &
                isCritical=.false.)])
        end if

        varname = "initial_dissolved_concentrations_soil"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%initialDissolvedConcsSoil = temp_array(:,:,1)
            deallocate(temp_array)
        else
            allocate(me%initialDissolvedConcsSoil(me%gridShape(1), me%gridShape(2)))
            me%initialDissolvedConcsSoil = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_dissolved_concentrations_soil; using zero", &
                isCritical=.false.)])
        end if

        varname = "initial_contaminant_concentrations_water"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do st = 1, C%contaminantDim(3)
                call var%getData(temp_array_4d)
                me%initialContaminantConcsWater(:,:,:,:,st) = temp_array_4d
            end do
            deallocate(temp_array_4d)
        else
            allocate(me%initialContaminantConcsWater( &
                me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
            me%initialContaminantConcsWater = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_contaminant_concentrations_water; using zero", &
                isCritical=.false.)])
        end if

        varname = "initial_dissolved_concentrations_water"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%initialDissolvedConcsWater = temp_array(:,:,1)
            deallocate(temp_array)
        else
            allocate(me%initialDissolvedConcsWater(me%gridShape(1), me%gridShape(2)))
            me%initialDissolvedConcsWater = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_dissolved_concentrations_water; using zero", &
                isCritical=.false.)])
        end if

        varname = "initial_contaminant_concentrations_sediment"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array_4d(me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary 4D array allocation failed"))
            end if
            do st = 1, C%contaminantDim(3)
                call var%getData(temp_array_4d)
                me%initialContaminantConcsSediment(:,:,:,:,st) = temp_array_4d
            end do
            deallocate(temp_array_4d)
        else
            allocate(me%initialContaminantConcsSediment( &
                me%gridShape(1), me%gridShape(2), C%contaminantDim(1), C%contaminantDim(2), C%contaminantDim(3)))
            me%initialContaminantConcsSediment = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_contaminant_concentrations_sediment; using zero", &
                isCritical=.false.)])
        end if

        varname = "initial_dissolved_concentrations_sediment"
        if (me%nc%hasVariable(varname)) then
            var = me%nc%getVariable(varname)
            allocate(temp_array(me%gridShape(1), me%gridShape(2), 1), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(message="Temporary array allocation failed"))
            end if
            call var%getData(temp_array)
            me%initialDissolvedConcsSediment = temp_array(:,:,1)
            deallocate(temp_array)
        else
            allocate(me%initialDissolvedConcsSediment(me%gridShape(1), me%gridShape(2)))
            me%initialDissolvedConcsSediment = 0.0_dp
            call LOGR%toFile(errors=[ErrorInstance( &
                message="Missing initial_dissolved_concentrations_sediment; using zero", &
                isCritical=.false.)])
        end if

    end subroutine readBatchVariablesDatabase

    !> Get the constants from the namelist file
    subroutine parseConstantsDatabase(me, constantsFile)
        class(Database)         :: me
        character(len=*)        :: constantsFile
        integer                 :: nmlIOStat
        character(len=256)      :: nmlIOMsg
        integer :: n_biota, n_contaminant_size_classes, n_default_spm_size_distribution, &
                n_default_matrixembedded_distribution_to_spm, n_vertical_distribution, &
                n_initial_c_org, n_k_growth, n_k_uptake_contaminant, n_k_elim_contaminant, &
                n_name, n_stored_fraction, n_k_uptake_dissolved, n_k_elim_dissolved, &
                n_uptake_from_form, n_harvest_in_month, n_porosity, n_initial_mass, &
                n_spm_density_by_size_class, n_fractional_composition_distribution, &
                n_estuary_mouth_coords, n_compartment, n_default_contaminant_size_distribution, &
                n_default_contaminant_form_distribution
        real :: estuary_mouth_coords(2)
        integer, allocatable :: default_contaminant_size_distribution(:), default_spm_size_distribution(:), &
                            default_matrixembedded_distribution_to_spm(:), vertical_distribution(:), &
                            harvest_in_month(:)
        real, allocatable :: stored_fraction(:), porosity(:), sedimentInitialMass(:), &
                            fractional_composition_distribution(:), spm_density_by_size_class(:), &
                            default_contaminant_form_distribution(:)
        real :: darcy_velocity, default_porosity, particle_density, estuary_tidal_S2, &
                estuary_mean_depth_expA, estuary_mean_depth_expB, estuary_width_expA, &
                estuary_width_expB, estuary_tidal_M2, estuary_meandering_factor, &
                river_meandering_factor, deposition_alpha, deposition_beta, &
                bank_erosion_alpha, bank_erosion_beta
        real(dp) :: hamaker_constant, resuspension_alpha, resuspension_beta, &
                    resuspension_alpha_estuary, resuspension_beta_estuary, k_diss_pristine, &
                    k_diss_transformed, k_transform_pristine, erosivity_a1, erosivity_a2, &
                    erosivity_a3, erosivity_b, contaminant_density, estuary_attachment_efficiency, &
                    soil_constant_attachment_efficiency, river_attachment_efficiency, &
                    sediment_transport_a, sediment_transport_b, sediment_transport_c, &
                    sediment_enrichment_k, sediment_enrichment_a, min_water_temperature, &
                    max_water_temperature, shear_rate
        real(dp), allocatable :: initial_C_org(:), k_growth(:), k_uptake_contaminant(:), &
                                k_elim_contaminant(:), k_uptake_dissolved(:), k_elim_dissolved(:), &
                                contaminant_size_classes(:)
        character(len=100), allocatable :: name(:), compartment(:)
        character(len=17), allocatable :: uptake_from_form(:)
        integer :: min_water_temperature_day_of_year, arable, coniferous, deciduous, grassland, &
                heathland, urban_capped, urban_gardens, urban_parks

        namelist /allocatable_array_sizes/ n_default_contaminant_size_distribution, &
            n_default_spm_size_distribution, n_default_matrixembedded_distribution_to_spm, &
            n_vertical_distribution, n_initial_c_org, n_k_growth, n_name, &
            n_stored_fraction, n_k_uptake_contaminant, n_k_elim_contaminant, &
            n_compartment, n_k_uptake_dissolved, n_k_elim_dissolved, n_uptake_from_form, &
            n_harvest_in_month, n_porosity, n_spm_density_by_size_class, &
            n_initial_mass, n_fractional_composition_distribution, n_estuary_mouth_coords, &
            n_contaminant_size_classes, n_default_contaminant_form_distribution
        namelist /n_biota_grp/ n_biota
        namelist /contaminant/ contaminant_density, default_contaminant_size_distribution, &
            contaminant_size_classes, k_diss_pristine, k_diss_transformed, k_transform_pristine, &
            default_contaminant_form_distribution
        namelist /biota/ initial_C_org, k_growth, k_elim_contaminant, k_uptake_contaminant, &
            name, stored_fraction, compartment, k_uptake_dissolved, k_elim_dissolved, &
            uptake_from_form, harvest_in_month
        namelist /earthworm_densities/ arable, coniferous, deciduous, grassland, heathland, &
            urban_capped, urban_gardens, urban_parks, vertical_distribution
        namelist /soil/ darcy_velocity, default_porosity, hamaker_constant, particle_density, &
            erosivity_a1, erosivity_a2, erosivity_a3, erosivity_b, soil_constant_attachment_efficiency, &
            sediment_transport_a, sediment_transport_b, sediment_transport_c
        namelist /water/ resuspension_alpha, resuspension_beta, resuspension_alpha_estuary, &
            resuspension_beta_estuary, estuary_tidal_m2, estuary_tidal_s2, estuary_mouth_coords, &
            estuary_mean_depth_expa, estuary_mean_depth_expb, estuary_width_expa, estuary_width_expb, &
            estuary_meandering_factor, river_meandering_factor, river_attachment_efficiency, &
            estuary_attachment_efficiency, deposition_alpha, deposition_beta, bank_erosion_alpha, &
            bank_erosion_beta, shear_rate, min_water_temperature, max_water_temperature, &
            min_water_temperature_day_of_year
        namelist /sediment/ porosity, sedimentInitialMass, fractional_composition_distribution, &
            default_spm_size_distribution, default_matrixembedded_distribution_to_spm, &
            sediment_enrichment_a, sediment_enrichment_k, spm_density_by_size_class

        ! Initialize variables
        n_biota = 0
        n_default_contaminant_form_distribution = 0
        contaminant_density = default_rho_contaminant
        k_diss_pristine = default_k_diss_pristine
        k_diss_transformed = default_k_diss_transformed
        k_transform_pristine = default_k_transform_pristine
        soil_constant_attachment_efficiency = defaultSoilAttachmentEfficiency
        river_attachment_efficiency = defaultRiverAttachmentEfficiency
        estuary_attachment_efficiency = defaultSoilAttachmentEfficiency
        resuspension_alpha_estuary = 0.0_dp
        resuspension_beta_estuary = 0.0_dp
        soil_constant_attachment_efficiency = real(defaultSoilAttachmentEfficiency, dp)
        river_attachment_efficiency = real(defaultRiverAttachmentEfficiency, dp)
        estuary_attachment_efficiency = defaultEstuaryAttachmentEfficiency
        darcy_velocity = defaultSoilDarcyVelocity
        k_diss_pristine = default_k_diss_pristine
        k_diss_transformed = default_k_diss_transformed
        k_transform_pristine = default_k_transform_pristine
        estuary_meandering_factor = 0.0
        river_meandering_factor = 0.0
        shear_rate = defaultShearRate
        min_water_temperature = defaultMinWaterTemperature
        max_water_temperature = defaultMaxWaterTemperature
        min_water_temperature_day_of_year = defaultMinWaterTemperatureDayOfYear
        sediment_transport_a = defaultSedimentTransport_a
        sediment_transport_b = defaultSedimentTransport_b
        sediment_transport_c = defaultSedimentTransport_c
        sediment_enrichment_k = defaultSedimentEnrichment_k
        sediment_enrichment_a = defaultSedimentEnrichment_a
        deposition_alpha = defaultDepositionAlpha
        deposition_beta = defaultDepositionBeta
        bank_erosion_alpha = defaultBankErosionAlpha
        bank_erosion_beta = defaultBankErosionBeta

        ! Open and read the NML file
        open(iouConstants, file=constantsFile, status="old", iostat=nmlIOStat)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance( &
                code=200, message="Failed to open constants file: " // trim(constantsFile)))
            return
        end if
        read(iouConstants, nml=allocatable_array_sizes, iostat=nmlIOStat)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance( &
                code=200, message="Failed to read allocatable_array_sizes namelist"))
            close(iouConstants)
            return
        end if
        rewind(iouConstants)

        ! Allocate arrays
        allocate(default_contaminant_size_distribution(n_default_contaminant_size_distribution), &
                default_spm_size_distribution(n_default_spm_size_distribution), &
                default_matrixembedded_distribution_to_spm(n_default_matrixembedded_distribution_to_spm), &
                vertical_distribution(n_vertical_distribution), &
                porosity(n_porosity), &
                sedimentInitialMass(n_initial_mass), &
                fractional_composition_distribution(n_fractional_composition_distribution), &
                spm_density_by_size_class(n_spm_density_by_size_class), &
                contaminant_size_classes(n_contaminant_size_classes), &
                default_contaminant_form_distribution(n_default_contaminant_form_distribution))

        ! Allocate class variables
        if (.not. allocated(me%defaultDistributionContaminant)) then
            allocate(me%defaultDistributionContaminant(n_default_contaminant_size_distribution))
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
        if (.not. allocated(me%contaminantSizeClasses)) then
            allocate(me%contaminantSizeClasses(n_contaminant_size_classes))
        end if
        if (.not. allocated(me%defaultContaminantFormDistribution)) then
            allocate(me%defaultContaminantFormDistribution(n_default_contaminant_form_distribution))
        end if

        ! Read namelists
        read(iouConstants, nml=n_biota_grp, iostat=nmlIOStat)
        rewind(iouConstants)
        me%nBiota = n_biota
        if (nmlIOStat .ge. 0) then
            ! Validate allocation sizes
            if (n_k_uptake_contaminant /= me%nBiota * n_contaminant_size_classes) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in k_uptake_contaminant size: expected " // &
                            trim(str(me%nBiota * n_contaminant_size_classes)) // &
                            ", got " // trim(str(n_k_uptake_contaminant))))
                close(iouConstants)
                return
            end if
            if (n_k_elim_contaminant /= me%nBiota * n_contaminant_size_classes) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in k_elim_contaminant size: expected " // &
                            trim(str(me%nBiota * n_contaminant_size_classes)) // &
                            ", got " // trim(str(n_k_elim_contaminant))))
                close(iouConstants)
                return
            end if
            if (n_initial_c_org /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in initial_C_org size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_initial_c_org))))
                close(iouConstants)
                return
            end if
            if (n_k_growth /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in k_growth size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_k_growth))))
                close(iouConstants)
                return
            end if
            if (n_name /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in name size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_name))))
                close(iouConstants)
                return
            end if
            if (n_stored_fraction /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in stored_fraction size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_stored_fraction))))
                close(iouConstants)
                return
            end if
            if (n_compartment /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in compartment size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_compartment))))
                close(iouConstants)
                return
            end if
            if (n_k_uptake_dissolved /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in k_uptake_dissolved size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_k_uptake_dissolved))))
                close(iouConstants)
                return
            end if
            if (n_k_elim_dissolved /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in k_elim_dissolved size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_k_elim_dissolved))))
                close(iouConstants)
                return
            end if
            if (n_uptake_from_form /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in uptake_from_form size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_uptake_from_form))))
                close(iouConstants)
                return
            end if
            if (n_harvest_in_month /= me%nBiota) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=900, &
                    message="Mismatch in harvest_in_month size: expected " // &
                            trim(str(me%nBiota)) // ", got " // trim(str(n_harvest_in_month))))
                close(iouConstants)
                return
            end if
            rewind(iouConstants)

            ! Allocate biota-related arrays
            allocate(initial_C_org(me%nBiota), k_growth(me%nBiota), &
                    k_uptake_contaminant(me%nBiota * n_contaminant_size_classes), &
                    k_elim_contaminant(me%nBiota * n_contaminant_size_classes), &
                    name(me%nBiota), stored_fraction(me%nBiota), &
                    compartment(me%nBiota), k_uptake_dissolved(me%nBiota), &
                    k_elim_dissolved(me%nBiota), uptake_from_form(me%nBiota), &
                    harvest_in_month(me%nBiota))

            ! Read biota namelist
            read(iouConstants, nml=biota, iostat=nmlIOStat)
            if (nmlIOStat /= 0) then
                call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read biota namelist"))
                close(iouConstants)
                return
            end if
            me%hasBiota = .true.
        end if
        rewind(iouConstants)

        ! Read other namelists
        read(iouConstants, nml=contaminant, iostat=nmlIOStat)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read contaminant namelist, using defaults"))
        end if
        rewind(iouConstants)

        read(iouConstants, nml=earthworm_densities, iostat=nmlIOStat, iomsg=nmlIOMsg)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read earthworm_densities namelist" &
                                                                             // " with message: " // trim(nmlIOMsg)))
            close(iouConstants)
            return
        end if
        rewind(iouConstants)

        read(iouConstants, nml=soil, iostat=nmlIOStat, iomsg=nmlIOMsg)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read soil namelist" &
                                                                             // " with message: " // trim(nmlIOMsg)))
            close(iouConstants)
            return
        end if
        rewind(iouConstants)

        read(iouConstants, nml=water, iostat=nmlIOStat, iomsg=nmlIOMsg)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read water namelist" &
                                                                             // " with message: " // trim(nmlIOMsg)))
            close(iouConstants)
            return
        end if
        rewind(iouConstants)

        read(iouConstants, nml=sediment, iostat=nmlIOStat, iomsg=nmlIOMsg)
        if (nmlIOStat /= 0) then
            call ERROR_HANDLER%trigger(error=ErrorInstance(code=200, message="Failed to read sediment namelist" &
                                                                             // " with message: " // trim(nmlIOMsg)))
            close(iouConstants)
            return
        end if
        rewind(iouConstants)
        close(iouConstants)

        ! Save to class variables
        me%contaminantDensity = contaminant_density
        me%contaminantSizeClasses = contaminant_size_classes
        if (size(default_contaminant_size_distribution) /= n_contaminant_size_classes) then
            call ERROR_HANDLER%trigger(error=ErrorInstance( &
                code=900, message="Mismatch in default_contaminant_size_distribution size: expected " // &
                trim(str(n_contaminant_size_classes)) // ", got " // &
                trim(str(size(default_contaminant_size_distribution)))))
            return
        end if
        me%defaultDistributionContaminant = default_contaminant_size_distribution / 100.0
        if (size(default_contaminant_form_distribution) /= C%contaminantDim(2)+1) then
            call ERROR_HANDLER%trigger(error=ErrorInstance( &
                code=900, message="Mismatch in default_contaminant_form_distribution size: expected " // &
                trim(str(C%contaminantDim(2)+1)) // ", got " // &
                trim(str(size(default_contaminant_form_distribution)))))
            return
        end if
        me%defaultContaminantFormDistribution = default_contaminant_form_distribution / 100.0
        me%nContaminantSizeClasses = n_contaminant_size_classes
        me%defaultSpmSizeDistribution = default_spm_size_distribution / 100.0
        me%defaultMatrixEmbeddedDistributionToSpm = default_matrixembedded_distribution_to_spm / 100.0
        me%soilDarcyVelocity = darcy_velocity
        me%soilDefaultPorosity = default_porosity
        me%soilHamakerConstant = hamaker_constant
        me%soilParticleDensity = particle_density
        me%soilConstantAttachmentEfficiency = soil_constant_attachment_efficiency
        me%soilErosivity_a1 = erosivity_a1
        me%soilErosivity_a2 = erosivity_a2
        me%soilErosivity_a3 = erosivity_a3
        me%soilErosivity_b = erosivity_b
        me%sedimentTransport_aConstant = sediment_transport_a
        me%sedimentTransport_bConstant = sediment_transport_b
        me%sedimentTransport_cConstant = sediment_transport_c
        me%earthwormDensityArable = arable
        me%earthwormDensityConiferous = coniferous
        me%earthwormDensityDeciduous = deciduous
        me%earthwormDensityGrassland = grassland
        me%earthwormDensityHeathland = heathland
        me%earthwormDensityUrbanCapped = urban_capped
        me%earthwormDensityUrbanGardens = urban_gardens
        me%earthwormDensityUrbanParks = urban_parks
        me%earthwormVerticalDistribution = vertical_distribution / 100.0
        if (me%hasBiota) then
            me%biotaName = name
            me%biotaInitial_C_org = initial_C_org
            me%biota_k_growth = k_growth
            me%biotaStoredFraction = stored_fraction
            me%biotaCompartment = compartment
            me%biotaUptakeFromForm = uptake_from_form
            me%biotaHarvestInMonth = harvest_in_month
            if (allocated(me%biota_k_uptake_contaminant)) deallocate(me%biota_k_uptake_contaminant)
            if (allocated(me%biota_k_elim_contaminant)) deallocate(me%biota_k_elim_contaminant)
            allocate(me%biota_k_uptake_contaminant(me%nBiota, C%contaminantDim(2)))
            allocate(me%biota_k_elim_contaminant(me%nBiota, C%contaminantDim(2)))
            me%biota_k_uptake_contaminant(:,1) = k_uptake_contaminant(1:me%nBiota)
            me%biota_k_elim_contaminant(:,1) = k_elim_contaminant(1:me%nBiota)
            if (C%contaminantDim(2) > 1) then
                me%biota_k_uptake_contaminant(:,2) = k_uptake_contaminant(me%nBiota+1:2*me%nBiota)
                me%biota_k_elim_contaminant(:,2) = k_elim_contaminant(me%nBiota+1:2*me%nBiota)
            end if
            if (.not. allocated(me%biota_k_uptake_dissolved)) then
                allocate(me%biota_k_uptake_dissolved(me%nBiota))
                me%biota_k_uptake_dissolved = k_uptake_dissolved
            end if
            if (.not. allocated(me%biota_k_elim_dissolved)) then
                allocate(me%biota_k_elim_dissolved(me%nBiota))
                me%biota_k_elim_dissolved = k_elim_dissolved
            end if
        end if
        me%riverMeanderingFactor = river_meandering_factor
        me%waterResuspensionAlpha = resuspension_alpha
        me%waterResuspensionBeta = resuspension_beta
        me%depositionAlphaConstant = deposition_alpha
        me%depositionBetaConstant = deposition_beta
        me%bankErosionAlphaConstant = bank_erosion_alpha
        me%bankErosionBetaConstant = bank_erosion_beta
        me%contaminant_k_diss_pristine = k_diss_pristine
        me%contaminant_k_diss_transformed = k_diss_transformed
        me%contaminant_k_transform_pristine = k_transform_pristine
        me%riverAttachmentEfficiency = river_attachment_efficiency
        me%estuaryAttachmentEfficiency = estuary_attachment_efficiency
        me%waterResuspensionAlphaEstuary = merge(resuspension_alpha_estuary, me%waterResuspensionAlpha, &
                                                resuspension_alpha_estuary /= 0.0_dp)
        me%waterResuspensionBetaEstuary = merge(resuspension_beta_estuary, me%waterResuspensionBeta, &
                                                resuspension_beta_estuary /= 0.0_dp)
        me%shearRate = shear_rate
        me%waterTemperature = me%calculateWaterTemperatureTimeSeries(min_water_temperature, &
                                                                    max_water_temperature, &
                                                                    min_water_temperature_day_of_year)
        me%estuaryTidalM2 = estuary_tidal_M2
        me%estuaryTidalS2 = estuary_tidal_S2
        me%estuaryMeanDepthExpA = estuary_mean_depth_expA
        me%estuaryMeanDepthExpB = estuary_mean_depth_expB
        me%estuaryWidthExpA = estuary_width_expA
        me%estuaryWidthExpB = estuary_width_expb
        me%estuaryMeanderingFactor = estuary_meandering_factor
        me%estuaryMouthCoords = estuary_mouth_coords
        me%sedimentPorosity = porosity
        me%sedimentFractionalComposition = fractional_composition_distribution
        me%sedimentEnrichment_k = sediment_enrichment_k
        me%sedimentEnrichment_a = sediment_enrichment_a
        me%spmDensityBySizeClass = spm_density_by_size_class
    end subroutine

    !> Elemental function for getting a mask from an int2 array, where the NetCDF
    !! fill value nf90_fill_int2 is used to mask values
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

    !> Calculate the meandering factor from cell size, based on Fekete et al. 2001 (https://doi.org/10.1029/2001WR900024).
    !! This method is empirical and was used in GWAVA. It assumes square cells, so how we'll average grid size to
    !! account for that, in case the cells aren't square. This value is used if meandering factors aren't specified in data.
    function calculateMeanderingFactorFromCellSizeDatabase(me) result(f_m)
        class(Database) :: me
        real            :: f_m
        f_m = 1.024 - 0.077 * log(100 / ((me%gridRes(1) + me%gridRes(2)) / 2))
    end function

    !> Calculate the water temperature timeseries using min and max water temperatures, and the Julian day
    !! (day of year) on which the min water temperature occurs. This function using those parameters to construct
    !! a cosine function representing the time series:
    !! $$
    !!  T_t = 0.5 (T_\text{max} - T_\text{min}) \cos\( \frac{2 D_t \pi}{366} - D_\text{min} \) + 0.5 (T_\text{max} + T_\text{min}) 
    !! $$
    function calculateWaterTemperatureTimeSeriesWaterBody(me, minTemp, maxTemp, minTempDay) result(waterTemperature)
        class(Database) :: me
        real(dp)        :: minTemp 
        real(dp)        :: maxTemp  
        integer         :: minTempDay
        real(dp)        :: waterTemperature(366)  
        integer         :: i
        integer         :: days(366)
        ! Integer range of days in year
        days = [(i, i = 1, 366, 1)]
        ! Calculate the water temperature timeseries using cos function
        waterTemperature = - 0.5 * (maxTemp - minTemp) * cos(days * 2 * C%pi / 366 - minTempDay) &
                           + (maxTemp + minTemp) / 2
    end function

    !> Audit the database
    function auditDatabase(me) result(rslt)
        class(Database) :: me           ! This Database
        type(Result)    :: rslt         ! Result object to return errors in
        integer         :: x, y, i      ! Iterators
        integer         :: xy_in(2)     ! Inflow x and y
        logical         :: simulationMaskError = .false.

        ! Is the simulation mask self-contained (no inflows to area to simulate)?
        if (C%hasSimulationMask) then
            do y = 1, me%gridShape(2)
                do x = 1, me%gridShape(1)
                    if (me%simulationMask(x,y)) then
                        ! We're in the area to simulate, so check if there are inflows from
                        ! outside the area to simulation
                        do i = 1, size(me%inflows, dim=2)
                            ! Is the inflow actually an inflow or a fill value
                            if (me%inflows(1,i,x,y) >= 0) then
                                xy_in = me%inflows(:,i,x,y)
                                if (.not. me%simulationMask(xy_in(1), xy_in(2))) then
                                    simulationMaskError = .true.
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end if

        if (simulationMaskError) then
            call rslt%addError(ErrorInstance( &
                message="Simulation mask provided has inflows from outside " // &
                        "the area to simulate. Please provide a simulation mask that " // &
                        "is self-contained." &
            ))
        end if

        ! Bounds checks for sediment calibration parameters
        if (any(me%depositionAlpha < 0.0_dp)) then
            call rslt%addError(ErrorInstance( &
                message="Value provided for deposition_alpha must be greater than or equal to zero. " // &
                        "At least one value provided is less than zero." &
            ))
        end if
        if (any(me%resuspensionAlpha < 0.0_dp)) then
            call rslt%addError(ErrorInstance( &
                message="Value provided for resuspension_alpha must be greater than or equal to zero. " // &
                        "At least one value provided is less than zero." &
            ))
        end if
        if (any(me%resuspensionBeta < 0.0_dp)) then
            call rslt%addError(ErrorInstance( &
                message="Value provided for resuspension_beta must be greater than or equal to zero. " // &
                        "At least one value provided is less than zero." &
            ))
        end if

        ! Does sediment fractional composition sum to unity?
        if (.not. isZero(1.0_dp - sum(me%sedimentFractionalComposition))) then
            call rslt%addError(ErrorInstance(message="sedimentFractionalComposition must sum to 1. Found: " // &
                str(sum(me%sedimentFractionalComposition))))
        end if
        if (any(me%defaultDistributionContaminant < 0.0)) then
            call rslt%addError(ErrorInstance(message="defaultDistributionContaminant must be non-negative."))
        end if
        if (.not. isZero(1.0_dp - sum(me%defaultDistributionContaminant))) then
            call rslt%addError(ErrorInstance(message="defaultDistributionContaminant must sum to 1. Found: " // &
                str(sum(me%defaultDistributionContaminant))))
        end if
        if (me%contaminantDensity <= 0.0) then
            call rslt%addError(ErrorInstance(message="contaminantDensity must be positive."))
        end if
        if (any(me%contaminantSizeClasses <= 0.0)) then
            call rslt%addError(ErrorInstance(message="contaminantSizeClasses must be positive."))
        end if

        ! Bounds checks for initial concentrations
        if (allocated(me%initialContaminantConcsSoil)) then
            if (any(me%initialContaminantConcsSoil < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsSoil must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%initialContaminantConcsWater)) then
            if (any(me%initialContaminantConcsWater < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsWater must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%initialContaminantConcsSediment)) then
            if (any(me%initialContaminantConcsSediment < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsSediment must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsSoil)) then
            if (any(me%initialDissolvedConcsSoil < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsSoil must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsWater)) then
            if (any(me%initialDissolvedConcsWater < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsWater must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsSediment)) then
            if (any(me%initialDissolvedConcsSediment < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsSediment must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if

        ! Bounds checks for dissolved emissions
        if (allocated(me%emissionsArealSoilDissolvedContaminant)) then
            if (any(me%emissionsArealSoilDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsArealSoilDissolvedContaminant must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%emissionsArealWaterDissolvedContaminant)) then
            if (any(me%emissionsArealWaterDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsArealWaterDissolvedContaminant must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%emissionsAtmosphericDryDepoDissolvedContaminant)) then
            if (any(me%emissionsAtmosphericDryDepoDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsAtmosphericDryDepoDissolvedContaminant must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%emissionsAtmosphericWetDepoDissolvedContaminant)) then
            if (any(me%emissionsAtmosphericWetDepoDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsAtmosphericWetDepoDissolvedContaminant must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
        if (allocated(me%emissionsPointWaterDissolvedContaminant)) then
            if (any(me%emissionsPointWaterDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsPointWaterDissolvedContaminant must be non-negative. " // &
                            "At least one value is negative." &
                ))
            end if
        end if
    end function

end module


! AUDITING TO DO:
!   array size checks, particularly variables for size classes of NM and SPM
!   sedimentPorosity: 0 <= x <= 1
!   water content at saturation is greater than water content at field capacity
!   are batch runs contiguous (dates follow on from each other for different chunks)
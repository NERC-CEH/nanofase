!> The DataInputModule contains the Database type, which is responsible for
!! data input to the model, as well as a variable (DATASET) of type(Database), which
!! can be imported into other modules, thus making the data parsed by the Database type
!! accessible throughout the model.
module DataInputModule
    use mo_netcdf
    use DefaultsModule
    use ConstantsDefaultsModule
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
        character(len=* )   :: inputFile, constantsFile
        type(Result)        :: rslt
        ! temps returned by mo_netcdf in Fortran order (reversed NetCDF dims)
        integer, allocatable :: outflow_dxy(:,:,:)
        integer, allocatable :: inflows_dwxy(:,:,:,:)
        integer, allocatable :: isHeadwaterInt_xy(:,:), isEstuaryInt_xy(:,:)
        integer, allocatable :: nWaterbodies_xy(:,:)
        integer, allocatable :: simulationMask(:,:)
        integer :: nx, ny

        ! Open the dataset and parse constants
        me%nc = NcDataset(inputFile, 'r')
        call me%parseConstants(constantsFile)

        ! GRID / COORDS
        var = me%nc%getVariable('grid_shape');  call var%getData(me%gridShape)
        var = me%nc%getVariable('grid_res');    call var%getData(me%gridRes)
        var = me%nc%getVariable('grid_bounds'); call var%getData(me%gridBounds)
        var = me%nc%getVariable('x');           call var%getData(me%x)
        var = me%nc%getVariable('y');           call var%getData(me%y)
        allocate(me%x_l(size(me%x))); me%x_l = me%x - 0.5 * me%gridRes(1)
        allocate(me%y_u(size(me%y))); me%y_u = me%y + 0.5 * me%gridRes(2)
        var = me%nc%getVariable('crs'); call var%getAttribute('crs_wkt', me%crsWKT)

        nx = me%gridShape(1)
        ny = me%gridShape(2)

        ! ROUTING (getData already reversed dims to Fortran order)
        ! outflow: file (y,x,d) -> returned (d,x,y) => model (d,x,y)
        var = me%nc%getVariable('outflow');  call var%getData(outflow_dxy)
        if (allocated(me%outflow)) deallocate(me%outflow)
        allocate(me%outflow( size(outflow_dxy,1), size(outflow_dxy,2), size(outflow_dxy,3) ))
        me%outflow = outflow_dxy
        deallocate(outflow_dxy)

        ! inflows: file (y,x,w,d) -> returned (d,w,x,y) => model (d,w,x,y)
        var = me%nc%getVariable('inflows');  call var%getData(inflows_dwxy)
        if (allocated(me%inflows)) deallocate(me%inflows)
        allocate(me%inflows( size(inflows_dwxy,1), size(inflows_dwxy,2), &
                            size(inflows_dwxy,3), size(inflows_dwxy,4) ))
        me%inflows = inflows_dwxy
        deallocate(inflows_dwxy)

        ! headwater / n_waterbodies / estuary: file (y,x) -> returned (x,y) => model (x,y)
        var = me%nc%getVariable('is_headwater');  call var%getData(isHeadwaterInt_xy)
        me%isHeadwater = ulgcl(isHeadwaterInt_xy)
        deallocate(isHeadwaterInt_xy)

        var = me%nc%getVariable('n_waterbodies'); call var%getData(nWaterbodies_xy)
        me%nWaterbodies = nWaterbodies_xy
        deallocate(nWaterbodies_xy)
        me%maxNWaterbodies = maxval(me%nWaterbodies)

        if (C%includeEstuary) then
            var = me%nc%getVariable('is_estuary'); call var%getData(isEstuaryInt_xy)
            me%isEstuary = ulgcl(isEstuaryInt_xy)
            deallocate(isEstuaryInt_xy)
        else
            allocate(me%isEstuary(nx, ny)); me%isEstuary = .false.
        end if

        ! Grid mask from nWaterbodies
        allocate(me%gridMask(nx, ny))
        me%gridMask = me%mask(me%nWaterbodies)

        ! Derive meandering factors from grid size if not set in constants
        if (isZero(me%riverMeanderingFactor))   me%riverMeanderingFactor   = &
            me%calculateMeanderingFactorFromCellSize()
        if (isZero(me%estuaryMeanderingFactor)) me%estuaryMeanderingFactor = &
            me%calculateMeanderingFactorFromCellSize()

        ! Chunk-varying variables
        call me%readBatchVariables()

        ! Close input dataset
        call me%nc%close()

        ! Simulation mask (same reversal: file (y,x) -> returned (x,y))
        if (C%hasSimulationMask) then
            nc_simulationMask = NcDataset(C%simulationMaskPath, 'r')
            var = nc_simulationMask%getVariable('simulation_mask')
            call var%getData(simulationMask)
            me%simulationMask = ulgcl(simulationMask)
            me%nNonMaskedCells = count(me%simulationMask)
        else
            allocate(me%simulationMask(nx, ny))
            me%simulationMask = .true.
            me%nNonMaskedCells = count(.not. me%gridMask)
        end if

        ! Audit & log
        call rslt%addErrors(.errors. me%audit())
        call rslt%addToTrace('Initialising database')
        call ERROR_HANDLER%trigger(errors=.errors.rslt)
        call LOGR%toFile("Initialising database: success")
        call LOGR%toConsole("Initialising database: "//COLOR_GREEN//"success"//COLOR_RESET)
    end subroutine

    !> Update the database based on data for a new chunk (k), or for the only chunk if this
    !! isn't a batch run.
    subroutine updateDatabase(me, k)
        class(Database) :: me
        integer         :: k

        ! Get the config options for this chunk
        C%inputFile   = C%batchInputFiles(k)
        C%constantsFile = C%batchConstantFiles(k)
        C%nTimeSteps  = C%batchNTimesteps(k)
        C%startDate   = C%batchStartDates(k)

        call me%parseConstants(C%constantsFile)

        me%nc = NcDataset(C%inputFile, 'r')

        ! Deallocate previous-chunk vars
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

        call me%readBatchVariables()
        call me%nc%close()
    end subroutine

    !> Read variables in for the new chunk as part of a batch run
    subroutine readBatchVariablesDatabase(me)
        class(Database)     :: me
        type(NcVariable)    :: var
        type(NcDimension)   :: p_dim
        logical :: haveCoordVar
        integer :: n
        integer :: alloc_stat
        integer :: nx, ny, nt, nforms, nsizes, np
        integer :: f_pris, f_mat, f_tra

        ! temp arrays with explicit ranks that match legacy file vars
        real(dp), allocatable :: A2(:,:)          ! (x,y)
        real(dp), allocatable :: A3(:,:,:)        ! (x,y,t)
        real(dp), allocatable :: COORD4(:,:,:,:)  ! (x,y,p,d)
        real(dp), allocatable :: A4(:,:,:,:)      ! (x,y,t,p) 

        nx     = me%gridShape(1)
        ny     = me%gridShape(2)
        nt     = C%nTimeSteps
        nsizes = C%contaminantDim(1)
        nforms = C%contaminantDim(2)

        ! Legacy form indices (cap to available number of forms)
        f_pris = 1
        f_mat  = merge(2, 1, nforms >= 2)
        f_tra  = merge(3, 1, nforms >= 3)

        !----------------------
        ! BASIC TIME SERIES
        !----------------------
        if (me%nc%hasVariable('quickflow')) then
            var = me%nc%getVariable('quickflow')         ! (t,y,x) in file
            if (allocated(me%quickflow)) deallocate(me%quickflow)
            allocate(me%quickflow(nx,ny,nt))
            call var%getData(me%quickflow)               ! library reverses -> (x,y,t)
        else
            if (allocated(me%quickflow)) deallocate(me%quickflow)
            allocate(me%quickflow(nx,ny,nt))
            me%quickflow = 0.0_dp
        end if

        if (me%nc%hasVariable('runoff')) then
            var = me%nc%getVariable('runoff')
            if (allocated(me%runoff)) deallocate(me%runoff)
            allocate(me%runoff(nx,ny,nt))
            call var%getData(me%runoff)
        else
            if (allocated(me%runoff)) deallocate(me%runoff)
            allocate(me%runoff(nx,ny,nt))
            me%runoff = 0.0_dp
        end if

        if (me%nc%hasVariable('precip')) then
            var = me%nc%getVariable('precip')
            if (allocated(me%precip)) deallocate(me%precip)
            allocate(me%precip(nx,ny,nt))
            call var%getData(me%precip)
        else
            if (allocated(me%precip)) deallocate(me%precip)
            allocate(me%precip(nx,ny,nt))
            me%precip = 0.0_dp
        end if

        if (me%nc%hasVariable('evap')) then
            var = me%nc%getVariable('evap')
            if (allocated(me%evap)) deallocate(me%evap)
            allocate(me%evap(nx,ny,nt))
            call var%getData(me%evap)
        else
            if (allocated(me%evap)) deallocate(me%evap)
            allocate(me%evap(nx,ny,nt))
            me%evap = 0.0_dp
        end if

        !----------------------
        ! POINT-SOURCE DIM
        !----------------------
        if (me%nc%hasDimension('p')) then
            p_dim              = me%nc%getDimension('p')
            me%maxPointSources = p_dim%getLength()
        else
            me%maxPointSources = 0
        end if
        np = max(1, me%maxPointSources)

        !----------------------
        ! DEALLOC & ALLOC EMISSIONS
        !----------------------
        if (allocated(me%emissionsArealSoilContaminant)) &
            deallocate(me%emissionsArealSoilContaminant)
        if (allocated(me%emissionsArealWaterContaminant)) &
            deallocate(me%emissionsArealWaterContaminant)
        if (allocated(me%emissionsAtmosphericDryDepoContaminant)) &
            deallocate(me%emissionsAtmosphericDryDepoContaminant)
        if (allocated(me%emissionsAtmosphericWetDepoContaminant)) &
            deallocate(me%emissionsAtmosphericWetDepoContaminant)
        if (allocated(me%emissionsPointWaterContaminant)) &
            deallocate(me%emissionsPointWaterContaminant)
        if (allocated(me%emissionsArealSoilDissolvedContaminant)) &
            deallocate(me%emissionsArealSoilDissolvedContaminant)
        if (allocated(me%emissionsArealWaterDissolvedContaminant)) &
            deallocate(me%emissionsArealWaterDissolvedContaminant)
        if (allocated(me%emissionsAtmosphericDryDepoDissolvedContaminant)) &
            deallocate(me%emissionsAtmosphericDryDepoDissolvedContaminant)
        if (allocated(me%emissionsAtmosphericWetDepoDissolvedContaminant)) &
            deallocate(me%emissionsAtmosphericWetDepoDissolvedContaminant)
        if (allocated(me%emissionsPointWaterDissolvedContaminant)) &
            deallocate(me%emissionsPointWaterDissolvedContaminant)
        if (allocated(me%emissionsPointWaterCoords)) &
            deallocate(me%emissionsPointWaterCoords)

        allocate( &
            me%emissionsArealSoilContaminant( nx, ny, nsizes, nforms, C%contaminantDim(3) ), &
            me%emissionsArealWaterContaminant( nx, ny, nsizes, nforms, C%contaminantDim(3) ), &
            me%emissionsAtmosphericDryDepoContaminant( nx, ny, nt, nsizes, nforms, &
                                                    C%contaminantDim(3) ), &
            me%emissionsAtmosphericWetDepoContaminant( nx, ny, nt, nsizes, nforms, &
                                                    C%contaminantDim(3) ), &
            me%emissionsPointWaterContaminant( nx, ny, nt, np, nsizes, nforms, &
                                            C%contaminantDim(3) ), &
            me%emissionsArealSoilDissolvedContaminant( nx, ny ), &
            me%emissionsArealWaterDissolvedContaminant( nx, ny ), &
            me%emissionsAtmosphericDryDepoDissolvedContaminant( nx, ny, nt ), &
            me%emissionsAtmosphericWetDepoDissolvedContaminant( nx, ny, nt ), &
            me%emissionsPointWaterDissolvedContaminant( nx, ny, nt ), &
            stat=alloc_stat )

        if (alloc_stat /= 0) then
            call ERROR_HANDLER%trigger( &
                error=ErrorInstance(message='Emission allocation failed') )
            return
        end if

        me%emissionsArealSoilContaminant                   = 0.0_dp
        me%emissionsArealWaterContaminant                  = 0.0_dp
        me%emissionsAtmosphericDryDepoContaminant          = 0.0_dp
        me%emissionsAtmosphericWetDepoContaminant          = 0.0_dp
        me%emissionsPointWaterContaminant                  = 0.0_dp
        me%emissionsArealSoilDissolvedContaminant          = 0.0_dp
        me%emissionsArealWaterDissolvedContaminant         = 0.0_dp
        me%emissionsAtmosphericDryDepoDissolvedContaminant = 0.0_dp
        me%emissionsAtmosphericWetDepoDissolvedContaminant = 0.0_dp
        me%emissionsPointWaterDissolvedContaminant         = 0.0_dp

        !-----------------------------------
        ! AREAL EMISSIONS (2-D y,x → (x,y))
        !-----------------------------------
        if (me%nc%hasVariable('emissions_areal_soil_pristine')) then
            var = me%nc%getVariable('emissions_areal_soil_pristine')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealSoilContaminant(:,:,1,f_pris,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealSoilContaminant(:,:,n,f_pris,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_soil_matrixembedded')) then
            var = me%nc%getVariable('emissions_areal_soil_matrixembedded')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealSoilContaminant(:,:,1,f_mat,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealSoilContaminant(:,:,n,f_mat,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_soil_transformed')) then
            var = me%nc%getVariable('emissions_areal_soil_transformed')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealSoilContaminant(:,:,1,f_tra,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealSoilContaminant(:,:,n,f_tra,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_water_pristine')) then
            var = me%nc%getVariable('emissions_areal_water_pristine')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealWaterContaminant(:,:,1,f_pris,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealWaterContaminant(:,:,n,f_pris,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_water_matrixembedded')) then
            var = me%nc%getVariable('emissions_areal_water_matrixembedded')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealWaterContaminant(:,:,1,f_mat,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealWaterContaminant(:,:,n,f_mat,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_water_transformed')) then
            var = me%nc%getVariable('emissions_areal_water_transformed')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealWaterContaminant(:,:,1,f_tra,FREE_CONTAMINANT) = A2
            do n = 2, nsizes
                me%emissionsArealWaterContaminant(:,:,n,f_tra,FREE_CONTAMINANT) = &
                    A2 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_soil_dissolved')) then
            var = me%nc%getVariable('emissions_areal_soil_dissolved')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealSoilDissolvedContaminant = A2
            deallocate(A2)
        end if

        if (me%nc%hasVariable('emissions_areal_water_dissolved')) then
            var = me%nc%getVariable('emissions_areal_water_dissolved')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%emissionsArealWaterDissolvedContaminant = A2
            deallocate(A2)
        end if

        !-----------------------------------------
        ! ATMOSPHERIC DEPOSITION (3-D t,y,x → (x,y,t))
        !-----------------------------------------
        if (me%nc%hasVariable('emissions_atmospheric_drydepo_pristine')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_pristine')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,f_pris,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,f_pris,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_drydepo_matrixembedded')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_matrixembedded')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,f_mat,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,f_mat,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_drydepo_transformed')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_transformed')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericDryDepoContaminant(:,:,:,1,f_tra,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericDryDepoContaminant(:,:,:,n,f_tra,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_pristine')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_pristine')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,f_pris,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,f_pris,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_matrixembedded')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_matrixembedded')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,f_mat,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,f_mat,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_transformed')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_transformed')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericWetDepoContaminant(:,:,:,1,f_tra,FREE_CONTAMINANT) = A3
            do n = 2, nsizes
                me%emissionsAtmosphericWetDepoContaminant(:,:,:,n,f_tra,FREE_CONTAMINANT) = &
                    A3 * me%defaultDistributionContaminant(n)
            end do
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_drydepo_dissolved')) then
            var = me%nc%getVariable('emissions_atmospheric_drydepo_dissolved')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericDryDepoDissolvedContaminant = A3
            deallocate(A3)
        end if

        if (me%nc%hasVariable('emissions_atmospheric_wetdepo_dissolved')) then
            var = me%nc%getVariable('emissions_atmospheric_wetdepo_dissolved')
            allocate(A3(nx,ny,nt)); call var%getData(A3)
            me%emissionsAtmosphericWetDepoDissolvedContaminant = A3
            deallocate(A3)
        end if

        !-----------------------------------------
        ! POINT-SOURCE COORDS (4-D x,y,p,2)
        !-----------------------------------------
        haveCoordVar = .false.
        if (me%nc%hasVariable('emissions_point_water_contaminant_coords')) then
            var = me%nc%getVariable('emissions_point_water_contaminant_coords')
            haveCoordVar = .true.
        else if (me%nc%hasVariable('emissions_point_water_pristine_coords')) then
            var = me%nc%getVariable('emissions_point_water_pristine_coords')
            haveCoordVar = .true.
        else if (me%nc%hasVariable('emissions_point_water_matrixembedded_coords')) then
            var = me%nc%getVariable('emissions_point_water_matrixembedded_coords')
            haveCoordVar = .true.
        end if

        if (allocated(me%emissionsPointWaterCoords)) &
            deallocate(me%emissionsPointWaterCoords)
        allocate(me%emissionsPointWaterCoords(nx, ny, me%maxPointSources, 2))

        if (haveCoordVar) then
            allocate(COORD4(nx, ny, me%maxPointSources, 2))
            call var%getData(COORD4)      ! library returns (x,y,p,d)
            me%emissionsPointWaterCoords = COORD4
            deallocate(COORD4)
        else
            me%emissionsPointWaterCoords = nf90_fill_double
        end if
        
        !-----------------------------------------
        ! POINT-SOURCE EMISSIONS (4-D p,t,y,x → (x,y,t,p))
        !-----------------------------------------
        if (me%maxPointSources > 0) then

            if (me%nc%hasVariable('emissions_point_water_pristine')) then
                var = me%nc%getVariable('emissions_point_water_pristine')
                allocate(A4(nx,ny,nt,np)); call var%getData(A4)   ! (x,y,t,p)
                ! size=1 takes raw; n=2..nsizes distributed
                me%emissionsPointWaterContaminant(:,:,:,1:np,1,f_pris,FREE_CONTAMINANT) = A4
                do n = 2, nsizes
                    me%emissionsPointWaterContaminant(:,:,:,1:np,n,f_pris,FREE_CONTAMINANT) = &
                        A4 * me%defaultDistributionContaminant(n)
                end do
                deallocate(A4)
            end if

            if (me%nc%hasVariable('emissions_point_water_matrixembedded')) then
                var = me%nc%getVariable('emissions_point_water_matrixembedded')
                allocate(A4(nx,ny,nt,np)); call var%getData(A4)   ! (x,y,t,p)
                me%emissionsPointWaterContaminant(:,:,:,1:np,1,f_mat,FREE_CONTAMINANT) = A4
                do n = 2, nsizes
                    me%emissionsPointWaterContaminant(:,:,:,1:np,n,f_mat,FREE_CONTAMINANT) = &
                        A4 * me%defaultDistributionContaminant(n)
                end do
                deallocate(A4)
            end if

            ! Optional: only if present in file
            if (me%nc%hasVariable('emissions_point_water_transformed')) then
                var = me%nc%getVariable('emissions_point_water_transformed')
                allocate(A4(nx,ny,nt,np)); call var%getData(A4)   ! (x,y,t,p)
                me%emissionsPointWaterContaminant(:,:,:,1:np,1,f_tra,FREE_CONTAMINANT) = A4
                do n = 2, nsizes
                    me%emissionsPointWaterContaminant(:,:,:,1:np,n,f_tra,FREE_CONTAMINANT) = &
                        A4 * me%defaultDistributionContaminant(n)
                end do
                deallocate(A4)
            end if

        end if

        !-----------------------------------
        ! INITIAL CONCENTRATIONS
        !-----------------------------------
        if (allocated(me%initialContaminantConcsSoil))     &
            deallocate(me%initialContaminantConcsSoil)
        if (allocated(me%initialContaminantConcsWater))    &
            deallocate(me%initialContaminantConcsWater)
        if (allocated(me%initialContaminantConcsSediment)) &
            deallocate(me%initialContaminantConcsSediment)
        if (allocated(me%initialDissolvedConcsSoil))       &
            deallocate(me%initialDissolvedConcsSoil)
        if (allocated(me%initialDissolvedConcsWater))      &
            deallocate(me%initialDissolvedConcsWater)
        if (allocated(me%initialDissolvedConcsSediment))   &
            deallocate(me%initialDissolvedConcsSediment)

        allocate(me%initialContaminantConcsSoil(   nx,ny,nsizes,nforms, &
                                                C%contaminantDim(3)))
        allocate(me%initialContaminantConcsWater(  nx,ny,nsizes,nforms, &
                                                C%contaminantDim(3)))
        allocate(me%initialContaminantConcsSediment(nx,ny,nsizes,nforms, &
                                                    C%contaminantDim(3)))
        allocate(me%initialDissolvedConcsSoil(     nx,ny))
        allocate(me%initialDissolvedConcsWater(    nx,ny))
        allocate(me%initialDissolvedConcsSediment( nx,ny))

        me%initialContaminantConcsSoil     = 0.0_dp
        me%initialContaminantConcsWater    = 0.0_dp
        me%initialContaminantConcsSediment = 0.0_dp
        me%initialDissolvedConcsSoil       = 0.0_dp
        me%initialDissolvedConcsWater      = 0.0_dp
        me%initialDissolvedConcsSediment   = 0.0_dp

        if (me%nc%hasVariable('initial_dissolved_concentrations_soil')) then
            var = me%nc%getVariable('initial_dissolved_concentrations_soil')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%initialDissolvedConcsSoil = A2
            deallocate(A2)
        end if
        if (me%nc%hasVariable('initial_dissolved_concentrations_water')) then
            var = me%nc%getVariable('initial_dissolved_concentrations_water')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%initialDissolvedConcsWater = A2
            deallocate(A2)
        end if
        if (me%nc%hasVariable('initial_dissolved_concentrations_sediment')) then
            var = me%nc%getVariable('initial_dissolved_concentrations_sediment')
            allocate(A2(nx,ny)); call var%getData(A2)
            me%initialDissolvedConcsSediment = A2
            deallocate(A2)
        end if

        ! Count point sources after coords are populated
        call me%calculateNPointSources(me%maxPointSources)
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
        resuspension_alpha_estuary = 0.0_dp
        resuspension_beta_estuary = 0.0_dp
        soil_constant_attachment_efficiency = real(defaultSoilAttachmentEfficiency, dp)
        river_attachment_efficiency = real(defaultRiverAttachmentEfficiency, dp)
        estuary_attachment_efficiency = defaultEstuaryAttachmentEfficiency
        darcy_velocity = defaultSoilDarcyVelocity
        estuary_meandering_factor = 0.0
        river_meandering_factor = 0.0
        !porosity = 0.0
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

    ! Compute number of point sources per (x,y) cell by inspecting coordinates.
    subroutine calculateNPointSourcesDatabase(me, maxPointSources)
        class(Database) :: me
        integer, intent(in) :: maxPointSources
        integer :: i, j, p
        real(dp) :: px, py

        if (allocated(me%nPointSources)) deallocate(me%nPointSources)
        allocate(me%nPointSources(me%gridShape(1), me%gridShape(2)))
        me%nPointSources = 0

        if (.not. allocated(me%emissionsPointWaterCoords)) return
        if (maxPointSources <= 0) return

        do j = 1, me%gridShape(2)
            do i = 1, me%gridShape(1)
                do p = 1, maxPointSources
                    px = me%emissionsPointWaterCoords(i, j, p, 1)
                    py = me%emissionsPointWaterCoords(i, j, p, 2)
                    if (px /= nf90_fill_double .and. py /= nf90_fill_double) then
                        if ((abs(px) > C%epsilon .or. abs(py) > C%epsilon) .and. &
                            .not. (px < -9.9e8_dp .and. py < -9.9e8_dp)) then
                            me%nPointSources(i, j) = me%nPointSources(i, j) + 1
                        end if
                    end if
                end do
            end do
        end do
    end subroutine calculateNPointSourcesDatabase


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
        real(dp)        :: minTemp, maxTemp
        integer         :: minTempDay
        real(dp)        :: waterTemperature(366)
        integer         :: i
        real(dp)        :: angle(366)

        ! angle = 2*pi*(day - day_min)/366
        do i = 1, 366
            angle(i) = 2.0_dp*C%pi * real(i - minTempDay, dp) / 366.0_dp
        end do

        waterTemperature = 0.5_dp*(maxTemp - minTemp) * cos(angle) + 0.5_dp*(maxTemp + minTemp)
    end function calculateWaterTemperatureTimeSeriesWaterBody


    !> Audit the database
    function auditDatabase(me) result(rslt)
        class(Database) :: me
        type(Result)    :: rslt
        integer         :: x, y, i
        integer         :: xi, yi
        integer         :: nx, ny
        logical         :: simulationMaskError

        simulationMaskError = .false.

        ! Is the simulation mask self-contained (no inflows to area to simulate)?
        if (C%hasSimulationMask) then
            nx = me%gridShape(1)
            ny = me%gridShape(2)
            do y = 1, ny
                do x = 1, nx
                    if (me%simulationMask(x, y)) then
                        ! me%inflows is (d,w,x,y) where d=2 holds (x,y) origin indices
                        do i = 1, size(me%inflows, dim=2)
                            xi = me%inflows(1, i, x, y)
                            yi = me%inflows(2, i, x, y)
                            ! Skip invalid/out-of-domain inflow indices
                            if (xi >= 1 .and. xi <= nx .and. yi >= 1 .and. yi <= ny) then
                                if (.not. me%simulationMask(xi, yi)) simulationMaskError = .true.
                            end if
                        end do
                    end if
                end do
            end do
        end if

        if (simulationMaskError) then
            call rslt%addError(ErrorInstance( &
                message="Simulation mask provided has inflows from outside the area to " // &
                        "simulate. Please provide a simulation mask that is self-contained." ))
        end if

        ! Bounds checks for sediment calibration parameters (arrays may be unallocated)
        if (allocated(me%depositionAlpha)) then
            if (any(me%depositionAlpha < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="Value provided for deposition_alpha must be >= 0. At least one < 0." ))
            end if
        end if
        if (allocated(me%resuspensionAlpha)) then
            if (any(me%resuspensionAlpha < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="Value provided for resuspension_alpha must be >= 0. At least one < 0." ))
            end if
        end if
        if (allocated(me%resuspensionBeta)) then
            if (any(me%resuspensionBeta < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="Value provided for resuspension_beta must be >= 0. At least one < 0." ))
            end if
        end if

        ! Does sediment fractional composition sum to unity?
        if (.not. isZero(1.0_dp - sum(me%sedimentFractionalComposition))) then
            call rslt%addError(ErrorInstance( &
                message="sedimentFractionalComposition must sum to 1. Found: " // &
                        str(sum(me%sedimentFractionalComposition)) ))
        end if

        if (any(me%defaultDistributionContaminant < 0.0)) then
            call rslt%addError(ErrorInstance( &
                message="defaultDistributionContaminant must be non-negative." ))
        end if
        if (.not. isZero(1.0_dp - sum(me%defaultDistributionContaminant))) then
            call rslt%addError(ErrorInstance( &
                message="defaultDistributionContaminant must sum to 1. Found: " // &
                        str(sum(me%defaultDistributionContaminant)) ))
        end if

        if (me%contaminantDensity <= 0.0) then
            call rslt%addError(ErrorInstance( &
                message="contaminantDensity must be positive." ))
        end if
        if (any(me%contaminantSizeClasses <= 0.0)) then
            call rslt%addError(ErrorInstance( &
                message="contaminantSizeClasses must be positive." ))
        end if

        ! Bounds checks for initial concentrations
        if (allocated(me%initialContaminantConcsSoil)) then
            if (any(me%initialContaminantConcsSoil < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsSoil must be non-negative. At least one < 0." ))
            end if
        end if
        if (allocated(me%initialContaminantConcsWater)) then
            if (any(me%initialContaminantConcsWater < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsWater must be non-negative. At least one < 0." ))
            end if
        end if
        if (allocated(me%initialContaminantConcsSediment)) then
            if (any(me%initialContaminantConcsSediment < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialContaminantConcsSediment must be non-negative. At least one < 0." ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsSoil)) then
            if (any(me%initialDissolvedConcsSoil < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsSoil must be non-negative. At least one < 0." ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsWater)) then
            if (any(me%initialDissolvedConcsWater < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsWater must be non-negative. At least one < 0." ))
            end if
        end if
        if (allocated(me%initialDissolvedConcsSediment)) then
            if (any(me%initialDissolvedConcsSediment < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="initialDissolvedConcsSediment must be non-negative. At least one < 0." ))
            end if
        end if

        ! Bounds checks for dissolved emissions
        if (allocated(me%emissionsArealSoilDissolvedContaminant)) then
            if (any(me%emissionsArealSoilDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsArealSoilDissolvedContaminant must be >= 0. At least one < 0." ))
            end if
        end if
        if (allocated(me%emissionsArealWaterDissolvedContaminant)) then
            if (any(me%emissionsArealWaterDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsArealWaterDissolvedContaminant must be >= 0. At least one < 0." ))
            end if
        end if
        if (allocated(me%emissionsAtmosphericDryDepoDissolvedContaminant)) then
            if (any(me%emissionsAtmosphericDryDepoDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsAtmosphericDryDepoDissolvedContaminant must be >= 0. " // &
                            "At least one < 0." ))
            end if
        end if
        if (allocated(me%emissionsAtmosphericWetDepoDissolvedContaminant)) then
            if (any(me%emissionsAtmosphericWetDepoDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsAtmosphericWetDepoDissolvedContaminant must be >= 0. " // &
                            "At least one < 0." ))
            end if
        end if
        if (allocated(me%emissionsPointWaterDissolvedContaminant)) then
            if (any(me%emissionsPointWaterDissolvedContaminant < 0.0_dp)) then
                call rslt%addError(ErrorInstance( &
                    message="emissionsPointWaterDissolvedContaminant must be >= 0. At least one < 0." ))
            end if
        end if
    end function

end module


! AUDITING TO DO:
!   array size checks, particularly variables for size classes of NM and SPM
!   sedimentPorosity: 0 <= x <= 1
!   water content at saturation is greater than water content at field capacity
!   are batch runs contiguous (dates follow on from each other for different chunks)
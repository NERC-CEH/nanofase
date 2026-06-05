!> Module container for the DataOutput class
module DataOutputModule
    use PFASEConstantsModule, only: PFAS_AQ, PFAS_SOL, PFAS_SPM, PFAS_AWI, PFAS_FOAM, PFAS_AIR
    use DefaultsModule, only: iouOutputSummary, iouOutputWater, &
        iouOutputSediment, iouOutputSoil, iouOutputSSD, iouOutputStats, iouOutputBiota
    use GlobalsModule, only: C, dp
    use DataInputModule, only: DATASET
    use LoggerModule, only: LOGR
    use AbstractEnvironmentModule
    use EnvironmentModule
    use AbstractGridCellModule
    use RiverReachModule
    use EstuaryReachModule
    use UtilModule
    use datetime_module
    use mo_netcdf
    use NetCDFOutputModule
    use NetCDFAggregatedOutputModule
    use ContaminantModule
    implicit none

! -----------------------------------------------------------------------------
! P-FASE UPDATE:
! This module has been converted from nanoparticle free/attached/SPM-size pools
! to PFAS phase pools: AQ, SOL, SPM, AWI, FOAM and AIR. Groundwater is handled
! as an exported boundary flux, not as an internal P-FASE compartment.
! -----------------------------------------------------------------------------

    !> The DataOutput class is responsible for writing output data to disk
    type, public :: DataOutput
        character(len=256)                  :: outputPath               !! Path to the output directory
        type(EnvironmentPointer)            :: env                      !! Pointer to the environment, to retrieve state variables
        class(NetCDFOutput), allocatable    :: ncout                    !! NetCDF output class
        ! Storing variables across timesteps for dynamics calculations
        real(dp), allocatable            :: previousSSDByLayer(:,:)
        real(dp), allocatable            :: previousSSD(:)
      contains
        procedure, public :: init => initDataOutput
        procedure, public :: initSedimentSizeDistribution => initSedimentSizeDistributionDataOutput
        procedure, public :: update => updateDataOutput
        procedure, public :: finalise => finaliseDataOutput
        procedure, public :: newChunk => newChunkDataOutput
        procedure, public :: finaliseChunk => finaliseChunkDataOutput
        procedure, private :: writeHeaders => writeHeadersDataOutput
        procedure, private :: writeHeadersSimulationSummary => writeHeadersSimulationSummaryDataOutput
        procedure, private :: writeHeadersWater => writeHeadersWaterDataOutput
        procedure, private :: writeHeadersSediment => writeHeadersSedimentDataOutput
        procedure, private :: writeHeadersSoil => writeHeadersSoilDataOutput
        procedure, private :: writeHeadersStats => writeHeadersStatsDataOutput
        procedure, private :: updateWater => updateWaterDataOutput
        procedure, private :: updateSediment => updateSedimentDataOutput
        procedure, private :: updateSoil => updateSoilDataOutput
        procedure, private :: updateBiota => updateBiotaDataOutput
        procedure, private :: writeHeadersBiota => writeHeadersBiotaDataOutput
        procedure, public :: updateSedimentSizeDistribution => updateSedimentSizeDistributionDataOutput
    end type

  contains

    !> Initialise the data output be creating the relevant output files and writing
    !! their headers and metadata
    subroutine initDataOutput(this, env)
        class(DataOutput)         :: this
        type(Environment), target   :: env
        
        ! Point the Environment object to that passed in
        this%env%item => env
        ! Allocate the appropriate NetCDF output object, depending on whether we're aggregating
        ! to grid cell or not
        if (C%includeWaterbodyBreakdown) then
            allocate(NetCDFOutput :: this%ncout)
        else 
            allocate(NetCDFAggregatedOutput :: this%ncout)
        end if

        if (C%writeNetCDF) then
            call this%ncout%init(env, 1)
        end if

        ! Open the files to write to
        open(iouOutputSummary, file=trim(C%outputPath) // 'summary' // trim(C%outputHash) // '.md')
        if (C%writeCSV) then
            open(iouOutputWater, file=trim(C%outputPath) // 'output_water' // trim(C%outputHash) // '.csv')
            open(iouOutputSediment, file=trim(C%outputPath) // 'output_sediment' // trim(C%outputHash) // '.csv')
            open(iouOutputSoil, file=trim(C%outputPath) // 'output_soil' // trim(C%outputHash) // '.csv')
            if (DATASET%hasBiota) &
                open(iouOutputBiota, file=trim(C%outputPath) // 'output_biota' // trim(C%outputHash) // '.csv')
        end if
        if (C%writeCompartmentStats) then
            open(iouOutputStats, file=trim(C%outputPath) // 'stats' // trim(C%outputHash) // '.csv')
        end if

        ! Write the headers for the files
        call this%writeHeaders()
    end subroutine

    !> Initialise the sediment size distribution steady state run output data file
    subroutine initSedimentSizeDistributionDataOutput(this)
        class(DataOutput)         :: this
        integer                   :: i, j
 
        ! Sediment begins with distribution given in the input data
        allocate(this%previousSSD, source=DATASET%sedimentInitialMass)
        allocate(this%previousSSDByLayer(C%nSedimentLayers, C%nSizeClassesSpm))
        do i = 1, C%nSedimentLayers
            this%previousSSDByLayer(i,:) = DATASET%sedimentInitialMass
        end do

        ! Open the SSD file and write the headers
        open(iouOutputSSD, file=trim(C%outputPath) // 'output_ssd' // trim(C%outputHash) // '.csv')
        if (C%writeMetadataAsComment) then
            write(iouOutputSSD, '(a)') "# NanoFASE model output data - SEDIMENT SIZE DISTRIBUTION."
            write(iouOutputSSD, '(a)') "# Output file for running model until sediment size distribution is at steady state."
            write(iouOutputSSD, '(a)') "# Each row represents a complete model run."
            write(iouOutputSSD, '(a)') "#\ti: model run index"
            write(iouOutputSSD, '(a)') "#\tssd_sci_all_layers: sediment size distribution, averaged across sediment layers"
            write(iouOutputSSD, '(a)') "#\tssd_sci_lj: sediment size distribution across size classes i, for layer j"
            write(iouOutputSSD, '(a)') "#\tdelta_max_lj: maximum difference between size distribution bins for layer j"
            write(iouOutputSSD, '(a)') "#\tdelta_max_all_layers: maximum difference for size distribution averaged across layers"
        end if
        write(iouOutputSSD, '(a)', advance='no') "i,"
        write(iouOutputSSD, '(*(a))', advance='no') ('ssd_sc'//trim(str(i))//'_all_layers,', i=1, C%nSizeClassesSpm)
        write(iouOutputSSD, '(*(a))', advance='no') (('ssd_sc'//trim(str(i))//'_l'//trim(str(j))//',', &
            i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))', advance='no') ('delta_max_l'//trim(str(i))//',', i=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))') 'delta_max_all_layers'
    end subroutine

    !> Save the output from the current timestep to the output files
    subroutine updateDataOutput(this, t, tInChunk)
        class(DataOutput)   :: this
        integer             :: t, tInChunk, x, y
        type(datetime)      :: date
        character(len=100)  :: dateISO
        real                :: easts, norths

        ! Get the date for this timestep
        date = C%batchStartDate + timedelta(t - 1)
        dateISO = date%isoformat()
        
        ! Loop through the grid cells and update each compartment
        do y = 1, size(this%env%item%colGridCells, dim=2)
            do x = 1, size(this%env%item%colGridCells, dim=1)
                ! Only write data if cell isn't masked
                if (DATASET%simulationMask(x,y)) then
                    easts = DATASET%x(x)
                    norths = DATASET%y(y)
                    call this%updateWater(t, tInChunk, x, y, dateISO, easts, norths)
                    call this%updateSediment(t, tInChunk, x, y, dateISO, easts, norths)
                    call this%updateSoil(t, tInChunk, x, y, dateISO, easts, norths)
                    call this%updateBiota(t, x, y, dateISO, easts, norths)
                    if (C%writeNetCDF) then
                        call this%ncout%updateWater(t, tInChunk, x, y)
                        call this%ncout%updateSediment(t, tInChunk, x, y)
                        call this%ncout%updateSoil(t, tInChunk, x, y)
                        call this%ncout%updateBiota(t, tInChunk, x, y)
                    end if
                end if
            end do
        end do
    end subroutine

    !> Update the water output file for the current timestep
    subroutine updateWaterDataOutput(this, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)   :: this                             !! The DataOutput instance
        integer             :: t, tInChunk, x, y
        character(len=*)    :: date
        real                :: easts, norths
        integer             :: i, w, f
        character(len=3)    :: reachType
        real(dp)            :: m_spm(C%nSizeClassesSpm)
        real(dp)            :: C_spm(C%nSizeClassesSpm)
        type(Contaminant)   :: m_contaminant, j_contaminant_outflow, j_contaminant_deposition, j_contaminant_resuspension
        real(dp)            :: C_contaminant, C_dissolved, C_attached
        real(dp)            :: vol
        real(dp)            :: s_aq, s_sol, s_dep_aq, s_dep_sol, s_res_aq, s_res_sol
        real(dp)            :: s_out_aq, s_out_sol
        type(Result0D)      :: r
        character(len=256)  :: tr = "DataOutputModule.f90%updateWaterDataOutput"

        if (.not. C%writeCSV) return

        if (C%includeWaterbodyBreakdown) then
            do w = 1, this%env%item%colGridCells(x,y)%item%nReaches
                associate (reach => this%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                    select type (reach)
                        type is (RiverReach);   reachType = 'riv'
                        type is (EstuaryReach); reachType = 'est'
                    end select

                    ! --- state ---
                    m_contaminant = reach%get_m_contaminant()
                    vol = reach%volume

                    r = m_contaminant%getConcentration(vol)
                    if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                        call r%addToTrace(tr)
                        call m_contaminant%finalise()
                        return
                    end if
                    C_contaminant = r%getDataAsRealDP()

                    if (vol > C%epsilon) then
                        C_dissolved = m_contaminant%m_dissolved / vol
                        C_attached  = sum(m_contaminant%get_phase(PFAS_SPM)) / vol
                    else
                        C_dissolved = 0.0_dp
                        C_attached  = 0.0_dp
                    end if

                    ! --- fluxes (guard against unallocated %c) ---
                    j_contaminant_outflow     = reach%j_contaminant_outflow
                    j_contaminant_deposition  = reach%j_contaminant_deposition
                    j_contaminant_resuspension= reach%j_contaminant_resuspension

                    if (allocated(m_contaminant%c)) then
                        s_aq = sum(m_contaminant%c(:,:,PFAS_AQ))
                        s_sol  = sum(m_contaminant%c(:,:,PFAS_SPM))
                    else
                        s_aq = 0.0_dp
                        s_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_deposition%c)) then
                        s_dep_aq = sum(j_contaminant_deposition%c(:,:,PFAS_AQ))
                        s_dep_sol  = sum(j_contaminant_deposition%c(:,:,PFAS_SPM))
                    else
                        s_dep_aq = 0.0_dp
                        s_dep_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_resuspension%c)) then
                        s_res_aq = sum(j_contaminant_resuspension%c(:,:,PFAS_AQ))
                        s_res_sol  = sum(j_contaminant_resuspension%c(:,:,PFAS_SPM))
                    else
                        s_res_aq = 0.0_dp
                        s_res_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_outflow%c)) then
                        s_out_aq = sum(j_contaminant_outflow%c(:,:,PFAS_AQ))
                        s_out_sol  = sum(j_contaminant_outflow%c(:,:,PFAS_SPM))
                    else
                        s_out_aq = 0.0_dp
                        s_out_sol  = 0.0_dp
                    end if

                    ! --- write row (keep original column order) ---
                    write(iouOutputWater, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                        trim(str(x)) // "," // trim(str(y)) // "," // &
                        trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(w)) // "," // reachType // "," // &
                        trim(str(s_aq)) // "," // &
                        trim(str(C_contaminant)) // "," // &
                        trim(str(s_sol)) // "," // &
                        trim(str(C_attached)) // "," // &
                        trim(str(m_contaminant%m_dissolved)) // "," // &
                        trim(str(C_dissolved)) // "," // &
                        trim(str(s_dep_aq)) // "," // &
                        trim(str(s_dep_sol)) // "," // &
                        trim(str(s_res_aq)) // "," // &
                        trim(str(s_res_sol)) // "," // &
                        trim(str(s_out_aq)) // "," // &
                        trim(str(s_out_sol)) // "," // &
                        trim(str(j_contaminant_outflow%m_dissolved)) // "," // &
                        trim(str(sum(reach%m_spm))) // "," // &
                        trim(str(sum(reach%C_spm))) // ","

                    do f = 1, C%contaminantDim(2)
                        if (allocated(m_contaminant%c)) then
                            write(iouOutputWater, '(a)', advance='no') trim(str(sum(m_contaminant%c(:,f,:)))) // ","
                        else
                            write(iouOutputWater, '(a)', advance='no') "0.0,"
                        end if
                    end do

                    if (C%includeSpmSizeClassBreakdown) then
                        write(iouOutputWater, '(*(a))', advance='no') (trim(str(reach%m_spm(i))) // "," // &
                            trim(str(reach%C_spm(i))) // ",", i=1, C%nSizeClassesSpm)
                    end if
                    if (C%includeSedimentFluxes) then
                        write(iouOutputWater, '(a)', advance='no') trim(str(sum(reach%j_spm%soilErosion))) // "," // &
                            trim(str(sum(reach%j_spm%deposition))) // "," // &
                            trim(str(sum(reach%j_spm%resuspension))) // "," // &
                            trim(str(sum(reach%j_spm%inflow))) // "," // &
                            trim(str(sum(reach%j_spm%outflow))) // "," // &
                            trim(str(sum(reach%j_spm%bankErosion))) // ","
                    end if
                    write(iouOutputWater, '(a)') trim(str(reach%volume)) // "," // trim(str(reach%depth)) // "," // &
                        trim(str(reach%Q%outflow / C%timeStep))

                    call m_contaminant%finalise()
                end associate
            end do

        else
            associate (cell => this%env%item%colGridCells(x,y)%item)
                if (cell%nReaches > 0) then
                    ! --- aggregated state ---
                    m_contaminant          = cell%get_m_contaminant_water()
                    j_contaminant_outflow  = cell%get_j_contaminant_outflow()
                    j_contaminant_deposition   = cell%get_j_contaminant_deposition()
                    j_contaminant_resuspension = cell%get_j_contaminant_resuspension()
                    vol = cell%getWaterVolume()

                    r = m_contaminant%getConcentration(vol)
                    if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                        call r%addToTrace(tr)
                        call m_contaminant%finalise()
                        return
                    end if
                    C_contaminant = r%getDataAsRealDP()

                    if (vol > C%epsilon) then
                        C_dissolved = m_contaminant%m_dissolved / vol
                        C_attached  = sum(m_contaminant%get_phase(PFAS_SPM)) / vol
                    else
                        C_dissolved = 0.0_dp
                        C_attached  = 0.0_dp
                    end if

                    if (allocated(m_contaminant%c)) then
                        s_aq = sum(m_contaminant%c(:,:,PFAS_AQ))
                        s_sol  = sum(m_contaminant%c(:,:,PFAS_SPM))
                    else
                        s_aq = 0.0_dp
                        s_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_deposition%c)) then
                        s_dep_aq = sum(j_contaminant_deposition%c(:,:,PFAS_AQ))
                        s_dep_sol  = sum(j_contaminant_deposition%c(:,:,PFAS_SPM))
                    else
                        s_dep_aq = 0.0_dp
                        s_dep_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_resuspension%c)) then
                        s_res_aq = sum(j_contaminant_resuspension%c(:,:,PFAS_AQ))
                        s_res_sol  = sum(j_contaminant_resuspension%c(:,:,PFAS_SPM))
                    else
                        s_res_aq = 0.0_dp
                        s_res_sol  = 0.0_dp
                    end if

                    if (allocated(j_contaminant_outflow%c)) then
                        s_out_aq = sum(j_contaminant_outflow%c(:,:,PFAS_AQ))
                        s_out_sol  = sum(j_contaminant_outflow%c(:,:,PFAS_SPM))
                    else
                        s_out_aq = 0.0_dp
                        s_out_sol  = 0.0_dp
                    end if

                    write(iouOutputWater, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                        trim(str(x)) // "," // trim(str(y)) // "," // &
                        trim(str(easts)) // "," // trim(str(norths)) // "," // cell%aggregatedReachType // "," // &
                        trim(str(s_aq)) // "," // &
                        trim(str(C_contaminant)) // "," // &
                        trim(str(s_sol)) // "," // &
                        trim(str(C_attached)) // "," // &
                        trim(str(m_contaminant%m_dissolved)) // "," // &
                        trim(str(C_dissolved)) // "," // &
                        trim(str(s_dep_aq)) // "," // &
                        trim(str(s_dep_sol)) // "," // &
                        trim(str(s_res_aq)) // "," // &
                        trim(str(s_res_sol)) // "," // &
                        trim(str(s_out_aq)) // "," // &
                        trim(str(s_out_sol)) // "," // &
                        trim(str(j_contaminant_outflow%m_dissolved)) // ","

                    m_spm = cell%get_m_spm()
                    C_spm = cell%get_C_spm()
                    write(iouOutputWater, '(a)', advance='no') trim(str(sum(m_spm))) // "," // &
                        trim(str(sum(C_spm))) // ","

                    do f = 1, C%contaminantDim(2)
                        if (allocated(m_contaminant%c)) then
                            write(iouOutputWater, '(a)', advance='no') trim(str(sum(m_contaminant%c(:,f,:)))) // ","
                        else
                            write(iouOutputWater, '(a)', advance='no') "0.0,"
                        end if
                    end do

                    if (C%includeSpmSizeClassBreakdown) then
                        write(iouOutputWater, '(*(a))', advance='no') (trim(str(m_spm(i))) // "," // &
                            trim(str(C_spm(i))) // ",", i=1, C%nSizeClassesSpm)
                    end if
                    if (C%includeSedimentFluxes) then
                        write(iouOutputWater, '(a)', advance='no') &
                            trim(str(sum(cell%get_j_spm_soilErosion()))) // "," // &
                            trim(str(sum(cell%get_j_spm_deposition()))) // "," // &
                            trim(str(sum(cell%get_j_spm_resuspension()))) // "," // &
                            trim(str(sum(cell%get_j_spm_inflow()))) // "," // &
                            trim(str(sum(cell%get_j_spm_outflow()))) // "," // &
                            trim(str(sum(cell%colRiverReaches(1)%item%j_spm%bankErosion))) // ","
                    end if
                    write(iouOutputWater, '(a)') trim(str(cell%getWaterVolume())) // "," // &
                        trim(str(cell%getWaterDepth())) // "," // &
                        trim(str(cell%get_Q_outflow() / C%timeStep))

                    call m_contaminant%finalise()
                end if
            end associate
        end if
    end subroutine


    !> Update the sediment output file on the current timestep
    subroutine updateSedimentDataOutput(this, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)   :: this
        integer             :: t, tInChunk, x, y
        character(len=*)    :: date
        real                :: easts, norths
        integer             :: w, l, f
        character(len=3)    :: reachType
        type(Contaminant)   :: m_contaminant, m_buried
        real(dp)            :: C_contaminant, C_byMass, C_byMass_layer
        type(Result0D)      :: r, res_l, res_get
        real(dp)            :: total_sediment_mass
        character(len=256)  :: tr = "DataOutputModule.f90%updateSedimentDataOutput"

        if (C%writeCSV) then
            if (C%includeWaterbodyBreakdown) then
                do w = 1, this%env%item%colGridCells(x,y)%item%nReaches
                    associate (reach => this%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                        select type (reach)
                            type is (RiverReach); reachType = 'riv'
                            type is (EstuaryReach); reachType = 'est'
                        end select
                        
                        res_get = reach%bedSediment%get_m_contaminant()
                        if (res_get%hasCriticalError() .or. .not. allocated(res_get%data)) then
                            call res_get%addToTrace(tr); return
                        end if
                        select type (data => res_get%getData())
                            type is (Contaminant); m_contaminant = data
                            class default; return
                        end select

                        r = m_contaminant%getConcentration(reach%bedArea * sum(C%sedimentLayerDepth))
                        if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                            call r%addToTrace(tr); call m_contaminant%finalise(); return
                        end if
                        C_contaminant = r%getDataAsRealDP()
                        
                        total_sediment_mass = reach%bedSediment%Mf_bed_all()
                        if (total_sediment_mass > C%epsilon) then
                            C_byMass = (sum(m_contaminant%c) + m_contaminant%m_dissolved) / total_sediment_mass
                        else
                            C_byMass = 0.0_dp
                        end if
                        
                        res_get = reach%bedSediment%get_m_contaminant_buried()
                        if (res_get%hasCriticalError() .or. .not. allocated(res_get%data)) then
                            call res_get%addToTrace(tr); return
                        end if
                        select type (data => res_get%getData())
                            type is (Contaminant); m_buried = data
                            class default; return
                        end select
                        
                        write(iouOutputSediment, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // trim(str(w)) // "," // reachType // "," // &
                            trim(str(sum(m_contaminant%c(:,:,PFAS_AQ)) * reach%bedArea)) // "," // &
                            trim(str(C_contaminant)) // "," // &
                            trim(str(C_byMass)) // ","
                        do f = 1, C%contaminantDim(2)
                            write(iouOutputSediment, '(a)', advance='no') &
                                trim(str(sum(m_contaminant%c(:,f,:)) * reach%bedArea)) // ","
                        end do
                        if (C%includeSedimentLayerBreakdown) then
                            do l = 1, C%nSedimentLayers
                                res_l = reach%bedSediment%get_m_contaminant_l(l)
                                if (res_l%hasCriticalError() .or. .not. allocated(res_l%data)) then
                                    call res_l%addToTrace(tr); cycle
                                end if
                                select type (data => res_l%getData())
                                    type is (Contaminant); m_contaminant = data
                                    class default; cycle
                                end select
                                r = m_contaminant%getConcentration(reach%bedSediment%colBedSedimentLayers(l)%item%V_layer())
                                if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                                    call r%addToTrace(tr); call m_contaminant%finalise(); call m_buried%finalise(); return
                                end if
                                
                                total_sediment_mass = reach%bedSediment%colBedSedimentLayers(l)%item%M_f_layer()
                                if (total_sediment_mass > C%epsilon) then
                                    C_byMass_layer = (sum(m_contaminant%c) + m_contaminant%m_dissolved) / total_sediment_mass
                                else
                                    C_byMass_layer = 0.0_dp
                                end if
                                
                                write(iouOutputSediment, '(a)', advance='no') trim(str(r%getDataAsRealDP())) // "," // &
                                    trim(str(C_byMass_layer)) // ","
                                call m_contaminant%finalise()
                            end do
                        end if
                        write(iouOutputSediment, '(a)') &
                            trim(str(sum(m_buried%c(:,:,PFAS_AQ)) * reach%bedArea)) // "," // &
                            trim(str(reach%bedArea)) // "," // &
                            trim(str(reach%bedSediment%Mf_bed_all() * reach%bedArea)) // "," // &
                            trim(str(reach%bedSediment%Mf_bed_all() / sum(C%sedimentLayerDepth)))
                        call m_contaminant%finalise()
                        call m_buried%finalise()
                    end associate
                end do
            else
                associate (cell => this%env%item%colGridCells(x,y)%item)
                    if (cell%nReaches > 0) then
                        ! Get contaminant mass directly (cell getters are not wrapped in Result0D)
                        m_contaminant = cell%get_m_contaminant_sediment()
                        
                        r = m_contaminant%getConcentration(cell%getBedSedimentArea() * sum(C%sedimentLayerDepth))
                        if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                            call r%addToTrace(tr); call m_contaminant%finalise(); return
                        end if
                        C_contaminant = r%getDataAsRealDP()
                        
                        total_sediment_mass = cell%getBedSedimentMass()
                        if (total_sediment_mass > C%epsilon) then
                            C_byMass = (sum(m_contaminant%c) + m_contaminant%m_dissolved) / total_sediment_mass
                        else
                            C_byMass = 0.0_dp
                        end if
                        
                        ! Get buried contaminant mass directly
                        m_buried = cell%get_m_contaminant_buried_sediment()
                        
                        write(iouOutputSediment, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // cell%aggregatedReachType // "," // &
                            trim(str(sum(m_contaminant%c(:,:,PFAS_AQ)))) // "," // &
                            trim(str(C_contaminant)) // "," // &
                            trim(str(C_byMass)) // ","
                        do f = 1, C%contaminantDim(2)
                            write(iouOutputSediment, '(a)', advance='no') trim(str(sum(m_contaminant%c(:,f,:)))) // ","
                        end do
                        if (C%includeSedimentLayerBreakdown) then
                            associate (bedSediment => cell%colRiverReaches(1)%item%bedSediment)
                                do l = 1, C%nSedimentLayers
                                    res_l = bedSediment%get_m_contaminant_l(l)
                                    if (res_l%hasCriticalError() .or. .not. allocated(res_l%data)) then
                                        call res_l%addToTrace(tr); cycle
                                    end if
                                    select type (data => res_l%getData())
                                        type is (Contaminant); m_contaminant = data
                                        class default; cycle
                                    end select
                                    r = m_contaminant%getConcentration(bedSediment%colBedSedimentLayers(l)%item%V_layer())
                                    if (r%hasCriticalError() .or. .not. allocated(r%data)) then
                                        call r%addToTrace(tr); call m_contaminant%finalise(); call m_buried%finalise(); return
                                    end if
                                    
                                    total_sediment_mass = bedSediment%colBedSedimentLayers(l)%item%M_f_layer()
                                    if (total_sediment_mass > C%epsilon) then
                                        C_byMass_layer = (sum(m_contaminant%c) + m_contaminant%m_dissolved) / total_sediment_mass
                                    else
                                        C_byMass_layer = 0.0_dp
                                    end if
                                    
                                    write(iouOutputSediment, '(a)', advance='no') trim(str(r%getDataAsRealDP())) // "," // &
                                        trim(str(C_byMass_layer)) // ","
                                    call m_contaminant%finalise()
                                end do
                            end associate
                        end if
                        write(iouOutputSediment, '(a)') &
                            trim(str(sum(m_buried%c(:,:,PFAS_AQ)))) // "," // &
                            trim(str(cell%getBedSedimentArea())) // "," // trim(str(cell%getBedSedimentMass())) // "," // &
                            trim(str(cell%getBedSedimentMass() / (cell%getBedSedimentArea() * sum(C%sedimentLayerDepth))))
                        call m_contaminant%finalise()
                        call m_buried%finalise()
                    end if
                end associate
            end if
        end if
    end subroutine

    subroutine updateSoilDataOutput(this, t, tInChunk, x, y, date, easts, norths)
        class(DataOutput)  :: this
        integer            :: t, tInChunk, x, y
        character(len=*)   :: date
        real               :: easts, norths
        integer            :: i, l, f
        type(Contaminant)  :: m_contaminant, m_eroded, m_buried
        real(dp)           :: C_contaminant, C_attached, C_dissolved
        real(dp)           :: C_layer_total, C_dissolved_layer
        real(dp)           :: profile_volume, profile_mass
        real(dp)           :: layer_volume, layer_mass

        if (C%writeCSV) then
            do i = 1, this%env%item%colGridCells(x,y)%item%nSoilProfiles
                associate (profile => this%env%item%colGridCells(x,y)%item%colSoilProfiles(i)%item)

                    ! --- masses in the whole profile ---
                    m_contaminant = profile%get_m_contaminant()

                    ! total *volume* of soil in profile (sum of layer volumes already in m^3)
                    profile_volume = sum([(profile%colSoilLayers(l)%item%volume, l = 1, C%nSoilLayers)])
                    ! convert to dry-soil mass [kg] using bulk density
                    profile_mass   = profile%bulkDensity * profile_volume

                    ! --- concentrations in kg/kg (mass / dry-soil mass) ---
                    if (profile_mass > C%epsilon) then
                        C_contaminant = ( sum(m_contaminant%get_phase(PFAS_AQ))        &
                                        + sum(m_contaminant%get_phase(PFAS_SOL))    &
                                        + m_contaminant%m_dissolved ) / profile_mass

                        C_attached    =  sum(m_contaminant%get_phase(PFAS_SOL)) / profile_mass
                        C_dissolved   =  m_contaminant%m_dissolved          / profile_mass
                    else
                        C_contaminant = 0.0_dp
                        C_attached    = 0.0_dp
                        C_dissolved   = 0.0_dp
                    end if

                    ! erosion/burial masses (unchanged)
                    m_eroded = profile%m_contaminant_eroded
                    m_buried = profile%m_contaminant_buried

                    ! -------- write CSV row header + profile totals --------
                    write(iouOutputSoil, '(a)', advance='no') trim(str(t)) // "," // trim(date) // "," // &
                        trim(str(x)) // "," // trim(str(y)) // "," // trim(str(easts)) // "," // trim(str(norths)) // "," // &
                        trim(str(i)) // "," // trim(profile%dominantLandUseName) // "," // &
                        trim(str(sum(m_contaminant%c(:,:,PFAS_AQ)))) // "," // &   ! m_contaminant_pristine_total(kg)
                        trim(str(sum(m_contaminant%c(:,:,PFAS_SOL)))) // "," // & ! m_contaminant_attached_total(kg)
                        trim(str(m_contaminant%m_dissolved)) // "," // &                        ! m_dissolved_total(kg)
                        trim(str(C_contaminant)) // "," // &                                    ! C_contaminant_total(kg/kg)
                        trim(str(C_attached)) // "," // &                                       ! C_contaminant_attached(kg/kg)
                        trim(str(C_dissolved)) // ","                                          ! C_dissolved_total(kg/kg)

                    ! per-form masses (unchanged)
                    do f = 1, C%contaminantDim(2)
                        write(iouOutputSoil, '(a)', advance='no') trim(str(sum(m_contaminant%c(:,f,:)))) // ","
                    end do

                    ! optional: state breakdown totals (unchanged, but keep order)
                    if (C%includeSoilStateBreakdown) then
                        write(iouOutputSoil, '(a)', advance='no') &
                            trim(str(sum(m_contaminant%get_phase(PFAS_AQ)))) // "," // &
                            trim(str(sum(m_contaminant%get_phase(PFAS_SOL)))) // ","
                    end if

                    ! -------- per-layer concentrations (kg/kg) --------
                    if (C%includeSoilLayerBreakdown) then
                        do l = 1, C%nSoilLayers
                            m_contaminant = profile%colSoilLayers(l)%item%m_contaminant

                            layer_volume = profile%colSoilLayers(l)%item%volume
                            layer_mass   = profile%bulkDensity * layer_volume

                            if (layer_mass > C%epsilon) then
                                C_layer_total    = ( sum(m_contaminant%get_phase(PFAS_AQ))       &
                                                + sum(m_contaminant%get_phase(PFAS_SOL))   &
                                                + m_contaminant%m_dissolved ) / layer_mass
                                C_dissolved_layer = m_contaminant%m_dissolved / layer_mass
                            else
                                C_layer_total     = 0.0_dp
                                C_dissolved_layer = 0.0_dp
                            end if

                            write(iouOutputSoil, '(a)', advance='no') &
                                trim(str(C_layer_total)) // "," // &
                                trim(str(C_dissolved_layer)) // ","

                            if (C%includeSoilStateBreakdown) then
                                write(iouOutputSoil, '(a)', advance='no') &
                                    trim(str(sum(m_contaminant%get_phase(PFAS_AQ)))) // "," // &
                                    trim(str(sum(m_contaminant%get_phase(PFAS_SOL)))) // ","
                            end if
                            call m_contaminant%finalise()
                        end do
                    end if

                    ! -------- erosion yields (unchanged) --------
                    if (C%includeSoilErosionYields) then
                        write(iouOutputSoil, '(a)', advance='no') &
                            trim(str(sum(profile%erodedSediment) * profile%area)) // "," // &
                            trim(str(sum(m_eroded%c(:,:,PFAS_AQ)))) // "," // &
                            trim(str(sum(m_eroded%c(:,:,PFAS_SOL)))) // ","
                    end if

                    ! -------- burial + bulk density (unchanged) --------
                    write(iouOutputSoil, '(a)') &
                        trim(str(sum(m_buried%c(:,:,PFAS_AQ)))) // "," // &
                        trim(str(sum(m_buried%c(:,:,PFAS_SOL)))) // "," // &
                        trim(str(m_buried%m_dissolved)) // "," // &
                        trim(str(profile%bulkDensity))

                    call m_contaminant%finalise()
                    call m_eroded%finalise()
                    call m_buried%finalise()
                end associate
            end do
        end if
    end subroutine

    function updateSedimentSizeDistributionDataOutput(this, i_model) result(delta_max)
        class(DataOutput)   :: this
        integer             :: i_model
        real(dp)            :: delta_max, m_sediment_byLayer(C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp)            :: sedimentSizeDistributionByLayer(C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp)            :: sedimentSizeDistribution(C%nSizeClassesSpm)
        integer             :: i, j
        real(dp)            :: delta_max_l(C%nSedimentLayers)

        m_sediment_byLayer = this%env%item%get_m_sediment_byLayer()
        sedimentSizeDistribution = sum(m_sediment_byLayer, dim=1) / sum(m_sediment_byLayer)
        do j = 1, C%nSedimentLayers
            sedimentSizeDistributionByLayer(j,:) = m_sediment_byLayer(j,:) / sum(m_sediment_byLayer(j,:))
            delta_max_l(j) = maxval(abs(this%previousSSDByLayer(j,:) - sedimentSizeDistributionByLayer(j,:)))
        end do
        delta_max = maxval(abs(this%previousSSD - sedimentSizeDistribution))
        write(iouOutputSSD, '(a)', advance='no') trim(str(i_model)) // ","
        write(iouOutputSSD, '(*(a))', advance='no') (trim(str(sedimentSizeDistribution(i))) // ",", i=1, C%nSizeClassesSpm)
        write(iouOutputSSD, '(*(a))', advance='no') ((trim(str(sedimentSizeDistributionByLayer(j,i))) // ",", &
            i=1, C%nSizeClassesSpm), j=1, C%nSedimentLayers)
        write(iouOutputSSD, '(*(a))', advance='no') (trim(str(delta_max_l(i)))//',', i=1, C%nSedimentLayers)
        write(iouOutputSSD, '(a)') trim(str(delta_max))
        this%previousSSD = sedimentSizeDistribution
        this%previousSSDByLayer = sedimentSizeDistributionByLayer
    end function

    subroutine finaliseDataOutput(this, iSteadyState)
        class(DataOutput)  :: this
        integer            :: iSteadyState
        real(dp)           :: timeUntilSteadyState
        type(Contaminant)  :: cont_soil, cont_water, cont_sediment
        real(dp)           :: total_mass_water, mean_mass_water, total_mass_sediment, mean_mass_sediment
        integer            :: t

        if (.not. C%runToSteadyState) then
            write(iouOutputSummary, *) "\n## PECs"
        else
            write(iouOutputSummary, *) "\n## PECs (final model iteration)"
        end if
        cont_soil = this%env%item%get_C_contaminant_soil()
        cont_water = this%env%item%get_C_contaminant_water()
        cont_sediment = this%env%item%get_C_contaminant_sediment()
        write(iouOutputSummary, *) "- Soil, spatial mean on final timestep: " // &
            trim(str(sum(cont_soil%c(:,:,PFAS_AQ)))) // " kg/kg soil"
        
        total_mass_water = 0.0_dp
        if (allocated(this%env%item%contaminant_water_t)) then
            do t = 1, size(this%env%item%contaminant_water_t)
                total_mass_water = total_mass_water + sum(this%env%item%contaminant_water_t(t)%c) &
                                                    + this%env%item%contaminant_water_t(t)%m_dissolved
            end do
            if (size(this%env%item%contaminant_water_t) > 0) then
                mean_mass_water = total_mass_water / size(this%env%item%contaminant_water_t)
            else
                mean_mass_water = 0.0_dp
            end if
        else
            mean_mass_water = 0.0_dp
        end if
        write(iouOutputSummary, *) "- Water, spatiotemporal mean: " // &
            trim(str(mean_mass_water)) // " kg/m3"

        total_mass_sediment = 0.0_dp
        if (allocated(this%env%item%contaminant_sediment_t)) then
            do t = 1, size(this%env%item%contaminant_sediment_t)
                total_mass_sediment = total_mass_sediment + sum(this%env%item%contaminant_sediment_t(t)%c) &
                                                        + this%env%item%contaminant_sediment_t(t)%m_dissolved
            end do
            if (size(this%env%item%contaminant_sediment_t) > 0) then
                mean_mass_sediment = total_mass_sediment / size(this%env%item%contaminant_sediment_t)
            else
                mean_mass_sediment = 0.0_dp
            end if
        else
            mean_mass_sediment = 0.0_dp
        end if
        write(iouOutputSummary, *) "- Sediment, spatiotemporal mean: " // &
            trim(str(mean_mass_sediment)) // " kg/kg sediment"

        call cont_soil%finalise()
        call cont_water%finalise()
        call cont_sediment%finalise()

        timeUntilSteadyState = iSteadyState * C%timeStep * C%nTimestepsInBatch
        if (C%runToSteadyState) then
            write(iouOutputSummary, *) "\n## Steady state"
            write(iouOutputSummary, *) "- Iterations until steady state: " // trim(str(iSteadyState))
            write(iouOutputSummary, *) "- Time until steady state: " // trim(str(timeUntilSteadyState)) // " s"
        end if

        close(iouOutputSummary); close(iouOutputWater); close(iouOutputSediment)
        close(iouOutputSoil); close(iouOutputSSD); close(iouOutputStats)
        if (DATASET%hasBiota .and. C%writeCSV) close(iouOutputBiota)
        
        call LOGR%add('Model output written to ' // trim(C%outputPath), COLOR_GREEN)
    end subroutine

    subroutine newChunkDataOutput(this, k)
        class(DataOutput)  :: this
        integer            :: k
        if (C%writeNetCDF .and. C%netCDFWriteMode == 'end') then
            call this%ncout%newChunk(k)
        end if
    end subroutine

    subroutine finaliseChunkDataOutput(this, tStart, isFinalChunk)
        class(DataOutput)  :: this
        integer            :: tStart
        logical            :: isFinalChunk
        if (C%writeNetCDF .and. C%netCDFWriteMode == 'end') then
            call this%ncout%finaliseChunk(tStart)
        end if
        if (C%writeNetCDF .and. isFinalChunk) then
            call this%ncout%close()
        end if
    end subroutine

    subroutine writeHeadersDataOutput(this)
        class(DataOutput)   :: this
        call this%writeHeadersSimulationSummary()
        if (C%writeCSV) then
            call this%writeHeadersWater()
            call this%writeHeadersSediment()
            call this%writeHeadersSoil()
            if (DATASET%hasBiota) call this%writeHeadersBiota()
        end if
        if (C%writeCompartmentStats) then
            call this%writeHeadersStats()
        end if
    end subroutine

    subroutine writeHeadersSimulationSummaryDataOutput(this)
        class(DataOutput)   :: this
        type(datetime)      :: simDatetime

        simDatetime = simDatetime%now()
        write(iouOutputSummary, '(a)') "# NanoFASE model simulation summary"
        write(iouOutputSummary, '(a)') " - Description: " // trim(C%runDescription)
        write(iouOutputSummary, '(a)') " - Simulation datetime: " // simDatetime%isoformat()
        write(iouOutputSummary, '(a)') " - Model version: " // C%modelVersion
        write(iouOutputSummary, '(a)') " - Is batch run? " // trim(str(C%isBatchRun))
        write(iouOutputSummary, '(a)') " - Number of batches: " // trim(str(C%nChunks))
        write(iouOutputSummary, '(a)', advance='no') " - Is steady state run? " // trim(str(C%runToSteadyState))
        if (C%runToSteadyState) then
            write(iouOutputSummary, '(a)') " (" // trim(C%steadyStateMode) // " mode)"
        else
            write(iouOutputSummary, '(a)') ""
        end if
        write(iouOutputSummary, *) "\n## Temporal domain"
        write(iouOutputSummary, *) "- Start date: " // C%batchStartDate%strftime('%Y-%m-%d')
        write(iouOutputSummary, *) "- End date: " // C%batchEndDate%strftime('%Y-%m-%d')
        write(iouOutputSummary, *) "- Timestep length: " // trim(str(C%timeStep)) // " s"
        write(iouOutputSummary, *) "- Number of timesteps: " // trim(str(C%nTimestepsInBatch))
        write(iouOutputSummary, *) "\n## Spatial domain"
        write(iouOutputSummary, *) "- Grid resolution: " // trim(str(DATASET%gridRes(1))) // ", " // &
            trim(str(DATASET%gridRes(2))) // " m" 
        write(iouOutputSummary, *) "- Grid bounds: " // trim(str(DATASET%gridBounds(1))) // ", " // &
            trim(str(DATASET%gridBounds(2))) // &
            ", " // trim(str(DATASET%gridBounds(3))) // ", " // trim(str(DATASET%gridBounds(4))) // " m"
        write(iouOutputSummary, *) "- Grid shape: " // trim(str(DATASET%gridShape(1))) // ", " // trim(str(DATASET%gridShape(2)))
        write(iouOutputSummary, *) "- Number of non-empty grid cells: " // trim(str(this%env%item%nGridCells))
        write(iouOutputSummary, *) "- Is simulation masked? " // trim(str(C%hasSimulationMask))
        write(iouOutputSummary, *) "- Number of non-masked grid cells: " // trim(str(DATASET%nNonMaskedCells))
    end subroutine

    subroutine writeHeadersWaterDataOutput(this)
        class(DataOutput)   :: this
        integer             :: i, f
        
        if (C%writeMetadataAsComment) then
            write(iouOutputWater, '(a)') "# NanoFASE model output data - WATER."
            write(iouOutputWater, '(a)') "# See summary.md for model run metadata."
            write(iouOutputWater, '(a)') "# Columns:"
            write(iouOutputWater, '(a)') "#\tt: timestep index"
            write(iouOutputWater, '(a)') "#\tdatetime: datetime of this timestep"
            write(iouOutputWater, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputWater, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            if (C%includeWaterbodyBreakdown) write(iouOutputWater, '(a)') "#\tw: waterbody index within this grid cell"
            if (C%includeWaterbodyBreakdown) then
                write(iouOutputWater, '(a)') "#\twaterbody_type: what type (river, estuary etc) is this waterbody?"
            else
                write(iouOutputWater, '(a)') "#\twaterbody_type: what is the dominant waterbody type in this cell?"
            end if
            write(iouOutputWater, '(a)') "#\tm_contaminant_pristine(kg), m_contaminant_attached(kg), m_dissolved(kg): " // &
                "contaminant mass (AQ, SOL, SPM, AWI, FOAM, AIR, kg)"
            write(iouOutputWater, '(a)') "#\tC_contaminant_total(kg/m3), C_contaminant_attached(kg/m3), C_dissolved(kg/m3): " // &
                "contaminant concentration (total, attached, dissolved, kg/m3)"
            write(iouOutputWater, '(a)') "#\tm_contaminant_pristine_deposited(kg), m_contaminant_attached_deposited(kg): " // &
                "deposited contaminant masses (kg)"
            write(iouOutputWater, '(a)') "#\tm_contaminant_pristine_resuspended(kg), m_contaminant_attached_resuspended(kg): " // &
                "resuspended contaminant masses (kg)"
            write(iouOutputWater, '(a)') "#\tm_contaminant_pristine_outflow(kg), " // &
                "m_contaminant_attached_outflow(kg), m_dissolved_outflow(kg): " // &
                "outflow contaminant masses (kg)"
            write(iouOutputWater, '(a)') "#\tm_spm(kg), C_spm(kg/m3): mass and concentration of SPM (kg, kg/m3)"
            write(iouOutputWater, '(a)') "#\tm_contaminant_form_f(kg): contaminant mass for form f (kg)"
            if (C%includeSpmSizeClassBreakdown) then
                write(iouOutputWater, '(a)') "#\tm_spm_sci(kg), C_spm_sci(kg/m3): " // &
                "mass and concentration of SPM in size class i (kg, kg/m3)"
            end if
            if (C%includeSedimentFluxes) then
                write(iouOutputWater, '(a)') "#\tm_spm_erosion(kg), m_spm_dep(kg), m_spm_res(kg), m_spm_inflow(kg), " // &
                    "m_spm_outflow(kg), m_spm_bank_erosion(kg): SPM fluxes (kg)"
            end if
            write(iouOutputWater, '(a)') "#\tvolume(m3), depth(m), flow(m3/s): volume (m3), depth (m), flow rate (m3/s)"
        end if
        write(iouOutputWater, '(a)', advance='no') "t,datetime,x,y,easts,norths,"
        if (C%includeWaterbodyBreakdown) write(iouOutputWater, '(a)', advance='no') "w,"
        write(iouOutputWater, '(a)', advance='no') "waterbody_type,m_contaminant_pristine(kg),C_contaminant_total(kg/m3)," // &
            "m_contaminant_attached(kg),C_contaminant_attached(kg/m3),m_dissolved(kg),C_dissolved(kg/m3)," // &
            "m_contaminant_pristine_deposited(kg),m_contaminant_attached_deposited(kg)," // &
            "m_contaminant_pristine_resuspended(kg),m_contaminant_attached_resuspended(kg)," // &
            "m_contaminant_pristine_outflow(kg),m_contaminant_attached_outflow(kg),m_dissolved_outflow(kg)," // &
            "m_spm(kg),C_spm(kg/m3),"
        write(iouOutputWater, '(*(a))', advance='no') ("m_contaminant_form" // trim(str(f)) // "(kg),", f=1, C%contaminantDim(2))
        if (C%includeSpmSizeClassBreakdown) then
            write(iouOutputWater, '(*(a))', advance="no") &
                ("m_spm_sc" // trim(str(i)) // "(kg),C_spm_sc" // trim(str(i)) // "(kg/m3),", i=1, C%nSizeClassesSpm) 
        end if
        if (C%includeSedimentFluxes) then
            write(iouOutputWater, '(a)', advance='no') "m_spm_erosion(kg),m_spm_dep(kg),m_spm_res(kg)," // &
                "m_spm_inflow(kg),m_spm_outflow(kg),m_spm_bank_erosion(kg),"
        end if
        write(iouOutputWater, '(a)') "volume(m3),depth(m),flow(m3/s)"
    end subroutine

    subroutine writeHeadersSedimentDataOutput(this)
        class(DataOutput)   :: this
        integer             :: i, f

        if (C%writeMetadataAsComment) then
            write(iouOutputSediment, '(a)') "# NanoFASE model output data - SEDIMENT."
            write(iouOutputSediment, '(a)') "# See summary.md for model run metadata."
            write(iouOutputSediment, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputSediment, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            if (C%includeWaterbodyBreakdown) write(iouOutputSediment, '(a)') "#\tw: waterbody index within this grid cell"
            if (C%includeWaterbodyBreakdown) then
                write(iouOutputSediment, '(a)') "#\twaterbody_type: what type (river, estuary etc) is this sediment in?"
            else
                write(iouOutputSediment, '(a)') "#\twaterbody_type: dominant waterbody type in this cell"
            end if
            write(iouOutputSediment, '(a)') "#\tm_contaminant_pristine_total(kg), " // &
                "C_contaminant_total(kg/m3), C_contaminant_total(kg/kg): " // &
                "contaminant mass (kg) and concentration (kg/m3, kg/kg dry weight) for all layers"
            write(iouOutputSediment, '(a)') "#\tm_contaminant_form_f(kg): contaminant mass for form f (kg)"
            if (C%includeSedimentLayerBreakdown) then
                write(iouOutputSediment, '(a)') "#\tC_contaminant_li(kg/m3), C_contaminant_li(kg/kg): contaminant conc for layer i"
            end if
            write(iouOutputSediment, '(a)') "#\tm_contaminant_pristine_buried(kg): contaminant mass buried (kg)"
            write(iouOutputSediment, '(a)') "#\tbed_area(m2): area of this bed sediment (m2)"
            write(iouOutputSediment, '(a)') "#\tsediment_mass(kg): total mass of fine sediment (kg)"
            write(iouOutputSediment, '(a)') "#\tsediment_density(kg/m3): average density of the sediment"
        end if
        write(iouOutputSediment, '(a)', advance="no") "t,datetime,x,y,easts,norths," 
        if (C%includeWaterbodyBreakdown) write(iouOutputSediment, '(a)', advance='no') "w,"
        write(iouOutputSediment, '(a)', advance='no') &
            "waterbody_type,m_contaminant_pristine_total(kg),C_contaminant_total(kg/m3)," // &
            "C_contaminant_total(kg/kg),"
        write(iouOutputSediment, '(*(a))', advance='no') ("m_contaminant_form" // trim(str(f)) // "(kg),", f=1, C%contaminantDim(2))
        if (C%includeSedimentLayerBreakdown) then
            write(iouOutputSediment, '(*(a))', advance="no") &
                ("C_contaminant_l" // trim(str(i)) // "(kg/m3),C_contaminant_l" &
                // trim(str(i)) // "(kg/kg),", i = 1, C%nSedimentLayers) 
        end if
        write(iouOutputSediment, '(a)') "m_contaminant_pristine_buried(kg),bed_area(m2),sediment_mass(kg),sediment_density(kg/m3)"
    end subroutine

    subroutine writeHeadersSoilDataOutput(this)
        class(DataOutput)   :: this
        integer             :: i, f

        if (C%writeMetadataAsComment) then
            write(iouOutputSoil, '(a)') "# NanoFASE model output data - SOIL."
            write(iouOutputSoil, '(a)') "# See summary.md for model run metadata."
            write(iouOutputSoil, '(a)') "# Columns:"
            write(iouOutputSoil, '(a)') "#\tt: timestep index"
            write(iouOutputSoil, '(a)') "#\tdatetime: datetime of this timestep"
            write(iouOutputSoil, '(a)') "#\tx, y: grid cell (eastings and northings) index"
            write(iouOutputSoil, '(a)') "#\teasts, norths: eastings and northings at the centre of this grid cell (m)"
            write(iouOutputSoil, '(a)') "#\tp: soil profile index within this cell"
            write(iouOutputSoil, '(a)') "#\tland_use: dominant land use of this soil profile"
            write(iouOutputSoil, '(a)') "#\tm_contaminant_pristine_total(kg), "// &
                "m_contaminant_attached_total(kg), m_dissolved_total(kg): " // &
                "contaminant mass (AQ, SOL, SPM, AWI, FOAM, AIR) in whole soil profile"
            write(iouOutputSoil, '(a)') "#\tC_contaminant_total(" // C%soilPECUnits // "), " // & 
                "C_contaminant_attached(" // C%soilPECUnits // &
                "), C_dissolved_total(" // C%soilPECUnits // "): contaminant concentration"
            write(iouOutputSoil, '(a)') "#\tm_contaminant_form_f(kg): contaminant mass for form f (kg)"
            if (C%includeSoilStateBreakdown) then
                write(iouOutputSoil, '(a)') "#\tC_contaminant_pristine_free(" // C%soilPECUnits // "), " // &
                    "C_contaminant_attached(" // C%soilPECUnits // &
                    "): free and attached contaminant concentration"
            end if
            if (C%includeSoilLayerBreakdown) then
                write(iouOutputSoil, '(a)') "#\tC_contaminant_li(" // C%soilPECUnits // "), " // &
                    "C_dissolved_li(" // C%soilPECUnits // &
                    "): contaminant concentration for layer i"
                if (C%includeSoilStateBreakdown) then
                    write(iouOutputSoil, '(a)') "#\tC_contaminant_pristine_free_li(" // C%soilPECUnits // "), " // &
                        "C_contaminant_attached_li(" // &
                        C%soilPECUnits // "): free and attached contaminant concentration for layer i"
                end if
            end if
            if (C%includeSoilErosionYields) then
                write(iouOutputSoil, '(a)') "#\tm_soil_eroded(kg), " // &
                    "m_contaminant_pristine_eroded(kg), m_contaminant_attached_eroded(kg): " // &
                    "mass of soil and contaminant eroded"
            end if
            write(iouOutputSoil, '(a)') "#\tm_contaminant_pristine_buried(kg), " // &
                "m_contaminant_attached_buried(kg), m_dissolved_buried(kg): " // &
                "mass of contaminant buried"
            write(iouOutputSoil, '(a)') "#\tbulk_density(kg/m3): bulk density of this soil profile"
        end if
        write(iouOutputSoil, '(a)', advance="no") "t,datetime,x,y,easts,norths,p,land_use," // &
            "m_contaminant_pristine_total(kg),m_contaminant_attached_total(kg),m_dissolved_total(kg)," // &
            "C_contaminant_total(" // C%soilPECUnits // "),C_contaminant_attached(" // C%soilPECUnits // &
            "),C_dissolved_total(" // C%soilPECUnits // "),"
        write(iouOutputSoil, '(*(a))', advance='no') ("m_contaminant_form" // trim(str(f)) // &
            "(" // C%soilPECUnits // "),", f=1, C%contaminantDim(2))
        if (C%includeSoilStateBreakdown) then
            write(iouOutputSoil, '(a)', advance="no") "C_contaminant_pristine_free(" // C%soilPECUnits // ")," // &
                "C_contaminant_attached(" // &
                C%soilPECUnits // "),"
        end if
        if (C%includeSoilLayerBreakdown) then
            write(iouOutputSoil, '(*(a))', advance="no") &
                ("C_contaminant_l" // trim(str(i)) // "(" // C%soilPECUnits // "),C_dissolved_l" // trim(str(i)) // &
                 "(" // C%soilPECUnits // "),", i = 1, C%nSoilLayers)
            if (C%includeSoilStateBreakdown) then
                write(iouOutputSoil, '(*(a))', advance="no") &
                    ("C_contaminant_pristine_free_l" // trim(str(i)) // "(" // C%soilPECUnits // "),C_contaminant_attached_l" // &
                     trim(str(i)) // "(" // C%soilPECUnits // "),", i = 1, C%nSoilLayers)
            end if
        end if
        if (C%includeSoilErosionYields) then
            write(iouOutputSoil, '(a)', advance='no') "m_soil_eroded(kg),m_contaminant_pristine_eroded(kg)," // & 
            "m_contaminant_attached_eroded(kg),"
        end if
        write(iouOutputSoil, '(a)') "m_contaminant_pristine_buried(kg),m_contaminant_attached_buried(kg)," // & 
            "m_dissolved_buried(kg),bulk_density(kg/m3)"
    end subroutine

    subroutine writeHeadersStatsDataOutput(this)
        class(DataOutput)  :: this
        if (C%writeMetadataAsComment) then
            write(iouOutputStats, '(a)') "# NanoFASE model output data - COMPARTMENT STATS."
            write(iouOutputStats, '(a)') "# This file contains summary statistics for each environmental compartment."
        end if
    end subroutine

    !> Write headers for the biota output CSV file
    subroutine writeHeadersBiotaDataOutput(this)
        class(DataOutput)   :: this
        integer             :: s

        if (C%writeMetadataAsComment) then
            write(iouOutputBiota, '(a)') "# P-FASE model output data - BIOTA."
            write(iouOutputBiota, '(a)') "# See summary.md for model run metadata."
            write(iouOutputBiota, '(a)') "# Columns:"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "t: timestep index"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "datetime: datetime at the start of this timestep"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "x, y: grid cell indices (longitudinal, latitudinal)"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "easts, norths: coordinate at centre of grid cell (m)"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "b: biota group index within this grid cell"
            write(iouOutputBiota, '(a)') "#" // achar(9) // "biota_name: name of the represented biota group"
            write(iouOutputBiota, '(a)') "#" // achar(9) // &
                "C_active_si(kg/kg dw): active PFAS body burden for species i"
            write(iouOutputBiota, '(a)') "#" // achar(9) // &
                "C_stored_si(kg/kg dw): stored PFAS body burden for species i"
        end if

        write(iouOutputBiota, '(a)', advance='no') "t,datetime,x,y,easts,norths,b,biota_name,"
        do s = 1, C%contaminantDim(1)
            write(iouOutputBiota, '(a)', advance='no') "C_active_s" // trim(str(s)) // "(kg/kg dw),"
        end do
        do s = 1, C%contaminantDim(1)
            if (s < C%contaminantDim(1)) then
                write(iouOutputBiota, '(a)', advance='no') "C_stored_s" // trim(str(s)) // "(kg/kg dw),"
            else
                write(iouOutputBiota, '(a)') "C_stored_s" // trim(str(s)) // "(kg/kg dw)"
            end if
        end do
    end subroutine

    !> Write biota state variables (C_active, C_stored) to the biota CSV output file
    subroutine updateBiotaDataOutput(this, t, x, y, date, easts, norths)
        class(DataOutput)   :: this
        integer, intent(in) :: t, x, y
        character(len=*)    :: date
        real                :: easts, norths
        integer             :: w, b, s, nSpecies
        logical             :: wroteAny

        if (.not. (C%writeCSV .and. DATASET%hasBiota)) return

        nSpecies = C%contaminantDim(1)
        wroteAny = .false.

        ! Loop over water reaches — collect water biota
        do w = 1, this%env%item%colGridCells(x,y)%item%nReaches
            associate (reach => this%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                do b = 1, reach%nBiota
                    associate (bio => reach%biota(b))
                        if (.not. allocated(bio%C_active) .or. .not. allocated(bio%C_stored)) cycle
                        write(iouOutputBiota, '(a)', advance='no') &
                            trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // &
                            trim(str(b)) // "," // trim(bio%name) // ","
                        do s = 1, nSpecies
                            if (s <= size(bio%C_active)) then
                                write(iouOutputBiota, '(a)', advance='no') trim(str(bio%C_active(s))) // ","
                            else
                                write(iouOutputBiota, '(a)', advance='no') "0.0,"
                            end if
                        end do
                        do s = 1, nSpecies
                            if (s <= size(bio%C_stored)) then
                                if (s < nSpecies) then
                                    write(iouOutputBiota, '(a)', advance='no') trim(str(bio%C_stored(s))) // ","
                                else
                                    write(iouOutputBiota, '(a)') trim(str(bio%C_stored(s)))
                                end if
                            else
                                if (s < nSpecies) then
                                    write(iouOutputBiota, '(a)', advance='no') "0.0,"
                                else
                                    write(iouOutputBiota, '(a)') "0.0"
                                end if
                            end if
                        end do
                        wroteAny = .true.
                    end associate
                end do
            end associate
        end do

        ! Loop over soil profiles — collect soil biota
        do b = 1, this%env%item%colGridCells(x,y)%item%nSoilProfiles
            associate (profile => this%env%item%colGridCells(x,y)%item%colSoilProfiles(b)%item)
                if (.not. allocated(profile%biota)) cycle
                do s = 1, size(profile%biota)
                    associate (bio => profile%biota(s))
                        if (.not. allocated(bio%C_active) .or. .not. allocated(bio%C_stored)) cycle
                        write(iouOutputBiota, '(a)', advance='no') &
                            trim(str(t)) // "," // trim(date) // "," // &
                            trim(str(x)) // "," // trim(str(y)) // "," // &
                            trim(str(easts)) // "," // trim(str(norths)) // "," // &
                            trim(str(s)) // "," // trim(bio%name) // ","
                        do b = 1, nSpecies
                            if (b <= size(bio%C_active)) then
                                write(iouOutputBiota, '(a)', advance='no') trim(str(bio%C_active(b))) // ","
                            else
                                write(iouOutputBiota, '(a)', advance='no') "0.0,"
                            end if
                        end do
                        do b = 1, nSpecies
                            if (b <= size(bio%C_stored)) then
                                if (b < nSpecies) then
                                    write(iouOutputBiota, '(a)', advance='no') trim(str(bio%C_stored(b))) // ","
                                else
                                    write(iouOutputBiota, '(a)') trim(str(bio%C_stored(b)))
                                end if
                            else
                                if (b < nSpecies) then
                                    write(iouOutputBiota, '(a)', advance='no') "0.0,"
                                else
                                    write(iouOutputBiota, '(a)') "0.0"
                                end if
                            end if
                        end do
                    end associate
                end do
            end associate
        end do
    end subroutine

end module

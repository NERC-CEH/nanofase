module NetCDFAggregatedOutputModule
    use GlobalsModule, only: C, dp, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
    use UtilModule
    use mo_netcdf, only: NcDataset, NcVariable, NcDimension, nf90_fill_int, nf90_fill_double
    use DataInputModule, only: DATASET
    use EnvironmentModule
    use AbstractEnvironmentModule, only: EnvironmentPointer
    use datetime_module
    use NetCDFOutputModule
    use ContaminantModule
    implicit none

    !> Class for outputting data to a NetCDF file, aggregated at the grid cell level
    type, public, extends(NetCDFOutput) :: NetCDFAggregatedOutput
        ! The NetCDF variables are inherited from NetCDFOutput; we define model output variables
        ! with aggregated dimensions (no waterbody dimension)
        real(dp), allocatable :: output_agg_water__m_contaminant(:,:,:,:)
        real(dp), allocatable :: output_agg_water__C_contaminant(:,:,:,:)
        real(dp), allocatable :: output_agg_water__C_contaminant_free(:,:,:)
        real(dp), allocatable :: output_agg_water__C_contaminant_attached(:,:,:)
        real(dp), allocatable :: output_agg_water__j_contaminant_outflow(:,:,:,:)
        real(dp), allocatable :: output_agg_water__j_contaminant_deposited(:,:,:,:)
        real(dp), allocatable :: output_agg_water__j_contaminant_resuspended(:,:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm(:,:,:)
        real(dp), allocatable :: output_agg_water__C_spm(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_erosion(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_deposition(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_resuspended(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_inflow(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_outflow(:,:,:)
        real(dp), allocatable :: output_agg_water__m_spm_bank_erosion(:,:,:)
        real(dp), allocatable :: output_agg_water__volume(:,:,:)
        real(dp), allocatable :: output_agg_water__depth(:,:,:)
        real(dp), allocatable :: output_agg_water__flow(:,:,:)
        real(dp), allocatable :: output_agg_sediment__m_contaminant_total(:,:,:,:)
        real(dp), allocatable :: output_agg_sediment__C_contaminant_total(:,:,:,:)
        real(dp), allocatable :: output_agg_sediment__C_contaminant_layers(:,:,:,:,:)
        real(dp), allocatable :: output_agg_sediment__m_contaminant_buried(:,:,:,:)
        real(dp), allocatable :: output_agg_sediment__bed_area(:,:,:)
        real(dp), allocatable :: output_agg_sediment__mass(:,:,:)
        real(dp), allocatable :: output_agg_soil__land_use(:,:)
        real(dp), allocatable :: output_agg_soil__bulk_density(:,:)
    contains
        procedure, public :: init => initNetCDFAggregatedOutput
        procedure, public :: updateWater => updateWaterNetCDFAggregatedOutput
        procedure, public :: updateSediment => updateSedimentNetCDFAggregatedOutput
        procedure, private :: initWater => initWaterNetCDFAggregatedOutput
        procedure, private :: initSediment => initSedimentNetCDFAggregatedOutput
        procedure, private :: createDimensions => createDimensionsNetCDFAggregatedOutput
        procedure, private :: allocateVariables => allocateVariablesNetCDFAggregatedOutput
        procedure, public :: newChunk => newChunkNetCDFAggregatedOutput
        procedure, public :: finaliseChunk => finaliseChunkNetCDFAggregatedOutput
    end type

contains

    !> Initialise the NetCDF output class by creating the NetCDF file and allocating space
    !! for the output variables (if we're in write-at-end mode and it's needed)
    subroutine initNetCDFAggregatedOutput(me, env, k)
        class(NetCDFAggregatedOutput):: me
        type(Environment), target :: env
        integer :: k
        
        ! Point the Environment object to that passed in
        me%env%item => env

        ! Create a NetCDF file for this chunk
        call me%initFile()

        ! Only allocate space for output variables if we're writing the NetCDF file at the end of the chunk,
        ! otherwise the output is written directly to the NetCDF file and these variables aren't needed
        if (C%netCDFWriteMode == 'end') then
            call me%allocateVariables(k)
        end if
    end subroutine

    !> Update either the NetCDF file or the in-memory output variables on this time step
    subroutine updateWaterNetCDFAggregatedOutput(me, t, tInChunk, x, y)
        class(NetCDFAggregatedOutput):: me
        integer :: t, tInChunk, x, y
        type(Contaminant) :: m_contaminant
        type(Contaminant) :: j_contaminant_outflow
        type(Contaminant) :: j_contaminant_deposited
        type(Contaminant) :: j_contaminant_resuspended
        real(dp) :: C_contaminant
        real(dp) :: C_dissolved
        real(dp) :: volume
        type(Result0D) :: r
        character(len=256) :: tr
        tr = "NetCDFAggregatedOutputModule%updateWaterNetCDFAggregatedOutput"

        associate (cell => me%env%item%colGridCells(x,y)%item)
            if (cell%nReaches > 0) then
                m_contaminant = cell%get_m_contaminant_water()
                r = m_contaminant%getConcentration(cell%getWaterVolume())
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    return
                end if
                if (.not. allocated(r%data)) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                    return
                end if

                C_contaminant = r%getDataAsRealDP()
                C_dissolved   = m_contaminant%m_dissolved / cell%getWaterVolume()
                j_contaminant_outflow     = cell%get_j_contaminant_outflow()
                j_contaminant_deposited   = cell%get_j_contaminant_deposition()
                j_contaminant_resuspended = cell%get_j_contaminant_resuspension()
                volume = cell%getWaterVolume()

                if (C%netCDFWriteMode == 'end') then
                    ! ---- form-first (form, x, y, t) ----
                    me%output_agg_water__m_contaminant(FREE_CONTAMINANT,    x, y, tInChunk) = &
                        sum(m_contaminant%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_water__m_contaminant(ATTACHED_CONTAMINANT, x, y, tInChunk) = &
                        sum(m_contaminant%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_water__m_contaminant(C%contaminantDim(3),  x, y, tInChunk) = &
                        m_contaminant%m_dissolved

                    ! store a value per form in the aggregated file
                    me%output_agg_water__C_contaminant(FREE_CONTAMINANT,     x, y, tInChunk) = C_contaminant
                    me%output_agg_water__C_contaminant(ATTACHED_CONTAMINANT, x, y, tInChunk) = C_contaminant
                    me%output_agg_water__C_contaminant(C%contaminantDim(3),  x, y, tInChunk) = C_dissolved

                    if (C%includeSoilStateBreakdown) then
                        me%output_agg_water__C_contaminant_free(   x, y, tInChunk) = &
                            sum(m_contaminant%get_free())     / cell%getWaterVolume()
                        me%output_agg_water__C_contaminant_attached(x, y, tInChunk) = &
                            sum(m_contaminant%get_attached()) / cell%getWaterVolume()
                    end if

                    me%output_agg_water__j_contaminant_outflow(FREE_CONTAMINANT,     x, y, tInChunk) = &
                        sum(j_contaminant_outflow%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_water__j_contaminant_outflow(ATTACHED_CONTAMINANT,  x, y, tInChunk) = &
                        sum(j_contaminant_outflow%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_water__j_contaminant_outflow(C%contaminantDim(3),   x, y, tInChunk) = &
                        j_contaminant_outflow%m_dissolved

                    me%output_agg_water__j_contaminant_deposited(FREE_CONTAMINANT,    x, y, tInChunk) = &
                        sum(j_contaminant_deposited%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_water__j_contaminant_deposited(ATTACHED_CONTAMINANT, x, y, tInChunk) = &
                        sum(j_contaminant_deposited%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_water__j_contaminant_deposited(C%contaminantDim(3),  x, y, tInChunk) = &
                        j_contaminant_deposited%m_dissolved

                    me%output_agg_water__j_contaminant_resuspended(FREE_CONTAMINANT,     x, y, tInChunk) = &
                        sum(j_contaminant_resuspended%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_water__j_contaminant_resuspended(ATTACHED_CONTAMINANT,  x, y, tInChunk) = &
                        sum(j_contaminant_resuspended%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_water__j_contaminant_resuspended(C%contaminantDim(3),   x, y, tInChunk) = &
                        j_contaminant_resuspended%m_dissolved

                    me%output_agg_water__m_spm( x, y, tInChunk) = sum(cell%get_m_spm())
                    me%output_agg_water__C_spm( x, y, tInChunk) = sum(cell%get_C_spm())

                    if (C%includeSedimentFluxes) then
                        me%output_agg_water__m_spm_erosion(    x, y, tInChunk) = sum(cell%get_j_spm_soilErosion())
                        me%output_agg_water__m_spm_deposition( x, y, tInChunk) = sum(cell%get_j_spm_deposition())
                        me%output_agg_water__m_spm_resuspended(x, y, tInChunk) = sum(cell%get_j_spm_resuspension())
                        me%output_agg_water__m_spm_inflow(     x, y, tInChunk) = sum(cell%get_j_spm_inflow())
                        me%output_agg_water__m_spm_outflow(    x, y, tInChunk) = sum(cell%get_j_spm_outflow())
                        me%output_agg_water__m_spm_bank_erosion(x, y, tInChunk) = sum(cell%get_j_spm_bankErosion())
                    end if

                    me%output_agg_water__volume(x, y, tInChunk) = volume
                    me%output_agg_water__depth( x, y, tInChunk) = cell%getWaterDepth()
                    me%output_agg_water__flow(  x, y, tInChunk) = cell%get_Q_outflow() / C%timeStep

                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__water__m_contaminant%setData([ &
                        sum(m_contaminant%c(:,:,FREE_CONTAMINANT)), &
                        sum(m_contaminant%c(:,:,ATTACHED_CONTAMINANT)), &
                        m_contaminant%m_dissolved ], start=[1, x, y, t])

                    call me%nc__water__C_contaminant%setData([ &
                        C_contaminant, &
                        C_contaminant, &
                        C_dissolved ], start=[1, x, y, t])

                    if (C%includeSoilStateBreakdown) then
                        call me%nc__water__C_contaminant_free%setData( &
                            sum(m_contaminant%get_free()) / cell%getWaterVolume(), start=[x, y, t])
                        call me%nc__water__C_contaminant_attached%setData( &
                            sum(m_contaminant%get_attached()) / cell%getWaterVolume(), start=[x, y, t])
                    end if

                    call me%nc__water__j_contaminant_outflow%setData([ &
                        sum(j_contaminant_outflow%c(:,:,FREE_CONTAMINANT)), &
                        sum(j_contaminant_outflow%c(:,:,ATTACHED_CONTAMINANT)), &
                        j_contaminant_outflow%m_dissolved ], start=[1, x, y, t])

                    call me%nc__water__j_contaminant_deposited%setData([ &
                        sum(j_contaminant_deposited%c(:,:,FREE_CONTAMINANT)), &
                        sum(j_contaminant_deposited%c(:,:,ATTACHED_CONTAMINANT)), &
                        j_contaminant_deposited%m_dissolved ], start=[1, x, y, t])

                    call me%nc__water__j_contaminant_resuspended%setData([ &
                        sum(j_contaminant_resuspended%c(:,:,FREE_CONTAMINANT)), &
                        sum(j_contaminant_resuspended%c(:,:,ATTACHED_CONTAMINANT)), &
                        j_contaminant_resuspended%m_dissolved ], start=[1, x, y, t])

                    call me%nc__water__m_spm%setData(sum(cell%get_m_spm()), start=[x, y, t])
                    call me%nc__water__C_spm%setData(sum(cell%get_C_spm()), start=[x, y, t])

                    if (C%includeSedimentFluxes) then
                        call me%nc__water__m_spm_erosion%setData(   sum(cell%get_j_spm_soilErosion()), start=[x, y, t])
                        call me%nc__water__m_spm_deposited%setData( sum(cell%get_j_spm_deposition()), start=[x, y, t])
                        call me%nc__water__m_spm_resuspended%setData(sum(cell%get_j_spm_resuspension()), start=[x, y, t])
                        call me%nc__water__m_spm_inflow%setData(    sum(cell%get_j_spm_inflow()),      start=[x, y, t])
                        call me%nc__water__m_spm_outflow%setData(   sum(cell%get_j_spm_outflow()),     start=[x, y, t])
                        call me%nc__water__m_spm_bank_erosion%setData(sum(cell%get_j_spm_bankErosion()), start=[x, y, t])
                    end if

                    call me%nc__water__volume%setData(volume, start=[x, y, t])
                    call me%nc__water__depth%setData(cell%getWaterDepth(), start=[x, y, t])
                    call me%nc__water__flow%setData(cell%get_Q_outflow() / C%timeStep, start=[x, y, t])
                end if
            end if
        end associate
    end subroutine

    !> Update either the NetCDF file or write to the in-memory variables for this timestep 
    subroutine updateSedimentNetCDFAggregatedOutput(me, t, tInChunk, x, y)
        class(NetCDFAggregatedOutput) :: me
        integer :: t, tInChunk, x, y, l
        real(dp), dimension(C%nSedimentLayers) :: C_cont_layers
        type(Contaminant) :: cont, cont_buried, layer_cont
        type(Result0D) :: r0
        character(len=256) :: tr
        tr = "NetCDFAggregatedOutputModule%updateSedimentNetCDFAggregatedOutput"

        associate(cell => me%env%item%colGridCells(x,y)%item)
            if (cell%nReaches > 0) then
                ! Total sediment-phase contaminant mass
                cont = cell%get_m_contaminant_sediment()
                ! Buried contaminant mass
                cont_buried = cell%get_m_contaminant_buried_sediment()
                ! Concentration in each sediment layer (total)
                do l = 1, C%nSedimentLayers
                    layer_cont = cell%get_C_contaminant_sediment_l_byVolume(l)
                    r0 = layer_cont%getConcentration(1.0_dp)  ! Volume already built into getter
                    if (r0%hasCriticalError()) then
                        call r0%addToTrace(tr)
                        call LOGR%toFile(errors=r0%errors)
                        return
                    end if
                    if (.not. allocated(r0%data)) then
                        call r0%addToTrace(tr)
                        call r0%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                        call LOGR%toFile(errors=r0%errors)
                        return
                    end if
                    C_cont_layers(l) = r0%getDataAsRealDP()
                end do

                if (C%netCDFWriteMode == 'end') then
                    ! ---- form-first (form, x, y, t) ----
                    me%output_agg_sediment__m_contaminant_total(FREE_CONTAMINANT,     x, y, tInChunk) = &
                        sum(cont%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_sediment__m_contaminant_total(ATTACHED_CONTAMINANT, x, y, tInChunk) = &
                        sum(cont%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_sediment__m_contaminant_total(C%contaminantDim(3),  x, y, tInChunk) = &
                        cont%m_dissolved

                    me%output_agg_sediment__m_contaminant_buried(FREE_CONTAMINANT,     x, y, tInChunk) = &
                        sum(cont_buried%c(:,:,FREE_CONTAMINANT))
                    me%output_agg_sediment__m_contaminant_buried(ATTACHED_CONTAMINANT, x, y, tInChunk) = &
                        sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_agg_sediment__m_contaminant_buried(C%contaminantDim(3),  x, y, tInChunk) = &
                        cont_buried%m_dissolved

                    me%output_agg_sediment__C_contaminant_total(FREE_CONTAMINANT,      x, y, tInChunk) = &
                        sum(cont%c(:,:,FREE_CONTAMINANT))      / cell%getBedSedimentMass()
                    me%output_agg_sediment__C_contaminant_total(ATTACHED_CONTAMINANT,  x, y, tInChunk) = &
                        sum(cont%c(:,:,ATTACHED_CONTAMINANT))  / cell%getBedSedimentMass()
                    me%output_agg_sediment__C_contaminant_total(C%contaminantDim(3),   x, y, tInChunk) = &
                        cont%m_dissolved / cell%getBedSedimentMass()

                    ! layers-first (layer, form, x, y, t); store total into FREE slot
                    me%output_agg_sediment__C_contaminant_layers(1:C%nSedimentLayers, FREE_CONTAMINANT, &
                                                                x, y, tInChunk) = C_cont_layers

                    me%output_agg_sediment__bed_area(x, y, tInChunk) = cell%getBedSedimentArea()
                    me%output_agg_sediment__mass(    x, y, tInChunk) = cell%getBedSedimentMass()

                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__sediment__m_contaminant_total%setData([ &
                        sum(cont%c(:,:,FREE_CONTAMINANT)), &
                        sum(cont%c(:,:,ATTACHED_CONTAMINANT)), &
                        cont%m_dissolved ], start=[1, x, y, t])

                    call me%nc__sediment__m_contaminant_buried%setData([ &
                        sum(cont_buried%c(:,:,FREE_CONTAMINANT)), &
                        sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT)), &
                        cont_buried%m_dissolved ], start=[1, x, y, t])

                    call me%nc__sediment__C_contaminant_total%setData([ &
                        sum(cont%c(:,:,FREE_CONTAMINANT))      / cell%getBedSedimentMass(), &
                        sum(cont%c(:,:,ATTACHED_CONTAMINANT))  / cell%getBedSedimentMass(), &
                        cont%m_dissolved / cell%getBedSedimentMass() ], start=[1, x, y, t])

                    ! write layers into FREE slot (form=1)
                    call me%nc__sediment__C_contaminant_layers%setData( &
                        C_cont_layers, start=[1, FREE_CONTAMINANT, x, y, t])

                    call me%nc__sediment__bed_area%setData(cell%getBedSedimentArea(), start=[x, y, t])
                    call me%nc__sediment__mass%setData(    cell%getBedSedimentMass(), start=[x, y, t])
                end if
            end if
        end associate
    end subroutine

    !> Create the variables for water (aggregated: [cont_form, x, y, t])
    subroutine initWaterNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput) :: me

        ! Mass by contaminant form (free, attached, dissolved)
        me%nc__water__m_contaminant = me%nc%setVariable('water__m_contaminant', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_contaminant%setAttribute('units', 'kg')
        call me%nc__water__m_contaminant%setAttribute('long_name', &
            'Mass of contaminant in surface water (free, attached, dissolved)')
        call me%nc__water__m_contaminant%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_contaminant%setAttribute('_FillValue', nf90_fill_double)

        ! Concentrations by form (total and – optionally – split)
        me%nc__water__C_contaminant = me%nc%setVariable('water__C_contaminant', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_contaminant%setAttribute('units', 'kg/m3')
        call me%nc__water__C_contaminant%setAttribute('long_name', &
            'Total concentration of contaminant in surface water')
        call me%nc__water__C_contaminant%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_contaminant%setAttribute('_FillValue', nf90_fill_double)

        if (C%includeSoilStateBreakdown) then
            me%nc__water__C_contaminant_free = me%nc%setVariable('water__C_contaminant_free', 'f64', &
                [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__C_contaminant_free%setAttribute('units', 'kg/m3')
            call me%nc__water__C_contaminant_free%setAttribute('long_name', &
                'Concentration of free contaminant in surface water')
            call me%nc__water__C_contaminant_free%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__C_contaminant_free%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__C_contaminant_attached = me%nc%setVariable('water__C_contaminant_attached', 'f64', &
                [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__C_contaminant_attached%setAttribute('units', 'kg/m3')
            call me%nc__water__C_contaminant_attached%setAttribute('long_name', &
                'Concentration of attached contaminant in surface water')
            call me%nc__water__C_contaminant_attached%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__C_contaminant_attached%setAttribute('_FillValue', nf90_fill_double)
        end if

        ! Contaminant fluxes by form
        me%nc__water__j_contaminant_outflow = me%nc%setVariable('water__j_contaminant_outflow', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_outflow%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_outflow%setAttribute('long_name', 'Mass of contaminant outflowing downstream')
        call me%nc__water__j_contaminant_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_outflow%setAttribute('_FillValue', nf90_fill_double)

        me%nc__water__j_contaminant_deposited = me%nc%setVariable('water__j_contaminant_deposited', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_deposited%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_deposited%setAttribute('long_name', 'Mass of contaminant deposited to bed sediment')
        call me%nc__water__j_contaminant_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_deposited%setAttribute('_FillValue', nf90_fill_double)

        me%nc__water__j_contaminant_resuspended = me%nc%setVariable('water__j_contaminant_resuspended', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_resuspended%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_resuspended%setAttribute('long_name', 'Mass of contaminant resuspended from bed sediment')
        call me%nc__water__j_contaminant_resuspended%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_resuspended%setAttribute('_FillValue', nf90_fill_double)

        ! SPM state
        me%nc__water__m_spm = me%nc%setVariable('water__m_spm', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_spm%setAttribute('units', 'kg')
        call me%nc__water__m_spm%setAttribute('long_name', 'Mass of suspended particulate matter in surface water')
        call me%nc__water__m_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_spm%setAttribute('_FillValue', nf90_fill_double)

        me%nc__water__C_spm = me%nc%setVariable('water__C_spm', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_spm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_spm%setAttribute('long_name', 'Concentration of suspended particulate matter in surface water')
        call me%nc__water__C_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_spm%setAttribute('_FillValue', nf90_fill_double)

        ! Optional SPM fluxes (use names that exist on NetCDFOutput: *_deposited*)
        if (C%includeSedimentFluxes) then
            me%nc__water__m_spm_erosion = me%nc%setVariable('water__m_spm_erosion', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_erosion%setAttribute('long_name', 'Mass of SPM eroded from soil')
            call me%nc__water__m_spm_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_erosion%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__m_spm_deposited = me%nc%setVariable('water__m_spm_deposited', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_deposited%setAttribute('units', 'kg')
            call me%nc__water__m_spm_deposited%setAttribute('long_name', 'Mass of SPM deposited to bed sediment')
            call me%nc__water__m_spm_deposited%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_deposited%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__m_spm_resuspended = me%nc%setVariable('water__m_spm_resuspended', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_resuspended%setAttribute('units', 'kg')
            call me%nc__water__m_spm_resuspended%setAttribute('long_name', 'Mass of SPM resuspended from bed sediment')
            call me%nc__water__m_spm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_resuspended%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__m_spm_inflow = me%nc%setVariable('water__m_spm_inflow', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_inflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_inflow%setAttribute('long_name', 'Mass of SPM inflowing from upstream')
            call me%nc__water__m_spm_inflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_inflow%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__m_spm_outflow = me%nc%setVariable('water__m_spm_outflow', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_outflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_outflow%setAttribute('long_name', 'Mass of SPM outflowing downstream')
            call me%nc__water__m_spm_outflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_outflow%setAttribute('_FillValue', nf90_fill_double)

            me%nc__water__m_spm_bank_erosion = me%nc%setVariable('water__m_spm_bank_erosion', 'f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_bank_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_bank_erosion%setAttribute('long_name', 'Mass of SPM eroded from river banks')
            call me%nc__water__m_spm_bank_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_bank_erosion%setAttribute('_FillValue', nf90_fill_double)
        end if

        ! Water volume/depth/flow
        me%nc__water__volume = me%nc%setVariable('water__volume', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__volume%setAttribute('units', 'm3')
        call me%nc__water__volume%setAttribute('long_name', 'Volume of water')
        call me%nc__water__volume%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__volume%setAttribute('_FillValue', nf90_fill_double)

        me%nc__water__depth = me%nc%setVariable('water__depth', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__depth%setAttribute('units', 'm')
        call me%nc__water__depth%setAttribute('standard_name', 'depth')
        call me%nc__water__depth%setAttribute('long_name', 'Depth of water')
        call me%nc__water__depth%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__depth%setAttribute('_FillValue', nf90_fill_double)

        me%nc__water__flow = me%nc%setVariable('water__flow', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__flow%setAttribute('units', 'm3/s')
        call me%nc__water__flow%setAttribute('long_name', 'Discharge out of the cell')
        call me%nc__water__flow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__flow%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    !> Create the variables for bed sediments
    subroutine initSedimentNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput) :: me
        me%nc__sediment__m_contaminant_total = me%nc%setVariable('sediment__m_contaminant_total', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_contaminant_total%setAttribute('units', 'kg')
        call me%nc__sediment__m_contaminant_total%setAttribute('long_name', &
            'Mass of contaminant in sediment (free, attached, dissolved)')
        call me%nc__sediment__m_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_contaminant_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_total = me%nc%setVariable('sediment__C_contaminant_total', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_total%setAttribute('units', 'kg/m3')
        call me%nc__sediment__C_contaminant_total%setAttribute('long_name', &
            'Concentration of contaminant across all sediment layers')
        call me%nc__sediment__C_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_layers = me%nc%setVariable('sediment__C_contaminant_layers', 'f64', &
            [me%sed_l_dim, me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_layers%setAttribute('units', 'kg/m3')
        call me%nc__sediment__C_contaminant_layers%setAttribute('long_name', &
            'Concentration of contaminant by sediment layer')
        call me%nc__sediment__C_contaminant_layers%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_layers%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__m_contaminant_buried = me%nc%setVariable('sediment__m_contaminant_buried', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_contaminant_buried%setAttribute('units', 'kg')
        call me%nc__sediment__m_contaminant_buried%setAttribute('long_name', &
            'Mass of contaminant buried from sediment')
        call me%nc__sediment__m_contaminant_buried%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_contaminant_buried%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__bed_area = me%nc%setVariable('sediment__bed_area', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__bed_area%setAttribute('units', 'm2')
        call me%nc__sediment__bed_area%setAttribute('long_name', 'Surface area of bed sediment')
        call me%nc__sediment__bed_area%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__bed_area%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__mass = me%nc%setVariable('sediment__mass', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__mass%setAttribute('units', 'kg')
        call me%nc__sediment__mass%setAttribute('long_name', 'Mass of fine sediment in bed sediment')
        call me%nc__sediment__mass%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__mass%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    !> Create the dimensions in the NetCDF file (aggregated: no waterbody dim)
    subroutine createDimensionsNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput) :: me

        me%t_dim                 = me%nc%setDimension('t',          C%nTimestepsInBatch)
        me%x_dim                 = me%nc%setDimension('x',          DATASET%gridShape(1))
        me%y_dim                 = me%nc%setDimension('y',          DATASET%gridShape(2))
        me%sed_l_dim             = me%nc%setDimension('sed_l',      C%nSedimentLayers)
        me%contaminant_form_dim  = me%nc%setDimension('contaminant_form', C%contaminantDim(3))
    end subroutine

    !> Allocate space for the in-memory output variables and fill with NetCDF fill value
    subroutine allocateVariablesNetCDFAggregatedOutput(me, k)
        class(NetCDFAggregatedOutput) :: me
        integer :: k
        real(dp), allocatable :: empty2DArray(:,:)
        real(dp), allocatable :: empty3DArray(:,:,:)
        real(dp), allocatable :: empty4DArray(:,:,:,:)
        real(dp), allocatable :: empty5DArraySediment(:,:,:,:,:)
        integer :: nx, ny, nt, nls

        nx  = DATASET%gridShape(1)
        ny  = DATASET%gridShape(2)
        nt  = C%batchNTimesteps(k)
        nls = C%nSoilLayers

        allocate(empty2DArray(nx, ny))
        allocate(empty3DArray(nx, ny, nt))
        allocate(empty4DArray(C%contaminantDim(3), nx, ny, nt))               ! (form,x,y,t)
        allocate(empty5DArraySediment(C%nSedimentLayers, C%contaminantDim(3), &
                                    nx, ny, nt))                             ! (layer,form,x,y,t)
        
        empty2DArray         = nf90_fill_double
        empty3DArray         = nf90_fill_double
        empty4DArray         = nf90_fill_double
        empty5DArraySediment = nf90_fill_double

        ! ---- aggregated WATER/SEDIMENT (form-first) ----
        allocate(me%output_agg_water__m_contaminant,           source=empty4DArray)
        allocate(me%output_agg_water__C_contaminant,           source=empty4DArray)
        allocate(me%output_agg_water__j_contaminant_outflow,   source=empty4DArray)
        allocate(me%output_agg_water__j_contaminant_deposited, source=empty4DArray)
        allocate(me%output_agg_water__j_contaminant_resuspended, source=empty4DArray)

        allocate(me%output_agg_water__m_spm, source=empty3DArray)
        allocate(me%output_agg_water__C_spm, source=empty3DArray)
        if (C%includeSedimentFluxes) then
            allocate(me%output_agg_water__m_spm_erosion,      source=empty3DArray)
            allocate(me%output_agg_water__m_spm_deposition,   source=empty3DArray)
            allocate(me%output_agg_water__m_spm_resuspended,  source=empty3DArray)
            allocate(me%output_agg_water__m_spm_inflow,       source=empty3DArray)
            allocate(me%output_agg_water__m_spm_outflow,      source=empty3DArray)
            allocate(me%output_agg_water__m_spm_bank_erosion, source=empty3DArray)
        end if
        allocate(me%output_agg_water__volume, source=empty3DArray)
        allocate(me%output_agg_water__depth,  source=empty3DArray)
        allocate(me%output_agg_water__flow,   source=empty3DArray)

        allocate(me%output_agg_sediment__m_contaminant_total, source=empty4DArray)
        allocate(me%output_agg_sediment__C_contaminant_total, source=empty4DArray)
        allocate(me%output_agg_sediment__C_contaminant_layers, source=empty5DArraySediment)
        allocate(me%output_agg_sediment__m_contaminant_buried, source=empty4DArray)
        allocate(me%output_agg_sediment__bed_area, source=empty3DArray)
        allocate(me%output_agg_sediment__mass,     source=empty3DArray)

        allocate(me%output_soil__m_contaminant_total(1:3, 1:nx, 1:ny, 1:nt))
        me%output_soil__m_contaminant_total = nf90_fill_double

        allocate(me%output_soil__C_contaminant_total(1:nx, 1:ny, 1:nt))
        me%output_soil__C_contaminant_total = nf90_fill_double

        if (C%includeSoilStateBreakdown) then
            allocate(me%output_soil__C_contaminant_free(1:nx, 1:ny, 1:nt))
            me%output_soil__C_contaminant_free = nf90_fill_double

            allocate(me%output_soil__C_contaminant_attached(1:nx, 1:ny, 1:nt))
            me%output_soil__C_contaminant_attached = nf90_fill_double

            allocate(me%output_soil__C_contaminant_free_layers(1:nls, 1:nx, 1:ny, 1:nt))
            me%output_soil__C_contaminant_free_layers = nf90_fill_double

            allocate(me%output_soil__C_contaminant_attached_layers(1:nls, 1:nx, 1:ny, 1:nt))
            me%output_soil__C_contaminant_attached_layers = nf90_fill_double
        end if

        if (C%includeSoilLayerBreakdown) then
            allocate(me%output_soil__C_contaminant_layers(1:nls, 1:nx, 1:ny, 1:nt))
            me%output_soil__C_contaminant_layers = nf90_fill_double
        end if

        if (C%includeSoilErosionYields) then
            allocate(me%output_soil__m_soil_eroded(1:nx, 1:ny, 1:nt))
            me%output_soil__m_soil_eroded = nf90_fill_double

            allocate(me%output_soil__m_contaminant_eroded(1:2, 1:nx, 1:ny, 1:nt))
            me%output_soil__m_contaminant_eroded = nf90_fill_double
        end if

        allocate(me%output_soil__m_contaminant_buried(1:3, 1:nx, 1:ny, 1:nt))
        me%output_soil__m_contaminant_buried = nf90_fill_double

        ! parent soil arrays — FIX DIM ORDER to (y,x)
        allocate(me%output_soil__bulk_density(1:ny, 1:nx))
        me%output_soil__bulk_density = nf90_fill_double

        deallocate(empty2DArray, empty3DArray, empty4DArray, empty5DArraySediment)
    end subroutine

    !> Reallocate output variable memory for a new chunk
    subroutine newChunkNetCDFAggregatedOutput(me, k)
        class(NetCDFAggregatedOutput) :: me
        integer :: k
        call me%allocateVariables(k)
    end subroutine

    !> Write the output variables to the NetCDF file
    subroutine finaliseChunkNetCDFAggregatedOutput(me, tStart)
        class(NetCDFAggregatedOutput) :: me
        integer :: tStart

        call me%nc__water__m_contaminant%setData(      me%output_agg_water__m_contaminant,        &
                                                    start=[1,1,1,tStart])
        call me%nc__water__C_contaminant%setData(      me%output_agg_water__C_contaminant,        &
                                                    start=[1,1,1,tStart])
        if (C%includeSoilStateBreakdown) then
            call me%nc__water__C_contaminant_free%setData(    me%output_agg_water__C_contaminant_free,    &
                                                            start=[1,1,tStart])
            call me%nc__water__C_contaminant_attached%setData(me%output_agg_water__C_contaminant_attached, &
                                                            start=[1,1,tStart])
        end if
        call me%nc__water__j_contaminant_outflow%setData(    me%output_agg_water__j_contaminant_outflow,   &
                                                            start=[1,1,1,tStart])
        call me%nc__water__j_contaminant_deposited%setData(  me%output_agg_water__j_contaminant_deposited, &
                                                            start=[1,1,1,tStart])
        call me%nc__water__j_contaminant_resuspended%setData(me%output_agg_water__j_contaminant_resuspended,&
                                                            start=[1,1,1,tStart])
        call me%nc__water__m_spm%setData(                   me%output_agg_water__m_spm,                   &
                                                            start=[1,1,tStart])
        call me%nc__water__C_spm%setData(                   me%output_agg_water__C_spm,                   &
                                                            start=[1,1,tStart])
        if (C%includeSedimentFluxes) then
            call me%nc__water__m_spm_erosion%setData(       me%output_agg_water__m_spm_erosion,           &
                                                            start=[1,1,tStart])
            call me%nc__water__m_spm_deposited%setData(     me%output_agg_water__m_spm_deposition,        &
                                                            start=[1,1,tStart])
            call me%nc__water__m_spm_resuspended%setData(   me%output_agg_water__m_spm_resuspended,       &
                                                            start=[1,1,tStart])
            call me%nc__water__m_spm_inflow%setData(        me%output_agg_water__m_spm_inflow,            &
                                                            start=[1,1,tStart])
            call me%nc__water__m_spm_outflow%setData(       me%output_agg_water__m_spm_outflow,           &
                                                            start=[1,1,tStart])
            call me%nc__water__m_spm_bank_erosion%setData(  me%output_agg_water__m_spm_bank_erosion,      &
                                                            start=[1,1,tStart])
        end if
        call me%nc__water__volume%setData(                  me%output_agg_water__volume,                  &
                                                            start=[1,1,tStart])
        call me%nc__water__depth%setData(                   me%output_agg_water__depth,                   &
                                                            start=[1,1,tStart])
        call me%nc__water__flow%setData(                    me%output_agg_water__flow,                    &
                                                            start=[1,1,tStart])

        call me%nc__sediment__m_contaminant_total%setData(  me%output_agg_sediment__m_contaminant_total,  &
                                                            start=[1,1,1,tStart])
        call me%nc__sediment__C_contaminant_total%setData(  me%output_agg_sediment__C_contaminant_total,  &
                                                            start=[1,1,1,tStart])
        call me%nc__sediment__C_contaminant_layers%setData( me%output_agg_sediment__C_contaminant_layers, &
                                                            start=[1,1,1,1,tStart])
        call me%nc__sediment__m_contaminant_buried%setData( me%output_agg_sediment__m_contaminant_buried, &
                                                            start=[1,1,1,tStart])
        call me%nc__sediment__bed_area%setData(             me%output_agg_sediment__bed_area,             &
                                                            start=[1,1,tStart])
        call me%nc__sediment__mass%setData(                 me%output_agg_sediment__mass,                 &
                                                            start=[1,1,tStart])

        ! Parent soil variables (grid-cell level)
        call me%nc__soil__m_contaminant_total%setData( me%output_soil__m_contaminant_total, &
                                                    start=[1,1,1,tStart])
        call me%nc__soil__C_contaminant_total%setData( me%output_soil__C_contaminant_total, &
                                                    start=[1,1,tStart])

        if (allocated(me%output_soil__C_contaminant_layers)) then
            call me%nc__soil__C_contaminant_layers%setData( me%output_soil__C_contaminant_layers, &
                                                            start=[1,1,1,tStart])
        end if
        if (allocated(me%output_soil__m_soil_eroded)) then
            call me%nc__soil__m_soil_eroded%setData(       me%output_soil__m_soil_eroded,       &
                                                        start=[1,1,tStart])
            call me%nc__soil__m_contaminant_eroded%setData(me%output_soil__m_contaminant_eroded, &
                                                        start=[1,1,1,tStart])
        end if
        call me%nc__soil__m_contaminant_buried%setData(    me%output_soil__m_contaminant_buried, &
                                                        start=[1,1,1,tStart])

        ! Optional static grid vars
        !call me%nc__soil__land_use%setData(    me%output_soil__land_use,     start=[1,1])
        !call me%nc__soil__bulk_density%setData(me%output_soil__bulk_density, start=[1,1])

        ! Deallocate
        deallocate(me%output_agg_water__m_contaminant)
        deallocate(me%output_agg_water__C_contaminant)
        if (C%includeSoilStateBreakdown) then
            deallocate(me%output_agg_water__C_contaminant_free)
            deallocate(me%output_agg_water__C_contaminant_attached)
        end if
        deallocate(me%output_agg_water__j_contaminant_outflow)
        deallocate(me%output_agg_water__j_contaminant_deposited)
        deallocate(me%output_agg_water__j_contaminant_resuspended)
        deallocate(me%output_agg_water__m_spm)
        deallocate(me%output_agg_water__C_spm)
        if (C%includeSedimentFluxes) then
            deallocate(me%output_agg_water__m_spm_erosion)
            deallocate(me%output_agg_water__m_spm_deposition)
            deallocate(me%output_agg_water__m_spm_resuspended)
            deallocate(me%output_agg_water__m_spm_inflow)
            deallocate(me%output_agg_water__m_spm_outflow)
            deallocate(me%output_agg_water__m_spm_bank_erosion)
        end if
        deallocate(me%output_agg_water__volume)
        deallocate(me%output_agg_water__depth)
        deallocate(me%output_agg_water__flow)
        deallocate(me%output_agg_sediment__m_contaminant_total)
        deallocate(me%output_agg_sediment__C_contaminant_total)
        deallocate(me%output_agg_sediment__C_contaminant_layers)
        deallocate(me%output_agg_sediment__m_contaminant_buried)
        deallocate(me%output_agg_sediment__bed_area)
        deallocate(me%output_agg_sediment__mass)

        ! Parent soil arrays
        if (allocated(me%output_soil__land_use)) deallocate(me%output_soil__land_use)
        deallocate(me%output_soil__m_contaminant_total)
        deallocate(me%output_soil__C_contaminant_total)
        if (allocated(me%output_soil__C_contaminant_free))             deallocate(me%output_soil__C_contaminant_free)
        if (allocated(me%output_soil__C_contaminant_attached))         deallocate(me%output_soil__C_contaminant_attached)
        if (allocated(me%output_soil__C_contaminant_free_layers))      deallocate(me%output_soil__C_contaminant_free_layers)
        if (allocated(me%output_soil__C_contaminant_attached_layers))  deallocate(me%output_soil__C_contaminant_attached_layers)
        if (allocated(me%output_soil__C_contaminant_layers))           deallocate(me%output_soil__C_contaminant_layers)
        if (allocated(me%output_soil__m_soil_eroded))                  deallocate(me%output_soil__m_soil_eroded)
        if (allocated(me%output_soil__m_contaminant_eroded))           deallocate(me%output_soil__m_contaminant_eroded)
        deallocate(me%output_soil__m_contaminant_buried)
        deallocate(me%output_soil__bulk_density)
    end subroutine
end module
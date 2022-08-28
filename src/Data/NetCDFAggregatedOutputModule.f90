module NetCDFAggregatedOutputModule
    use GlobalsModule, only: C, dp
    use UtilModule
    use mo_netcdf, only: NcDataset, NcVariable, NcDimension, nf90_fill_int, nf90_fill_double
    use DataInputModule, only: DATASET
    use EnvironmentModule
    use AbstractEnvironmentModule, only: EnvironmentPointer
    use datetime_module
    use NetCDFOutputModule

    !> Class for outputting data to a NetCDF file, aggregated at the grid cell level
    type, public, extends(NetCDFOutput) :: NetCDFAggregatedOutput
        ! The NetCDF variables are all already defined in the NetCDFOutput class, so we just
        ! need to define the model output variables with different dimensions (water and sediment)
        real(dp), allocatable       :: output_agg_water__m_nm(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_transformed(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_dissolved(:,:,:)
        real(dp), allocatable       :: output_agg_water__C_nm(:,:,:)
        real(dp), allocatable       :: output_agg_water__C_transformed(:,:,:)
        real(dp), allocatable       :: output_agg_water__C_dissolved(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_nm_outflow(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_transformed_outflow(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_dissolved_outflow(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_nm_deposited(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_transformed_deposited(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_nm_resuspended(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_transformed_resuspended(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm(:,:,:)
        real(dp), allocatable       :: output_agg_water__C_spm(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_erosion(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_deposition(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_resuspended(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_inflow(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_outflow(:,:,:)
        real(dp), allocatable       :: output_agg_water__m_spm_bank_erosion(:,:,:)
        real(dp), allocatable       :: output_agg_water__volume(:,:,:)
        real(dp), allocatable       :: output_agg_water__depth(:,:,:)
        real(dp), allocatable       :: output_agg_water__flow(:,:,:)
        real(dp), allocatable       :: output_agg_sediment__m_nm_total(:,:,:)
        real(dp), allocatable       :: output_agg_sediment__C_nm_total(:,:,:)
        real(dp), allocatable       :: output_agg_sediment__C_nm_layers(:,:,:,:)
        real(dp), allocatable       :: output_agg_sediment__m_nm_buried(:,:,:)
        real(dp), allocatable       :: output_agg_sediment__bed_area(:,:,:)
        real(dp), allocatable       :: output_agg_sediment__mass(:,:,:)
      contains
        procedure, public   :: init => initNetCDFAggregatedOutput
        procedure, public   :: updateWater => updateWaterNetCDFAggregatedOutput
        procedure, public   :: updateSediment => updateSedimentNetCDFAggregatedOutput
        procedure, private  :: initWater => initWaterNetCDFAggregatedOutput
        procedure, private  :: initSediment => initSedimentNetCDFAggregatedOutput
        procedure, private  :: createDimensions => createDimensionsNetCDFAggregatedOutput
        procedure, private  :: allocateVariables => allocateVariablesNetCDFAggregatedOutput
        procedure, public   :: newChunk => newChunkNetCDFAggregatedOutput
        procedure, public   :: finaliseChunk => finaliseChunkNetCDFAggregatedOutput
    end type

  contains

    !> Initialise the NetCDF output class by creating the NetCDF file and allocating space
    !! for the output variables (if we're in write-at-end mode and it's needed)
    subroutine initNetCDFAggregatedOutput(me, env, k)
        class(NetCDFAggregatedOutput)   :: me           !! This NetCDFAggregatedOutput class
        type(Environment), target       :: env          !! The environment, with model run variable stored in it
        integer                         :: k            !! Chunk index
        
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
        class(NetCDFAggregatedOutput) :: me !! This NetCDFAggregatedOutput class
        integer             :: t            !! Timestep index for whole batch
        integer             :: tInChunk     !! Timestep index for this chunk
        integer             :: x            !! Grid cell x index
        integer             :: y            !! Grid cell y index
        
        associate (cell => me%env%item%colGridCells(x,y)%item)
            if (cell%nReaches > 0) then
                ! If we're in 'write at end' mode, then store this timestep's output to the output arrays, indexed
                ! by the timestep in the current chunk (because we write the NetCDF file at the end of each chunk)
                if (C%netCDFWriteMode == 'end') then
                    me%output_agg_water__m_nm(x,y,tInChunk) = sum(cell%get_m_np_water())
                    me%output_agg_water__m_transformed(x,y,tInChunk) = sum(cell%get_m_transformed_water())
                    me%output_agg_water__m_dissolved(x,y,tInChunk) = cell%get_m_dissolved_water()
                    me%output_agg_water__C_nm(x,y,tInChunk) = sum(cell%get_C_np_water())
                    me%output_agg_water__C_transformed(x,y,tInChunk) = sum(cell%get_C_transformed_water())
                    me%output_agg_water__C_dissolved(x,y,tInChunk) = cell%get_C_dissolved_water()
                    me%output_agg_water__m_nm_outflow(x,y,tInChunk) = sum(cell%get_j_nm_outflow())
                    me%output_agg_water__m_transformed_outflow(x,y,tInChunk) = sum(cell%get_j_transformed_outflow())
                    me%output_agg_water__m_dissolved_outflow(x,y,tInChunk) = cell%get_j_dissolved_outflow()
                    me%output_agg_water__m_nm_deposited(x,y,tInChunk) = sum(cell%get_j_nm_deposition())
                    me%output_agg_water__m_transformed_deposited(x,y,tInChunk) = sum(cell%get_j_transformed_deposition())
                    me%output_agg_water__m_nm_resuspended(x,y,tInChunk) = sum(cell%get_j_nm_resuspension())
                    me%output_agg_water__m_transformed_resuspended(x,y,tInChunk) = sum(cell%get_j_transformed_resuspension())
                    me%output_agg_water__m_spm(x,y,tInChunk) = sum(cell%get_m_spm())
                    me%output_agg_water__C_spm(x,y,tInChunk) = sum(cell%get_C_spm())
                    if (C%includeSedimentFluxes) then
                        me%output_agg_water__m_spm_erosion(x,y,tInChunk) = sum(cell%get_j_spm_soilErosion())
                        me%output_agg_water__m_spm_deposition(x,y,tInChunk) = sum(cell%get_j_spm_deposition())
                        me%output_agg_water__m_spm_resuspended(x,y,tInChunk) = sum(cell%get_j_spm_resuspension())
                        me%output_agg_water__m_spm_inflow(x,y,tInChunk) = sum(cell%get_j_spm_inflow())
                        me%output_agg_water__m_spm_outflow(x,y,tInChunk) = sum(cell%get_j_spm_outflow())
                        me%output_agg_water__m_spm_bank_erosion(x,y,tInChunk) = sum(cell%get_j_spm_bankErosion())
                    end if
                    me%output_agg_water__volume(x,y,tInChunk) = cell%getWaterVolume()
                    me%output_agg_water__depth(x,y,tInChunk) = cell%getWaterDepth()
                    me%output_agg_water__flow(x,y,tInChunk) = cell%get_Q_outflow() / C%timeStep
                ! If we're in iterative write mode, then write straight to the NetCDF file, which is time-indexed
                ! by the whole batch, not just this chunk
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__water__m_nm%setData(sum(cell%get_m_np_water()), start=[x,y,t]) 
                    call me%nc__water__m_transformed%setData(sum(cell%get_m_transformed_water()), start=[x,y,t]) 
                    call me%nc__water__m_dissolved%setData(cell%get_m_dissolved_water(), start=[x,y,t]) 
                    call me%nc__water__C_nm%setData(sum(cell%get_C_np_water()), start=[x,y,t]) 
                    call me%nc__water__C_transformed%setData(sum(cell%get_C_transformed_water()), start=[x,y,t]) 
                    call me%nc__water__C_dissolved%setData(cell%get_C_dissolved_water(), start=[x,y,t]) 
                    call me%nc__water__m_nm_outflow%setData(sum(cell%get_j_nm_outflow()), start=[x,y,t]) 
                    call me%nc__water__m_transformed_outflow%setData(sum(cell%get_j_transformed_outflow()), start=[x,y,t]) 
                    call me%nc__water__m_dissolved_outflow%setData(cell%get_j_dissolved_outflow(), start=[x,y,t]) 
                    call me%nc__water__m_nm_deposited%setData(sum(cell%get_j_nm_deposition()), start=[x,y,t]) 
                    call me%nc__water__m_transformed_deposited%setData(sum(cell%get_j_transformed_deposition()), start=[x,y,t]) 
                    call me%nc__water__m_nm_resuspended%setData(sum(cell%get_j_nm_resuspension()), start=[x,y,t]) 
                    call me%nc__water__m_transformed_resuspended%setData(sum(cell%get_j_transformed_resuspension()), &
                                                                            start=[x,y,t]) 
                    call me%nc__water__m_spm%setData(sum(cell%get_m_spm()), start=[x,y,t]) 
                    call me%nc__water__C_spm%setData(sum(cell%get_C_spm()), start=[x,y,t]) 
                    if (C%includeSedimentFluxes) then
                        call me%nc__water__m_spm_erosion%setData(sum(cell%get_j_spm_soilErosion()), start=[x,y,t]) 
                        call me%nc__water__m_spm_deposited%setData(sum(cell%get_j_spm_deposition()), start=[x,y,t]) 
                        call me%nc__water__m_spm_resuspended%setData(sum(cell%get_j_spm_resuspension()), start=[x,y,t]) 
                        call me%nc__water__m_spm_inflow%setData(sum(cell%get_j_spm_inflow()), start=[x,y,t]) 
                        call me%nc__water__m_spm_outflow%setData(sum(cell%get_j_spm_outflow()), start=[x,y,t]) 
                        call me%nc__water__m_spm_bank_erosion%setData(sum(cell%get_j_spm_bankErosion()), start=[x,y,t])
                    end if 
                    call me%nc__water__volume%setData(cell%getWaterVolume(), start=[x,y,t])
                    call me%nc__water__depth%setData(cell%getWaterDepth(), start=[x,y,t])
                    call me%nc__water__flow%setData(cell%get_Q_outflow() / C%timeStep, start=[x,y,t])
                end if
            end if
        end associate

    end subroutine

    !> Update either the NetCDF file or write to the in-memory variables for this timestep 
    subroutine updateSedimentNetCDFAggregatedOutput(me, t, tInChunk, x, y)
        class(NetCDFAggregatedOutput) :: me     !! This NetCDFAggregatedOutput instance
        integer             :: t                !! Current timestep in batch
        integer             :: tInChunk         !! Current timestep in chunk
        integer             :: x, y             !! Grid cell indices
        integer             :: l                ! Sediment layer index

        associate(cell => me%env%item%colGridCells(x,y)%item)
            ! If we're in 'write at end' mode, then store this timestep's output to the output arrays, indexed
            ! by the timestep in the current chunk (because we write the NetCDF file at the end of each chunk)
            if (cell%nReaches > 0) then
                if (C%netCDFWriteMode == 'end') then
                    me%output_agg_sediment__m_nm_total(x,y,tInChunk) = sum(cell%get_m_np_sediment())
                    me%output_agg_sediment__C_nm_total(x,y,tInChunk) = sum(cell%get_C_np_sediment())
                    do l = 1, C%nSedimentLayers
                        me%output_agg_sediment__C_nm_layers(l,x,y,tInChunk) = sum(cell%get_C_np_sediment_l(l))
                    end do
                    me%output_agg_sediment__m_nm_buried(x,y,tInChunk) = sum(cell%get_m_np_buried_sediment())
                    me%output_agg_sediment__bed_area(x,y,tInChunk) = cell%getBedSedimentArea()
                    me%output_agg_sediment__mass(x,y,tInChunk) = cell%getBedSedimentMass()
                ! If we're in iterative write mode, then write straight to the NetCDF file, which is time-indexed
                ! by the whole batch, not just this chunk
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__sediment__m_nm_total%setData(sum(cell%get_m_np_sediment()), start=[x,y,t])
                    call me%nc__sediment__C_nm_total%setData(sum(cell%get_C_np_sediment()), start=[x,y,t])
                    do l = 1, C%nSedimentLayers
                        call me%nc__sediment__C_nm_layers%setData(sum(cell%get_C_np_sediment_l(l)), start=[l,x,y,t])
                    end do
                    call me%nc__sediment__m_nm_buried%setData(sum(cell%get_m_np_buried_sediment()), start=[x,y,t])
                    call me%nc__sediment__bed_area%setData(cell%getBedSedimentArea(), start=[x,y,t])
                    call me%nc__sediment__mass%setData(cell%getBedSedimentMass(), start=[x,y,t])
                end if
            end if
        end associate
    end subroutine

    !> Create the variables for water
    subroutine initWaterNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput) :: me         !! This NetCDFAggregatedOutput class

        ! SPM mass and concentration
        me%nc__water__m_spm = me%nc%setVariable('water__m_spm','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_spm%setAttribute('units', 'kg')
        call me%nc__water__m_spm%setAttribute('long_name', 'Mass of suspended particulate matter in surface water')
        call me%nc__water__m_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_spm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_spm = me%nc%setVariable('water__C_spm','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_spm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_spm%setAttribute('long_name', 'Concentration of suspended particulate matter in surface water')
        call me%nc__water__C_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_spm%setAttribute('_FillValue', nf90_fill_double)
        ! NM mass
        me%nc__water__m_nm = me%nc%setVariable('water__m_nm','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm%setAttribute('units', 'kg')
        call me%nc__water__m_nm%setAttribute('long_name', 'Mass of pristine NM in surface water')
        call me%nc__water__m_nm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed = me%nc%setVariable('water__m_transformed','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed%setAttribute('units', 'kg')
        call me%nc__water__m_transformed%setAttribute('long_name', 'Mass of transformed NM in surface water')
        call me%nc__water__m_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_dissolved = me%nc%setVariable('water__m_dissolved','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_dissolved%setAttribute('units', 'kg')
        call me%nc__water__m_dissolved%setAttribute('long_name', 'Mass of dissolved species in surface water')
        call me%nc__water__m_dissolved%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_dissolved%setAttribute('_FillValue', nf90_fill_double)
        ! NM concentration
        me%nc__water__C_nm = me%nc%setVariable('water__C_nm','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_nm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_nm%setAttribute('long_name', 'Concentration of pristine NM in surface water')
        call me%nc__water__C_nm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_nm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_transformed = me%nc%setVariable('water__C_transformed','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_transformed%setAttribute('units', 'kg/m3')
        call me%nc__water__C_transformed%setAttribute('long_name', 'Concentration of transformed NM in surface water')
        call me%nc__water__C_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_transformed%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_dissolved = me%nc%setVariable('water__C_dissolved','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_dissolved%setAttribute('units', 'kg/m3')
        call me%nc__water__C_dissolved%setAttribute('long_name', 'Concentration of dissolved species in surface water')
        call me%nc__water__C_dissolved%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_dissolved%setAttribute('_FillValue', nf90_fill_double)
        ! NM flows
        me%nc__water__m_nm_outflow = me%nc%setVariable('water__m_nm_outflow','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_nm_outflow%setAttribute('long_name', 'Mass of pristine NM outflowing downstream')
        call me%nc__water__m_nm_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_outflow = me%nc%setVariable('water__m_transformed_outflow','f64', &
                                                                [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_outflow%setAttribute('long_name', 'Mass of transformed NM outflowing downstream')
        call me%nc__water__m_transformed_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_dissolved_outflow = me%nc%setVariable('water__m_dissolved_outflow','f64', &
                                                              [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_dissolved_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_dissolved_outflow%setAttribute('long_name', 'Mass of dissolved species outflowing downstream')
        call me%nc__water__m_dissolved_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_dissolved_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_nm_deposited = me%nc%setVariable('water__m_nm_deposited','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_deposited%setAttribute('units', 'kg')
        call me%nc__water__m_nm_deposited%setAttribute('long_name', 'Mass of NM deposited to bed sediment')
        call me%nc__water__m_nm_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_deposited%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_deposited = me%nc%setVariable('water__m_transformed_deposited', &
                                                                        'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_deposited%setAttribute('long_name', 'Mass of transformed NM deposited to bed sediment')
        call me%nc__water__m_transformed_deposited%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed_deposited%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_nm_resuspended = me%nc%setVariable('water__m_nm_resuspended','f64', &
                                                           [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_resuspended%setAttribute('units', 'kg')
        call me%nc__water__m_nm_resuspended%setAttribute('long_name', 'Mass of pristine NM resuspended from bed sediment')
        call me%nc__water__m_nm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_resuspended%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_resuspended = me%nc%setVariable('water__m_transformed_resuspended', &
                                                                          'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_resuspended%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_resuspended%setAttribute('long_name', &
                                                                  'Mass of transformed NM resuspended from bed sediment')
        call me%nc__water__m_transformed_resuspended%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed_resuspended%setAttribute('_FillValue', nf90_fill_double)
        ! SPM flows
        if (C%includeSedimentFluxes) then
            me%nc__water__m_spm_erosion = me%nc%setVariable('water__m_spm_erosion','f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_erosion%setAttribute('long_name', 'Mass of suspended particulate matter from soil erosion')
            call me%nc__water__m_spm_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_erosion%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_deposited = me%nc%setVariable('water__m_spm_deposited','f64', &
                                                              [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_deposited%setAttribute('units', 'kg')
            call me%nc__water__m_spm_deposited%setAttribute('long_name', &
                                                            'Mass of suspended particulate matter deposited to bed sediment')
            call me%nc__water__m_spm_deposited%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_deposited%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_resuspended = me%nc%setVariable('water__m_spm_resuspended','f64', &
                                                                [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_resuspended%setAttribute('units', 'kg')
            call me%nc__water__m_spm_resuspended%setAttribute('long_name', &
                                                             'Mass of suspended particulate matter resuspended from bed sediment')
            call me%nc__water__m_spm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_resuspended%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_inflow = me%nc%setVariable('water__m_spm_inflow','f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_inflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_inflow%setAttribute('long_name', &
                                                         'Mass of suspended particulate matter inflowing from upstream')
            call me%nc__water__m_spm_inflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_inflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_outflow = me%nc%setVariable('water__m_spm_outflow','f64', [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_outflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_outflow%setAttribute('long_name', &
                                                          'Mass of suspended particulate matter outflowing downstream')
            call me%nc__water__m_spm_outflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_outflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_bank_erosion = me%nc%setVariable('water__m_spm_bank_erosion','f64', &
                                                                 [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_bank_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_bank_erosion%setAttribute('long_name', &
                                                               'Mass of suspended particulate matter from bank erosion')
            call me%nc__water__m_spm_bank_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_bank_erosion%setAttribute('_FillValue', nf90_fill_double)
        end if
        ! Water
        me%nc__water__volume = me%nc%setVariable('water__volume','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__volume%setAttribute('units', 'm3')
        call me%nc__water__volume%setAttribute('long_name', 'Volume of water')
        call me%nc__water__volume%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__volume%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__depth = me%nc%setVariable('water__depth','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__depth%setAttribute('units', 'm')
        call me%nc__water__depth%setAttribute('standard_name', 'depth')
        call me%nc__water__depth%setAttribute('long_name', 'Depth of water')
        call me%nc__water__depth%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__depth%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__flow = me%nc%setVariable('water__flow','f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__flow%setAttribute('units', 'm3/s')
        call me%nc__water__flow%setAttribute('standard_name', 'water_volume_transport_in_river_channel')
        call me%nc__water__flow%setAttribute('long_name', 'Flow of water at outflow of grid cell')
        call me%nc__water__flow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__flow%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    !> Create variables for bed sediments
    subroutine initSedimentNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput) :: me     !! This NetCDFAggregatedOutput class

        me%nc__sediment__m_nm_total = me%nc%setVariable('sediment__m_nm_total', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_nm_total%setAttribute('units', 'kg')
        call me%nc__sediment__m_nm_total%setAttribute('long_name', 'Mass of pristine NM in sediment')
        call me%nc__sediment__m_nm_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_nm_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_nm_total = me%nc%setVariable('sediment__C_nm_total', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_nm_total%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_nm_total%setAttribute('long_name', 'Mass concentration of pristine NM across all sediment layers')
        call me%nc__sediment__C_nm_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_nm_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_nm_layers = me%nc%setVariable('sediment__C_nm_layers', 'f64', &
                                                         [me%sed_l_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_nm_layers%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_nm_layers%setAttribute('long_name', 'Mass concentration of pristine NM by sediment layer')
        call me%nc__sediment__C_nm_layers%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_nm_layers%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__m_nm_buried = me%nc%setVariable('sediment__m_nm_buried', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_nm_buried%setAttribute('units', 'kg')
        call me%nc__sediment__m_nm_buried%setAttribute('long_name', 'Mass of pristine NM buried from sediment')
        call me%nc__sediment__m_nm_buried%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_nm_buried%setAttribute('_FillValue', nf90_fill_double)
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

    !> Create the dimensions in the NetCDF file. This in included as a separate function so
    !! that we can create different dimensions in the aggregated vs non-aggregated NetCDF files
    subroutine createDimensionsNetCDFAggregatedOutput(me)
        class(NetCDFAggregatedOutput)     :: me
        ! Create the dimensions
        me%t_dim = me%nc%setDimension('t', C%nTimestepsInBatch)
        me%x_dim = me%nc%setDimension('x', DATASET%gridShape(1))
        me%y_dim = me%nc%setDimension('y', DATASET%gridShape(2))
        me%sed_l_dim = me%nc%setDimension('sed_l', C%nSedimentLayers)
        me%soil_l_dim = me%nc%setDimension('soil_l', C%nSoilLayers)
    end subroutine

    !> Allocate space for the in-memory output variables and fill with NetCDF fill value.
    !! Only call this if we're in iterative write mode.
    subroutine allocateVariablesNetCDFAggregatedOutput(me, k)
        class(NetCDFAggregatedOutput) :: me                         !! This NetCDFAggregatedOutput class
        integer                 :: k                                ! The current chunk
        real(dp), allocatable   :: empty2DArray(:,:)                ! 2D array filled with nf90 fill value
        real(dp), allocatable   :: empty3DArray(:,:,:)              ! 3D array filled with nf90 fill value
        real(dp), allocatable   :: empty4DArraySoil(:,:,:,:)        ! 4D array filled with nf90 fill value, with soil layer dim
        real(dp), allocatable   :: empty4DArraySediment(:,:,:,:)    ! 4D array filled with nf90 fill value, with sediment layer dim
        ! Allocate the empty array to be the current size for this chunk
        allocate(empty2DArray(DATASET%gridShape(1), DATASET%gridShape(2)))
        allocate(empty3DArray(DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty4DArraySoil(C%nSoilLayers, DATASET%gridShape(1), &
                 DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty4DArraySediment(C%nSedimentLayers, DATASET%gridShape(1), &
                 DATASET%gridShape(2), C%batchNTimesteps(k)))
        ! Allocate all water output variables to the correct shape, and fill with the NetCDF fill value
        empty2DArray = nf90_fill_double
        empty3DArray = nf90_fill_double
        empty4DArraySoil = nf90_fill_double
        empty4DArraySediment = nf90_fill_double
        allocate(me%output_agg_water__m_nm, source=empty3DArray)
        allocate(me%output_agg_water__m_transformed, source=empty3DArray)
        allocate(me%output_agg_water__m_dissolved, source=empty3DArray)
        allocate(me%output_agg_water__C_nm, source=empty3DArray)
        allocate(me%output_agg_water__C_transformed, source=empty3DArray)
        allocate(me%output_agg_water__C_dissolved, source=empty3DArray)
        allocate(me%output_agg_water__m_nm_outflow, source=empty3DArray)
        allocate(me%output_agg_water__m_transformed_outflow, source=empty3DArray)
        allocate(me%output_agg_water__m_dissolved_outflow, source=empty3DArray)
        allocate(me%output_agg_water__m_nm_deposited, source=empty3DArray)
        allocate(me%output_agg_water__m_transformed_deposited, source=empty3DArray)
        allocate(me%output_agg_water__m_nm_resuspended, source=empty3DArray)
        allocate(me%output_agg_water__m_transformed_resuspended, source=empty3DArray)
        allocate(me%output_agg_water__m_spm, source=empty3DArray)
        allocate(me%output_agg_water__C_spm, source=empty3DArray)
        if (C%includeSedimentFluxes) then
            allocate(me%output_agg_water__m_spm_erosion, source=empty3DArray)
            allocate(me%output_agg_water__m_spm_deposition, source=empty3DArray)
            allocate(me%output_agg_water__m_spm_resuspended, source=empty3DArray)
            allocate(me%output_agg_water__m_spm_inflow, source=empty3DArray)
            allocate(me%output_agg_water__m_spm_outflow, source=empty3DArray)
            allocate(me%output_agg_water__m_spm_bank_erosion, source=empty3DArray)
        end if
        allocate(me%output_agg_water__volume, source=empty3DArray)
        allocate(me%output_agg_water__depth, source=empty3DArray)
        allocate(me%output_agg_water__flow, source=empty3DArray)
        ! Allocate the sediment variables and fill with the NetCDF fill value
        allocate(me%output_agg_sediment__m_nm_total, source=empty3DArray)
        allocate(me%output_agg_sediment__C_nm_total, source=empty3DArray)
        allocate(me%output_agg_sediment__C_nm_layers, source=empty4DArraySediment)
        allocate(me%output_agg_sediment__m_nm_buried, source=empty3DArray)
        allocate(me%output_agg_sediment__bed_area, source=empty3DArray)
        allocate(me%output_agg_sediment__mass, source=empty3DArray)
        ! Allocate the sediment variables and fill with NetCDF fill value
        allocate(me%output_soil__land_use, source=empty2DArray)
        allocate(me%output_soil__m_nm_total, source=empty3DArray)
        allocate(me%output_soil__m_transformed_total, source=empty3DArray)
        allocate(me%output_soil__m_dissolved_total, source=empty3DArray)
        allocate(me%output_soil__C_nm_total, source=empty3DArray)
        allocate(me%output_soil__C_transformed_total, source=empty3DArray)
        allocate(me%output_soil__C_dissolved_total, source=empty3DArray)
        if (C%includeSoilStateBreakdown) then
            allocate(me%output_soil__C_nm_free, source=empty3DArray)
            allocate(me%output_soil__C_transformed_free, source=empty3DArray)
            allocate(me%output_soil__C_nm_att, source=empty3DArray)
            allocate(me%output_soil__C_transformed_att, source=empty3DArray)
        end if
        if (C%includeSoilLayerBreakdown) then
            allocate(me%output_soil__C_nm_layers, source=empty4DArraySoil)
            allocate(me%output_soil__C_transformed_layers, source=empty4DArraySoil)
            allocate(me%output_soil__C_dissolved_layers, source=empty4DArraySoil)
            if (C%includeSoilStateBreakdown) then
                allocate(me%output_soil__C_nm_free_layers, source=empty4DArraySoil)
                allocate(me%output_soil__C_transformed_free_layers, source=empty4DArraySoil)
                allocate(me%output_soil__C_nm_att_layers, source=empty4DArraySoil)
                allocate(me%output_soil__C_transformed_att_layers, source=empty4DArraySoil)
            end if
        end if
        if (C%includeSoilErosionYields) then
            allocate(me%output_soil__m_soil_eroded, source=empty3DArray)
            allocate(me%output_soil__m_nm_eroded, source=empty3DArray)
            allocate(me%output_soil__m_transformed_eroded, source=empty3DArray)
        end if
        allocate(me%output_soil__m_nm_buried, source=empty3DArray)
        allocate(me%output_soil__m_transformed_buried, source=empty3DArray)
        allocate(me%output_soil__m_dissolved_buried, source=empty3DArray)
        allocate(me%output_soil__bulk_density, source=empty2DArray)
    end subroutine

    !> Reallocate output variable memory for a new chunk. This subroutine should
    !! only be called if we're writing to the NetCDF file, in write-at-end mode
    !! and at the start of a new chunk, so be sure of that when calling it
    subroutine newChunkNetCDFAggregatedOutput(me, k)
        class(NetCDFAggregatedOutput)   :: me       !! This NetCDF output class
        integer                         :: k        !! This chunk index
        ! Allocate the variables. They should have been deallocated at the end of the previous chunk
        call me%allocateVariables(k)
    end subroutine

    !> Write the output variables to the NetCDF file. This subroutine should be called
    !! at the end of a chunk if we're in write-at-end mode and writing to a NetCDF file
    subroutine finaliseChunkNetCDFAggregatedOutput(me, tStart)
        class(NetCDFAggregatedOutput)   :: me               !! This NetCDF output class
        integer                         :: tStart           !! Timestep index at the start of this chunk
        ! Write the data from this chunk to the NetCDF file, water first
        call me%nc__water__m_nm%setData(me%output_agg_water__m_nm, start=[1,1,tStart]) 
        call me%nc__water__m_transformed%setData(me%output_agg_water__m_transformed, start=[1,1,tStart]) 
        call me%nc__water__m_dissolved%setData(me%output_agg_water__m_dissolved, start=[1,1,tStart]) 
        call me%nc__water__C_nm%setData(me%output_agg_water__C_nm, start=[1,1,tStart]) 
        call me%nc__water__C_transformed%setData(me%output_agg_water__C_transformed, start=[1,1,tStart]) 
        call me%nc__water__C_dissolved%setData(me%output_agg_water__C_dissolved, start=[1,1,tStart]) 
        call me%nc__water__m_nm_outflow%setData(me%output_agg_water__m_nm_outflow, start=[1,1,tStart]) 
        call me%nc__water__m_transformed_outflow%setData(me%output_agg_water__m_transformed_outflow, start=[1,1,tStart]) 
        call me%nc__water__m_dissolved_outflow%setData(me%output_agg_water__m_dissolved_outflow, start=[1,1,tStart]) 
        call me%nc__water__m_nm_deposited%setData(me%output_agg_water__m_nm_deposited, start=[1,1,tStart]) 
        call me%nc__water__m_transformed_deposited%setData(me%output_agg_water__m_transformed_deposited, start=[1,1,tStart]) 
        call me%nc__water__m_nm_resuspended%setData(me%output_agg_water__m_nm_resuspended, start=[1,1,tStart]) 
        call me%nc__water__m_transformed_resuspended%setData(me%output_agg_water__m_transformed_resuspended, start=[1,1,tStart]) 
        call me%nc__water__m_spm%setData(me%output_agg_water__m_spm, start=[1,1,tStart]) 
        call me%nc__water__C_spm%setData(me%output_agg_water__C_spm, start=[1,1,tStart]) 
        if (C%includeSedimentFluxes) then
            call me%nc__water__m_spm_erosion%setData(me%output_agg_water__m_spm_erosion, start=[1,1,tStart]) 
            call me%nc__water__m_spm_deposited%setData(me%output_agg_water__m_spm_deposition, start=[1,1,tStart]) 
            call me%nc__water__m_spm_resuspended%setData(me%output_agg_water__m_spm_resuspended, start=[1,1,tStart]) 
            call me%nc__water__m_spm_inflow%setData(me%output_agg_water__m_spm_inflow, start=[1,1,tStart]) 
            call me%nc__water__m_spm_outflow%setData(me%output_agg_water__m_spm_outflow, start=[1,1,tStart]) 
            call me%nc__water__m_spm_bank_erosion%setData(me%output_agg_water__m_spm_bank_erosion, start=[1,1,tStart])
        end if
        call me%nc__water__volume%setData(me%output_agg_water__volume, start=[1,1,tStart])
        call me%nc__water__depth%setData(me%output_agg_water__depth, start=[1,1,tStart])
        call me%nc__water__flow%setData(me%output_agg_water__flow, start=[1,1,tStart])
        ! Sediment
        call me%nc__sediment__m_nm_total%setData(me%output_agg_sediment__m_nm_total, start=[1,1,tStart])
        call me%nc__sediment__C_nm_total%setData(me%output_agg_sediment__C_nm_total, start=[1,1,tStart])
        call me%nc__sediment__C_nm_layers%setData(me%output_agg_sediment__C_nm_layers, start=[1,1,1,tStart])
        call me%nc__sediment__m_nm_buried%setData(me%output_agg_sediment__m_nm_buried, start=[1,1,tStart])
        call me%nc__sediment__bed_area%setData(me%output_agg_sediment__bed_area, start=[1,1,tStart])
        call me%nc__sediment__mass%setData(me%output_agg_sediment__mass, start=[1,1,tStart])
        ! Soil
        call me%nc__soil__m_nm_total%setData(me%output_soil__m_nm_total, start=[1,1,tStart])
        call me%nc__soil__m_transformed_total%setData(me%output_soil__m_transformed_total, start=[1,1,tStart])
        call me%nc__soil__m_dissolved_total%setData(me%output_soil__m_dissolved_total, start=[1,1,tStart])
        call me%nc__soil__C_nm_total%setData(me%output_soil__C_nm_total, start=[1,1,tStart])
        call me%nc__soil__C_transformed_total%setData(me%output_soil__C_transformed_total, start=[1,1,tStart])
        call me%nc__soil__C_dissolved_total%setData(me%output_soil__C_dissolved_total, start=[1,1,tStart])
        if (C%includeSoilStateBreakdown) then
            call me%nc__soil__C_nm_free%setData(me%output_soil__C_nm_free, start=[1,1,tStart])
            call me%nc__soil__C_transformed_free%setData(me%output_soil__C_transformed_free, start=[1,1,tStart])
            call me%nc__soil__C_nm_att%setData(me%output_soil__C_nm_att, start=[1,1,tStart])
            call me%nc__soil__C_transformed_att%setData(me%output_soil__C_transformed_att, start=[1,1,tStart])
        end if
        if (C%includeSoilLayerBreakdown) then
            call me%nc__soil__C_nm_layers%setData(me%output_soil__C_nm_layers, start=[1,1,1,tStart])
            call me%nc__soil__C_transformed_layers%setData(me%output_soil__C_transformed_layers, start=[1,1,1,tStart])
            call me%nc__soil__C_dissolved_layers%setData(me%output_soil__C_dissolved_layers, start=[1,1,1,tStart])
            if (C%includeSoilStateBreakdown) then
                call me%nc__soil__C_nm_free_layers%setData(me%output_soil__C_nm_free_layers, start=[1,1,1,tStart])
                call me%nc__soil__C_transformed_free_layers%setData(me%output_soil__C_transformed_free_layers, start=[1,1,1,tStart])
                call me%nc__soil__C_nm_att_layers%setData(me%output_soil__C_nm_att_layers, start=[1,1,1,tStart])
                call me%nc__soil__C_transformed_att_layers%setData(me%output_soil__C_transformed_att_layers, start=[1,1,1,tStart])
            end if
        end if
        if (C%includeSoilErosionYields) then
            call me%nc__soil__m_soil_eroded%setData(me%output_soil__m_soil_eroded, start=[1,1,tStart])
            call me%nc__soil__m_nm_eroded%setData(me%output_soil__m_nm_eroded, start=[1,1,tStart])
            call me%nc__soil__m_transformed_eroded%setData(me%output_soil__m_transformed_eroded, start=[1,1,tStart])
        end if
        call me%nc__soil__m_nm_buried%setData(me%output_soil__m_nm_buried, start=[1,1,tStart])
        call me%nc__soil__m_transformed_buried%setData(me%output_soil__m_transformed_buried, start=[1,1,tStart])
        call me%nc__soil__m_dissolved_buried%setData(me%output_soil__m_dissolved_buried, start=[1,1,tStart])
        ! Deallocate the output variables
        deallocate(me%output_agg_water__m_nm)
        deallocate(me%output_agg_water__m_transformed)
        deallocate(me%output_agg_water__m_dissolved)
        deallocate(me%output_agg_water__C_nm)
        deallocate(me%output_agg_water__C_transformed)
        deallocate(me%output_agg_water__C_dissolved)
        deallocate(me%output_agg_water__m_nm_outflow)
        deallocate(me%output_agg_water__m_transformed_outflow)
        deallocate(me%output_agg_water__m_dissolved_outflow)
        deallocate(me%output_agg_water__m_nm_deposited)
        deallocate(me%output_agg_water__m_transformed_deposited)
        deallocate(me%output_agg_water__m_nm_resuspended)
        deallocate(me%output_agg_water__m_transformed_resuspended)
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
        deallocate(me%output_agg_sediment__m_nm_total)
        deallocate(me%output_agg_sediment__C_nm_total)
        deallocate(me%output_agg_sediment__C_nm_layers)
        deallocate(me%output_agg_sediment__m_nm_buried)
        deallocate(me%output_agg_sediment__bed_area)
        deallocate(me%output_agg_sediment__mass)
        deallocate(me%output_soil__land_use)
        deallocate(me%output_soil__m_nm_total)
        deallocate(me%output_soil__m_transformed_total)
        deallocate(me%output_soil__m_dissolved_total)
        deallocate(me%output_soil__C_nm_total)
        deallocate(me%output_soil__C_transformed_total)
        deallocate(me%output_soil__C_dissolved_total)
        if (C%includeSoilStateBreakdown) then
            deallocate(me%output_soil__C_nm_free)
            deallocate(me%output_soil__C_transformed_free)
            deallocate(me%output_soil__C_nm_att)
            deallocate(me%output_soil__C_transformed_att)
        end if
        if (C%includeSoilLayerBreakdown) then
            deallocate(me%output_soil__C_nm_layers)
            deallocate(me%output_soil__C_transformed_layers)
            deallocate(me%output_soil__C_dissolved_layers)
            if (C%includeSoilStateBreakdown) then
                deallocate(me%output_soil__C_nm_free_layers)
                deallocate(me%output_soil__C_transformed_free_layers)
                deallocate(me%output_soil__C_nm_att_layers)
                deallocate(me%output_soil__C_transformed_att_layers)
            end if
        end if
        if (C%includeSoilErosionYields) then
            deallocate(me%output_soil__m_soil_eroded)
            deallocate(me%output_soil__m_nm_eroded)
            deallocate(me%output_soil__m_transformed_eroded)
        end if
        deallocate(me%output_soil__m_nm_buried)
        deallocate(me%output_soil__m_transformed_buried)
        deallocate(me%output_soil__m_dissolved_buried)
        deallocate(me%output_soil__bulk_density)
    end subroutine

end module
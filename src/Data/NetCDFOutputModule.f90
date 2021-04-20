module NetCDFOutputModule
    use Globals, only: C, dp
    use mo_netcdf, only: NcDataset, NcVariable, NcDimension, nf90_fill_int, nf90_fill_double
    use classDatabase, only: DATASET
    use classEnvironment1
    use spcEnvironment, only: EnvironmentPointer
    use datetime_module

    !> Class for outputting data to a NetCDF file
    type, public :: NetCDFOutput
        type(NcDataset)             :: nc                                   !! The NetCDF file to write to
        type(EnvironmentPointer)    :: env                                  !! Pointer to the environment, to retrieve state variables
        type(NcDimension)           :: t_dim, x_dim, y_dim, w_dim, sed_l_dim, soil_l_dim
        ! The NetCDF variables
        type(NcVariable)            :: nc__water__waterbody_type
        type(NcVariable)            :: nc__water__m_nm
        type(NcVariable)            :: nc__water__m_transformed
        type(NcVariable)            :: nc__water__m_dissolved
        type(NcVariable)            :: nc__water__C_nm
        type(NcVariable)            :: nc__water__C_transformed
        type(NcVariable)            :: nc__water__C_dissolved
        type(NcVariable)            :: nc__water__m_nm_outflow
        type(NcVariable)            :: nc__water__m_transformed_outflow
        type(NcVariable)            :: nc__water__m_dissolved_outflow
        type(NcVariable)            :: nc__water__m_nm_deposited
        type(NcVariable)            :: nc__water__m_transformed_deposited
        type(NcVariable)            :: nc__water__m_nm_resuspended
        type(NcVariable)            :: nc__water__m_transformed_resuspended
        type(NcVariable)            :: nc__water__m_spm
        type(NcVariable)            :: nc__water__C_spm
        type(NcVariable)            :: nc__water__m_spm_erosion
        type(NcVariable)            :: nc__water__m_spm_deposited
        type(NcVariable)            :: nc__water__m_spm_resuspended
        type(NcVariable)            :: nc__water__m_spm_inflow
        type(NcVariable)            :: nc__water__m_spm_outflow
        type(NcVariable)            :: nc__water__m_spm_bank_erosion
        type(NcVariable)            :: nc__water__volume
        type(NcVariable)            :: nc__water__depth
        type(NcVariable)            :: nc__water__flow
        type(NcVariable)            :: nc__sediment__m_nm_total
        type(NcVariable)            :: nc__sediment__C_nm_total
        type(NcVariable)            :: nc__sediment__C_nm_layers
        type(NcVariable)            :: nc__sediment__m_nm_buried
        type(NcVariable)            :: nc__sediment__bed_area
        type(NcVariable)            :: nc__sediment__mass

        ! Model output variables
        real(dp), allocatable       :: output_water__waterbody_type(:,:)
        real(dp), allocatable       :: output_water__m_nm(:,:,:,:)
        real(dp), allocatable       :: output_water__m_transformed(:,:,:,:)
        real(dp), allocatable       :: output_water__m_dissolved(:,:,:,:)
        real(dp), allocatable       :: output_water__C_nm(:,:,:,:)
        real(dp), allocatable       :: output_water__C_transformed(:,:,:,:)
        real(dp), allocatable       :: output_water__C_dissolved(:,:,:,:)
        real(dp), allocatable       :: output_water__m_nm_outflow(:,:,:,:)
        real(dp), allocatable       :: output_water__m_transformed_outflow(:,:,:,:)
        real(dp), allocatable       :: output_water__m_dissolved_outflow(:,:,:,:)
        real(dp), allocatable       :: output_water__m_nm_deposited(:,:,:,:)
        real(dp), allocatable       :: output_water__m_transformed_deposited(:,:,:,:)
        real(dp), allocatable       :: output_water__m_nm_resuspended(:,:,:,:)
        real(dp), allocatable       :: output_water__m_transformed_resuspended(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm(:,:,:,:)
        real(dp), allocatable       :: output_water__C_spm(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_erosion(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_deposition(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_resuspended(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_inflow(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_outflow(:,:,:,:)
        real(dp), allocatable       :: output_water__m_spm_bank_erosion(:,:,:,:)
        real(dp), allocatable       :: output_water__volume(:,:,:,:)
        real(dp), allocatable       :: output_water__depth(:,:,:,:)
        real(dp), allocatable       :: output_water__flow(:,:,:,:)
        real(dp), allocatable       :: output_sediment__m_nm_total(:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_nm_total(:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_nm_layers(:,:,:,:,:)
        real(dp), allocatable       :: output_sediment__m_nm_buried(:,:,:,:)
        real(dp), allocatable       :: output_sediment__bed_area(:,:,:,:)
        real(dp), allocatable       :: output_sediment__mass(:,:,:,:)
        
      contains
        procedure, public   :: init
        procedure, public   :: updateWater
        procedure, public   :: updateSediment
        procedure, public   :: updateSoil
        procedure, private  :: initFile
        procedure, private  :: initWater
        procedure, private  :: initSediment
        procedure, private  :: initSoil
        procedure, private  :: allocateVariables
        procedure, public   :: newChunk
        procedure, public   :: finaliseChunk
        procedure, public   :: close => closeNetCDF
    end type

  contains

    !> Initialise the NetCDF output class by creating the NetCDF file and allocating space
    !! for the output variables (if we're in write-at-end mode and it's needed)
    subroutine init(me, env, k)
        class(NetCDFOutput)         :: me           !! This NetCDFOutput class
        type(Environment1), target  :: env
        integer                     :: k            !! Chunk index
        
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
    subroutine updateWater(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me           !! This NetCDFOutput class
        integer             :: t            !! Timestep index for whole batch
        integer             :: tInChunk     !! Timestep index for this chunk
        integer             :: x            !! Grid cell x index
        integer             :: y            !! Grid cell y index
        integer             :: w            ! Waterbody index
        
        ! Loop through the reaches in cell (x,y)
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate(reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                ! If we're in 'write at end' mode, then store this timestep's output to the output arrays, indexed
                ! by the timestep in the current chunk (because we write the NetCDF file at the end of each chunk)
                if (C%netCDFWriteMode == 'end') then
                    me%output_water__m_nm(w,x,y,tInChunk) = sum(reach%m_np)
                    me%output_water__m_transformed(w,x,y,tInChunk) = sum(reach%m_transformed)
                    me%output_water__m_dissolved(w,x,y,tInChunk) = reach%m_dissolved
                    me%output_water__C_nm(w,x,y,tInChunk) = sum(reach%C_np)
                    me%output_water__C_transformed(w,x,y,tInChunk) = sum(reach%C_transformed)
                    me%output_water__C_dissolved(w,x,y,tInChunk) = reach%C_dissolved
                    me%output_water__m_nm_outflow(w,x,y,tInChunk) = sum(reach%obj_j_nm%outflow)
                    me%output_water__m_transformed_outflow(w,x,y,tInChunk) = sum(reach%obj_j_nm_transformed%outflow)
                    me%output_water__m_dissolved_outflow(w,x,y,tInChunk) = reach%obj_j_dissolved%outflow
                    me%output_water__m_nm_deposited(w,x,y,tInChunk) = sum(reach%obj_j_nm%deposition)
                    me%output_water__m_transformed_deposited(w,x,y,tInChunk) = sum(reach%obj_j_nm_transformed%deposition)
                    me%output_water__m_nm_resuspended(w,x,y,tInChunk) = sum(reach%obj_j_nm%resuspension)
                    me%output_water__m_transformed_resuspended(w,x,y,tInChunk) = sum(reach%obj_j_nm_transformed%resuspension)
                    me%output_water__m_spm(w,x,y,tInChunk) = sum(reach%m_spm)
                    me%output_water__C_spm(w,x,y,tInChunk) = sum(reach%C_spm)
                    if (C%includeSedimentFluxes) then
                        me%output_water__m_spm_erosion(w,x,y,tInChunk) = sum(reach%obj_j_spm%soilErosion)
                        me%output_water__m_spm_deposition(w,x,y,tInChunk) = sum(reach%obj_j_spm%deposition)
                        me%output_water__m_spm_resuspended(w,x,y,tInChunk) = sum(reach%obj_j_spm%resuspension)
                        me%output_water__m_spm_inflow(w,x,y,tInChunk) = sum(reach%obj_j_spm%inflow)
                        me%output_water__m_spm_outflow(w,x,y,tInChunk) = sum(reach%obj_j_spm%outflow)
                        me%output_water__m_spm_bank_erosion(w,x,y,tInChunk) = sum(reach%obj_j_spm%bankErosion)
                    end if
                    me%output_water__volume(w,x,y,tInChunk) = reach%volume
                    me%output_water__depth(w,x,y,tInChunk) = reach%depth
                    me%output_water__flow(w,x,y,tInChunk) = reach%obj_Q%outflow / C%timeStep
                ! If we're in iterative write mode, then write straight to the NetCDF file, which is time-indexed
                ! by the whole batch, not just this chunk
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__water__m_nm%setData(sum(reach%m_np), start=[w,x,y,t]) 
                    call me%nc__water__m_transformed%setData(sum(reach%m_transformed), start=[w,x,y,t]) 
                    call me%nc__water__m_dissolved%setData(reach%m_dissolved, start=[w,x,y,t]) 
                    call me%nc__water__C_nm%setData(sum(reach%C_np), start=[w,x,y,t]) 
                    call me%nc__water__C_transformed%setData(sum(reach%m_transformed), start=[w,x,y,t]) 
                    call me%nc__water__C_dissolved%setData(reach%C_dissolved, start=[w,x,y,t]) 
                    call me%nc__water__m_nm_outflow%setData(sum(reach%obj_j_nm%outflow), start=[w,x,y,t]) 
                    call me%nc__water__m_transformed_outflow%setData(sum(reach%obj_j_nm_transformed%outflow), start=[w,x,y,t]) 
                    call me%nc__water__m_dissolved_outflow%setData(reach%obj_j_dissolved%outflow, start=[w,x,y,t]) 
                    call me%nc__water__m_nm_deposited%setData(sum(reach%obj_j_nm%deposition), start=[w,x,y,t]) 
                    call me%nc__water__m_transformed_deposited%setData(sum(reach%obj_j_nm_transformed%deposition), start=[w,x,y,t]) 
                    call me%nc__water__m_nm_resuspended%setData(sum(reach%obj_j_nm%resuspension), start=[w,x,y,t]) 
                    call me%nc__water__m_transformed_resuspended%setData(sum(reach%obj_j_nm_transformed%resuspension), &
                                                                         start=[w,x,y,t]) 
                    call me%nc__water__m_spm%setData(sum(reach%m_spm), start=[w,x,y,t]) 
                    call me%nc__water__C_spm%setData(sum(reach%C_spm), start=[w,x,y,t]) 
                    if (C%includeSedimentFluxes) then
                        call me%nc__water__m_spm_erosion%setData(sum(reach%obj_j_spm%soilErosion), start=[w,x,y,t]) 
                        call me%nc__water__m_spm_deposited%setData(sum(reach%obj_j_spm%deposition), start=[w,x,y,t]) 
                        call me%nc__water__m_spm_resuspended%setData(sum(reach%obj_j_spm%resuspension), start=[w,x,y,t]) 
                        call me%nc__water__m_spm_inflow%setData(sum(reach%obj_j_spm%inflow), start=[w,x,y,t]) 
                        call me%nc__water__m_spm_outflow%setData(sum(reach%obj_j_spm%outflow), start=[w,x,y,t]) 
                        call me%nc__water__m_spm_bank_erosion%setData(sum(reach%obj_j_spm%bankErosion), start=[w,x,y,t])
                    end if 
                    call me%nc__water__volume%setData(reach%volume, start=[w,x,y,t])
                    call me%nc__water__depth%setData(reach%depth, start=[w,x,y,t])
                    call me%nc__water__flow%setData(reach%obj_Q%outflow / C%timeStep, start=[w,x,y,t])
                end if
            end associate
        end do

    end subroutine

    !> Update either the NetCDF file or write to the in-memory variables for this timestep 
    subroutine updateSediment(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me               !! This NetCDFOutput instance
        integer             :: t                !! Current timestep in batch
        integer             :: tInChunk         !! Current timestep in chunk
        integer             :: x, y             !! Grid cell indices
        integer             :: w                ! Waterbody index
        integer             :: l                ! Sediment layer index

        ! Loop through the reaches in cell (x,y)
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate(reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                ! If we're in 'write at end' mode, then store this timestep's output to the output arrays, indexed
                ! by the timestep in the current chunk (because we write the NetCDF file at the end of each chunk)
                if (C%netCDFWriteMode == 'end') then
                    me%output_sediment__m_nm_total(w,x,y,tInChunk) = sum(reach%bedSediment%get_m_np()) &
                                                                     * reach%bedArea
                    me%output_sediment__C_nm_total(w,x,y,tInChunk) = sum(reach%bedSediment%get_C_np_byMass())
                    do l = 1, C%nSedimentLayers
                        me%output_sediment__C_nm_layers(l,w,x,y,tInChunk) = sum(reach%bedSediment%get_C_np_l_byMass(l))
                    end do
                    me%output_sediment__m_nm_buried(w,x,y,tInChunk) = sum(reach%bedSediment%get_m_np_buried())
                    me%output_sediment__bed_area(w,x,y,tInChunk) = reach%bedArea
                    me%output_sediment__mass(w,x,y,tInChunk) = reach%bedSediment%Mf_bed_all() * reach%bedArea
                ! If we're in iterative write mode, then write straight to the NetCDF file, which is time-indexed
                ! by the whole batch, not just this chunk
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__sediment__m_nm_total%setData(sum(reach%bedSediment%get_m_np()) * reach%bedArea, &
                                                     start=[w,x,y,t])
                    call me%nc__sediment__C_nm_total%setData(sum(reach%bedSediment%get_C_np_byMass()), start=[w,x,y,t])
                    do l = 1, C%nSedimentLayers
                        call me%nc__sediment__C_nm_layers%setData(sum(reach%bedSediment%get_C_np_l_byMass(l)), &
                                                          start=[l,w,x,y,t])
                    end do
                    call me%nc__sediment__m_nm_buried%setData(sum(reach%bedSediment%get_m_np_buried()), start=[w,x,y,t])
                    call me%nc__sediment__bed_area%setData(reach%bedArea, start=[w,x,y,t])
                    call me%nc__sediment__mass%setData(reach%bedSediment%Mf_bed_all() * reach%bedArea, start=[w,x,y,t])
                end if
            end associate
        end do
    end subroutine

    !> Update either the NetCDF file or the in-memory output variables on this time step
    subroutine updateSoil(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me
        integer             :: t            !! Timestep index for whole batch
        integer             :: tInChunk     !! Timestep index for this chunk
        integer             :: x            !! Grid cell x index
        integer             :: y            !! Grid cell y index
    end subroutine

    !> Create the NetCDF file and fill with variables and their attributes
    subroutine initFile(me)
        class(NetCDFOutput) :: me           !! This NetCDFOutput class
        type(datetime)      :: simDatetime  ! Datetime that the simulation we performed
        type(NcVariable)    :: var          ! NetCDF variable
        integer             :: t(C%nTimestepsInBatch)   ! Time record dimension
        integer             :: waterbodyType(DATASET%gridShape(1), DATASET%gridShape(2))    ! Waterbody type

        ! Create the NetCDF file
        me%nc = NcDataset(trim(C%outputPath) // 'output.nc', 'w')

        ! Metadata to describe the NetCDF file
        call me%nc%setAttribute('title', 'NanoFASE model output data: ' // trim(C%runDescription))
        call me%nc%setAttribute('source', 'NanoFASE model v' // C%modelVersion // &
                                ': https://github.com/nerc-ceh/nanofase/tree/' // C%modelVersion)
        simDatetime = simDatetime%now()             ! Chaining functions doesn't work in Fortran...
        call me%nc%setAttribute('history', simDatetime%isoformat() // &
                                ': File created and data written by NanoFASE model')
        call me%nc%setAttribute('Conventions', 'CF-1.8')
        call me%nc%setAttribute('coordinates', 'spatial_ref')               ! Needed for xarray to recognise spatial_ref as a coordinate, not a variable
        call me%nc%setAttribute('acronyms', 'NM = nanomaterial; SPM = suspended particulate matter')

        ! Set the CRS, based on input data (we haven't changed the CRS in the model). We're calling this 'spatial_ref' because
        ! rioxarray looks for this name as default if the grid_mapping attribute isn't present, and CF conventions don't care
        ! what you call it. Interesting conversation on the topic here: https://github.com/opendatacube/datacube-core/issues/837
        var = me%nc%setVariable('spatial_ref', 'i32')
        call var%setAttribute('spatial_ref', trim(DATASET%crsWKT))          ! GDAL/Arc recognises spatial_ref to define CRS
        call var%setAttribute('crs_wkt', trim(DATASET%crsWKT))              ! CF conventions recommends crs_wkt
        call var%setAttribute('epsg_code', DATASET%epsgCode)                ! Not a standard, but might be useful instead of having to decipher WKT

        ! Create the dimensions
        me%t_dim = me%nc%setDimension('t', C%nTimestepsInBatch)
        me%x_dim = me%nc%setDimension('x', DATASET%gridShape(1))
        me%y_dim = me%nc%setDimension('y', DATASET%gridShape(2))
        me%w_dim = me%nc%setDimension('w', 7)
        me%sed_l_dim = me%nc%setDimension('sed_l', C%nSedimentLayers)
        me%soil_l_dim = me%nc%setDimension('soil_l', C%nSoilLayers)

        ! Create the record dimensions
        var = me%nc%setVariable('t', 'i32', [me%t_dim])
        call var%setAttribute('units', 'seconds since ' // C%batchStartDate%isoformat())
        call var%setAttribute('standard_name', 'time')
        call var%setAttribute('calendar', 'gregorian')
        ! Create an array for the time dimension
        do i = 1, C%nTimeStepsInBatch
            t(i) = i * C%timeStep
        end do
        call var%setData(t)
        ! x coordinate
        var = me%nc%setVariable('x', 'i32', [me%x_dim])
        call var%setAttribute('units', 'm')
        call var%setAttribute('standard_name', 'projection_x_coordinate')
        call var%setAttribute('axis', 'X')
        call var%setData(DATASET%x)
        ! y coordinate
        var = me%nc%setVariable('y', 'i32', [me%y_dim])
        call var%setAttribute('units', 'm')
        call var%setAttribute('standard_name', 'projection_y_coordinate')
        call var%setAttribute('axis', 'Y')
        call var%setData(DATASET%y)

        ! Create the variables
        ! TODO change to aggregated waterbody type
        where (DATASET%isEstuary .and. .not. DATASET%gridMask)
            waterbodyType = 2
        elsewhere (.not. DATASET%isEstuary .and. .not. DATASET%gridMask)
            waterbodyType = 1
        elsewhere
            waterbodyType = nf90_fill_int
        end where
        ! Waterbody type
        me%nc__water__waterbody_type = me%nc%setVariable('waterbody_type', 'i32', [me%x_dim, me%y_dim])
        call me%nc__water__waterbody_type%setAttribute('description', '1 = river, 2 = estuary')
        call me%nc__water__waterbody_type%setAttribute('long_name', 'Type of waterbody')
        call me%nc__water__waterbody_type%setAttribute('grid_mapping', 'spatial_ref')
        ! Set the fill value explicitly. Though we're using the default fill value, some applications (like xarray)
        ! don't pick this up, so it's best to be explicit
        call me%nc__water__waterbody_type%setAttribute('_FillValue', nf90_fill_int)
        call me%nc__water__waterbody_type%setData(waterbodyType)

        ! Create the variables for water, sediment and soil
        call me%initWater()
        call me%initSediment()
        call me%initSoil()

    end subroutine

    !> Create the variables for water
    subroutine initWater(me)
        class(NetCDFOutput) :: me

        ! SPM mass and concentration
        me%nc__water__m_spm = me%nc%setVariable('water__m_spm','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_spm%setAttribute('units', 'kg')
        call me%nc__water__m_spm%setAttribute('long_name', 'Mass of suspended particulate matter in surface water')
        call me%nc__water__m_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_spm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_spm = me%nc%setVariable('water__C_spm','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_spm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_spm%setAttribute('long_name', 'Concentration of suspended particulate matter in surface water')
        call me%nc__water__C_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_spm%setAttribute('_FillValue', nf90_fill_double)
        ! NM mass
        me%nc__water__m_nm = me%nc%setVariable('water__m_nm','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm%setAttribute('units', 'kg')
        call me%nc__water__m_nm%setAttribute('long_name', 'Mass of NM in surface water')
        call me%nc__water__m_nm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed = me%nc%setVariable('water__m_transformed','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed%setAttribute('units', 'kg')
        call me%nc__water__m_transformed%setAttribute('long_name', 'Mass of transformed NM in surface water')
        call me%nc__water__m_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_dissolved = me%nc%setVariable('water__m_dissolved','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_dissolved%setAttribute('units', 'kg')
        call me%nc__water__m_dissolved%setAttribute('long_name', 'Mass of dissolved species in surface water')
        call me%nc__water__m_dissolved%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_dissolved%setAttribute('_FillValue', nf90_fill_double)
        ! NM concentration
        me%nc__water__C_nm = me%nc%setVariable('water__C_nm','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_nm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_nm%setAttribute('long_name', 'Concentration of NM in surface water')
        call me%nc__water__C_nm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_nm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_transformed = me%nc%setVariable('water__C_transformed','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_transformed%setAttribute('units', 'kg/m3')
        call me%nc__water__C_transformed%setAttribute('long_name', 'Concentration of transformed NM in surface water')
        call me%nc__water__C_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_transformed%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_dissolved = me%nc%setVariable('water__C_dissolved','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_dissolved%setAttribute('units', 'kg/m3')
        call me%nc__water__C_dissolved%setAttribute('long_name', 'Concentration of dissolved species in surface water')
        call me%nc__water__C_dissolved%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_dissolved%setAttribute('_FillValue', nf90_fill_double)
        ! NM flows
        me%nc__water__m_nm_outflow = me%nc%setVariable('water__m_np_outflow','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_nm_outflow%setAttribute('long_name', 'Mass of NM outflowing downstream')
        call me%nc__water__m_nm_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_outflow = me%nc%setVariable('water__m_transformed_outflow','f64', &
                                                                [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_outflow%setAttribute('long_name', 'Mass of transformed NM outflowing downstream')
        call me%nc__water__m_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_dissolved_outflow = me%nc%setVariable('water__m_dissolved_outflow','f64', &
                                                              [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_dissolved_outflow%setAttribute('units', 'kg')
        call me%nc__water__m_dissolved_outflow%setAttribute('long_name', 'Mass of dissolved species outflowing downstream')
        call me%nc__water__m_dissolved_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_dissolved_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_nm_deposited = me%nc%setVariable('water__m_nm_deposited','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_deposited%setAttribute('units', 'kg')
        call me%nc__water__m_nm_deposited%setAttribute('long_name', 'Mass of NM deposited to bed sediment')
        call me%nc__water__m_nm_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_deposited%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_deposited = me%nc%setVariable('water__m_transformed_deposited', &
                                                                        'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_deposited%setAttribute('long_name', 'Mass of transformed NM deposited to bed sediment')
        call me%nc__water__m_transformed_deposited%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed_deposited%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_nm_resuspended = me%nc%setVariable('water__m_nm_resuspended','f64', &
                                                           [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_nm_resuspended%setAttribute('units', 'kg')
        call me%nc__water__m_nm_resuspended%setAttribute('long_name', 'Mass of NM resuspended from bed sediment')
        call me%nc__water__m_nm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_nm_resuspended%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_transformed_resuspended = me%nc%setVariable('water__m_transformed_resuspended', &
                                                                          'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_transformed_resuspended%setAttribute('units', 'kg')
        call me%nc__water__m_transformed_resuspended%setAttribute('long_name', &
                                                                  'Mass of transformed NM resuspended from bed sediment')
        call me%nc__water__m_transformed%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_transformed%setAttribute('_FillValue', nf90_fill_double)
        ! SPM flows
        if (C%includeSedimentFluxes) then
            me%nc__water__m_spm_erosion = me%nc%setVariable('water__m_spm_erosion','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_erosion%setAttribute('long_name', 'Mass of suspended particulate matter from soil erosion')
            call me%nc__water__m_spm_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_erosion%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_deposited = me%nc%setVariable('water__m_spm_deposited','f64', &
                                                              [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_deposited%setAttribute('units', 'kg')
            call me%nc__water__m_spm_deposited%setAttribute('long_name', &
                                                            'Mass of suspended particulate matter deposited to bed sediment')
            call me%nc__water__m_spm_deposited%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_deposited%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_resuspended = me%nc%setVariable('water__m_spm_resuspended','f64', &
                                                                [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_resuspended%setAttribute('units', 'kg')
            call me%nc__water__m_spm_resuspended%setAttribute('long_name', &
                                                             'Mass of suspended particulate matter resuspended from bed sediment')
            call me%nc__water__m_spm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_resuspended%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_inflow = me%nc%setVariable('water__m_spm_inflow','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_inflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_inflow%setAttribute('long_name', &
                                                         'Mass of suspended particulate matter inflowing from upstream')
            call me%nc__water__m_spm_inflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_inflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_outflow = me%nc%setVariable('water__m_spm_outflow','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_outflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_outflow%setAttribute('long_name', &
                                                          'Mass of suspended particulate matter outflowing downstream')
            call me%nc__water__m_spm_outflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_outflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_bank_erosion = me%nc%setVariable('water__m_spm_bank_erosion','f64', &
                                                                 [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_bank_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_bank_erosion%setAttribute('long_name', &
                                                               'Mass of suspended particulate matter from bank erosion')
            call me%nc__water__m_spm_bank_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_bank_erosion%setAttribute('_FillValue', nf90_fill_double)
        end if
        ! Water
        me%nc__water__volume = me%nc%setVariable('water__volume','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__volume%setAttribute('units', 'm3')
        call me%nc__water__volume%setAttribute('long_name', 'Volume of water')
        call me%nc__water__volume%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__volume%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__depth = me%nc%setVariable('water__depth','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__depth%setAttribute('units', 'm')
        call me%nc__water__depth%setAttribute('standard_name', 'depth')
        call me%nc__water__depth%setAttribute('long_name', 'Depth of water')
        call me%nc__water__depth%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__depth%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__flow = me%nc%setVariable('water__flow','f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__flow%setAttribute('units', 'm3/s')
        call me%nc__water__flow%setAttribute('standard_name', 'water_volume_transport_in_river_channel')
        call me%nc__water__flow%setAttribute('long_name', 'Flow of water')
        call me%nc__water__flow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__flow%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    !> Create variables for bed sediments
    subroutine initSediment(me)
        class(NetCDFOutput) :: me

        me%nc__sediment__m_nm_total = me%nc%setVariable('sediment__m_nm_total', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_nm_total%setAttribute('units', 'kg')
        call me%nc__sediment__m_nm_total%setAttribute('long_name', 'Mass of NM in sediment')
        call me%nc__sediment__m_nm_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_nm_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_nm_total = me%nc%setVariable('sediment__C_nm_total', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_nm_total%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_nm_total%setAttribute('long_name', 'Mass concentration of NM across all sediment layers')
        call me%nc__sediment__C_nm_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_nm_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_nm_layers = me%nc%setVariable('sediment__C_nm_layers', 'f64', &
                                                         [me%sed_l_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_nm_layers%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_nm_layers%setAttribute('long_name', 'Mass concentration of NM by sediment layer')
        call me%nc__sediment__C_nm_layers%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_nm_layers%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__m_nm_buried = me%nc%setVariable('sediment__m_nm_buried', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_nm_buried%setAttribute('units', 'kg')
        call me%nc__sediment__m_nm_buried%setAttribute('long_name', 'Mass of NM buried from sediment')
        call me%nc__sediment__m_nm_buried%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_nm_buried%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__bed_area = me%nc%setVariable('sediment__bed_area', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__bed_area%setAttribute('units', 'm2')
        call me%nc__sediment__bed_area%setAttribute('long_name', 'Surface area of bed sediment')
        call me%nc__sediment__bed_area%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__bed_area%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__mass = me%nc%setVariable('sediment__mass', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__mass%setAttribute('units', 'kg')
        call me%nc__sediment__mass%setAttribute('long_name', 'Mass of fine sediment in bed sediment')
        call me%nc__sediment__mass%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__mass%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    subroutine initSoil(me)
        class(NetCDFOutput) :: me
        ! Create some soily variables
    end subroutine

    !> Allocate space for the in-memory output variables and fill with NetCDF fill value.
    !! Only call this if we're in iterative write mode.
    subroutine allocateVariables(me, k)
        class(NetCDFOutput)     :: me
        integer                 :: k
        real(dp), allocatable   :: emptyArray(:,:,:,:)
        ! Allocate the empty array to be the current size for this chunk
        allocate(emptyArray(7, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        ! Allocate all water output variables to the correct shape, and fill with the NetCDF fill value
        emptyArray = nf90_fill_double
        allocate(me%output_water__m_nm, source=emptyArray)
        allocate(me%output_water__m_transformed, source=emptyArray)
        allocate(me%output_water__m_dissolved, source=emptyArray)
        allocate(me%output_water__C_nm, source=emptyArray)
        allocate(me%output_water__C_transformed, source=emptyArray)
        allocate(me%output_water__C_dissolved, source=emptyArray)
        allocate(me%output_water__m_nm_outflow, source=emptyArray)
        allocate(me%output_water__m_transformed_outflow, source=emptyArray)
        allocate(me%output_water__m_dissolved_outflow, source=emptyArray)
        allocate(me%output_water__m_nm_deposited, source=emptyArray)
        allocate(me%output_water__m_transformed_deposited, source=emptyArray)
        allocate(me%output_water__m_nm_resuspended, source=emptyArray)
        allocate(me%output_water__m_transformed_resuspended, source=emptyArray)
        allocate(me%output_water__m_spm, source=emptyArray)
        allocate(me%output_water__C_spm, source=emptyArray)
        if (C%includeSedimentFluxes) then
            allocate(me%output_water__m_spm_erosion, source=emptyArray)
            allocate(me%output_water__m_spm_deposition, source=emptyArray)
            allocate(me%output_water__m_spm_resuspended, source=emptyArray)
            allocate(me%output_water__m_spm_inflow, source=emptyArray)
            allocate(me%output_water__m_spm_outflow, source=emptyArray)
            allocate(me%output_water__m_spm_bank_erosion, source=emptyArray)
        end if
        allocate(me%output_water__volume, source=emptyArray)
        allocate(me%output_water__depth, source=emptyArray)
        allocate(me%output_water__flow, source=emptyArray)
    end subroutine

    !> Reallocate output variable memory for a new chunk. This subroutine should
    !! only be called if we're writing to the NetCDF file, in write-at-end mode
    !! and at the start of a new chunk, so be sure of that when calling it
    subroutine newChunk(me, k)
        class(NetCDFOutput) :: me       !! This NetCDF output class
        integer             :: k        !! This chunk index
        ! Allocate the variables. They should have been deallocated at the end of the previous chunk
        call me%allocateVariables(k)
    end subroutine

    !> Write the output variables to the NetCDF file. This subroutine should be called
    !! at the end of a chunk if we're in write-at-end mode and writing to a NetCDF file
    subroutine finaliseChunk(me, tStart)
        class(NetCDFOutput) :: me               !! This NetCDF output class
        integer             :: tStart           !! Timestep index at the start of this chunk
        ! Write the data from this chunk to the NetCDF file, water first
        call me%nc__water__m_nm%setData(me%output_water__m_nm, start=[1,1,1,tStart]) 
        call me%nc__water__m_transformed%setData(me%output_water__m_transformed, start=[1,1,1,tStart]) 
        call me%nc__water__m_dissolved%setData(me%output_water__m_dissolved, start=[1,1,1,tStart]) 
        call me%nc__water__C_nm%setData(me%output_water__C_nm, start=[1,1,1,tStart]) 
        call me%nc__water__C_transformed%setData(me%output_water__C_transformed, start=[1,1,1,tStart]) 
        call me%nc__water__C_dissolved%setData(me%output_water__C_dissolved, start=[1,1,1,tStart]) 
        call me%nc__water__m_nm_outflow%setData(me%output_water__m_nm_outflow, start=[1,1,1,tStart]) 
        call me%nc__water__m_transformed_outflow%setData(me%output_water__m_transformed_outflow, start=[1,1,1,tStart]) 
        call me%nc__water__m_dissolved_outflow%setData(me%output_water__m_dissolved_outflow, start=[1,1,1,tStart]) 
        call me%nc__water__m_nm_deposited%setData(me%output_water__m_nm_deposited, start=[1,1,1,tStart]) 
        call me%nc__water__m_transformed_deposited%setData(me%output_water__m_transformed_deposited, start=[1,1,1,tStart]) 
        call me%nc__water__m_nm_resuspended%setData(me%output_water__m_nm_resuspended, start=[1,1,1,tStart]) 
        call me%nc__water__m_transformed_resuspended%setData(me%output_water__m_transformed_resuspended, start=[1,1,1,tStart]) 
        call me%nc__water__m_spm%setData(me%output_water__m_spm, start=[1,1,1,tStart]) 
        call me%nc__water__C_spm%setData(me%output_water__C_spm, start=[1,1,1,tStart]) 
        if (C%includeSedimentFluxes) then
            call me%nc__water__m_spm_erosion%setData(me%output_water__m_spm_erosion, start=[1,1,1,tStart]) 
            call me%nc__water__m_spm_deposited%setData(me%output_water__m_spm_deposition, start=[1,1,1,tStart]) 
            call me%nc__water__m_spm_resuspended%setData(me%output_water__m_spm_resuspended, start=[1,1,1,tStart]) 
            call me%nc__water__m_spm_inflow%setData(me%output_water__m_spm_inflow, start=[1,1,1,tStart]) 
            call me%nc__water__m_spm_outflow%setData(me%output_water__m_spm_outflow, start=[1,1,1,tStart]) 
            call me%nc__water__m_spm_bank_erosion%setData(me%output_water__m_spm_bank_erosion, start=[1,1,1,tStart])
        end if
        call me%nc__water__volume%setData(me%output_water__volume, start=[1,1,1,tStart])
        call me%nc__water__depth%setData(me%output_water__depth, start=[1,1,1,tStart])
        call me%nc__water__flow%setData(me%output_water__flow, start=[1,1,1,tStart])
        ! Sediment
        call me%nc__sediment__m_nm_total%setData(me%output_sediment__m_nm_buried, start=[1,1,1,tStart])
        call me%nc__sediment__C_nm_total%setData(me%output_sediment__C_nm_total, start=[1,1,1,tStart])
        call me%nc__sediment__C_nm_layers%setData(me%output_sediment__C_nm_layers, start=[1,1,1,tStart])
        call me%nc__sediment__m_nm_buried%setData(me%output_sediment__m_nm_buried, start=[1,1,1,tStart])
        call me%nc__sediment__bed_area%setData(me%output_sediment__bed_area, start=[1,1,1,tStart])
        call me%nc__sediment__mass%setData(me%output_sediment__mass, start=[1,1,1,tStart])
        ! Deallocate the output variables
        deallocate(me%output_water__m_nm)
        deallocate(me%output_water__m_transformed)
        deallocate(me%output_water__m_dissolved)
        deallocate(me%output_water__C_nm)
        deallocate(me%output_water__C_transformed)
        deallocate(me%output_water__C_dissolved)
        deallocate(me%output_water__m_nm_outflow)
        deallocate(me%output_water__m_transformed_outflow)
        deallocate(me%output_water__m_dissolved_outflow)
        deallocate(me%output_water__m_nm_deposited)
        deallocate(me%output_water__m_transformed_deposited)
        deallocate(me%output_water__m_nm_resuspended)
        deallocate(me%output_water__m_transformed_resuspended)
        deallocate(me%output_water__m_spm)
        deallocate(me%output_water__C_spm)
        if (C%includeSedimentFluxes) then
            deallocate(me%output_water__m_spm_erosion)
            deallocate(me%output_water__m_spm_deposition)
            deallocate(me%output_water__m_spm_resuspended)
            deallocate(me%output_water__m_spm_inflow)
            deallocate(me%output_water__m_spm_outflow)
            deallocate(me%output_water__m_spm_bank_erosion)
        end if
        deallocate(me%output_water__volume)
        deallocate(me%output_water__depth)
        deallocate(me%output_water__flow)
        deallocate(me%output_sediment__m_nm_total)
        deallocate(me%output_sediment__C_nm_total)
        deallocate(me%output_sediment__C_nm_layers)
        deallocate(me%output_sediment__m_nm_buried)
        deallocate(me%output_sediment__bed_area)
        deallocate(me%output_sediment__mass)
    end subroutine
    
    !> Close the NetCDF dataset
    subroutine closeNetCDF(me)
        class(NetCDFOutput) :: me
        call me%nc%close()
    end subroutine

end module
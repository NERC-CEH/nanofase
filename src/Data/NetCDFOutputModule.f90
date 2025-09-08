module NetCDFOutputModule
    use GlobalsModule, only: C, dp, FREE_CONTAMINANT, ATTACHED_CONTAMINANT
    use UtilModule
    use mo_netcdf, only: NcDataset, NcVariable, NcDimension, nf90_fill_int, nf90_fill_double
    use DataInputModule, only: DATASET
    use ContaminantModule, only: Contaminant  
    use ResultModule     
    use WaterBodyModule, only: WaterBody
    use EnvironmentModule
    use AbstractEnvironmentModule, only: EnvironmentPointer
    use AbstractBedSedimentModule
    use datetime_module

    implicit none

    !> Class for outputting data to a NetCDF file
    type, public :: NetCDFOutput
        type(NcDataset)             :: nc                                   !! The NetCDF file to write to
        type(EnvironmentPointer)    :: env                                  !! Pointer to the environment, to retrieve state variables
        type(NcDimension)           :: t_dim, x_dim, y_dim, w_dim, sed_l_dim, soil_l_dim, contaminant_form_dim
        ! The NetCDF variables
        type(NcVariable)            :: nc__water__waterbody_type
        type(NcVariable)            :: nc__water__m_contaminant
        type(NcVariable)            :: nc__water__C_contaminant
        type(NcVariable)            :: nc__water__C_contaminant_free
        type(NcVariable)            :: nc__water__C_contaminant_attached
        type(NcVariable)            :: nc__water__j_contaminant_outflow
        type(NcVariable)            :: nc__water__j_contaminant_deposited
        type(NcVariable)            :: nc__water__j_contaminant_resuspended
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
        type(NcVariable)            :: nc__sediment__m_contaminant_total
        type(NcVariable)            :: nc__sediment__C_contaminant_total
        type(NcVariable)            :: nc__sediment__C_contaminant_free
        type(NcVariable)            :: nc__sediment__C_contaminant_attached
        type(NcVariable)            :: nc__sediment__C_contaminant_layers
        type(NcVariable)            :: nc__sediment__m_contaminant_buried
        type(NcVariable)            :: nc__sediment__bed_area
        type(NcVariable)            :: nc__sediment__mass
        type(NcVariable)            :: nc__soil__land_use
        type(NcVariable)            :: nc__soil__m_contaminant_total
        type(NcVariable)            :: nc__soil__C_contaminant_total
        type(NcVariable)            :: nc__soil__C_contaminant_free
        type(NcVariable)            :: nc__soil__C_contaminant_attached
        type(NcVariable)            :: nc__soil__C_contaminant_layers
        type(NcVariable)            :: nc__soil__C_contaminant_free_layers
        type(NcVariable)            :: nc__soil__C_contaminant_attached_layers
        type(NcVariable)            :: nc__soil__m_soil_eroded
        type(NcVariable)            :: nc__soil__m_contaminant_eroded
        type(NcVariable)            :: nc__soil__m_contaminant_buried
        type(NcVariable)            :: nc__soil__bulk_density

        ! Model output variables
        real(dp), allocatable       :: output_water__waterbody_type(:,:)
        real(dp), allocatable       :: output_water__m_contaminant(:,:,:,:,:)
        real(dp), allocatable       :: output_water__C_contaminant(:,:,:,:)
        real(dp), allocatable       :: output_water__C_contaminant_free(:,:,:,:)
        real(dp), allocatable       :: output_water__C_contaminant_attached(:,:,:,:)
        real(dp), allocatable       :: output_water__j_contaminant_outflow(:,:,:,:,:)
        real(dp), allocatable       :: output_water__j_contaminant_deposited(:,:,:,:,:)
        real(dp), allocatable       :: output_water__j_contaminant_resuspended(:,:,:,:,:)
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
        real(dp), allocatable       :: output_sediment__m_contaminant_total(:,:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_contaminant_total(:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_contaminant_free(:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_contaminant_attached(:,:,:,:)
        real(dp), allocatable       :: output_sediment__C_contaminant_layers(:,:,:,:,:)
        real(dp), allocatable       :: output_sediment__m_contaminant_buried(:,:,:,:,:)
        real(dp), allocatable       :: output_sediment__bed_area(:,:,:,:)
        real(dp), allocatable       :: output_sediment__mass(:,:,:,:)
        real(dp), allocatable       :: output_soil__land_use(:,:)
        real(dp), allocatable       :: output_soil__m_contaminant_total(:,:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_total(:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_free(:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_attached(:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_layers(:,:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_free_layers(:,:,:,:)
        real(dp), allocatable       :: output_soil__C_contaminant_attached_layers(:,:,:,:)
        real(dp), allocatable       :: output_soil__m_soil_eroded(:,:,:)
        real(dp), allocatable       :: output_soil__m_contaminant_eroded(:,:,:,:)
        real(dp), allocatable       :: output_soil__m_contaminant_buried(:,:,:,:)
        real(dp), allocatable       :: output_soil__bulk_density(:,:)
        
    contains
        procedure, public   :: init => initNetCDFOutput
        procedure, public   :: updateWater => updateWaterNetCDFOutput
        procedure, public   :: updateSediment => updateSedimentNetCDFOutput
        procedure, public   :: updateSoil => updateSoilNetCDFOutput
        procedure, public   :: initFile => initFileNetCDFOutput
        procedure, private  :: initWater => initWaterNetCDFOutput
        procedure, private  :: initSediment => initSedimentNetCDFOutput
        procedure, public   :: initSoil => initSoilNetCDFOutput
        procedure, private  :: createDimensions => createDimensionsNetCDFOutput
        procedure, private  :: allocateVariables => allocateVariablesNetCDFOutput
        procedure, public   :: newChunk => newChunkNetCDFOutput
        procedure, public   :: finaliseChunk => finaliseChunkNetCDFOutput
        procedure, public   :: close => closeNetCDFOutput
    end type

  contains

    !> Initialise the NetCDF output class by creating the NetCDF file and allocating space
    !! for the output variables (if we're in write-at-end mode and it's needed)
    subroutine initNetCDFOutput(me, env, k)
        class(NetCDFOutput)         :: me           !! This NetCDFOutput class
        type(Environment), target   :: env
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
    subroutine updateWaterNetCDFOutput(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me           !! This NetCDFOutput class
        integer             :: t            !! Timestep index for whole batch
        integer             :: tInChunk     !! Timestep index for this chunk
        integer             :: x            !! Grid cell x index
        integer             :: y            !! Grid cell y index
        integer             :: w            !! Waterbody index
        type(Contaminant)   :: cont        !! Contaminant object for total mass
        type(Contaminant)   :: j_cont_outflow
        type(Contaminant)   :: j_cont_deposited
        type(Contaminant)   :: j_cont_resuspended
        real(dp)            :: C_total
        real(dp)            :: C_dissolved
        type(Result)        :: r_create
        type(Result0D)      :: r
        character(len=256)  :: tr = "NetCDFOutputModule%updateWater"
        
        ! Loop through the reaches in cell (x,y)
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate(reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item)
                ! Initialize contaminant objects
                r_create = cont%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    return
                end if
                r_create = j_cont_outflow%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    call j_cont_outflow%finalise()
                    return
                end if
                r_create = j_cont_deposited%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    call j_cont_outflow%finalise()
                    call j_cont_deposited%finalise()
                    return
                end if
                r_create = j_cont_resuspended%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    call j_cont_outflow%finalise()
                    call j_cont_deposited%finalise()
                    call j_cont_resuspended%finalise()
                    return
                end if

                ! Get contaminant data
                cont = reach%get_m_contaminant()  ! Direct assignment since it returns Contaminant
                r = cont%getConcentration(reach%volume)
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    call cont%finalise()
                    call j_cont_outflow%finalise()
                    call j_cont_deposited%finalise()
                    call j_cont_resuspended%finalise()
                    return
                end if
                if (.not. allocated(r%data)) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                    call cont%finalise()
                    call j_cont_outflow%finalise()
                    call j_cont_deposited%finalise()
                    call j_cont_resuspended%finalise()
                    return
                end if
                C_total = r%getDataAsRealDP()
                C_dissolved = cont%m_dissolved / reach%volume

                ! Get contaminant fluxes
                j_cont_outflow = reach%j_contaminant_outflow
                j_cont_deposited = reach%j_contaminant_deposition
                j_cont_resuspended = reach%j_contaminant_resuspension

                if (C%netCDFWriteMode == 'end') then
                    me%output_water__m_contaminant(w,x,y,tInChunk,1) = sum(cont%c(:,:,FREE_CONTAMINANT))
                    me%output_water__m_contaminant(w,x,y,tInChunk,2) = sum(cont%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_water__m_contaminant(w,x,y,tInChunk,3) = cont%m_dissolved
                    me%output_water__C_contaminant(w,x,y,tInChunk) = C_total
                    me%output_water__C_contaminant_free(w,x,y,tInChunk) = sum(cont%get_free()) / reach%volume
                    me%output_water__C_contaminant_attached(w,x,y,tInChunk) = sum(cont%get_attached()) / reach%volume
                    me%output_water__j_contaminant_outflow(w,x,y,tInChunk,1) = sum(j_cont_outflow%c(:,:,FREE_CONTAMINANT))
                    me%output_water__j_contaminant_outflow(w,x,y,tInChunk,2) = sum(j_cont_outflow%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_water__j_contaminant_outflow(w,x,y,tInChunk,3) = j_cont_outflow%m_dissolved
                    me%output_water__j_contaminant_deposited(w,x,y,tInChunk,1) = sum(j_cont_deposited%c(:,:,FREE_CONTAMINANT))
                    me%output_water__j_contaminant_deposited(w,x,y,tInChunk,2) = sum(j_cont_deposited%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_water__j_contaminant_deposited(w,x,y,tInChunk,3) = j_cont_deposited%m_dissolved
                    me%output_water__j_contaminant_resuspended(w,x,y,tInChunk,1) = &
                    sum(j_cont_resuspended%c(:,:,FREE_CONTAMINANT))
                    me%output_water__j_contaminant_resuspended(w,x,y,tInChunk,2) = &
                    sum(j_cont_resuspended%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_water__j_contaminant_resuspended(w,x,y,tInChunk,3) = j_cont_resuspended%m_dissolved
                    me%output_water__m_spm(w,x,y,tInChunk) = sum(reach%m_spm)
                    me%output_water__C_spm(w,x,y,tInChunk) = sum(reach%C_spm)
                    if (C%includeSedimentFluxes) then
                        me%output_water__m_spm_erosion(w,x,y,tInChunk) = sum(reach%j_spm%soilErosion)
                        me%output_water__m_spm_deposition(w,x,y,tInChunk) = sum(reach%j_spm%deposition)
                        me%output_water__m_spm_resuspended(w,x,y,tInChunk) = sum(reach%j_spm%resuspension)
                        me%output_water__m_spm_inflow(w,x,y,tInChunk) = sum(reach%j_spm%inflow)
                        me%output_water__m_spm_outflow(w,x,y,tInChunk) = sum(reach%j_spm%outflow)
                        me%output_water__m_spm_bank_erosion(w,x,y,tInChunk) = sum(reach%j_spm%bankErosion)
                    end if
                    me%output_water__volume(w,x,y,tInChunk) = reach%volume
                    me%output_water__depth(w,x,y,tInChunk) = reach%depth
                    me%output_water__flow(w,x,y,tInChunk) = reach%Q%outflow / C%timeStep
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__water__m_contaminant%setData([sum(cont%c(:,:,FREE_CONTAMINANT)), &
                                                            sum(cont%c(:,:,ATTACHED_CONTAMINANT)), &
                                                            cont%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__water__C_contaminant%setData(C_total, start=[w,x,y,t])
                    call me%nc__water__C_contaminant_free%setData(sum(cont%get_free()) / reach%volume, start=[w,x,y,t])
                    call me%nc__water__C_contaminant_attached%setData(sum(cont%get_attached()) / reach%volume, start=[w,x,y,t])
                    call me%nc__water__j_contaminant_outflow%setData([sum(j_cont_outflow%c(:,:,FREE_CONTAMINANT)), &
                                                                    sum(j_cont_outflow%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                    j_cont_outflow%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__water__j_contaminant_deposited%setData([sum(j_cont_deposited%c(:,:,FREE_CONTAMINANT)), &
                                                                        sum(j_cont_deposited%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                        j_cont_deposited%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__water__j_contaminant_resuspended%setData([sum(j_cont_resuspended%c(:,:,FREE_CONTAMINANT)), &
                                                                        sum(j_cont_resuspended%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                        j_cont_resuspended%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__water__m_spm%setData(sum(reach%m_spm), start=[w,x,y,t])
                    call me%nc__water__C_spm%setData(sum(reach%C_spm), start=[w,x,y,t])
                    if (C%includeSedimentFluxes) then
                        call me%nc__water__m_spm_erosion%setData(sum(reach%j_spm%soilErosion), start=[w,x,y,t])
                        call me%nc__water__m_spm_deposited%setData(sum(reach%j_spm%deposition), start=[w,x,y,t])
                        call me%nc__water__m_spm_resuspended%setData(sum(reach%j_spm%resuspension), start=[w,x,y,t])
                        call me%nc__water__m_spm_inflow%setData(sum(reach%j_spm%inflow), start=[w,x,y,t])
                        call me%nc__water__m_spm_outflow%setData(sum(reach%j_spm%outflow), start=[w,x,y,t])
                        call me%nc__water__m_spm_bank_erosion%setData(sum(reach%j_spm%bankErosion), start=[w,x,y,t])
                    end if
                    call me%nc__water__volume%setData(reach%volume, start=[w,x,y,t])
                    call me%nc__water__depth%setData(reach%depth, start=[w,x,y,t])
                    call me%nc__water__flow%setData(reach%Q%outflow / C%timeStep, start=[w,x,y,t])
                end if

                ! Clean up contaminant objects
                call cont%finalise()
                call j_cont_outflow%finalise()
                call j_cont_deposited%finalise()
                call j_cont_resuspended%finalise()
            end associate
        end do
    end subroutine

    !> Update either the NetCDF file or write to the in-memory variables for this timestep 
    subroutine updateSedimentNetCDFOutput(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me               !! This NetCDFOutput instance
        integer             :: t                !! Current timestep in batch
        integer             :: tInChunk         !! Current timestep in chunk
        integer             :: x, y             !! Grid cell indices
        integer             :: w                !! Waterbody index
        integer             :: l                !! Sediment layer index
        type(Contaminant)   :: cont             !! Contaminant object for total mass
        type(Contaminant)   :: cont_buried      !! Contaminant object for buried mass
        type(Contaminant)   :: layer_cont
        real(dp)            :: C_total, C_dissolved
        real(dp)            :: C_contaminant_layers(C%nSedimentLayers)
        type(Result)        :: r_create
        type(Result0D)      :: r                !! Changed to Result0D in previous fix
        character(len=256)  :: tr = "NetCDFOutputModule%updateSediment"
        real(dp)            :: sediment_volume  !! Total water volume in sediment layers

        ! Loop through the reaches in cell (x,y)
        do w = 1, me%env%item%colGridCells(x,y)%item%nReaches
            associate(reach => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item, &
                    sediment => me%env%item%colGridCells(x,y)%item%colRiverReaches(w)%item%bedSediment)
                ! Initialize contaminant objects
                r_create = cont%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    return
                end if
                r_create = cont_buried%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if

                ! Get total contaminant mass
                r = sediment%get_m_contaminant()
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if
                select type (data => r%getData())
                    type is (Contaminant)
                        cont = data
                    class default
                        call r%addToTrace(tr)
                        call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D for get_m_contaminant"))
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                end select

                sediment_volume = sum(sediment%V_w_by_layer())
                if (sediment_volume <= C%epsilon) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=900, message="Zero or negative sediment volume"))
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if
                r = cont%getConcentration(sediment_volume)
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if
                if (.not. allocated(r%data)) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if
                C_total = r%getDataAsRealDP()
                C_dissolved = cont%m_dissolved / sediment_volume

                ! Get buried contaminant mass
                r = sediment%get_m_contaminant_buried()
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    call cont%finalise()
                    call cont_buried%finalise()
                    return
                end if
                select type (data => r%getData())
                    type is (Contaminant)
                        cont_buried = data
                    class default
                        call r%addToTrace(tr)
                        call r%addError(ErrorInstance(code=106, &
                        message="Invalid data type in Result0D for get_m_contaminant_buried"))
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                end select

                ! Get contaminant concentration by layer
                do l = 1, C%nSedimentLayers
                    r = sediment%get_m_contaminant_l(l)
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    if (.not. allocated(r%data)) then
                        call r%addToTrace(tr)
                        call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    select type (data => r%getData())
                        type is (Contaminant)
                            layer_cont = data
                        class default
                            call r%addToTrace(tr)
                            call r%addError(ErrorInstance(code=106, message="Invalid data type in Result0D"))
                            call cont%finalise()
                            call cont_buried%finalise()
                            return
                    end select
                    r = layer_cont%getConcentration(sediment%colBedSedimentLayers(l)%item%V_w_layer())
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    if (.not. allocated(r%data)) then
                        call r%addToTrace(tr)
                        call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                        call cont%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    C_contaminant_layers(l) = r%getDataAsRealDP()
                end do

                if (C%netCDFWriteMode == 'end') then
                    me%output_sediment__m_contaminant_total(w,x,y,tInChunk,1) = sum(cont%c(:,:,FREE_CONTAMINANT))
                    me%output_sediment__m_contaminant_total(w,x,y,tInChunk,2) = sum(cont%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_sediment__m_contaminant_total(w,x,y,tInChunk,3) = cont%m_dissolved
                    me%output_sediment__C_contaminant_total(w,x,y,tInChunk) = C_total
                    me%output_sediment__C_contaminant_free(w,x,y,tInChunk) = sum(cont%get_free()) / sediment_volume
                    me%output_sediment__C_contaminant_attached(w,x,y,tInChunk) = sum(cont%get_attached()) / sediment_volume
                    me%output_sediment__C_contaminant_layers(w,x,y,tInChunk,:) = C_contaminant_layers
                    me%output_sediment__m_contaminant_buried(w,x,y,tInChunk,1) = sum(cont_buried%c(:,:,FREE_CONTAMINANT))
                    me%output_sediment__m_contaminant_buried(w,x,y,tInChunk,2) = sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_sediment__m_contaminant_buried(w,x,y,tInChunk,3) = cont_buried%m_dissolved
                    me%output_sediment__bed_area(w,x,y,tInChunk) = reach%bedArea
                    me%output_sediment__mass(w,x,y,tInChunk) = sediment%Mf_bed_all() * reach%bedArea
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__sediment__m_contaminant_total%setData([sum(cont%c(:,:,FREE_CONTAMINANT)), &
                                                                    sum(cont%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                    cont%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__sediment__C_contaminant_total%setData(C_total, start=[w,x,y,t])
                    call me%nc__sediment__C_contaminant_free%setData(sum(cont%get_free()) / sediment_volume, start=[w,x,y,t])
                    call me%nc__sediment__C_contaminant_attached%setData(sum(cont%get_attached()) &
                    / sediment_volume, start=[w,x,y,t])
                    call me%nc__sediment__C_contaminant_layers%setData(C_contaminant_layers, start=[1,w,x,y,t])
                    call me%nc__sediment__m_contaminant_buried%setData([sum(cont_buried%c(:,:,FREE_CONTAMINANT)), &
                                                                        sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                        cont_buried%m_dissolved], start=[1,w,x,y,t])
                    call me%nc__sediment__bed_area%setData(reach%bedArea, start=[w,x,y,t])
                    call me%nc__sediment__mass%setData(sediment%Mf_bed_all() * reach%bedArea, start=[w,x,y,t])
                end if

                ! Clean up contaminant objects
                call cont%finalise()
                call cont_buried%finalise()
            end associate
        end do
    end subroutine

    !> Update either the NetCDF file or the in-memory output variables on this time step
    subroutine updateSoilNetCDFOutput(me, t, tInChunk, x, y)
        class(NetCDFOutput) :: me
        integer             :: t            !! Timestep index for whole batch
        integer             :: tInChunk     !! Timestep index for this chunk
        integer             :: x            !! Grid cell x index
        integer             :: y            !! Grid cell y index
        integer             :: p            !! Soil profile index
        integer             :: l            !! Soil layer index
        type(Contaminant)   :: cont
        type(Contaminant)   :: cont_eroded
        type(Contaminant)   :: cont_buried
        real(dp)            :: C_total, C_dissolved
        real(dp)            :: C_contaminant_layers(C%nSoilLayers)
        real(dp)            :: C_contaminant_free_layers(C%nSoilLayers)
        real(dp)            :: C_contaminant_attached_layers(C%nSoilLayers)
        type(Result)        :: r_create
        type(Result0D)      :: r
        character(len=256)  :: tr = "NetCDFOutputModule%updateSoil"
        real(dp)            :: profile_volume  !! Total volume of the soil profile

        do p = 1, me%env%item%colGridCells(x,y)%item%nSoilProfiles
            associate(profile => me%env%item%colGridCells(x,y)%item%colSoilProfiles(p)%item)
                ! Initialize contaminant objects
                r_create = cont%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    return
                end if
                r_create = cont_eroded%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    return
                end if
                r_create = cont_buried%create()
                if (r_create%hasCriticalError()) then
                    call r_create%addToTrace(tr)
                    call cont%finalise()
                    call cont_eroded%finalise()
                    return
                end if

                ! Get total contaminant mass
                cont = profile%get_m_contaminant()  ! Direct assignment since it returns Contaminant

                ! Get profile volume
                profile_volume = sum([(profile%colSoilLayers(l)%item%volume, l=1, C%nSoilLayers)])
                if (profile_volume <= C%epsilon) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=900, message="Zero or negative soil profile volume"))
                    call cont%finalise()
                    call cont_eroded%finalise()
                    call cont_buried%finalise()
                    return
                end if
                r = cont%getConcentration(profile_volume)
                if (r%hasCriticalError()) then
                    call r%addToTrace(tr)
                    call cont%finalise()
                    call cont_eroded%finalise()
                    call cont_buried%finalise()
                    return
                end if
                if (.not. allocated(r%data)) then
                    call r%addToTrace(tr)
                    call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                    call cont%finalise()
                    call cont_eroded%finalise()
                    call cont_buried%finalise()
                    return
                end if
                C_total = r%getDataAsRealDP()
                C_dissolved = cont%m_dissolved / profile_volume

                ! Get eroded and buried contaminant masses
                cont_eroded = profile%m_contaminant_eroded
                cont_buried = profile%m_contaminant_buried

                ! Get contaminant concentrations by layer
                do l = 1, C%nSoilLayers
                    r = profile%colSoilLayers(l)%item%m_contaminant%getConcentration(profile%colSoilLayers(l)%item%volume)
                    if (r%hasCriticalError()) then
                        call r%addToTrace(tr)
                        call cont%finalise()
                        call cont_eroded%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    if (.not. allocated(r%data)) then
                        call r%addToTrace(tr)
                        call r%addError(ErrorInstance(code=901, message="Result0D data not allocated"))
                        call cont%finalise()
                        call cont_eroded%finalise()
                        call cont_buried%finalise()
                        return
                    end if
                    C_contaminant_layers(l) = r%getDataAsRealDP()
                    C_contaminant_free_layers(l) = sum(profile%colSoilLayers(l)%item%m_contaminant%get_free()) / &
                                                profile%colSoilLayers(l)%item%volume
                    C_contaminant_attached_layers(l) = sum(profile%colSoilLayers(l)%item%m_contaminant%get_attached()) / &
                                                    profile%colSoilLayers(l)%item%volume
                end do

                if (C%netCDFWriteMode == 'end') then
                    me%output_soil__m_contaminant_total(x,y,tInChunk,1) = sum(cont%c(:,:,FREE_CONTAMINANT))
                    me%output_soil__m_contaminant_total(x,y,tInChunk,2) = sum(cont%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_soil__m_contaminant_total(x,y,tInChunk,3) = cont%m_dissolved
                    me%output_soil__C_contaminant_total(x,y,tInChunk) = C_total
                    if (C%includeSoilStateBreakdown) then
                        me%output_soil__C_contaminant_free(x,y,tInChunk) = sum(cont%get_free()) / profile_volume
                        me%output_soil__C_contaminant_attached(x,y,tInChunk) = sum(cont%get_attached()) / profile_volume
                        me%output_soil__C_contaminant_free_layers(1:C%nSoilLayers,x,y,tInChunk) = C_contaminant_free_layers
                        me%output_soil__C_contaminant_attached_layers(1:C%nSoilLayers,x,y,tInChunk) = C_contaminant_attached_layers
                    end if
                    if (C%includeSoilLayerBreakdown) then
                        me%output_soil__C_contaminant_layers(1:C%nSoilLayers,x,y,tInChunk) = C_contaminant_layers
                    end if
                    me%output_soil__m_soil_eroded(x,y,tInChunk) = sum(profile%erodedSediment) * profile%area
                    me%output_soil__m_contaminant_eroded(x,y,tInChunk,1) = sum(cont_eroded%c(:,:,FREE_CONTAMINANT))
                    me%output_soil__m_contaminant_eroded(x,y,tInChunk,2) = sum(cont_eroded%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_soil__m_contaminant_buried(x,y,tInChunk,1) = sum(cont_buried%c(:,:,FREE_CONTAMINANT))
                    me%output_soil__m_contaminant_buried(x,y,tInChunk,2) = sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT))
                    me%output_soil__m_contaminant_buried(x,y,tInChunk,3) = cont_buried%m_dissolved
                else if (C%netCDFWriteMode == 'itr') then
                    call me%nc__soil__m_contaminant_total%setData([sum(cont%c(:,:,FREE_CONTAMINANT)), &
                                                                sum(cont%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                cont%m_dissolved], start=[1,x,y,t])
                    call me%nc__soil__C_contaminant_total%setData(C_total, start=[x,y,t])
                    if (C%includeSoilStateBreakdown) then
                        call me%nc__soil__C_contaminant_free%setData(sum(cont%get_free()) / profile_volume, start=[x,y,t])
                        call me%nc__soil__C_contaminant_attached%setData(sum(cont%get_attached()) / profile_volume, start=[x,y,t])
                        call me%nc__soil__C_contaminant_free_layers%setData(C_contaminant_free_layers, start=[1,x,y,t])
                        call me%nc__soil__C_contaminant_attached_layers%setData(C_contaminant_attached_layers, start=[1,x,y,t])
                    end if
                    if (C%includeSoilLayerBreakdown) then
                        call me%nc__soil__C_contaminant_layers%setData(C_contaminant_layers, start=[1,x,y,t])
                    end if
                    call me%nc__soil__m_soil_eroded%setData(sum(profile%erodedSediment) * profile%area, start=[x,y,t])
                    call me%nc__soil__m_contaminant_eroded%setData([sum(cont_eroded%c(:,:,FREE_CONTAMINANT)), &
                                                                    sum(cont_eroded%c(:,:,ATTACHED_CONTAMINANT))], start=[1,x,y,t])
                    call me%nc__soil__m_contaminant_buried%setData([sum(cont_buried%c(:,:,FREE_CONTAMINANT)), &
                                                                    sum(cont_buried%c(:,:,ATTACHED_CONTAMINANT)), &
                                                                    cont_buried%m_dissolved], start=[1,x,y,t])
                end if

                ! Clean up contaminant objects
                call cont%finalise()
                call cont_eroded%finalise()
                call cont_buried%finalise()
            end associate
        end do
    end subroutine

    !> Create the NetCDF file and fill with variables and their attributes
    subroutine initFileNetCDFOutput(me)
        class(NetCDFOutput) :: me
        type(datetime)      :: simDatetime
        type(NcVariable)    :: var
        integer             :: i
        integer             :: t(C%nTimestepsInBatch)
        integer             :: waterbodyType(DATASET%gridShape(1), DATASET%gridShape(2))

        ! Create file + metadata (unchanged) ...
        me%nc = NcDataset(trim(C%outputPath)//'output'//trim(C%outputHash)//'.nc', 'w')

        call me%nc%setAttribute('title', trim('NanoFASE model output data: '//trim(C%runDescription)))
        call me%nc%setAttribute('source', trim('NanoFASE model v'//trim(C%modelVersion)// &
                                    ': https://github.com/nerc-ceh/nanofase/tree/'//trim(C%modelVersion)))
        simDatetime = simDatetime%now()
        call me%nc%setAttribute('history', trim(simDatetime%isoformat()// &
                                    ' - model run completed'))

        ! Encourage xarray to treat the CRS as a coordinate (unchanged) ...
        call me%nc%setAttribute('coordinates', 'spatial_ref')

        ! CRS variable (unchanged, but attribute strings trimmed)
        var = me%nc%setVariable('spatial_ref', 'i32')
        call var%setAttribute('spatial_ref', trim(DATASET%crsWKT))
        call var%setAttribute('crs_wkt', trim(DATASET%crsWKT))
        call var%setAttribute('epsg_code', DATASET%epsgCode)

        ! --- IMPORTANT ---
        ! Dynamic dispatch: this calls the *derived* createDimensions()
        call me%createDimensions()

        ! Record time dimension (unchanged; with trim)
        var = me%nc%setVariable('t', 'i32', [me%t_dim])
        call var%setAttribute('units', trim('seconds since '//C%batchStartDate%isoformat()))
        call var%setAttribute('standard_name', 'time')
        call var%setAttribute('calendar', 'gregorian')
        do i = 1, C%nTimeStepsInBatch
            t(i) = i * C%timeStep
        end do
        call var%setData(t)

        ! x, y coordinates (unchanged; trim attributes)
        var = me%nc%setVariable('x', 'i32', [me%x_dim])
        call var%setAttribute('units', 'm')
        call var%setAttribute('standard_name', 'projection_x_coordinate')
        call var%setAttribute('axis', 'X')
        call var%setData(DATASET%x)

        var = me%nc%setVariable('y', 'i32', [me%y_dim])
        call var%setAttribute('units', 'm')
        call var%setAttribute('standard_name', 'projection_y_coordinate')
        call var%setAttribute('axis', 'Y')
        call var%setData(DATASET%y)

        ! Waterbody type (unchanged; grid_mapping string trimmed)
        where (DATASET%isEstuary .and. .not. DATASET%gridMask .and. DATASET%nWaterbodies > 0)
            waterbodyType = 2
        elsewhere (.not. DATASET%isEstuary .and. .not. DATASET%gridMask .and. DATASET%nWaterbodies > 0)
            waterbodyType = 1
        elsewhere
            waterbodyType = 0
        end where

        me%nc__water__waterbody_type = me%nc%setVariable('water__waterbody_type', 'i32', [me%x_dim, me%y_dim])
        call me%nc__water__waterbody_type%setAttribute('long_name', 'waterbody type (0 land, 1 river, 2 estuary)')
        call me%nc__water__waterbody_type%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__waterbody_type%setAttribute('_FillValue', nf90_fill_int)
        call me%nc__water__waterbody_type%setData(waterbodyType)

        ! --- IMPORTANT ---
        ! Virtual calls: the derived (aggregated) overrides will run here.
        call me%initWater()
        call me%initSediment()
        call me%initSoil()
    end subroutine

    !> Create the variables for water
    subroutine initWaterNetCDFOutput(me)
        class(NetCDFOutput) :: me

        me%contaminant_form_dim = me%nc%setDimension('contaminant_form', 3)
        me%nc__water__m_contaminant = me%nc%setVariable('water__m_contaminant', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_contaminant%setAttribute('units', 'kg')
        call me%nc__water__m_contaminant%setAttribute &
        ('long_name', 'Mass of contaminant in surface water (pristine, attached, dissolved)')
        call me%nc__water__m_contaminant%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_contaminant%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_contaminant = me%nc%setVariable('water__C_contaminant', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_contaminant%setAttribute('units', 'kg/m3')
        call me%nc__water__C_contaminant%setAttribute('long_name', 'Total concentration of contaminant in surface water')
        call me%nc__water__C_contaminant%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_contaminant%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_contaminant_free = me%nc%setVariable('water__C_contaminant_free', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_contaminant_free%setAttribute('units', 'kg/m3')
        call me%nc__water__C_contaminant_free%setAttribute('long_name', &
        'Concentration of free pristine contaminant in surface water')
        call me%nc__water__C_contaminant_free%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_contaminant_free%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_contaminant_attached = me%nc%setVariable('water__C_contaminant_attached', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_contaminant_attached%setAttribute('units', 'kg/m3')
        call me%nc__water__C_contaminant_attached%setAttribute('long_name', &
        'Concentration of attached contaminant in surface water')
        call me%nc__water__C_contaminant_attached%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_contaminant_attached%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__j_contaminant_outflow = me%nc%setVariable('water__j_contaminant_outflow', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_outflow%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_outflow%setAttribute('long_name', &
        'Mass of contaminant outflowing downstream')
        call me%nc__water__j_contaminant_outflow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_outflow%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__j_contaminant_deposited = me%nc%setVariable('water__j_contaminant_deposited', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_deposited%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_deposited%setAttribute('long_name', &
        'Mass of contaminant deposited to bed sediment')
        call me%nc__water__j_contaminant_deposited%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_deposited%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__j_contaminant_resuspended = me%nc%setVariable('water__j_contaminant_resuspended', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__j_contaminant_resuspended%setAttribute('units', 'kg')
        call me%nc__water__j_contaminant_resuspended%setAttribute('long_name', &
        'Mass of contaminant resuspended from bed sediment')
        call me%nc__water__j_contaminant_resuspended%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__j_contaminant_resuspended%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__m_spm = me%nc%setVariable('water__m_spm', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__m_spm%setAttribute('units', 'kg')
        call me%nc__water__m_spm%setAttribute('long_name', &
        'Mass of suspended particulate matter in surface water')
        call me%nc__water__m_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__m_spm%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__C_spm = me%nc%setVariable('water__C_spm', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__C_spm%setAttribute('units', 'kg/m3')
        call me%nc__water__C_spm%setAttribute('long_name', &
        'Concentration of suspended particulate matter in surface water')
        call me%nc__water__C_spm%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__C_spm%setAttribute('_FillValue', nf90_fill_double)
        if (C%includeSedimentFluxes) then
            me%nc__water__m_spm_erosion = me%nc%setVariable('water__m_spm_erosion', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_erosion%setAttribute('long_name', &
            'Mass of suspended particulate matter from soil erosion')
            call me%nc__water__m_spm_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_erosion%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_deposited = me%nc%setVariable('water__m_spm_deposited', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_deposited%setAttribute('units', 'kg')
            call me%nc__water__m_spm_deposited%setAttribute('long_name', &
            'Mass of suspended particulate matter deposited to bed sediment')
            call me%nc__water__m_spm_deposited%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_deposited%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_resuspended = me%nc%setVariable('water__m_spm_resuspended', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_resuspended%setAttribute('units', 'kg')
            call me%nc__water__m_spm_resuspended%setAttribute('long_name', &
            'Mass of suspended particulate matter resuspended from bed sediment')
            call me%nc__water__m_spm_resuspended%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_resuspended%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_inflow = me%nc%setVariable('water__m_spm_inflow', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_inflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_inflow%setAttribute('long_name', &
            'Mass of suspended particulate matter inflowing from upstream')
            call me%nc__water__m_spm_inflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_inflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_outflow = me%nc%setVariable('water__m_spm_outflow', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_outflow%setAttribute('units', 'kg')
            call me%nc__water__m_spm_outflow%setAttribute('long_name', &
            'Mass of suspended particulate matter outflowing downstream')
            call me%nc__water__m_spm_outflow%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_outflow%setAttribute('_FillValue', nf90_fill_double)
            me%nc__water__m_spm_bank_erosion = me%nc%setVariable('water__m_spm_bank_erosion', &
            'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__water__m_spm_bank_erosion%setAttribute('units', 'kg')
            call me%nc__water__m_spm_bank_erosion%setAttribute('long_name', &
            'Mass of suspended particulate matter from bank erosion')
            call me%nc__water__m_spm_bank_erosion%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__water__m_spm_bank_erosion%setAttribute('_FillValue', nf90_fill_double)
        end if
        me%nc__water__volume = me%nc%setVariable('water__volume', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__volume%setAttribute('units', 'm3')
        call me%nc__water__volume%setAttribute('long_name', 'Volume of water')
        call me%nc__water__volume%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__volume%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__depth = me%nc%setVariable('water__depth', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__depth%setAttribute('units', 'm')
        call me%nc__water__depth%setAttribute('standard_name', 'depth')
        call me%nc__water__depth%setAttribute('long_name', 'Depth of water')
        call me%nc__water__depth%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__depth%setAttribute('_FillValue', nf90_fill_double)
        me%nc__water__flow = me%nc%setVariable('water__flow', 'f64', [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__water__flow%setAttribute('units', 'm3/s')
        call me%nc__water__flow%setAttribute('standard_name', 'water_volume_transport_in_river_channel')
        call me%nc__water__flow%setAttribute('long_name', 'Flow of water')
        call me%nc__water__flow%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__water__flow%setAttribute('_FillValue', nf90_fill_double)
    end subroutine

    !> Create variables for bed sediments
    subroutine initSedimentNetCDFOutput(me)
        class(NetCDFOutput) :: me

        me%nc__sediment__m_contaminant_total = me%nc%setVariable('sediment__m_contaminant_total', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_contaminant_total%setAttribute('units', 'kg')
        call me%nc__sediment__m_contaminant_total%setAttribute('long_name', &
        'Mass of contaminant in sediment (pristine, attached, dissolved)')
        call me%nc__sediment__m_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_contaminant_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_total = me%nc%setVariable('sediment__C_contaminant_total', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_total%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_contaminant_total%setAttribute('long_name', &
        'Total mass concentration of contaminant across all sediment layers')
        call me%nc__sediment__C_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_total%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_free = me%nc%setVariable('sediment__C_contaminant_free', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_free%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_contaminant_free%setAttribute('long_name', &
        'Mass concentration of free pristine contaminant in sediment')
        call me%nc__sediment__C_contaminant_free%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_free%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_attached = me%nc%setVariable('sediment__C_contaminant_attached', 'f64', &
            [me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_attached%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_contaminant_attached%setAttribute('long_name', &
        'Mass concentration of attached contaminant in sediment')
        call me%nc__sediment__C_contaminant_attached%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_attached%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__C_contaminant_layers = me%nc%setVariable('sediment__C_contaminant_layers', 'f64', &
            [me%sed_l_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__C_contaminant_layers%setAttribute('units', 'kg/kg')
        call me%nc__sediment__C_contaminant_layers%setAttribute('long_name', &
        'Total mass concentration of contaminant by sediment layer')
        call me%nc__sediment__C_contaminant_layers%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__C_contaminant_layers%setAttribute('_FillValue', nf90_fill_double)
        me%nc__sediment__m_contaminant_buried = me%nc%setVariable('sediment__m_contaminant_buried', 'f64', &
            [me%contaminant_form_dim, me%w_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__sediment__m_contaminant_buried%setAttribute('units', 'kg')
        call me%nc__sediment__m_contaminant_buried%setAttribute('long_name', &
        'Mass of contaminant buried from sediment')
        call me%nc__sediment__m_contaminant_buried%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__sediment__m_contaminant_buried%setAttribute('_FillValue', nf90_fill_double)
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


    !> Create the soil variables in the NetCDF file
    subroutine initSoilNetCDFOutput(me)
        use, intrinsic :: ieee_arithmetic
        class(NetCDFOutput) :: me
        type(NcDimension)   :: eroded_contaminant_form_dim

        ! -----------------------------
        ! Land-use category (argmax over categories)
        ! INPUT shape: DATASET%landUse(l, y, x)
        ! OUTPUT shape: (x, y) integer category index
        ! -----------------------------
        integer, parameter        :: i4 = selected_int_kind(9)
        integer                   :: nx, ny, ncat, ix, iy, k, kmax
        integer(i4), allocatable  :: land_use_idx(:,:)
        integer, parameter :: sp = kind(1.0)
        real(sp), allocatable :: bd(:,:)
        integer :: i, j, ny_in, nx_in

        nx   = DATASET%gridShape(1)
        ny   = DATASET%gridShape(2)
        ncat = size(DATASET%landUse, 1)

        allocate(land_use_idx(nx, ny))
        land_use_idx = 0_i4

        do iy = 1, ny
            do ix = 1, nx
                kmax = 1
                do k = 2, ncat
                    if (DATASET%landUse(k, iy, ix) > DATASET%landUse(kmax, iy, ix)) kmax = k
                end do
                land_use_idx(ix, iy) = int(kmax, kind=i4)
            end do
        end do

        me%nc__soil__land_use = me%nc%setVariable('land_use', 'i32', [me%x_dim, me%y_dim])
        call me%nc__soil__land_use%setAttribute('units', '-')
        call me%nc__soil__land_use%setAttribute('long_name', 'Land use')
        call me%nc__soil__land_use%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__land_use%setAttribute('category_lookup', '1: urban_no_soil. 2: urban_parks_leisure. ' // &
                                                '3: urban_industrial_soil. 4: urban_green_residential. 5: arable. ' // &
                                                '6: grassland. 7: deciduous. 8: coniferous. 9: heathland. 10: water. ' // &
                                                '11: desert. 12/other: other')
        call me%nc__soil__land_use%setData(land_use_idx)
        deallocate(land_use_idx)

        ! -----------------------------
        ! Mass & concentration (unchanged)
        ! -----------------------------
        me%nc__soil__m_contaminant_total = me%nc%setVariable('soil__m_contaminant_total', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__soil__m_contaminant_total%setAttribute('units', 'kg')
        call me%nc__soil__m_contaminant_total%setAttribute('long_name', &
            'Mass of contaminant in soil (free, attached, dissolved)')
        call me%nc__soil__m_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__m_contaminant_total%setAttribute('_FillValue', nf90_fill_double)

        me%nc__soil__C_contaminant_total = me%nc%setVariable('soil__C_contaminant_total', 'f64', &
            [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__soil__C_contaminant_total%setAttribute('units', C%soilPECUnits)
        call me%nc__soil__C_contaminant_total%setAttribute('long_name', 'Total concentration of contaminant in soil')
        call me%nc__soil__C_contaminant_total%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__C_contaminant_total%setAttribute('_FillValue', nf90_fill_double)

        if (C%includeSoilStateBreakdown) then
            me%nc__soil__C_contaminant_free = me%nc%setVariable('soil__C_contaminant_free', 'f64', &
                [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__soil__C_contaminant_free%setAttribute('units', C%soilPECUnits)
            call me%nc__soil__C_contaminant_free%setAttribute('long_name', 'Concentration of free contaminant in soil')
            call me%nc__soil__C_contaminant_free%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__soil__C_contaminant_free%setAttribute('_FillValue', nf90_fill_double)

            me%nc__soil__C_contaminant_attached = me%nc%setVariable('soil__C_contaminant_attached', 'f64', &
                [me%x_dim, me%y_dim, me%t_dim])
            call me%nc__soil__C_contaminant_attached%setAttribute('units', C%soilPECUnits)
            call me%nc__soil__C_contaminant_attached%setAttribute('long_name', 'Concentration of attached contaminant in soil')
            call me%nc__soil__C_contaminant_attached%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__soil__C_contaminant_attached%setAttribute('_FillValue', nf90_fill_double)

            me%nc__soil__C_contaminant_free_layers = me%nc%setVariable('soil__C_contaminant_free_layers', 'f64', &
                [me%soil_l_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__soil__C_contaminant_free_layers%setAttribute('units', C%soilPECUnits)
            call me%nc__soil__C_contaminant_free_layers%setAttribute('long_name', 'Concentration of free contaminant by soil layer')
            call me%nc__soil__C_contaminant_free_layers%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__soil__C_contaminant_free_layers%setAttribute('_FillValue', nf90_fill_double)

            me%nc__soil__C_contaminant_attached_layers = me%nc%setVariable('soil__C_contaminant_attached_layers', 'f64', &
                [me%soil_l_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__soil__C_contaminant_attached_layers%setAttribute('units', C%soilPECUnits)
            call me%nc__soil__C_contaminant_attached_layers%setAttribute('long_name', &
            'Concentration of attached contaminant by soil layer')
            call me%nc__soil__C_contaminant_attached_layers%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__soil__C_contaminant_attached_layers%setAttribute('_FillValue', nf90_fill_double)
        end if

        if (C%includeSoilLayerBreakdown) then
            me%nc__soil__C_contaminant_layers = me%nc%setVariable('soil__C_contaminant_layers', 'f64', &
                [me%soil_l_dim, me%x_dim, me%y_dim, me%t_dim])
            call me%nc__soil__C_contaminant_layers%setAttribute('units', C%soilPECUnits)
            call me%nc__soil__C_contaminant_layers%setAttribute('long_name', 'Total concentration of contaminant by soil layer')
            call me%nc__soil__C_contaminant_layers%setAttribute('grid_mapping', 'spatial_ref')
            call me%nc__soil__C_contaminant_layers%setAttribute('_FillValue', nf90_fill_double)
        end if

        me%nc__soil__m_soil_eroded = me%nc%setVariable('soil__m_soil_eroded', 'f64', [me%x_dim, me%y_dim, me%t_dim])
        call me%nc__soil__m_soil_eroded%setAttribute('units', 'kg')
        call me%nc__soil__m_soil_eroded%setAttribute('long_name', 'Mass of soil eroded')
        call me%nc__soil__m_soil_eroded%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__m_soil_eroded%setAttribute('_FillValue', nf90_fill_double)

        eroded_contaminant_form_dim = me%nc%setDimension('eroded_contaminant_form', C%contaminantDim(2) - 1)

        me%nc__soil__m_contaminant_eroded = me%nc%setVariable('soil__m_contaminant_eroded', 'f64', &
            [eroded_contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__soil__m_contaminant_eroded%setAttribute('units', 'kg')
        call me%nc__soil__m_contaminant_eroded%setAttribute('long_name', &
        'Mass of contaminant eroded from soil (free, attached)')
        call me%nc__soil__m_contaminant_eroded%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__m_contaminant_eroded%setAttribute('_FillValue', nf90_fill_double)

        me%nc__soil__m_contaminant_buried = me%nc%setVariable('soil__m_contaminant_buried', 'f64', &
            [me%contaminant_form_dim, me%x_dim, me%y_dim, me%t_dim])
        call me%nc__soil__m_contaminant_buried%setAttribute('units', 'kg')
        call me%nc__soil__m_contaminant_buried%setAttribute('long_name', 'Mass of contaminant buried from soil')
        call me%nc__soil__m_contaminant_buried%setAttribute('grid_mapping', 'spatial_ref')
        call me%nc__soil__m_contaminant_buried%setAttribute('_FillValue', nf90_fill_double)

        ! -----------------------------
        ! Bulk density (INPUT is real(4) :: soilBulkDensity(y,x))
        ! Define NetCDF var with (y,x) to match memory layout, write as f32, and sanitize.
        ! -----------------------------
        ny_in = size(DATASET%soilBulkDensity, 1)
        nx_in = size(DATASET%soilBulkDensity, 2)

        ! NetCDF var dims match the array order (y, x)
        me%nc__soil__bulk_density = me%nc%setVariable('soil__bulk_density', 'f32', [me%y_dim, me%x_dim])
        call me%nc__soil__bulk_density%setAttribute('units', 'kg/m3')
        call me%nc__soil__bulk_density%setAttribute('long_name', 'Bulk density of the soil')
        call me%nc__soil__bulk_density%setAttribute('grid_mapping', 'spatial_ref')

        allocate(bd(ny_in, nx_in))
        bd = real(DATASET%soilBulkDensity, kind=sp)

        do j = 1, nx_in
            do i = 1, ny_in
                if (.not. ieee_is_finite(bd(i,j))) bd(i,j) = 0.0_sp
                if (bd(i,j) < 0.0_sp)            bd(i,j) = 0.0_sp
                if (abs(bd(i,j)) < 1.0e-30_sp)   bd(i,j) = 0.0_sp   ! squash denormals
            end do
        end do

        call me%nc__soil__bulk_density%setData(bd)
        deallocate(bd)
    end subroutine

    subroutine createDimensionsNetCDFOutput(me)
        class(NetCDFOutput)     :: me
        ! Create the dimensions
        me%t_dim = me%nc%setDimension('t', C%nTimestepsInBatch)
        me%x_dim = me%nc%setDimension('x', DATASET%gridShape(1))
        me%y_dim = me%nc%setDimension('y', DATASET%gridShape(2))
        me%sed_l_dim = me%nc%setDimension('sed_l', C%nSedimentLayers)
        me%soil_l_dim = me%nc%setDimension('soil_l', C%nSoilLayers)
        me%w_dim = me%nc%setDimension('w', 7)
    end subroutine

    !> Allocate space for the in-memory output variables and fill with NetCDF fill value.
    !! Only call this if we're in iterative write mode.
    subroutine allocateVariablesNetCDFOutput(me, k)
        class(NetCDFOutput) :: me
        integer             :: k
        real(dp), allocatable :: empty2DArray(:,:)
        real(dp), allocatable :: empty3DArray(:,:,:)
        real(dp), allocatable :: empty4DArray(:,:,:,:)
        real(dp), allocatable :: empty4DArraySoil(:,:,:,:)
        real(dp), allocatable :: empty5DArray(:,:,:,:,:)
        real(dp), allocatable :: empty5DArraySediment(:,:,:,:,:)

        allocate(empty2DArray(DATASET%gridShape(1), DATASET%gridShape(2)))
        allocate(empty3DArray(DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty4DArray(7, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty4DArraySoil(C%nSoilLayers, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty5DArray(3, 7, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        allocate(empty5DArraySediment(C%nSedimentLayers, 7, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)))
        empty2DArray = nf90_fill_double
        empty3DArray = nf90_fill_double
        empty4DArray = nf90_fill_double
        empty4DArraySoil = nf90_fill_double
        empty5DArray = nf90_fill_double
        empty5DArraySediment = nf90_fill_double
        allocate(me%output_water__waterbody_type, source=empty2DArray)
        allocate(me%output_water__m_contaminant(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_water__C_contaminant, source=empty4DArray)
        allocate(me%output_water__C_contaminant_free, source=empty4DArray)
        allocate(me%output_water__C_contaminant_attached, source=empty4DArray)
        allocate(me%output_water__j_contaminant_outflow(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_water__j_contaminant_deposited(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_water__j_contaminant_resuspended(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_water__m_spm, source=empty4DArray)
        allocate(me%output_water__C_spm, source=empty4DArray)
        if (C%includeSedimentFluxes) then
            allocate(me%output_water__m_spm_erosion, source=empty4DArray)
            allocate(me%output_water__m_spm_deposition, source=empty4DArray)
            allocate(me%output_water__m_spm_resuspended, source=empty4DArray)
            allocate(me%output_water__m_spm_inflow, source=empty4DArray)
            allocate(me%output_water__m_spm_outflow, source=empty4DArray)
            allocate(me%output_water__m_spm_bank_erosion, source=empty4DArray)
        end if
        allocate(me%output_water__volume, source=empty4DArray)
        allocate(me%output_water__depth, source=empty4DArray)
        allocate(me%output_water__flow, source=empty4DArray)
        allocate(me%output_sediment__m_contaminant_total(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_sediment__C_contaminant_total, source=empty4DArray)
        allocate(me%output_sediment__C_contaminant_free, source=empty4DArray)
        allocate(me%output_sediment__C_contaminant_attached, source=empty4DArray)
        allocate(me%output_sediment__C_contaminant_layers(C%nSedimentLayers, 7, &
                DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)), &
                source=empty5DArraySediment)
        allocate(me%output_sediment__m_contaminant_buried(7, DATASET%gridShape(1), &
                DATASET%gridShape(2), C%batchNTimesteps(k), 3), source=empty5DArray)
        allocate(me%output_sediment__bed_area, source=empty4DArray)
        allocate(me%output_sediment__mass, source=empty4DArray)
        allocate(me%output_soil__land_use, source=empty2DArray)
        allocate(me%output_soil__m_contaminant_total(3, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)), &
                source=empty4DArray(1:3,:,:,:))
        allocate(me%output_soil__C_contaminant_total, source=empty3DArray)
        if (C%includeSoilStateBreakdown) then
            allocate(me%output_soil__C_contaminant_free, source=empty3DArray)
            allocate(me%output_soil__C_contaminant_attached, source=empty3DArray)
            allocate(me%output_soil__C_contaminant_free_layers, source=empty4DArraySoil)
            allocate(me%output_soil__C_contaminant_attached_layers, source=empty4DArraySoil)
        end if
        if (C%includeSoilLayerBreakdown) then
            allocate(me%output_soil__C_contaminant_layers, source=empty4DArraySoil)
        end if
        if (C%includeSoilErosionYields) then
            allocate(me%output_soil__m_soil_eroded, source=empty3DArray)
            allocate(me%output_soil__m_contaminant_eroded(2, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)), &
                    source=empty4DArray(1:2,:,:,:))
        end if
        allocate(me%output_soil__m_contaminant_buried(3, DATASET%gridShape(1), DATASET%gridShape(2), C%batchNTimesteps(k)), &
                source=empty4DArray(1:3,:,:,:))
        allocate(me%output_soil__bulk_density, source=empty2DArray)
        deallocate(empty2DArray, empty3DArray, empty4DArray, empty4DArraySoil, empty5DArray, empty5DArraySediment)
    end subroutine

    !> Reallocate output variable memory for a new chunk. This subroutine should
    !! only be called if we're writing to the NetCDF file, in write-at-end mode
    !! and at the start of a new chunk, so be sure of that when calling it
    subroutine newChunkNetCDFOutput(me, k)
        class(NetCDFOutput) :: me       !! This NetCDF output class
        integer             :: k        !! This chunk index
        ! Allocate the variables. They should have been deallocated at the end of the previous chunk
        call me%allocateVariables(k)
    end subroutine

    !> Write the output variables to the NetCDF file. This subroutine should be called
    !! at the end of a chunk if we're in write-at-end mode and writing to a NetCDF file
    subroutine finaliseChunkNetCDFOutput(me, tStart)
        class(NetCDFOutput) :: me               !! This NetCDF output class
        integer             :: tStart           !! Timestep index at the start of this chunk
        ! Write the data from this chunk to the NetCDF file, water first
        call me%nc__water__m_contaminant%setData(me%output_water__m_contaminant, start=[1,1,1,1,tStart])
        call me%nc__water__C_contaminant%setData(me%output_water__C_contaminant, start=[1,1,1,1,tStart])
        call me%nc__water__j_contaminant_outflow%setData(me%output_water__j_contaminant_outflow, start=[1,1,1,1,tStart])
        call me%nc__water__j_contaminant_deposited%setData(me%output_water__j_contaminant_deposited, start=[1,1,1,1,tStart])
        call me%nc__water__j_contaminant_resuspended%setData(me%output_water__j_contaminant_resuspended, start=[1,1,1,1,tStart])
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
        call me%nc__sediment__m_contaminant_total%setData(me%output_sediment__m_contaminant_total, start=[1,1,1,1,tStart])
        call me%nc__sediment__C_contaminant_total%setData(me%output_sediment__C_contaminant_total, start=[1,1,1,1,tStart])
        call me%nc__sediment__C_contaminant_layers%setData(me%output_sediment__C_contaminant_layers, start=[1,1,1,1,1,tStart])
        call me%nc__sediment__m_contaminant_buried%setData(me%output_sediment__m_contaminant_buried, start=[1,1,1,1,tStart])
        call me%nc__sediment__bed_area%setData(me%output_sediment__bed_area, start=[1,1,1,tStart])
        call me%nc__sediment__mass%setData(me%output_sediment__mass, start=[1,1,1,tStart])
        call me%nc__soil__m_contaminant_total%setData(me%output_soil__m_contaminant_total, start=[1,1,1,tStart])
        call me%nc__soil__C_contaminant_total%setData(me%output_soil__C_contaminant_total, start=[1,1,1,tStart])
        if (allocated(me%output_soil__C_contaminant_layers)) then
            call me%nc__soil__C_contaminant_layers%setData(me%output_soil__C_contaminant_layers, start=[1,1,1,1,tStart])
        end if
        if (allocated(me%output_soil__m_soil_eroded)) then
            call me%nc__soil__m_soil_eroded%setData(me%output_soil__m_soil_eroded, start=[1,1,tStart])
            call me%nc__soil__m_contaminant_eroded%setData(me%output_soil__m_contaminant_eroded, start=[1,1,1,tStart])
        end if
        call me%nc__soil__m_contaminant_buried%setData(me%output_soil__m_contaminant_buried, start=[1,1,1,tStart])
        deallocate(me%output_water__m_contaminant)
        deallocate(me%output_water__C_contaminant)
        deallocate(me%output_water__j_contaminant_outflow)
        deallocate(me%output_water__j_contaminant_deposited)
        deallocate(me%output_water__j_contaminant_resuspended)
        deallocate(me%output_water__m_spm)
        deallocate(me%output_water__C_spm)
        if (allocated(me%output_water__m_spm_erosion)) then
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
        deallocate(me%output_sediment__m_contaminant_total)
        deallocate(me%output_sediment__C_contaminant_total)
        deallocate(me%output_sediment__C_contaminant_layers)
        deallocate(me%output_sediment__m_contaminant_buried)
        deallocate(me%output_sediment__bed_area)
        deallocate(me%output_sediment__mass)
        deallocate(me%output_soil__land_use)
        deallocate(me%output_soil__m_contaminant_total)
        deallocate(me%output_soil__C_contaminant_total)
        if (allocated(me%output_soil__C_contaminant_layers)) deallocate(me%output_soil__C_contaminant_layers)
        if (allocated(me%output_soil__m_soil_eroded)) then
            deallocate(me%output_soil__m_soil_eroded)
            deallocate(me%output_soil__m_contaminant_eroded)
        end if
        deallocate(me%output_soil__m_contaminant_buried)
        deallocate(me%output_soil__bulk_density)
    end subroutine
    
    !> Close the NetCDF dataset
    subroutine closeNetCDFOutput(me)
        class(NetCDFOutput) :: me
        call me%nc%close()
    end subroutine

end module
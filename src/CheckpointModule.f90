module CheckpointModule
    use spcEnvironment, only: EnvironmentPointer
    use classEnvironment1
    use DefaultsModule, only: ioUnitCheckpoint
    use Globals, only: dp, C, ERROR_HANDLER
    use classDatabase, only: DATASET
    use classLogger, only: LOGR
    use ErrorInstanceModule
    use ResultModule
    use UtilModule
    implicit none
    private

    type, public :: Checkpoint
        type(EnvironmentPointer) :: env         !! Pointer to the environment, to pull state variables from
        character(len=256) :: checkpointFile    !! Path to the checkpoint file to dump state variables to

      contains
        procedure, public :: init => init
        procedure, public :: save => save
        procedure, public :: reinstate => reinstate
    end type

  contains

    !> Initialise the Checkpoint module
    subroutine init(me, env, checkpointFile)
        class(Checkpoint)           :: me               !! This Checkpoint instance
        type(Environment1), target  :: env              !! Pointer to the Environment object
        character(len=*)            :: checkpointFile   !! Path to the checkpoint file
        ! Point to the environment object
        me%env%item => env
        ! Store the checkpoint file path
        me%checkpointFile = checkpointFile
    end subroutine

    !> Create a checkpoint by saving the current dynamic state of the model to file.
    !! This routine loops through all grid cells and their compartments, constructs
    !! spatial arrays of dynamic variables (through which are passed between timesteps)
    !! and saves these to a binary checkpoint file
    subroutine save(me, t)
        class(Checkpoint)       :: me               !! This Checkpoint instance
        integer                 :: t                !! The current timestep
        integer                 :: i, j, k, l, m    ! Iterators
        ! Variables to save
        ! TODO allow multiple soil profiles
        ! Soil profile
        real(dp) :: soilProfile_m_np(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilProfile_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilProfile_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), 1)
        ! Soil layers
        real(dp) :: soilLayer_m_np(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilLayer_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), 1, &
            C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilLayer_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers)
        real(dp) :: soilLayer_V_w(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers)
        ! Waterbodies
        real(dp) :: water_volume(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies))
        real(dp) :: water_bedArea(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies)) 
        real(dp) :: water_Q(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 10)                           ! Dim 10 = max 7 inflows + outflow + runoff + transfers
        real(dp) :: water_Q_final(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 10)
        real(dp) :: water_j_spm(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 11, C%nSizeClassesSPM)    ! Dim: 11 = max 7 inflows + outflow + dep/res + runoff + transfers
        real(dp) :: water_j_spm_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 11, C%nSizeClassesSPM)
        real(dp) :: water_j_np(DATASET%gridShape(1), DATASET%gridShape(2), &            ! Dim 13 + ps = max 7 inflows + outflow + dep/res + runoff + transfers + max 2 diffuse + n point sources
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_np_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_transformed(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_transformed_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources)
        real(dp) :: water_j_dissolved_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources)
        real(dp) :: water_m_spm(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSizeClassesSPM)
        real(dp) :: water_m_np(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies))
        ! Sediment
        real(dp) :: sediment_m_np(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers + 3, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: sedimentLayer_M_f(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_M_f_backup(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_V_w(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_f_comp(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm, C%nFracCompsSpm)
        real(dp) :: sedimentLayer_pd_comp(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm, C%nFracCompsSpm)

        ! There will be empty elements in the water arrays, as the number of waterbodies, inflows and emissions
        ! varies between each grid cell. So, set to zero so we're at least storing a small number
        water_Q = 0.0_dp
        water_Q_final = 0.0_dp
        water_j_spm = 0.0_dp
        water_j_spm_final = 0.0_dp
        water_j_np = 0.0_dp
        water_j_np_final = 0.0_dp
        water_j_transformed = 0.0_dp
        water_j_transformed_final = 0.0_dp
        water_j_dissolved = 0.0_dp
        water_j_dissolved_final = 0.0_dp
        water_m_spm = 0.0_dp
        water_m_np = 0.0_dp
        water_m_transformed = 0.0_dp
        water_m_dissolved = 0.0_dp

        ! Open the binary checkpoint file, opting to replace any existing contents
        open(ioUnitCheckpoint, &
             file=trim(me%checkpointFile), &
             form='unformatted', &
             status='replace')

        ! Now we need to get spatial arrays of the state variables to save from the
        ! different compartments. We will do the looping through grid cells here
        ! as opposed to in the environment class, so that it's all done within the
        ! same loop. Non-dynamic data will be re-created from input data on restart,
        ! so we only save variables that alter on each time step here
        do j = 1, size(me%env%item%colGridCells, dim=2)
            do i = 1, size(me%env%item%colGridCells, dim=1)
                associate (cell => me%env%item%colGridCells(i,j)%item)
                
                    ! Soil
                    do k = 1, cell%nSoilProfiles
                        associate (profile => cell%colSoilProfiles(k)%item)
                            ! Soil profile dynamic properties
                            soilProfile_m_np(i,j,k,:,:,:) = profile%m_np
                            soilProfile_m_transformed(i,j,k,:,:,:) = profile%m_transformed
                            soilProfile_m_dissolved(i,j,k) = profile%m_dissolved
                            do l = 1, C%nSoilLayers
                                associate (layer => profile%colSoilLayers(l)%item)
                                    ! Soil layer dynamic properties
                                    soilLayer_m_np(i,j,k,l,:,:,:) = layer%m_np
                                    soilLayer_m_transformed(i,j,k,l,:,:,:) = layer%m_transformed
                                    soilLayer_m_dissolved(i,j,k,l) = layer%m_dissolved
                                    soilLayer_V_w(i,j,k,l) = layer%V_w
                                end associate
                            end do
                            ! TODO soil biota
                        end associate
                    end do

                    ! Water
                    do k = 1, cell%nReaches
                        associate (water => cell%colRiverReaches(k)%item)
                            ! Waterbody dynamic properties
                            water_volume(i,j,k) = water%volume
                            water_bedArea(i,j,k) = water%bedArea
                            water_Q(i,j,k,:3+water%nInflows) = water%Q
                            water_Q_final(i,j,k,:3+water%nInflows) = water%Q_final
                            water_j_spm(i,j,k,:4+water%nInflows,:) = water%j_spm
                            water_j_spm_final(i,j,k,:4+water%nInflows,:) = water%j_spm_final
                            water_j_np(i,j,k,:4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:) = water%j_np
                            water_j_np_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:) = water%j_np_final
                            water_j_transformed(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:) = water%j_transformed
                            water_j_transformed_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:) = water%j_transformed_final
                            water_j_dissolved(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources) = water%j_dissolved
                            water_j_dissolved_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources) = water%j_dissolved_final
                            water_m_spm(i,j,k,:) = water%m_spm
                            water_m_np(i,j,k,:,:,:) = water%m_np
                            water_m_transformed(i,j,k,:,:,:) = water%m_transformed
                            water_m_dissolved(i,j,k) = water%m_dissolved

                            ! Sediment
                            associate (sediment => water%bedSediment)
                                sediment_m_np(i,j,k,:,:,:,:) = sediment%M_np
                                ! Sediment layers
                                do l = 1, C%nSedimentLayers
                                    associate (layer => sediment%colBedSedimentLayers(l)%item)
                                        do m = 1, C%nSizeClassesSpm
                                            sedimentLayer_M_f(i,j,k,l,m) = layer%colFineSediment(m)%M_f()
                                            sedimentLayer_M_f_backup(i,j,k,l,m) = layer%colFineSediment(m)%M_f_backup()
                                            sedimentLayer_V_w(i,j,k,l,m) = layer%colFineSediment(m)%V_w()
                                            sedimentLayer_f_comp(i,j,k,l,m,:) = layer%colFineSediment(m)%f_comp
                                            sedimentLayer_pd_comp(i,j,k,l,m,:) = layer%colFineSediment(m)%pd_comp
                                        end do
                                    end associate
                                end do
                            end associate
                        end associate
                    end do

                end associate
            end do
        end do

        ! Grid properties, used to check this checkpoint is compatible with the model run we want to reinstate it to
        write(ioUnitCheckpoint) DATASET%gridBounds, DATASET%gridRes
        ! Write the timestep first, in case we want to use that to resume the model run from
        write(ioUnitCheckpoint) t
        ! Now the compartment specific stuff we obtained above
        write(ioUnitCheckpoint) soilProfile_m_np, soilProfile_m_transformed, soilProfile_m_dissolved
        write(ioUnitCheckpoint) soilLayer_m_np, soilLayer_m_transformed, soilLayer_m_dissolved, soilLayer_V_w
        write(ioUnitCheckpoint) water_volume, water_bedArea, water_Q, water_Q_final, water_j_spm, water_j_spm_final, &
            water_j_np, water_j_np_final, water_j_transformed, water_j_transformed_final, water_j_dissolved, &
            water_j_dissolved_final, water_m_spm, water_m_np, water_m_transformed, water_m_dissolved
        write(ioUnitCheckpoint) sediment_m_np, sedimentLayer_M_f, sedimentLayer_M_f_backup, sedimentLayer_V_w, &
            sedimentLayer_f_comp, sedimentLayer_pd_comp
        ! Close the file
        close(ioUnitCheckpoint)
        
        ! Log that we've successfully created a checkpoint
        call LOGR%toConsole('Saving checkpoint to '//trim(me%checkpointFile)//': \x1B[32msuccess\x1B[0m')
        call LOGR%toFile('Saving checkpoint to '//trim(me%checkpointFile)//': success')

    end subroutine

    !> Reinstate the model run from the checkpoint file
    subroutine reinstate(me, preserve_timestep)
        class(Checkpoint)       :: me                                   !! This Checkpoint instance
        logical, optional       :: preserve_timestep                    !! Should the restarted run preserve the model timestep at the end of saved run?
        integer                 :: i, j, k, l, m                        ! Iterators
        integer                 :: ioStat                               ! IO status, for checking the checkpoint file
        integer                 :: t                                    ! Timestep
        real                    :: gridRes(2), gridBounds(4)            ! Grid properties, for checking the checkpoint is compatible with this model run
        ! Soil profile
        real(dp) :: soilProfile_m_np(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%npDim(1), C%npDim(2), C%npDim(3))       ! TODO allow multiple soil profiles
        real(dp) :: soilProfile_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%npDim(1), C%npDim(2), C%npDim(3))       ! TODO allow multiple soil profiles
        real(dp) :: soilProfile_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), 1)       ! TODO allow multiple soil profiles
        ! Soil layers
        real(dp) :: soilLayer_m_np(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilLayer_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), 1, &
            C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: soilLayer_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers)
        real(dp) :: soilLayer_V_w(DATASET%gridShape(1), DATASET%gridShape(2), 1, C%nSoilLayers)
        ! Water
        real(dp) :: water_volume(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies))
        real(dp) :: water_bedArea(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies)) 
        real(dp) :: water_Q(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 10)
        real(dp) :: water_Q_final(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 10)
        real(dp) :: water_j_spm(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies), 11, C%nSizeClassesSPM)
        real(dp) :: water_j_spm_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 11, C%nSizeClassesSPM)
        real(dp) :: water_j_np(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_np_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_transformed(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_transformed_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_j_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources)
        real(dp) :: water_j_dissolved_final(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), 13 + DATASET%maxPointSources)
        real(dp) :: water_m_spm(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSizeClassesSPM)
        real(dp) :: water_m_np(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_m_transformed(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: water_m_dissolved(DATASET%gridShape(1), DATASET%gridShape(2), maxval(DATASET%nWaterbodies))
        ! Sediment
        real(dp) :: sediment_m_np(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers + 3, C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp) :: sedimentLayer_M_f(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_M_f_backup(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_V_w(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm)
        real(dp) :: sedimentLayer_f_comp(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm, C%nFracCompsSpm)
        real(dp) :: sedimentLayer_pd_comp(DATASET%gridShape(1), DATASET%gridShape(2), &
            maxval(DATASET%nWaterbodies), C%nSedimentLayers, C%nSizeClassesSpm, C%nFracCompsSpm)

        ! If preserve timestep not present, then default to false
        if (.not. present(preserve_timestep)) preserve_timestep = .false.

        ! Open the checkpoint file, read in the grid properties and use these to check if the checkpoint file
        ! is compatible with the current grid setup
        open(ioUnitCheckpoint, file=trim(me%checkpointFile), form='unformatted', status='old')
        read(ioUnitCheckpoint) gridBounds, gridRes

        if (any(abs(gridBounds - DATASET%gridBounds) > C%epsilon)) then
            call ERROR_HANDLER%trigger( &
                error=ErrorInstance(message="Grid bounds of checkpoint and current simulation do not match. " // &
                    "Grid setup must be identical to reinstate a checkpoint. Checkpoint bounds: " // &
                    trim(adjustl(str(gridBounds))) // ". Simulation bounds: " // trim(adjustl(str(DATASET%gridBounds))) // ".") &
            )
        else if (any(abs(gridRes - DATASET%gridRes) > C%epsilon)) then
            call ERROR_HANDLER%trigger( &
                error=ErrorInstance(message="Grid resolution of checkpoint and current simulation do not match. " // &
                    "Grid setup must be identical to reinstate a checkpoint. Checkpoint resolution: " // &
                    trim(adjustl(str(gridRes))) // ". Simulation resolution: " // trim(adjustl(str(DATASET%gridRes))) // ".") &
            )
        end if

        read(ioUnitCheckpoint) t
        read(ioUnitCheckpoint, iostat=ioStat) soilProfile_m_np, soilProfile_m_transformed, soilProfile_m_dissolved
        ! If there is a read error, it's likely the geographical scenario is different
        if (ioStat /= 0) then
            print *, ioStat
            call ERROR_HANDLER%trigger( &
                error=ErrorInstance(message="Error reading from checkpoint file. Are you sure the checkpoint you " // &
                    "are trying to reinstate is the same geographical scenario as this model run?") &
            )
        end if
        read(ioUnitCheckpoint) soilLayer_m_np, soilLayer_m_transformed, soilLayer_m_dissolved, soilLayer_V_w
        read(ioUnitCheckpoint) water_volume, water_bedArea, water_Q, water_Q_final, water_j_spm, water_j_spm_final, &
            water_j_np, water_j_np_final, water_j_transformed, water_j_transformed_final, water_j_dissolved, &
            water_j_dissolved_final, water_m_spm, water_m_np, water_m_transformed, water_m_dissolved
        read(ioUnitCheckpoint) sediment_m_np, sedimentLayer_M_f, sedimentLayer_M_f_backup, sedimentLayer_V_w, &
            sedimentLayer_f_comp, sedimentLayer_pd_comp
        close(ioUnitCheckpoint)

        ! Now we've read in those variables, we need to reinstate them.
        ! First, should we reinstate the model timestep from the checkpoint?
        if (preserve_timestep) then
            C%t0 = t
        end if

        ! Loop through all the grid cells and use the checkpoint data to set
        ! their dynamic state variables. Basically the opposite of me%save()
        do j = 1, size(me%env%item%colGridCells, dim=2)
            do i = 1, size(me%env%item%colGridCells, dim=1)
                associate (cell => me%env%item%colGridCells(i,j)%item)
                
                    ! Soil
                    do k = 1, cell%nSoilProfiles
                        associate (profile => cell%colSoilProfiles(k)%item)
                            ! Soil profile dynamic properties
                            profile%m_np = soilProfile_m_np(i,j,k,:,:,:) 
                            profile%m_transformed = soilProfile_m_transformed(i,j,k,:,:,:) 
                            profile%m_dissolved = soilProfile_m_dissolved(i,j,k) 
                            ! CHECK: m_np_eroded
                            do l = 1, C%nSoilLayers
                                associate (layer => profile%colSoilLayers(l)%item)
                                    ! Soil layer dynamic properties
                                    layer%m_np = soilLayer_m_np(i,j,k,l,:,:,:) 
                                    layer%m_transformed = soilLayer_m_transformed(i,j,k,l,:,:,:) 
                                    layer%m_dissolved = soilLayer_m_dissolved(i,j,k,l) 
                                    layer%V_w = soilLayer_V_w(i,j,k,l) 
                                end associate
                            end do
                            ! TODO soil biota
                        end associate
                    end do

                    ! Water
                    do k = 1, cell%nReaches
                        associate (water => cell%colRiverReaches(k)%item)
                            ! Waterbody dynamic properties
                            water%volume = water_volume(i,j,k)
                            water%bedArea = water_bedArea(i,j,k)
                            water%Q = water_Q(i,j,k,:3+water%nInflows) 
                            water%Q_final = water_Q_final(i,j,k,:3+water%nInflows) 
                            water%j_spm = water_j_spm(i,j,k,:4+water%nInflows,:) 
                            water%j_spm_final = water_j_spm_final(i,j,k,:4+water%nInflows,:) 
                            water%j_np = water_j_np(i,j,k,:4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:) 
                            water%j_np_final = water_j_np_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:)  
                            water%j_transformed = water_j_transformed(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:)  
                            water%j_transformed_final =  water_j_transformed_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources,:,:,:)  
                            water%j_dissolved = water_j_dissolved(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources)  
                            water%j_dissolved_final = water_j_dissolved_final(i,j,k, &
                                :4+water%nInflows+water%nDiffuseSources+water%nPointSources)  
                            water%m_spm = water_m_spm(i,j,k,:) 
                            water%m_np = water_m_np(i,j,k,:,:,:) 
                            water%m_transformed = water_m_transformed(i,j,k,:,:,:) 
                            water%m_dissolved = water_m_dissolved(i,j,k) 

                            ! Sediment
                            associate (sediment => water%bedSediment)
                                sediment%M_np = sediment_m_np(i,j,k,:,:,:,:) 
                                ! Sediment layers
                                do l = 1, C%nSedimentLayers
                                    associate (layer => sediment%colBedSedimentLayers(l)%item)
                                        do m = 1, C%nSizeClassesSpm
                                            call layer%colFineSediment(m)%set( &
                                                Mf_in = sedimentLayer_M_f(i,j,k,l,m), &
                                                Vw_in = sedimentLayer_V_w(i,j,k,l,m) &
                                            )
                                            call layer%colFineSediment(m)%backup_M_f()  
                                            layer%colFineSediment(m)%f_comp = sedimentLayer_f_comp(i,j,k,l,m,:)
                                            layer%colFineSediment(m)%pd_comp = sedimentLayer_pd_comp(i,j,k,l,m,:) 
                                        end do
                                    end associate
                                end do
                            end associate
                        end associate
                    end do

                end associate
            end do
        end do

        ! Log that we've successfully reinstated a checkpoint
        call LOGR%toConsole('Reinstating checkpoint from '//trim(me%checkpointFile)//': \x1B[32msuccess\x1B[0m')
        call LOGR%toFile('Reinstating checkpoint from '//trim(me%checkpointFile)//': success')

    end subroutine

end module
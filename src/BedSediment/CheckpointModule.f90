module CheckpointModule
    use spcEnvironment, only: EnvironmentPointer
    use classEnvironment1
    use DefaultsModule, only: ioUnitCheckpoint
    use Globals, only: dp, C
    use classDatabase, only: DATASET
    use classLogger, only: LOGR
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
        integer                 :: i, j, k, l       ! Iterators
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
                associate(cell => me%env%item%colGridCells(i,j)%item)
                
                    ! Soil
                    do k = 1, cell%nSoilProfiles
                        associate(profile => cell%colSoilProfiles(k)%item)
                            ! Soil profile dynamic properties
                            soilProfile_m_np(i,j,k,:,:,:) = profile%m_np
                            soilProfile_m_transformed(i,j,k,:,:,:) = profile%m_transformed
                            soilProfile_m_dissolved(i,j,k) = profile%m_dissolved
                            ! CHECK: m_np_eroded
                            do l = 1, C%nSoilLayers
                                associate(layer => profile%colSoilLayers(l)%item)
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
                        associate(water => cell%colRiverReaches(k)%item)
                            ! Waterbody dynamic properties
                            water_volume(i,j,k) = water%volume
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
                        end associate
                    end do

                end associate
            end do
        end do

        ! Write the timestep first, in case we want to use that to resume the model run from
        write(ioUnitCheckpoint) t
        ! Now the compartment specific stuff we obtained above
        write(ioUnitCheckpoint) soilProfile_m_np, soilProfile_m_transformed, soilProfile_m_dissolved
        write(ioUnitCheckpoint) soilLayer_m_np, soilLayer_m_transformed, soilLayer_m_dissolved, soilLayer_V_w
        write(ioUnitCheckpoint) water_volume, water_Q, water_Q_final, water_j_spm, water_j_spm_final, &
            water_j_np, water_j_np_final, water_j_transformed, water_j_transformed_final, water_j_dissolved, &
            water_j_dissolved_final, water_m_spm, water_m_np, water_m_transformed, water_m_dissolved
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
        integer                 :: i, j, k, l                           ! Iterators
        integer                 :: t                                    ! Timestep
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

        ! If preserve timestep not present, then default to false
        if (.not. present(preserve_timestep)) preserve_timestep = .false.

        open(ioUnitCheckpoint, file=trim(me%checkpointFile), form='unformatted', status='old')
        read(ioUnitCheckpoint) t
        read(ioUnitCheckpoint) soilProfile_m_np, soilProfile_m_transformed, soilProfile_m_dissolved
        read(ioUnitCheckpoint) soilLayer_m_np, soilLayer_m_transformed, soilLayer_m_dissolved, soilLayer_V_w
        read(ioUnitCheckpoint) water_volume, water_Q, water_Q_final, water_j_spm, water_j_spm_final, &
            water_j_np, water_j_np_final, water_j_transformed, water_j_transformed_final, water_j_dissolved, &
            water_j_dissolved_final, water_m_spm, water_m_np, water_m_transformed, water_m_dissolved
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
                associate(cell => me%env%item%colGridCells(i,j)%item)
                
                    ! Soil
                    do k = 1, cell%nSoilProfiles
                        associate(profile => cell%colSoilProfiles(k)%item)
                            ! Soil profile dynamic properties
                            profile%m_np = soilProfile_m_np(i,j,k,:,:,:) 
                            profile%m_transformed = soilProfile_m_transformed(i,j,k,:,:,:) 
                            profile%m_dissolved = soilProfile_m_dissolved(i,j,k) 
                            ! CHECK: m_np_eroded
                            do l = 1, C%nSoilLayers
                                associate(layer => profile%colSoilLayers(l)%item)
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
                        associate(water => cell%colRiverReaches(k)%item)
                            ! Waterbody dynamic properties
                            water%volume = water_volume(i,j,k) 
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
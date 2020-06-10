!> Module containing the `SoilProfile1` class definition.
module classSoilProfile1
    use Globals                                                 ! Global definitions and constants
    use UtilModule                                              ! Useful functions
    use mo_netcdf                                               ! Input/output handling
    use netcdf, only: nf90_fill_real
    use datetime_module
    use ResultModule, only: Result                              ! Error handling classes
    use spcSoilProfile                                          ! Parent class
    use classSoilLayer1                                         ! SoilLayers will be contained in the SoilProfile
    use classDatabase, only: DATASET
    implicit none

    !> A `SoilProfile` class acts as a container for a collection of
    !! `SoilLayer` objects, which collectively define the layout of
    !! the `SoilProfile`.
    type, public, extends(SoilProfile) :: SoilProfile1
      contains
        procedure :: create => createSoilProfile1                   ! Create the SoilProfile object
        procedure :: destroy => destroySoilProfile1                 ! Remove the SoilProfile object and all contained objects
        procedure :: update => updateSoilProfile1                   ! Update on every timestep (e.g., perform routing of water through soil)
        procedure :: percolate => percolateSoilProfile1             ! Percolate soil on a given time step
        procedure :: erode => erodeSoilProfile1                     ! Erode soil on a given time step
        procedure :: bioturbation => bioturbationSoilProfile1       ! Bioturbate soil on a given time step
        procedure :: imposeSizeDistribution => imposeSizeDistributionSoilProfile1 ! Impose size distribution on mass of sediment
        procedure :: calculateAverageGrainSize => calculateAverageGrainSizeSoilProfile1 ! Calculate the average grain diameter from soil texture
        procedure :: parseInputData => parseInputDataSoilProfile1   ! Parse the data from the input file and store in object properties
        procedure :: parseNewBatchData => parseNewBatchDataSoilProfile1
        ! Getters
        procedure :: get_C_np => get_C_np_SoilProfile1
    end type

  contains
    !> Creating the `SoilProfil`e parses input data and fills
    !! the corresponding object properties, as well as setting
    !! up the contained `SoilLayers`.
    function createSoilProfile1(me, &
                                x, &
                                y, &
                                p, &
                                slope, &
                                n_river, &
                                area, &
                                q_precip_timeSeries, &
                                q_evap_timeSeries) result(r)
        class(SoilProfile1) :: me                           !! The `SoilProfile` instance.
        integer             :: x                            !! Containing `GridCell` x index
        integer             :: y                            !! Containing `GridCell` y index
        integer             :: p                            !! `SoilProfile` reference (redundant for now as only one `SoilProfile` per `GridCell`)
        real(dp)            :: slope                        !! Slope of the containing `GridCell` [m/m]
        real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
        real(dp)            :: area                         !! The surface area of the `SoilProfile` [m3]
        real(dp), allocatable :: q_precip_timeSeries(:)     !! Precipitation time series [m/timestep]
        real(dp), allocatable :: q_evap_timeSeries(:)       !! Evaporation time series [m/timestep]
        type(Result)        :: r                            !! The `Result` object
        integer             :: l                            ! Soil layer iterator
        type(SoilLayer1), allocatable :: sl                 ! Temporary SoilLayer1 variable

        me%ref = ref("SoilProfile", x, y, p)                ! Generate the reference name for the SoilProfile

        ! Allocate the object properties that need to be
        allocate(me%usle_C(C%nTimeSteps), &
            me%usle_alpha_half(C%nTimeSteps), &
            me%erodedSediment(C%nSizeClassesSpm), &
            me%distributionSediment(C%nSizeClassesSpm), &
            me%m_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_buried(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_eroded(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_in(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_buried(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_eroded(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_in(C%npDim(1), C%npDim(2), C%npDim(3)), &
            ! me%C_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%colSoilLayers(C%nSoilLayers))
        ! Initialise variables
        me%x = x                                            ! GridCell x index
        me%y = y                                            ! GridCell y index
        me%p = p                                            ! SoilProfile index within the GridCell
        me%slope = slope
        me%n_river = n_river
        me%area = area                                      ! Surface area
        allocate(me%q_precip_timeSeries, source=q_precip_timeSeries)            ! [m/timestep]
        allocate(me%q_evap_timeSeries, source=q_evap_timeSeries)                ! [m/timestep]
        me%V_buried = 0.0_dp                                ! Volume of water "lost" from the bottom of SoilProfile
        me%m_np_buried = 0.0_dp                             ! Mass of NM "lost" from the bottom of the SoilProfile
        me%m_np = 0.0_dp                                    ! Nanomaterial mass
        me%m_np_eroded = 0.0_dp
        me%m_np_in = 0.0_dp
        me%m_transformed = 0.0_dp
        me%m_transformed_eroded = 0.0_dp
        me%m_transformed_in = 0.0_dp
        me%m_transformed_buried = 0.0_dp
        me%m_dissolved = 0.0_dp
        me%m_dissolved_in = 0.0_dp
        me%m_dissolved_buried = 0.0_dp
        ! me%C_np = 0.0_dp
        
        ! Parse and store input data in this object's properties
        call r%addErrors(.errors. me%parseInputData())
        if (r%hasCriticalError()) return                    ! Return early if there are critical errors

        ! Set up the SoilLayers
        ! TODO: Different types of SoilLayer
        do l = 1, C%nSoilLayers
            allocate(sl)        ! Must be allocated on every time step
            ! Create the SoilLayer and add any errors to Result object
            call r%addErrors(.errors. &
                sl%create( &
                    me%x, &
                    me%y, &
                    me%p, &
                    l, &
                    me%WC_sat, &
                    me%WC_FC, &
                    me%K_s, &
                    me%area, &
                    me%bulkDensity, &
                    me%d_grain, &
                    me%porosity, &
                    me%earthwormDensity * DATASET%earthwormVerticalDistribution(l) &    ! Split earthworm density into vertical distribution
                ) &
            )
            call move_alloc(sl, me%colSoilLayers(l)%item)   ! This automatically deallocates sl
        end do
        call r%addToTrace("Creating " // trim(me%ref))      ! Add this procedure to the error trace
    end function

    !> Destroy this SoilProfile
    function destroySoilProfile1(me) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        type(Result) :: r                                       !! `Result` object to return
    end function

    !> Perform the simulation of the `SoilProfile` for the current time step:
    !! <ul>
    !!  <li>Percolate soil through all SoilLayers</li>
    !!  <li>Erode sediment</li>
    !! </ul>
    function updateSoilProfile1(me, t, j_np_diffuseSource, j_transformed_diffuseSource, j_dissolved_diffuseSource) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        integer :: t                                            !! The current timestep
        real(dp) :: j_np_diffuseSource(:,:,:)                   !! Diffuse source of NM for this timestep [kg/m2/timestep]
        real(dp) :: j_transformed_diffuseSource(:,:,:)          !! Diffuse source of NM for this timestep [kg/m2/timestep]
        real(dp) :: j_dissolved_diffuseSource                   !! Diffuse source of NM for this timestep [kg/m2/timestep]
        type(Result) :: r                                       !! Result object to return
        integer :: i, j, k
        ! real(dp) :: C_np_l(C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3))

        if (.not. me%isUrban) then
            ! Set the timestep-specific object properties
            me%q_precip = me%q_precip_timeSeries(t)                 ! Get the relevant time step's precipitation [m/timestep]
            me%q_evap = me%q_evap_timeSeries(t)                     ! and evaporation [m/timestep]
            me%q_in = max(me%q_precip - me%q_evap, 0.0_dp)          ! Infiltration = precip - evap. This is supplied to SoilLayer_1 [m/timestep]. Minimum = 0.
                ! TODO: Should the minimum q_in be 0, or should evaporation be allowed to remove water from top soil layer?

            ! Add NM from the diffuse source
            me%m_np = me%m_np + j_np_diffuseSource * me%area          ! j_np_diffuseSource is in kg/m2/timestep
            me%m_np_in = j_np_diffuseSource * me%area
            me%m_transformed_in = j_transformed_diffuseSource * me%area
            me%m_transformed = me%m_transformed + me%m_transformed_in
            me%m_dissolved_in = j_dissolved_diffuseSource * me%area
            me%m_dissolved = me%m_dissolved + me%m_dissolved_in
            
            ! Perform percolation, erosion and bioturbation simluations
            call r%addErrors([ &
                .errors. me%erode(t), &
                .errors. me%percolate(t, j_np_diffuseSource, j_transformed_diffuseSource, j_dissolved_diffuseSource), &
                .errors. me%bioturbation() &
            ])

            ! Remove buried NM (eroded NM removed in me%erode)
            ! TODO unify where me%m_np is updated (or deprecate, see me%erode())
            me%m_np = me%m_np - me%m_np_buried

            ! Update mean concentration across the layers for this profile (used for output)
            ! do i = 1, C%nSoilLayers
            !     C_np_l(i, :, :, :) = me%colSoilLayers(i)%item%C_np
            ! end do
            ! do k = 1, C%npDim(3)
            !     do j = 1, C%npDim(2)
            !         do i = 1, C%npDim(1)
            !             if (.not. isZero(me%C_np(i,j,k))) then
            !                 me%C_np(i,j,k) = sum(C_np_l(:,i,j,k)) / C%nSoilLayers
            !             else
            !                 me%C_np(i,j,k) = 0.0_dp
            !             end if
            !         end do
            !     end do
            ! end do

        else
            ! If this is an urban cell, presume nothing for the moment
            me%erodedSediment = 0
        end if

        ! Add this procedure to the Result object's trace                               
        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function

    !> Percolate water through the `SoilProfile`, by looping through `SoilLayer`s
    !! and running their individual percolation procedures, and then passing
    !! percolated and pooled flows between `SoilLayer`s. Pooled water from top
    !! `SoilLayer` forms surface runoff, and "lost" water from bottom `SoilLayer`
    !! is kept track of in `me%V_buried`
    function percolateSoilProfile1(me, t, j_np_diffuseSource, j_transformed_diffuseSource, j_dissolved_diffuseSource) result(r)
        class(SoilProfile1) :: me                               !! This `SoilProfile1` instance
        integer             :: t                                !! The current time step
        real(dp)            :: j_np_diffuseSource(:,:,:)        !! Difffuse source of NM for this timestep [kg/m2/timestep]
        real(dp)            :: j_transformed_diffuseSource(:,:,:)
        real(dp)            :: j_dissolved_diffuseSource
        type(Result)        :: r                                !! The `Result` object to return
        integer             :: l, i                             ! Loop iterator for SoilLayers
        real(dp)            :: q_l_in                           ! Temporary water inflow for a particular SoilLayer
        real(dp)            :: m_np_l_in(C%npDim(1), &          ! Temporary NM inflow for particular SoilLayer
                                         C%npDim(2), &
                                         C%npDim(3))
        real(dp)            :: m_transformed_l_in(C%npDim(1), C%npDim(2), C%npDim(3))
        real(dp)            :: m_dissolved_l_in

        ! Loop through SoilLayers and percolate 
        do l = 1, C%nSoilLayers
            if (l == 1) then
                 ! If it's the first SoilLayer, water and NM inflow will be from precip - ET
                 ! and the diffuse source, respectively
                q_l_in = me%q_in                                    ! [m3/m2/timestep]
                m_np_l_in = j_np_diffuseSource * me%area            ! [kg/timestep]
                m_transformed_l_in = j_transformed_diffuseSource * me%area
                m_dissolved_l_in = j_dissolved_diffuseSource * me%area
            else
                ! Otherwise, they'll be from the layer above
                q_l_in = me%colSoilLayers(l-1)%item%V_perc          ! [m3/m2/timestep]
                m_np_l_in = me%colSoilLayers(l-1)%item%m_np_perc    ! [kg/timestep]
                m_transformed_l_in = me%colSoilLayers(l-1)%item%m_transformed_perc  ! [kg/timestep]
                m_dissolved_l_in = me%colSoilLayers(l-1)%item%m_dissolved_perc      ! [kg/timestep]
            end if

            ! Run the percolation simulation for individual layer, setting V_perc, V_pool, m_np_perc etc.
            call r%addErrors(.errors. &
                me%colSoilLayers(l)%item%update(t, q_l_in, m_np_l_in, m_transformed_l_in, m_dissolved_l_in) &
            )
            ! If there is pooled water, we must push up to the previous layer, recursively
            ! for each SoilLayer above this
            do i = 1, l
                ! Check if the layer beneath has pooled any water
                if (me%colSoilLayers(l-i+1)%item%V_pool > 0) then
                    if (l-i == 0) then                          ! If it's the top soil layer, track how much pooled above soil
                        me%V_pool = me%colSoilLayers(l-i+1)%item%V_pool
                    else                                        ! Else, add pooled volume to layer above
                        call r%addErrors(.errors. &
                            me%colSoilLayers(l-i)%item%addPooledWater( &
                                me%colSoilLayers(l-i+1)%item%V_pool &
                            ) &
                        )
                    end if
                end if
            end do
        end do

        ! Keep track of "lost" NM and water from the bottom soil layer. Not cumulative.
        me%V_buried = me%colSoilLayers(C%nSoilLayers)%item%V_perc
        me%m_np_buried = me%colSoilLayers(C%nSoilLayers)%item%m_np_perc
        me%m_transformed_buried = me%colSoilLayers(C%nSoilLayers)%item%m_transformed_perc
        me%m_dissolved_buried = me%colSoilLayers(C%nSoilLayers)%item%m_dissolved_perc

        ! Add this procedure to the Result object's trace
        call r%addToTrace("Percolating water on time step #" // trim(str(t)))
    end function

    !> Calculate the soil erosion for this timestep and updates this `GridCell`'s
    !! `erodedSediment` property accordingly. Soil erosion based on RUSLE, with
    !! R-factor derived from kinetic energy calculated by Davison method:
    !! [Davison et al. 2005](https://doi.org/10.1016/j.scitotenv.2005.02.002).
    !! Europe specific parameterisation include in a_1, a_2, a_3, I30 and b
    !! parameters (provided by input data). K factor based on [modified Morgan
    !! Finney](https://doi.org/10.1002/esp.1530) and in g/J. Using these units, R-factor
    !! is simply equal to kinetic energy (J/m2) and sediment yield is in g/m2.
    function erodeSoilProfile1(me, t) result(rslt)
        class(SoilProfile1) :: me
        integer             :: t
        type(Result)        :: rslt
        real(dp)            :: E_k
        real(dp)            :: K_MMF
        real(dp)            :: erodedSedimentTotal
        type(datetime)      :: currentDate
        integer             :: julianDay
        real(dp)            :: m_soil_l1
        real(dp)            :: propEroded
        real(dp)            :: erodedNP(C%nSizeClassesNM)
        integer             :: i

        ! TODO This function only works with daily timesteps

        ! Convert the current date to Julian day number (https://en.wikipedia.org/wiki/Julian_day).
        ! date2num converts to number of days since 0001-01-01, and 1721423 is the Julian day
        ! number of 0001-01-01.
        currentDate = C%startDate + timedelta(days=t-1)
        julianDay = date2num(currentDate) + 1721423
        ! Then calculate the kinetic energy [J/m2/day]. Precip needs converting to [mm/day] from [m/timestep].
        E_k = (me%erosivity_a1 + me%erosivity_a2 * cos(julianDay * (2*C%pi/365) + me%erosivity_a3)) &
                * (me%q_precip_timeSeries(t)*1.0e3)**me%erosivity_b
        ! Now the modified MMF version of K, dependent on sand, silt and clay content [g/J]
        K_MMF = 0.1*(me%clayContent/100.0_dp) + 0.3*(me%sandContent/100.0_dp) + 0.5*(me%siltContent/100.0_dp)
        ! Total eroded sediment [g/m2/day]
        erodedSedimentTotal = E_k * K_MMF * me%usle_C(t) * me%usle_P * me%usle_LS
        ! Split this into a size distribution and convert to [kg/m2/day]
        me%erodedSediment = me%imposeSizeDistribution(erodedSedimentTotal*1.0e-3)
        
        ! The top soil layer deals with eroding NM
        call rslt%addErrors(.errors. me%colSoilLayers(1)%item%erode(me%erodedSediment, me%bulkDensity, me%area))
        ! Remove this eroded soil from the total m_np in the profile
        ! TODO Depracate me%m_np for the whole profile, as it
        ! means updating NM mass in both the profile and the individual layers

        do i = 1, C%nSizeClassesNM
            ! Transfer NM eroded from attached to heteroaggregated, by imposing the size distribution
            ! as for eroded SPM. The logic here is that the soil the NM is attached to will end up
            ! as SPM and thus the NM attached it will be heteroaggregated rather than attached/bound.
            me%m_np_eroded(i,1,3:) = me%imposeSizeDistribution(me%colSoilLayers(1)%item%m_np_eroded(i,1,2))     ! [kg/gridcell/timestep]
            me%m_transformed_eroded(i,1,3:) = me%imposeSizeDistribution(me%colSoilLayers(1)%item%m_transformed_eroded(i,1,2))
        end do
        ! TODO why is attached being set? m_np_eroded has double the mass it should now
        me%m_np_eroded(:,1,2) = me%colSoilLayers(1)%item%m_np_eroded(:,1,2)
        me%m_transformed_eroded(:,1,2) = me%colSoilLayers(1)%item%m_transformed_eroded(:,1,2)
        me%m_np(:,1,2) = me%m_np(:,1,2) - me%m_np_eroded(:,1,2)     ! Remove the eroded NM from the soil
        me%m_transformed(:,1,2) = me%m_transformed(:,1,2) - me%m_transformed_eroded(:,1,2) 
    end function

    !> Perform bioturbation on a time step by mixing calculated depth of two layers together
    function bioturbationSoilProfile1(me) result(rslt)
        class(SoilProfile1) :: me           !! This `SoilProfile1` instance
        type(Result)        :: rslt         !! The `Result` object to return
        integer             :: i            ! Iterator
        real                :: fractionOfLayerToMix
        ! Only model bioturbation if config file has asked us to
        if (C%includeBioturbation) then
            ! Perform bioturbation for each layer, except final layer
            ! TODO set some proper boundary conditions
            do i = 1, C%nSoilLayers - 1
                fractionOfLayerToMix = me%colSoilLayers(i)%item%calculateBioturbationRate() * C%timeStep
                ! Only attached NM are mixed
                me%colSoilLayers(i)%item%m_np(:,1,2) = me%colSoilLayers(i)%item%m_np(:,1,2) &
                    + fractionOfLayerToMix * (me%colSoilLayers(i+1)%item%m_np(:,1,2) - me%colSoilLayers(i)%item%m_np(:,1,2))
                me%colSoilLayers(i+1)%item%m_np(:,1,2) = me%colSoilLayers(i+1)%item%m_np(:,1,2) &
                    + fractionOfLayerToMix * (me%colSoilLayers(i)%item%m_np(:,1,2) - me%colSoilLayers(i+1)%item%m_np(:,1,2))
                ! Same for transformed NM
                me%colSoilLayers(i)%item%m_transformed(:,1,2) &
                    = me%colSoilLayers(i)%item%m_transformed(:,1,2) + fractionOfLayerToMix &
                    * (me%colSoilLayers(i+1)%item%m_transformed(:,1,2) - me%colSoilLayers(i)%item%m_transformed(:,1,2))
                me%colSoilLayers(i+1)%item%m_transformed(:,1,2) &
                    = me%colSoilLayers(i+1)%item%m_transformed(:,1,2) + fractionOfLayerToMix &
                    * (me%colSoilLayers(i)%item%m_transformed(:,1,2) - me%colSoilLayers(i+1)%item%m_transformed(:,1,2))
            end do
        end if
    end function

    !> Impose a size class distribution on a total mass to split it up
    !! into separate size classes. If no distribution has been specified
    !! for this `SoilProfile`, then a default global size distribution is used
    function imposeSizeDistributionSoilProfile1(me, mass) result(distribution)
        class(SoilProfile1) :: me                               !! This `SoilProfile` instance
        real(dp)            :: mass                             !! The mass to split into size classes
        real(dp)            :: distribution(C%nSizeClassesSpm)  !! The resulting distribution
        integer             :: s                                ! Loop iterator for size classes
        do s = 1, C%nSizeClassesSpm
            distribution(s) = mass*me%distributionSediment(s)
        end do
    end function

    !> Calculate the average grain size from soil texture properties, using RUSLE handbook
    !! http://www.grr.ulaval.ca/gae_3005/Documents/References/RUSLE/ah703_ch3.pdf (p76):
    !! d_grain = exp(%clay * ln(0.001) + %silt * ln(0.026) + %sand * ln(1.025))
    function calculateAverageGrainSizeSoilProfile1(me, clay, silt, sand) result(d_grain)
        class(SoilProfile1) :: me           ! This soil profile
        real :: clay, silt, sand            ! Percentage clay, silt and sand
        real :: d_grain                     ! The average grain size
        d_grain = 1e-3 * exp(0.01 * (clay * log(0.001) + silt * log(0.026) + sand * log(1.025)))
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including the allocation of arrays that depend on
    !! this input data
    function parseInputDataSoilProfile1(me) result(r)
        class(SoilProfile1)     :: me                       !! This `SoilProfile` instance
        type(Result)            :: r                        !! `Result` object to return
        integer                 :: i                        ! Iterator
        integer                 :: landUse                  ! Index of max land use fraction in this profile
        type(NcVariable)        :: var

        me%distributionSediment = DATASET%defaultSpmSizeDistribution
        me%bulkDensity = DATASET%soilBulkDensity(me%x, me%y)
        me%WC_sat = DATASET%soilWaterContentSaturation(me%x, me%y)
        me%WC_FC = DATASET%soilWaterContentFieldCapacity(me%x, me%y)
        me%K_s = DATASET%soilHydraulicConductivity(me%x, me%y)
        ! Soil hydraulic properties contain no data where in urban areas. For the moment,
        ! until land cover properly incorporated into model, we'll use this as a proxy
        ! for urban areas (which therefore contain no soil profile). In the future, we should
        ! account for this properly by splitting grid cells into different soil profiles.
        if (me%WC_sat == nf90_fill_real) me%WC_sat = 0.8
        if (me%WC_FC == nf90_fill_real) me%WC_FC = 0.5
        if (me%K_s == nf90_fill_real) me%K_s = 1e-6
        if (me%bulkDensity == nf90_fill_real) me%bulkDensity = 1220

        me%clayContent = DATASET%soilTextureClayContent(me%x, me%y)
        me%sandContent = DATASET%soilTextureSandContent(me%x, me%y)
        me%siltContent = DATASET%soilTextureSiltContent(me%x, me%y)
        me%coarseFragContent = DATASET%soilTextureCoarseFragContent(me%x, me%y)
        if (me%coarseFragContent == nf90_fill_real) then
            me%coarseFragContent = 0.0
        end if
        ! Calculate the average grain diameter from soil texture
        me%d_grain = me%calculateAverageGrainSize(me%clayContent, me%siltContent, me%sandContent)
        me%porosity = DATASET%soilDefaultPorosity       ! TODO change to be spatial

        ! USLE params
        me%usle_C = DATASET%soilUsleCFactor(me%x, me%y)
        ! TODO make usle_C not temporal
        if ((me%usle_C(1) == nf90_fill_double) .or. (me%usle_C(1) < 0) .or. (me%usle_C(1) > 100)) then
            me%usle_C = 0.00055095          ! Pick a small value to represent urban, if there's no data
        end if
        me%usle_P = DATASET%soilUslePFactor(me%x, me%y)
        if (me%usle_P == nf90_fill_double) then
            me%usle_P = 1.0                 ! If there's no data, assume no support practice
        end if
        me%usle_LS = DATASET%soilUsleLSFactor(me%x, me%y)
        if (me%usle_LS == nf90_fill_double) then
            me%usle_LS = 0.3                 ! Pick an average value if there's no data
        end if

        ! Get earthworm density from land use. Select the maximum land use fraction and use all
        ! of profile is that.
        landUse = maxloc(DATASET%landUse(me%x, me%y, :), dim=1)
        ! TODO get these values more intelligently
        select case (landUse)
            case (1)
                me%earthwormDensity = DATASET%earthwormDensityUrbanCapped
            case (2)
                me%earthwormDensity = DATASET%earthwormDensityUrbanParks
            case (3)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
            case (4)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
            case (5)
                me%earthwormDensity = DATASET%earthwormDensityArable
            case (6)
                me%earthwormDensity = DATASET%earthwormDensityGrassland
            case (7)
                me%earthwormDensity = DATASET%earthwormDensityDeciduous
            case (8)
                me%earthwormDensity = DATASET%earthwormDensityConiferous
            case (9)
                me%earthwormDensity = DATASET%earthwormDensityHeathland
            case default
                me%earthwormDensity = 0.0_dp
        end select

        ! Auditing
        call r%addError( &
            ERROR_HANDLER%equal( &
                value = sum(me%distributionSediment), &
                criterion = 1.0, &
                message = "Specified size class distribution for sediments " &
                            // "does not sum to 100%." &
            ) &
        )

        me%erosivity_a1 = DATASET%soilErosivity_a1
        me%erosivity_a2 = DATASET%soilErosivity_a2
        me%erosivity_a3 = DATASET%soilErosivity_a3
        me%erosivity_b = DATASET%soilErosivity_b

        ! Add this procedure to the trace
        call r%addToTrace('Parsing input data')
    end function

    subroutine parseNewBatchDataSoilProfile1(me)
        class(SoilProfile1) :: me
        integer :: landUse

        me%bulkDensity = DATASET%soilBulkDensity(me%x, me%y)
        me%WC_sat = DATASET%soilWaterContentSaturation(me%x, me%y)
        me%WC_FC = DATASET%soilWaterContentFieldCapacity(me%x, me%y)
        me%K_s = DATASET%soilHydraulicConductivity(me%x, me%y)
        ! Soil hydraulic properties contain no data where in urban areas. For the moment,
        ! until land cover properly incorporated into model, we'll use this as a proxy
        ! for urban areas (which therefore contain no soil profile). In the future, we should
        ! account for this properly by splitting grid cells into different soil profiles.
        if (me%WC_sat == nf90_fill_real) me%WC_sat = 0.8
        if (me%WC_FC == nf90_fill_real) me%WC_FC = 0.5
        if (me%K_s == nf90_fill_real) me%K_s = 1e-6
        if (me%bulkDensity == nf90_fill_real) me%bulkDensity = 1220

        me%clayContent = DATASET%soilTextureClayContent(me%x, me%y)
        me%sandContent = DATASET%soilTextureSandContent(me%x, me%y)
        me%siltContent = DATASET%soilTextureSiltContent(me%x, me%y)
        me%coarseFragContent = DATASET%soilTextureCoarseFragContent(me%x, me%y)
        if (me%coarseFragContent == nf90_fill_real) then
            me%coarseFragContent = 0.0
        end if
        ! Calculate the average grain diameter from soil texture
        me%d_grain = me%calculateAverageGrainSize(me%clayContent, me%siltContent, me%sandContent)
        me%porosity = DATASET%soilDefaultPorosity       ! TODO change to be spatial

        ! USLE params
        me%usle_C = DATASET%soilUsleCFactor(me%x, me%y)
        ! TODO make usle_C not temporal
        if ((me%usle_C(1) == nf90_fill_double) .or. (me%usle_C(1) < 0) .or. (me%usle_C(1) > 100)) then
            me%usle_C = 0.00055095          ! Pick a small value to represent urban, if there's no data
        end if
        me%usle_P = DATASET%soilUslePFactor(me%x, me%y)
        if (me%usle_P == nf90_fill_double) then
            me%usle_P = 1.0                 ! If there's no data, assume no support practice
        end if
        me%usle_LS = DATASET%soilUsleLSFactor(me%x, me%y)
        if (me%usle_LS == nf90_fill_double) then
            me%usle_LS = 0.3                 ! Pick an average value if there's no data
        end if

        ! Get earthworm density from land use. Select the maximum land use fraction and use all
        ! of profile is that.
        landUse = maxloc(DATASET%landUse(me%x, me%y, :), dim=1)
        ! TODO get these values more intelligently
        select case (landUse)
            case (1)
                me%earthwormDensity = DATASET%earthwormDensityUrbanCapped
            case (2)
                me%earthwormDensity = DATASET%earthwormDensityUrbanParks
            case (3)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
            case (4)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
            case (5)
                me%earthwormDensity = DATASET%earthwormDensityArable
            case (6)
                me%earthwormDensity = DATASET%earthwormDensityGrassland
            case (7)
                me%earthwormDensity = DATASET%earthwormDensityDeciduous
            case (8)
                me%earthwormDensity = DATASET%earthwormDensityConiferous
            case (9)
                me%earthwormDensity = DATASET%earthwormDensityHeathland
            case default
                me%earthwormDensity = 0.0_dp
        end select
    end subroutine

    !> Calculate the mean PEC across all soil layers for this soil profile
    function get_C_np_SoilProfile1(me) result(C_np)
        class(SoilProfile1) :: me                                               !! This SoilProfile instance
        real(dp)            :: C_np(C%npDim(1), C%npDim(2), C%npDim(3))         !! Mass concentration of NM [kg/kg soil]
        integer             :: i                                                ! Iterator
        real(dp)            :: C_np_l(C%nSoilLayers, C%npDim(1), C%npDim(2), C%npDim(3)) ! Per layer concentration [kg/kg soil]
        real(dp)            :: depths(C%nSoilLayers)
        ! Loop over soil layers and get NM conc and layer depth
        do i = 1, C%nSoilLayers
            C_np_l(i, :, :, :) = me%colSoilLayers(i)%item%C_np
            depths(i) = me%colSoilLayers(i)%item%depth
        end do
        ! Get the weighted average across soil layers, using the depths as the weight
        C_np = weightedAverage(C_np_l, depths)
    end function

end module

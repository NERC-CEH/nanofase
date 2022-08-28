!> Module containing the SoilProfile class definition
module SoilProfileModule
    use GlobalsModule
    use UtilModule
    use mo_netcdf
    use netcdf, only: nf90_fill_real
    use datetime_module
    use ResultModule, only: Result
    use AbstractSoilProfileModule
    use SoilLayerModule
    use DataInputModule, only: DATASET
    implicit none

    !> A SoilProfile class acts as a container for a collection of SoilLayer objects,
    !! which collectively define the layout of the SoilProfile
    type, public, extends(AbstractSoilProfile) :: SoilProfile
      contains
        procedure :: create => createSoilProfile
        procedure :: update => updateSoilProfile
        procedure :: percolate => percolateSoilProfile
        procedure :: erode => erodeSoilProfile
        procedure :: bioturbation => bioturbationSoilProfile
        procedure :: imposeSizeDistribution => imposeSizeDistributionSoilProfile
        procedure :: calculateAverageGrainSize => calculateAverageGrainSizeSoilProfile
        procedure :: calculateSizeDistribution => calculateSizeDistributionSoilProfile
        procedure :: parseInputData => parseInputDataSoilProfile
        procedure :: parseNewBatchData => parseNewBatchDataSoilProfile
        ! Getters
        procedure :: get_m_np => get_m_np_SoilProfile
        procedure :: get_m_transformed => get_m_transformed_SoilProfile
        procedure :: get_m_dissolved => get_m_dissolved_SoilProfile
        procedure :: get_C_np => get_C_np_SoilProfile
        procedure :: get_C_transformed => get_C_transformed_SoilProfile
        procedure :: get_C_dissolved => get_C_dissolved_SoilProfile
    end type

  contains
    !> Creating the SoilProfile parses input data and fills the corresponding object properties,
    !! as well as setting up the contained SoilLayers
    function createSoilProfile(me, x, y, p, n_river, area, q_precip_timeSeries, &
                               q_evap_timeSeries) result(r)
        class(SoilProfile)  :: me                           !! The `SoilProfile` instance.
        integer             :: x                            !! Containing `GridCell` x index
        integer             :: y                            !! Containing `GridCell` y index
        integer             :: p                            !! `SoilProfile` reference (redundant for now as only one `SoilProfile` per `GridCell`)
        real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
        real(dp)            :: area                         !! The surface area of the `SoilProfile` [m3]
        real, allocatable   :: q_precip_timeSeries(:)       !! Precipitation time series [m/timestep]
        real, allocatable   :: q_evap_timeSeries(:)         !! Evaporation time series [m/timestep]
        type(Result)        :: r                            !! The `Result` object
        integer             :: l                            ! Soil layer iterator
        type(SoilLayer), allocatable :: sl                  ! Temporary SoilLayer variable

        ! Generate the reference name for this SoilProfile
        me%ref = ref("SoilProfile", x, y, p)
        ! Allocate the object properties that need to be
        allocate(me%erodedSediment(C%nSizeClassesSpm), &
            me%distributionSediment(C%nSizeClassesSpm), &
            me%m_np(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_buried(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_eroded(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_np_in(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_buried(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_eroded(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%m_transformed_in(C%npDim(1), C%npDim(2), C%npDim(3)), &
            me%colSoilLayers(C%nSoilLayers))
        ! Initialise variables
        me%x = x                                            ! GridCell x index
        me%y = y                                            ! GridCell y index
        me%p = p                                            ! SoilProfile index within the GridCell
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
        
        ! Parse and store input data in this object's properties
        call r%addErrors(.errors. me%parseInputData())
        if (r%hasCriticalError()) return                    ! Return early if there are critical errors

        ! Set up the SoilLayers
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
            call move_alloc(sl, me%colSoilLayers(l)%item)
        end do
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Perform the simulation of the SoilProfile for the current time step, including
    !! percolation of soil through soil layers and soil erosion
    function updateSoilProfile(me, t, j_np_diffuseSource, j_transformed_diffuseSource, j_dissolved_diffuseSource) result(r)
        class(SoilProfile)  :: me                                   !! This `SoilProfile` instance
        integer             :: t                                    !! The current timestep
        real(dp)            :: j_np_diffuseSource(:,:,:)            !! Diffuse source of NM for this timestep [kg/m2/timestep]
        real(dp)            :: j_transformed_diffuseSource(:,:,:)   !! Diffuse source of NM for this timestep [kg/m2/timestep]
        real(dp)            :: j_dissolved_diffuseSource            !! Diffuse source of NM for this timestep [kg/m2/timestep]
        type(Result)        :: r                                    !! Result object to return

        ! Reset for this timestep
        me%V_pool = 0.0_dp

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

        else
            ! If this is an urban cell, presume no erosion
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
    function percolateSoilProfile(me, t, j_np_diffuseSource, j_transformed_diffuseSource, j_dissolved_diffuseSource) result(r)
        class(SoilProfile)  :: me                               !! This `SoilProfile` instance
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
                if (abs(me%colSoilLayers(l-i+1)%item%V_pool) > C%epsilon) then
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

    !> Calculate the soil erosion for this timestep and updates this GridCell's `erodedSediment` property.
    !! Soil erosion based on RUSLE, with R-factor derived from kinetic energy calculated by Davison method:
    !! [Davison et al. 2005](https://doi.org/10.1016/j.scitotenv.2005.02.002). Europe specific parameterisation
    !! include in a1, a2, a3 and b parameters (provided by input data). K factor based on [modified Morgan
    !! Finney](https://doi.org/10.1002/esp.1530) and in g/J. Using these units, R-factor is simply equal to
    !! kinetic energy (J/m2) and sediment yield is in g/m2.
    !! Note that here we're just calculating the total sediment yield, *not* the amount this is transported to
    !! the reaches, which is scaled by the sediment transport capacity, as calculated by reaches.
    function erodeSoilProfile(me, t) result(rslt)
        class(SoilProfile)  :: me
        integer             :: t
        type(Result)        :: rslt
        real(dp)            :: E_k
        real(dp)            :: K_MMF
        real(dp)            :: erodedSedimentTotal
        type(datetime)      :: currentDate
        integer             :: julianDay
        integer             :: i

        ! Only calculate erosion yield if we're meant to be
        if (C%includeSoilErosion) then
            ! TODO This function only works with daily timesteps

            ! Convert the current date to Julian day number (https://en.wikipedia.org/wiki/Julian_day).
            ! date2num converts to number of days since 0001-01-01, and 1721423 is the Julian day
            ! number of 0001-01-01.
            currentDate = C%startDate + timedelta(days=t-1)
            julianDay = currentDate%yearday()
            ! Then calculate the kinetic energy [J/m2/day]. Precip needs converting to [mm/day] from [m/timestep].
            E_k = (me%erosivity_a1 + me%erosivity_a2 * cos(julianDay * (2*C%pi/365) + me%erosivity_a3)) &
                    * (me%q_precip_timeSeries(t)*1.0e3)**me%erosivity_b
            ! Now the modified MMF version of K, dependent on sand, silt and clay content [g/J]
            K_MMF = 0.1*(me%clayContent/100.0_dp) + 0.3*(me%sandContent/100.0_dp) + 0.5*(me%siltContent/100.0_dp)
            ! Total eroded sediment [g/m2/day]
            erodedSedimentTotal = E_k * K_MMF * me%usle_C * me%usle_P * me%usle_LS
            ! Split this into a size distribution and convert to [kg/m2/day]
            me%erodedSediment = me%imposeSizeDistribution(erodedSedimentTotal*1.0e-3)
        else
            ! If we're not meant to be modelling erosion, then set yield to zero
            me%erodedSediment = 0.0_dp
        end if

        ! The top soil layer deals with eroding NM
        call rslt%addErrors(.errors. me%colSoilLayers(1)%item%erode(me%erodedSediment, me%bulkDensity, me%area))
        ! Remove this eroded soil from the total m_np in the profile
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
    function bioturbationSoilProfile(me) result(rslt)
        class(SoilProfile)  :: me           !! This `SoilProfile` instance
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

    !> Impose a size class distribution on a total mass to split it up into separate size classes.
    !! If no distribution has been specified for this `SoilProfile`, then a default global size
    !! distribution is used. Clay enrichment is calculated by the calculateClayEnrichment function,
    !! based on clay enrichment factors from the input data (or defaults).
    function imposeSizeDistributionSoilProfile(me, mass) result(distribution)
        class(SoilProfile)  :: me                               !! This `SoilProfile` instance
        real(dp)            :: mass                             !! The mass to split into size classes
        real(dp)            :: distribution(C%nSizeClassesSpm)  !! The resulting distribution
        distribution = mass * me%distributionSediment
    end function

    !> Re-bin the clay-silt-sand content into the binned sediment size classes used in the model
    function calculateSizeDistributionSoilProfile(me, clay, silt, sand, enrichClay) result(ssd)
        class(SoilProfile) :: me                                    !! This soil profile
        real    :: clay, silt, sand                                 !! Percentage clay, silt and sand
        logical :: enrichClay                                       !! Should we enrich the clay content of the sediment?
        real    :: ssd(C%nSizeClassesSpm)                           !! Calculated sediment size distribution
        real    :: texture(3)                                       !! Array to store clay, silt and sand content in
        real    :: clayEnrichmentRatio                              ! Clay enrichment ratio
        real    :: dClay                                            ! Change in clay content
        real    :: textureEnriched(3)                               ! Texture distribution, clay enriched
        real    :: texture_bins(3,2)                                ! Array to store texture size class bounds in
        real    :: ssd_bins(C%nSizeClassesSpm,2)                    ! Array to store sediment size class bounds in
        real    :: frac_ssd_in_texture_bin(3,C%nSizeClassesSpm)     ! Fraction of SSD bin in texture bin
        integer :: i, j                                             ! Iterators
        logical :: not_in_ssd_bin                                   ! Is this texture bin within this SSD bin?
        real    :: lower, upper                                     ! Lower and upper bounds of overlap between texture and SSD bins
        real    :: ssd_(3,C%nSizeClassesSpm)                        ! Temporary SSD array, before summing across SSD dimension
        ! Bins for texture content, based on definition of clay, silt and sand. First bins
        ! have non-zero lower bound to avoid numerical errors when logging
        texture = [clay, silt, sand] / 100.0
        if (enrichClay) then
            clayEnrichmentRatio = 0.26 + 1 / (1 - texture(3))               ! Ref: Stefano and Ferro, 2002: https://doi.org/10.1006/bioe.2001.0034
            dClay = texture(1) * clayEnrichmentRatio - texture(1)           ! Change in clay content due to enrichment
            textureEnriched = [texture(1) * clayEnrichmentRatio, texture(2) - dClay / 2, texture(3) - dClay / 2]
        else
            textureEnriched = texture
        end if
        texture_bins = log(reshape([1e-9, 0.002, 0.06, 0.002, 0.06, 2.0], [3,2]))
        ssd_bins(1,1) = log(1e-9)
        do i = 1, C%nSizeClassesSpm
            ! Set the upper bound for this bin to the diameter given in config, then set the
            ! lower bound for the next bin to the same
            ssd_bins(i,2) = log(C%d_spm(i) * 1e3)
            if (i < C%nSizeClassesSpm) then
                ssd_bins(i+1,1) = log(C%d_spm(i) * 1e3)
            end if
        end do
        ! Loop through texture bins and calculate the fraction of each SSD bin in that texture bin
        do i = 1, 3
            do j = 1, C%nSizeClassesSpm
                not_in_ssd_bin = .false.
                ! Lower overlap bound
                if (texture_bins(i,1) <= ssd_bins(j,2)) then
                    lower = max(texture_bins(i,1), ssd_bins(j,1))
                else
                    not_in_ssd_bin = .true.
                end if
                ! Upper overlap bound
                if (texture_bins(i,2) >= ssd_bins(j,1)) then
                    upper = min(texture_bins(i,2), ssd_bins(j,2))
                else
                    not_in_ssd_bin = .true.
                end if
                ! Set the fraction of SSD bin in this texture bin, based on lower and upper bounds
                if (not_in_ssd_bin) then
                    frac_ssd_in_texture_bin(i,j) = 0.0
                else
                    frac_ssd_in_texture_bin(i,j) = (upper - lower) / (texture_bins(i,2) - texture_bins(i,1))
                end if
            end do
            ssd_(i,:) = textureEnriched(i) * frac_ssd_in_texture_bin(i,:)
        end do
        ! Sum the ssd_ array into the final sediment distribution
        ssd = sum(ssd_, dim=1)
    end function

    !> Calculate the average grain size from soil texture properties, using RUSLE handbook
    !! http://www.grr.ulaval.ca/gae_3005/Documents/References/RUSLE/ah703_ch3.pdf (p76):
    !! d_grain = exp(%clay * ln(0.001) + %silt * ln(0.026) + %sand * ln(1.025))
    function calculateAverageGrainSizeSoilProfile(me, clay, silt, sand) result(d_grain)
        class(SoilProfile) :: me            !! This soil profile
        real :: clay, silt, sand            !! Percentage clay, silt and sand
        real :: d_grain                     !! The average grain size
        d_grain = 1e-3 * exp(0.01 * (clay * log(0.001) + silt * log(0.026) + sand * log(1.025)))
    end function

    !> Get the data from the input file and set object properties
    !! accordingly, including the allocation of arrays that depend on
    !! this input data
    function parseInputDataSoilProfile(me) result(r)
        class(SoilProfile)     :: me                        !! This `SoilProfile` instance
        type(Result)            :: r                        !! `Result` object to return
        integer                 :: landUse                  ! Index of max land use fraction in this profile

        me%distributionSediment = DATASET%defaultSpmSizeDistribution ! TODO we can probably get rid of this, but check
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
        me%distributionSediment = me%calculateSizeDistribution( &
            me%clayContent, &
            me%siltContent, &
            me%sandContent, &
            C%includeClayEnrichment &
        )
        me%porosity = DATASET%soilDefaultPorosity       ! TODO change to be spatial

        ! USLE params
        me%usle_C = DATASET%soilUsleCFactor(me%x, me%y)
        if (me%usle_C == nf90_fill_double) then
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
        ! of profile as that
        landUse = maxloc(DATASET%landUse(me%x, me%y, :), dim=1)
        ! TODO get these values more intelligently
        select case (landUse)
            case (1)
                me%earthwormDensity = DATASET%earthwormDensityUrbanCapped
                me%dominantLandUseName = 'urban_no_soil'
            case (2)
                me%earthwormDensity = DATASET%earthwormDensityUrbanParks
                me%dominantLandUseName = 'urban_parks_leisure'
            case (3)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
                me%dominantLandUseName = 'urban_industrial_soil'
            case (4)
                me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
                me%dominantLandUseName = 'urban_green_residential'
            case (5)
                me%earthwormDensity = DATASET%earthwormDensityArable
                me%dominantLandUseName = 'arable'
            case (6)
                me%earthwormDensity = DATASET%earthwormDensityGrassland
                me%dominantLandUseName = 'grassland'
            case (7)
                me%earthwormDensity = DATASET%earthwormDensityDeciduous
                me%dominantLandUseName = 'deciduous'
            case (8)
                me%earthwormDensity = DATASET%earthwormDensityConiferous
                me%dominantLandUseName = 'coniferous'
            case (9)
                me%earthwormDensity = DATASET%earthwormDensityHeathland
                me%dominantLandUseName = 'heathland'
            case (10)
                me%earthwormDensity = 0.0_dp
                me%dominantLandUseName = 'water'
            case (11)
                me%earthwormDensity = 0.0_dp
                me%dominantLandUseName = 'desert'
            case default
                me%earthwormDensity = 0.0_dp
                me%dominantLandUseName = 'other'
        end select

        ! Auditing
        call r%addError( &
            ERROR_HANDLER%equal( &
                value = sum(me%distributionSediment), &
                criterion = 1.0_dp, &
                message = "Grain size distribution does not sum to 100%. " &
                            // "Have you set sediment size classes correctly?" &
            ) &
        )

        me%erosivity_a1 = DATASET%soilErosivity_a1
        me%erosivity_a2 = DATASET%soilErosivity_a2
        me%erosivity_a3 = DATASET%soilErosivity_a3
        me%erosivity_b = DATASET%soilErosivity_b

        ! Add this procedure to the trace
        call r%addToTrace('Parsing input data')
    end function

    subroutine parseNewBatchDataSoilProfile(me)
        class(SoilProfile) :: me
        integer :: landUse

        ! These timeseries are passed to soil profile in create(), so we need to set again here
        deallocate(me%q_evap_timeSeries, me%q_precip_timeSeries)
        allocate(me%q_evap_timeSeries, source=DATASET%evap(me%x, me%y, :))
        allocate(me%q_precip_timeSeries, source=DATASET%precip(me%x, me%y, :))

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
        if (me%usle_C == nf90_fill_double) then
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

    !> Calculate the mean NM PEC across all soil layers for this soil profile
    function get_C_np_SoilProfile(me) result(C_np)
        class(SoilProfile)     :: me                        !! This SoilProfile instance
        real(dp), allocatable   :: C_np(:,:,:)              !! Mass concentration of NM [kg/kg soil]
        ! For some reason, ifort 18 won't compile if C_np isn't allocatable. Same for the other getter functions
        allocate(C_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        C_np = me%get_m_np() / (me%bulkDensity * me%area * sum(C%soilLayerDepth))
    end function

    !> Calculate the mean transformed NM PEC across all soil layers for this soil profile
    function get_C_transformed_SoilProfile(me) result(C_transformed)
        class(SoilProfile)     :: me                                !! This SoilProfile instance
        real(dp), allocatable   :: C_transformed(:,:,:)             !! Mass concentration of NM [kg/kg soil]
        allocate(C_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))
        C_transformed = me%get_m_transformed() / (me%bulkDensity * me%area * sum(C%soilLayerDepth))
    end function

    !> Calculate the mean dissolved species PEC across all soil layers for this soil profile
    function get_C_dissolved_SoilProfile(me) result(C_dissolved)
        class(SoilProfile) :: me                            !! This SoilProfile instance
        real(dp)            :: C_dissolved                  !! Mass concentration of dissolved species [kg/kg soil]
        C_dissolved = me%get_m_dissolved() / (me%bulkDensity * me%area * sum(C%soilLayerDepth))
    end function

    !> Get the total NM mass in the soil profile
    function get_m_np_SoilProfile(me) result(m_np)
        class(SoilProfile)     :: me                !! This SoilProfile instance
        real(dp), allocatable   :: m_np(:,:,:)      !! NM mass in the soil profile [kg]
        integer                 :: i                ! Iterator
        allocate(m_np(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_np = 0.0_dp
        ! Loop through the soil layers and sum m_np
        do i = 1, C%nSoilLayers
            m_np = m_np + me%colSoilLayers(i)%item%m_np
        end do
    end function

    !> Get the total transformed NM mass in the soil profile
    function get_m_transformed_SoilProfile(me) result(m_transformed)
        class(SoilProfile)     :: me                        !! This SoilProfile instance
        real(dp), allocatable   :: m_transformed(:,:,:)     !! Transformed NM mass in the soil profile [kg]
        integer                 :: i                        ! Iterator
        allocate(m_transformed(C%npDim(1), C%npDim(2), C%npDim(3)))
        m_transformed = 0.0_dp
        ! Loop through the soil layers and sum m_transformed
        do i = 1, C%nSoilLayers
            m_transformed = m_transformed + me%colSoilLayers(i)%item%m_transformed
        end do
    end function

    !> Get the total dissolved NM mass in the soil profile
    function get_m_dissolved_SoilProfile(me) result(m_dissolved)
        class(SoilProfile) :: me                !! This SoilProfile instance 
        real(dp)            :: m_dissolved      !! Dissolved NM mass in the soil profile [kg]
        integer             :: i                ! Iterator
        m_dissolved = 0.0_dp
        ! Loop through the soil layers and sum m_dissolved
        do i = 1, C%nSoilLayers
            m_dissolved = m_dissolved + me%colSoilLayers(i)%item%m_dissolved
        end do
    end function
end module

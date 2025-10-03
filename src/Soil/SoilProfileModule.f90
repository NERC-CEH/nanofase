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
    use ContaminantModule
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
        procedure :: get_m_contaminant => get_m_contaminant_SoilProfile
        procedure :: get_C_contaminant => get_C_contaminant_SoilProfile
    end type

contains

    !> Creating the SoilProfile parses input data and fills the corresponding object properties,
    !! as well as setting up the contained SoilLayers
    function createSoilProfile(me, x, y, p, n_river, area, q_precip_timeSeries, &
                            q_evap_timeSeries) result(r)
        class(SoilProfile)  :: me                           !! The `SoilProfile` instance.
        integer             :: x                            !! Containing `GridCell` x index
        integer             :: y                            !! Containing `GridCell` y index
        integer             :: p                            !! `SoilProfile` reference
        real(dp)            :: n_river                      !! Manning's roughness coefficient for the `GridCell`'s rivers [-]
        real(dp)            :: area                         !! The surface area of the `SoilProfile` [m2]
        real, allocatable   :: q_precip_timeSeries(:)       !! Precipitation time series [m/timestep]
        real, allocatable   :: q_evap_timeSeries(:)         !! Evaporation time series [m/timestep]
        type(Result)        :: r                            !! The `Result` object
        integer             :: l                            ! Soil layer iterator
        type(SoilLayer), allocatable :: sl                  ! Temporary SoilLayer variable
        real                :: T_water_t                    ! Water temperature for initialization [deg C]
        type(datetime)      :: currentDate                  ! Current date for water temperature
        integer             :: allocStat                    ! Allocation status

        ! Generate the reference name for this SoilProfile
        me%ref = ref("SoilProfile", x, y, p)
        ! Allocate the object properties that need to be
        allocate(me%erodedSediment(C%nSizeClassesSpm), &
                me%distributionSediment(C%nSizeClassesSpm), &
                me%colSoilLayers(C%nSoilLayers), &
                stat=allocStat)
        if (allocStat /= 0) then
            call r%addError(ErrorInstance(code=901, message="Failed to allocate arrays"))
            return
        end if
        ! Initialise variables
        me%x = x
        me%y = y
        me%p = p
        me%n_river = n_river
        me%area = area
        allocate(me%q_precip_timeSeries, source=q_precip_timeSeries)
        allocate(me%q_evap_timeSeries, source=q_evap_timeSeries)
        me%V_buried = 0.0_dp

        ! Initialize Contaminant objects
        r = me%m_contaminant%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if
        r = me%m_contaminant_in%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if
        r = me%m_contaminant_buried%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if
        r = me%m_contaminant_eroded%create()
        if (r%hasCriticalError()) then
            call ERROR_HANDLER%trigger(errors=.errors.r)
            return
        end if

        ! Parse and store input data in this object's properties
        call r%addErrors(.errors. me%parseInputData())
        if (r%hasCriticalError()) return

        ! Set up the SoilLayers
        do l = 1, C%nSoilLayers
            allocate(sl)
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
                    me%earthwormDensity * DATASET%earthwormVerticalDistribution(l) &
                ))
            call move_alloc(sl, me%colSoilLayers(l)%item)
        end do
        call r%addToTrace("Creating " // trim(me%ref))
    end function

    !> Perform the simulation of the SoilProfile for the current time step, including
    !! percolation of soil through soil layers and soil erosion
    function updateSoilProfile(me, t, j_contaminant_diffuseSource) result(r)
        class(SoilProfile), intent(inout) :: me
        integer, intent(in) :: t
        type(Contaminant), intent(in) :: j_contaminant_diffuseSource
        type(Result) :: r
        integer :: l
        type(datetime) :: currentDate
        real(dp) :: T_water_t

        ! Reset for this timestep
        me%V_pool = 0.0_dp

        if (.not. me%isUrban) then
            ! Set timestep-specific properties
            me%q_precip = me%q_precip_timeSeries(t)
            me%q_evap   = me%q_evap_timeSeries(t)
            me%q_in     = max(me%q_precip - me%q_evap, 0.0_dp)

            ! --- NEW ORDER OF OPERATIONS ---
            ! 1. Perform in-soil transformations (e.g., attachment) BEFORE erosion
            currentDate = C%startDate + timedelta(t-1)
            T_water_t   = DATASET%waterTemperature(currentDate%yearday())
            do l = 1, C%nSoilLayers
                call me%colSoilLayers(l)%item%update_contaminant_state(T_water_t)
            end do

            ! 2. Now perform erosion, percolation (with diffuse source), and bioturbation
            call r%addErrors([.errors. me%erode(t), &
                              .errors. me%percolate(t, j_contaminant_diffuseSource), &
                              .errors. me%bioturbation()])

            ! 3. Update total mass in profile by removing buried mass
            call me%m_contaminant%add_scaled(me%m_contaminant_buried, -1.0_dp)
        else
            me%erodedSediment = 0.0_dp
        end if

        call r%addToTrace("Updating " // trim(me%ref) // " on timestep #" // trim(str(t)))
    end function updateSoilProfile

    !> Percolate water through the `SoilProfile`, by looping through `SoilLayer`s
    !! and running their individual percolation procedures, and then passing
    !! percolated and pooled flows between `SoilLayer`s. Pooled water from top
    !! `SoilLayer` forms surface runoff, and "lost" water from bottom `SoilLayer`
    !! is kept track of in `me%V_buried`
    function percolateSoilProfile(me, t, j_contaminant_diffuseSource) result(r)
        class(SoilProfile)  :: me                                  !! This `SoilProfile` instance
        integer             :: t                                   !! The current time step
        type(Contaminant), intent(in) :: j_contaminant_diffuseSource
        type(Result)        :: r                                   !! The `Result` object to return
        integer             :: l, i                                ! Loop iterator for SoilLayers
        real(dp)            :: q_l_in                              ! Temporary water inflow for a particular SoilLayer
        type(Contaminant)   :: j_contaminant_l_in

        ! Loop through SoilLayers and percolate 
        do l = 1, C%nSoilLayers
            if (l == 1) then
                 ! If it's the first SoilLayer, water and contaminant inflow will be from precip - ET
                 ! and the diffuse source, respectively
                q_l_in = me%q_in                                    ! [m3/m2/timestep]
                call j_contaminant_l_in%multiply_scalar(j_contaminant_diffuseSource, me%area)
            else
                ! Otherwise, they'll be from the layer above
                q_l_in = me%colSoilLayers(l-1)%item%V_perc
                j_contaminant_l_in = me%colSoilLayers(l-1)%item%j_contaminant_perc
            end if

            ! Run the percolation simulation for individual layer, setting V_perc, V_pool, m_contaminant_perc etc.
            call r%addErrors(.errors. me%colSoilLayers(l)%item%update(t, q_l_in, j_contaminant_l_in))

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

        ! Keep track of "lost" Contaminant and water from the bottom soil layer. Not cumulative.
         me%V_buried = me%colSoilLayers(C%nSoilLayers)%item%V_perc
        me%m_contaminant_buried = me%colSoilLayers(C%nSoilLayers)%item%j_contaminant_perc

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
        integer             :: n, f
        type(Result)        :: r

        ! Only calculate erosion yield if we're meant to be
        if (C%includeSoilErosion) then
            ! TODO This function only works with daily timesteps

            ! Convert the current date to Julian day number
            currentDate = C%startDate + timedelta(days=t-1)
            julianDay = currentDate%yearday()
            ! Calculate the kinetic energy [J/m2/day]. Precip needs converting to [mm/day] from [m/timestep].
            E_k = (me%erosivity_a1 + me%erosivity_a2 * cos(julianDay * (2*C%pi/365) + me%erosivity_a3)) &
                    * (me%q_precip_timeSeries(t)*1.0e3)**me%erosivity_b
            ! Modified MMF version of K, dependent on sand, silt and clay content [g/J]
            K_MMF = 0.1*(me%clayContent/100.0_dp) + 0.3*(me%sandContent/100.0_dp) + 0.5*(me%siltContent/100.0_dp)
            ! Total eroded sediment [g/m2/day]
            erodedSedimentTotal = E_k * K_MMF * me%usle_C * me%usle_P * me%usle_LS
            ! Split this into a size distribution and convert to [kg/m2/day]
            me%erodedSediment = me%imposeSizeDistribution(erodedSedimentTotal*1.0e-3)
            ! Call SoilLayer%erode with correct arguments
            call rslt%addErrors(.errors. me%colSoilLayers(1)%item%erode( &
                me%erodedSediment, me%bulkDensity, me%area))
            ! Transition attached to heteroaggregated states
            do n = 1, C%contaminantDim(1)
                do f = 1, C%contaminantDim(2)
                    me%m_contaminant_eroded%c(n,f,SPM_CONTAMINANT_START:) = &
                        me%imposeSizeDistribution(me%m_contaminant_eroded%c(n,f,ATTACHED_CONTAMINANT))
                    me%m_contaminant_eroded%c(n,f,ATTACHED_CONTAMINANT) = 0.0_dp
                end do
            end do
            call me%m_contaminant%add_scaled(me%m_contaminant_eroded, -1.0_dp)
        else
            ! If not modelling erosion, set yield to zero
            me%erodedSediment = 0.0_dp
            r = me%m_contaminant_eroded%create()
            if (r%hasCriticalError()) then
                call ERROR_HANDLER%trigger(errors=.errors.r)
                call rslt%addErrors(.errors.r)
                return
            end if
            me%m_contaminant_eroded%c = 0.0_dp
            me%m_contaminant_eroded%m_dissolved = 0.0_dp
        end if
        call rslt%addToTrace("Eroding soil on time step #" // trim(str(t)))
    end function

    !> Perform bioturbation on a time step by mixing calculated depth of two layers together
    function bioturbationSoilProfile(me) result(rslt)
        class(SoilProfile)  :: me           !! This `SoilProfile` instance
        type(Result)        :: rslt         !! The `Result` object to return
        integer             :: i, j, k      ! Iterator
        real(dp)            :: fractionOfLayerToMix
        type(Contaminant)   :: temp         ! Temporary Contaminant object
        type(Result)        :: r            ! Result object for error handling
        ! Only model bioturbation if config file has asked us to
        if (C%includeBioturbation) then
            ! Initialize temp Contaminant object
            r = temp%create()
            if (r%hasCriticalError()) then
                call ERROR_HANDLER%trigger(errors=.errors.r)
                call rslt%addErrors(.errors.r)
                return
            end if
            ! Perform bioturbation for each layer, except final layer
            do i = 1, C%nSoilLayers - 1
                fractionOfLayerToMix = me%colSoilLayers(i)%item%calculateBioturbationRate() * C%timeStep
                ! Direct state mixing (no separate method needed)
                associate (upper => me%colSoilLayers(i)%item%m_contaminant, &
                        lower => me%colSoilLayers(i+1)%item%m_contaminant)
                    temp = upper * fractionOfLayerToMix
                    call upper%add(-temp)
                    call lower%add(temp)
                    temp = lower * fractionOfLayerToMix
                    call lower%add(-temp)
                    call upper%add(temp)
                end associate
            end do
        end if
        call rslt%addToTrace("Performing bioturbation on " // trim(me%ref))
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
        real(dp):: ssd_bins(C%nSizeClassesSpm,2)                    ! Array to store sediment size class bounds in
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
        class(SoilProfile)     :: me
        type(Result)           :: r
        integer                :: landUse
        logical                :: haveSoil2D, haveLU3D
        integer                :: nx, ny, nlux, nluy, nluc

        ! Defensive checks on dataset shapes before indexing
        haveSoil2D = .false.
        if (allocated(DATASET%soilBulkDensity)) then
            nx = size(DATASET%soilBulkDensity, 1)  ! y
            ny = size(DATASET%soilBulkDensity, 2)  ! x
            if (nx > 0 .and. ny > 0 .and. me%x >= 1 .and. me%y >= 1 &
                .and. me%y <= nx .and. me%x <= ny) haveSoil2D = .true.
        end if

        haveLU3D = .false.
        if (allocated(DATASET%landUse)) then
            nlux = size(DATASET%landUse, 1)  ! categories
            nluy = size(DATASET%landUse, 2)  ! y
            nluc = size(DATASET%landUse, 3)  ! x
            if (nlux > 0 .and. nluy > 0 .and. nluc > 0 .and. &
                me%y >= 1 .and. me%x >= 1 .and. me%y <= nluy .and. me%x <= nluc) haveLU3D = .true.
        end if

        ! Base SPM distribution (kept even in fallback mode)
        me%distributionSediment = DATASET%defaultSpmSizeDistribution

        if (haveSoil2D) then
            me%bulkDensity = DATASET%soilBulkDensity(me%y, me%x)
            me%WC_sat      = DATASET%soilWaterContentSaturation(me%y, me%x)
            me%WC_FC       = DATASET%soilWaterContentFieldCapacity(me%y, me%x)
            me%K_s         = DATASET%soilHydraulicConductivity(me%y, me%x)

            if (me%WC_sat      == nf90_fill_real)    me%WC_sat      = 0.8
            if (me%WC_FC       == nf90_fill_real)    me%WC_FC       = 0.5
            if (me%K_s         == nf90_fill_real)    me%K_s         = 1e-6
            if (me%bulkDensity == nf90_fill_real)    me%bulkDensity = 1220.0

            me%clayContent       = DATASET%soilTextureClayContent(me%y, me%x)
            me%sandContent       = DATASET%soilTextureSandContent(me%y, me%x)
            me%siltContent       = DATASET%soilTextureSiltContent(me%y, me%x)
            me%coarseFragContent = DATASET%soilTextureCoarseFragContent(me%y, me%x)
            if (abs(100.0 - me%clayContent - me%sandContent - me%siltContent) > 0.1) then
                me%clayContent = 18.0; me%sandContent = 46.0; me%siltContent = 36.0
            end if
            if (me%coarseFragContent == nf90_fill_real) me%coarseFragContent = 0.0

            me%d_grain  = me%calculateAverageGrainSize(me%clayContent, me%siltContent, me%sandContent)
            me%porosity = DATASET%soilDefaultPorosity

            me%usle_C  = DATASET%soilUsleCFactor(me%y, me%x);  if (me%usle_C  == nf90_fill_double) me%usle_C  = 0.00055095
            me%usle_P  = DATASET%soilUslePFactor(me%y, me%x);  if (me%usle_P  == nf90_fill_double) me%usle_P  = 1.0
            me%usle_LS = DATASET%soilUsleLSFactor(me%y, me%x); if (me%usle_LS == nf90_fill_double) me%usle_LS = 0.3

            if (haveLU3D) then
                landUse = maxloc(DATASET%landUse(:, me%y, me%x), dim=1)  ! FIX: category along dim 1
            else
                landUse = 5
            end if

            select case (landUse)
                case (1)
                    me%earthwormDensity   = DATASET%earthwormDensityUrbanCapped
                    me%dominantLandUseName= 'urban_no_soil'
                case (2)
                    me%earthwormDensity   = DATASET%earthwormDensityUrbanParks
                    me%dominantLandUseName= 'urban_parks_leisure'
                case (3)
                    me%earthwormDensity   = DATASET%earthwormDensityUrbanGardens
                    me%dominantLandUseName= 'urban_industrial_soil'
                case (4)
                    me%earthwormDensity   = DATASET%earthwormDensityUrbanGardens
                    me%dominantLandUseName= 'urban_green_residential'
                case (5)
                    me%earthwormDensity   = DATASET%earthwormDensityArable
                    me%dominantLandUseName= 'arable'
                case (6)
                    me%earthwormDensity   = DATASET%earthwormDensityGrassland
                    me%dominantLandUseName= 'grassland'
                case (7)
                    me%earthwormDensity   = DATASET%earthwormDensityDeciduous
                    me%dominantLandUseName= 'deciduous'
                case (8)
                    me%earthwormDensity   = DATASET%earthwormDensityConiferous
                    me%dominantLandUseName= 'coniferous'
                case (9)
                    me%earthwormDensity   = DATASET%earthwormDensityHeathland
                    me%dominantLandUseName= 'heathland'
                case (10)
                    me%earthwormDensity   = 0.0_dp
                    me%dominantLandUseName= 'water'
                case (11)
                    me%earthwormDensity   = 0.0_dp
                    me%dominantLandUseName= 'desert'
                case default
                    me%earthwormDensity   = 0.0_dp
                    me%dominantLandUseName= 'other'
            end select

            me%isUrban = (me%dominantLandUseName == 'urban_no_soil')

        else
            !--- Fallback path: no soil grids -> treat as water/urban-no-soil; use safe defaults ---
            me%bulkDensity = 1220.0_dp
            me%WC_sat      = 0.8_dp
            me%WC_FC       = 0.5_dp
            me%K_s         = 1.0e-6_dp

            me%clayContent       = 18.0
            me%sandContent       = 46.0
            me%siltContent       = 36.0
            me%coarseFragContent = 0.0
            me%d_grain = me%calculateAverageGrainSize(me%clayContent, me%siltContent, me%sandContent)
            me%distributionSediment = me%calculateSizeDistribution( &
                me%clayContent, me%siltContent, me%sandContent, C%includeClayEnrichment )

            me%porosity = DATASET%soilDefaultPorosity
            me%usle_C   = 0.00055095_dp
            me%usle_P   = 1.0_dp
            me%usle_LS  = 0.3_dp

            me%earthwormDensity    = 0.0_dp
            me%dominantLandUseName = 'water'
            me%isUrban             = .true.
        end if

        ! Auditing
        call r%addError( &
            ERROR_HANDLER%equal( value=sum(me%distributionSediment), criterion=1.0_dp, epsilon=1e-3, &
            message="Grain size distribution does not sum to 1 (100%). Have you set sediment size classes correctly?" ) )

        me%erosivity_a1 = DATASET%soilErosivity_a1
        me%erosivity_a2 = DATASET%soilErosivity_a2
        me%erosivity_a3 = DATASET%soilErosivity_a3
        me%erosivity_b  = DATASET%soilErosivity_b

        call r%addToTrace('Parsing input data (soil profile)')
    end function

    subroutine parseNewBatchDataSoilProfile(me)
        class(SoilProfile) :: me
        integer            :: landUse
        logical            :: haveSoil2D, haveLU3D
        integer            :: nx, ny, nlux, nluy, nluc

        ! Refresh time series
        deallocate(me%q_evap_timeSeries, me%q_precip_timeSeries)
        allocate(me%q_evap_timeSeries,   source=DATASET%evap(me%x, me%y, :))
        allocate(me%q_precip_timeSeries, source=DATASET%precip(me%x, me%y, :))

        ! Check availability of spatial layers
        haveSoil2D = .false.
        if (allocated(DATASET%soilBulkDensity)) then
            nx = size(DATASET%soilBulkDensity, 1)
            ny = size(DATASET%soilBulkDensity, 2)
            if (nx > 0 .and. ny > 0 .and. me%x >= 1 .and. me%y >= 1 &
                .and. me%x <= nx .and. me%y <= ny) haveSoil2D = .true.
        end if

        haveLU3D = .false.
        if (allocated(DATASET%landUse)) then
            nlux = size(DATASET%landUse, 1)
            nluy = size(DATASET%landUse, 2)
            nluc = size(DATASET%landUse, 3)
            if (nlux > 0 .and. nluy > 0 .and. nluc > 0 .and. &
                me%y >= 1 .and. me%x >= 1 .and. me%y <= nluy .and. me%x <= nluc) haveLU3D = .true.
        end if

        if (haveSoil2D) then
            me%bulkDensity = DATASET%soilBulkDensity(me%x, me%y)
            me%WC_sat      = DATASET%soilWaterContentSaturation(me%x, me%y)
            me%WC_FC       = DATASET%soilWaterContentFieldCapacity(me%x, me%y)
            me%K_s         = DATASET%soilHydraulicConductivity(me%x, me%y)
            if (me%WC_sat      == nf90_fill_real)    me%WC_sat      = 0.8
            if (me%WC_FC       == nf90_fill_real)    me%WC_FC       = 0.5
            if (me%K_s         == nf90_fill_real)    me%K_s         = 1e-6
            if (me%bulkDensity == nf90_fill_real)    me%bulkDensity = 1220.0

            me%clayContent       = DATASET%soilTextureClayContent(me%x, me%y)
            me%sandContent       = DATASET%soilTextureSandContent(me%x, me%y)
            me%siltContent       = DATASET%soilTextureSiltContent(me%x, me%y)
            me%coarseFragContent = DATASET%soilTextureCoarseFragContent(me%x, me%y)
            if (abs(100.0 - me%clayContent - me%sandContent - me%siltContent) > 0.1) then
                me%clayContent = 18.0
                me%sandContent = 46.0
                me%siltContent = 36.0
            end if
            if (me%coarseFragContent == nf90_fill_real) me%coarseFragContent = 0.0

            me%d_grain  = me%calculateAverageGrainSize(me%clayContent, me%siltContent, me%sandContent)
            me%porosity = DATASET%soilDefaultPorosity

            me%usle_C  = DATASET%soilUsleCFactor(me%x, me%y);  if (me%usle_C  == nf90_fill_double) me%usle_C  = 0.00055095
            me%usle_P  = DATASET%soilUslePFactor(me%x, me%y);  if (me%usle_P  == nf90_fill_double) me%usle_P  = 1.0
            me%usle_LS = DATASET%soilUsleLSFactor(me%x, me%y); if (me%usle_LS == nf90_fill_double) me%usle_LS = 0.3

            if (haveLU3D) then
                landUse = maxloc(DATASET%landUse(:, me%y, me%x), dim=1)
            else
                landUse = 5
            end if
            select case (landUse)
                case (1);  me%earthwormDensity = DATASET%earthwormDensityUrbanCapped
                case (2);  me%earthwormDensity = DATASET%earthwormDensityUrbanParks
                case (3);  me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
                case (4);  me%earthwormDensity = DATASET%earthwormDensityUrbanGardens
                case (5);  me%earthwormDensity = DATASET%earthwormDensityArable
                case (6);  me%earthwormDensity = DATASET%earthwormDensityGrassland
                case (7);  me%earthwormDensity = DATASET%earthwormDensityDeciduous
                case (8);  me%earthwormDensity = DATASET%earthwormDensityConiferous
                case (9);  me%earthwormDensity = DATASET%earthwormDensityHeathland
                case default
                    me%earthwormDensity = 0.0_dp
            end select
            me%isUrban = (landUse == 1)

        else
            ! No soil grids in this batch: keep model stable with defaults
            me%bulkDensity = 1220.0_dp
            me%WC_sat      = 0.8_dp
            me%WC_FC       = 0.5_dp
            me%K_s         = 1.0e-6_dp
            me%d_grain     = me%calculateAverageGrainSize(18.0, 36.0, 46.0)
            me%porosity    = DATASET%soilDefaultPorosity
            me%usle_C      = 0.00055095_dp
            me%usle_P      = 1.0_dp
            me%usle_LS     = 0.3_dp
            me%earthwormDensity = 0.0_dp
            me%isUrban     = .true.
        end if
    end subroutine


    function get_m_contaminant_SoilProfile(me) result(m_contaminant)
        class(SoilProfile) :: me
        type(Contaminant) :: m_contaminant
        type(Result) :: r
        integer :: i
        r = m_contaminant%create()
        do i = 1, C%nSoilLayers
            call m_contaminant%add(me%colSoilLayers(i)%item%m_contaminant)
        end do
    end function

    ! Return 3-D concentration array for the whole profile (same shape as Contaminant%c)
    function get_C_contaminant_SoilProfile(me) result(C_contaminant)
        class(SoilProfile) :: me ! CORRECTED: Removed intent(in)
        real(dp), allocatable             :: C_contaminant(:,:,:)
        type(Contaminant)                 :: mtot
        real(dp)                          :: V_profile
        integer                           :: l

        ! total contaminant mass across all layers (same shape as %c)
        mtot = me%get_m_contaminant()

        ! total profile volume = sum of layer volumes
        V_profile = 0.0_dp
        do l = 1, C%nSoilLayers
            V_profile = V_profile + me%colSoilLayers(l)%item%volume
        end do

        allocate(C_contaminant(size(mtot%c,1), size(mtot%c,2), size(mtot%c,3)))
        if (V_profile > C%epsilon) then
            C_contaminant = mtot%c / V_profile
        else
            C_contaminant = 0.0_dp
        end if
    end function get_C_contaminant_SoilProfile
end module

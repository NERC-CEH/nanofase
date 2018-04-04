# Data requirements

List of the parameters required in the input data, along with any defaults. See [here](/doc/config.md) for config file options required. **Required** fields throw an error when they're not included. **Recommended** fields default to something else but this default should be used with caution.

The [convention](/doc/conventions.md) is that input data is parsed in an object's `parseInputData()` method. Format of the below list is `input_data_var_name[dimensions]`&#129146; `internalModelVarName`. If no `internalModelVarName` specified, it is the same of `input_data_var_name`. Required dimensions are given below the data. See [conventions](/doc/conventions.md) for variable naming conventions.

*Note: SubRiver will eventually be removed and GridCells will directly contain RiverReaches, modifying this input file structure somewhat.*

- **`global`**
    + `spm_size_classes[s]` &#129146; `spmSizeClasses`: Sizes of the binned SPM size classes.
    + `np_size_classes[n]` &#129146; `npSizeClasses`: Sizes of the binned NP size classes.
    + `spm_particle_densities[f]` &#129146; `spmParticleDensities`: Densities of the different SPM compositional fractions.
    + `temperature[t]` &#129146; `T`: Temperature to be used if none supplied in spatially-explicit data.
- **`Environment`**
    + **`GridCell_{x}_{y}`**
        * `size[d]` &#129146; `dx` and `dy`: *Defaults to default_grid_size from [config.nml](/doc/config.md).* GridCell x and y size.
        * `type` &#129146; *no internal variable - type dictates the class to be instantiated for the GridCell object*: Integer representing the "type" of GridCell to be simulated. Currently only one type available. Different types will encompass different algorithms and procedures [-].
        * `n_river_reaches` &#129146; `nRiverReaches`: *Defaults to 0.* Number of RiverReaches contained within this reach [-]. 
        * `reach_types[r]` &#129146; *not currently implemented in model*: Integer array representing the "type" of RiverReaches to be created within this GridCell. Different tpyes will encompass different algorithsm and procedures [-].
        * `runoff[t]` &#129146; `Q_runoff_timeSeries`: *Defaults to 0.* Time series of runoff flows [m/s in data file, converted to m/timestep in model].
        * `precip[t]` &#129146; `Q_precip_timeSeries`: *Defaults to 0.* Time series of precipitation data [m/s in data file, converted to m/timestep in model].
        * `evap[t]` &#129146; `Q_evap_timeSeries`: *Defaults to 0.* Time series of evaporation data [m/s in data file, converted to m/timestep in model].
        * `quickflow[t]` &#129146; `Q_quickflow_timeSeries`: *Defaults to 0.* Time series for quickflow data from hydrology model [m/s in data file, converted to m/timestep in model].
        * `slope`: **Required**. Average slope of the GridCell [m/m].
        * `n_river`: **Recommended**, *defaults to 0.035*. Manning's coefficient for the RiverReaches in the GridCell. Defaults to that for [natural streams and major rivers](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).
        * `T_water[t]` &#129146; `T_water_timeSeries`: **Recommended**, *defaults to default_water_temperature from [config.nml](/doc/config.md)*. Time series of water temperatures in this GridCell [<sup>o</sup>C].
        * **`SoilProfile_{x}_{y}_{p}`**
            - `n_soil_layers` &#129146; `nSoilLayers`: **Required**. Number of SoilLayers in the SoilProfile [-].
            - `WC_sat`: **Required**. Water content at saturation, fraction between 0 and 1 [m3/m3].
            - `WC_FC`: **Required**. Water content at field capacity, fraction between 0 and 1 [m3/m3].
            - `K_s`: **Required**. Saturated hydraulic conductivity [m/s].
            - `distribution_sediment[s]` &#129146; `distributionSediment`: Disribution used to split sediment mass into size classes [-].
            - `rusle2015_eroded_sediment[t]` &#129146; `rusle2015_erodedSediment` : RUSLE2015 data for eroded sediment. Used only for comparison.
            - `usle_C[t]` : Time series of USLE C-factors [-].
            - `usle_C_min[t]` : If `usle_C` not present, estimate C-factor from minimum C. Defaults to 1 if it and `usle_C_av` not present.
            - `usle_C_av[t]` : If `usle_C_min` not present, estimate it from average annual C. If not present, `usle_C_min` defaults to 1.
            - `usle_rsd[t]` : *Defaults to 0*. Residue on surface [kg/ha].
            - `usle_K` : **Required**. Soil erodibility factor [t ha h ha-1 MJ-1 mm-1].
            - `usle_P` : *Defaults to 1*. Support practice factor [-].
            - `usle_LS` : **Required**. Topographic factor [-].
            - `usle_rock` : *Defaults to 0*. % rock in top of soil profile.
            - `usle_alpha_half` : **Recommended**, *defaults to 0.33*. Fraction of rainfall falling during half-hour maximum [-].
            - `usle_area_hru` : **Required**. Area of the HRU corresponding to the GridCell [ha].
            - `usle_area_sb` : *Defaults to usle_area_hru*. Area of the subbasin corresponding to the GridCell [km2].
            - `usle_L_sb` : **Recommended**, *defaults to 50 m*. Hillslope length for the subbasin [m].
            - `usle_n_sb` : **Recommended**, *defaults to 0.01*. Manning's coefficient for the subbasin. Defaults to that for fallow land with no residue ([SWAT Documentation, pp. 111](http://swat.tamu.edu/media/99192/swat2009-theory.pdf)).
            - `usle_slp_sb` : **Recommended**, *defaults to GridCell slope*. Slope of the subbasin. [m]
            - `usle_slp_ch` : **Recommended**, *defaults to GridCell slope*. Slope of the channel. [km]
            - `usle_L_ch` : **Required**. The length of the hillslope channel [km].
            - **`SoilLayer_{l}`**
                + `depth`: *Defaults to [`config%defaultSoilLayerDepth`](/doc/config.md)*. Depth of the soil layer [m].
        * **`RiverReach_{x}_{y}_{r}`**
            - `alpha_hetero`: *Defaults to [`config%default_alpha_hetero`](/doc/config.md)*. Attachment efficiency for modelled NM [-].
            - `alpha_res`: **Required**. Calibration factor for maximum resuspendable particle size [-]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>7</sub>.
            - `beta_res`: **Required**. Calibration factor for resuspension [s2/kg]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>8</sub>.
            + `f_m`: *Defaults to 1 (no meandering)*. Meandering factor such that actual river length = linear river length * f_m [-].
            + `slope`: **Required**. Slope of the RiverReach [m/m]. *TODO: Should default to the GridCell slope, if not present.*
            + `inflows[in][riverReachRef]`: *Defaults to no inflows*. An array of references to RiverReaches that inflow to this RiverReach, in the form [x, y, r] (e.g. RiverReach_1_2_3 is specified by [1, 2, 3]).

        * **`SubRiver_{x}_{y}_{s}`**
            - `nInflows` : **Required**. Number of inflow SubRivers.
            - `reachTypes` : **Required**. Type of RiverReaches contained in the SubRiver.
            - **`inflow_{i}`**
                + `gridX`: **Required**. GridCell x reference of inflow to this SubRiver.
                + `gridY`: **Required**. GridCell y reference of inflow to this SubRiver.
                + `subRiver`: **Required**. SubRiver reference of inflow to this SubRiver.
            - **`RiverReach_{x}_{y}_{s}_{r}`**
                + `slope` &#129146; `S`: **Required**. Slope of the RiverReach [m/m].
                
                + `alpha_res`: **Required**. Calibration factor for maximum resuspendable particle size [-]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>7</sub>.
                + `beta_res`: **Required**. Calibration factor for resuspension [s2/kg]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>8</sub>.
                + `spm(nTimeSteps,nSizeClassesSpm)` &#129146; `j_spm_runoff_timeSeries(nTimeSteps,nSizeClassesSpm)` : *Soon to be deprecated and obtained from SoilProfile calculations instead*. SPM runoff inflow to the reach [kg/s in data, kg/timestep in model].



    ## Dimensions
# Data requirements

List of the parameters required in the input data, along with any defaults. See [here](/doc/config.md) for config file options required. **Required** fields throw an error when they're not included. **Recommended** fields default to something else but this default should be used with caution.

The [convention](/doc/conventions.md) is that input data is parsed in an object's `parseInputData()` method (not currently implemented in all classes). Format of the below list is `input_data_var_name(dimensions)`&#129146; `internal_model_var_name`. If no `internal_model_var_name` specified, it is the same of `input_data_var_name`.

*Note: SubRiver will eventually be removed and GridCells will directly contain RiverReaches, modifying this input file structure somewhat.*

- **`Environment`**
    + **`GridCell_{x}_{y}`**
        * `runoff(nTimeSteps)` &#129146; `Q_runoff_timeSeries`: *Defaults to 0.* Time series of runoff flows [m3/s - will soon be changed to m/s].
        * `precip(nTimeSteps)` &#129146; `Q_precip_timeSeries`: *Defaults to 0.* Time series of precipitation data [m/s].
        * `evap(nTimeSteps)` &#129146; `Q_evap_timeSeries`: *Defaults to 0.*Time series of evaporation data [m/s].
        * `slope`: **Required**. Average slope of the GridCell [m/m].
        * `nSubRivers`: **Required**. Number of SubRivers in GridCell
        * `n_river`: **Recommended**, *defaults to 0.035*. Manning's coefficient for the RiverReaches in the GridCell. Defaults to that for [natural streams and major rivers](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).
        * **`SoilProfile_{x}_{y}_{p}`**
            - `nSoilLayers`: **Required**. Number of SoilLayers in the SoilProfile [-].
            - `WC_sat`: **Required**. Water content at saturation, fraction between 0 and 1 [m3/m3].
            - `WC_FC`: **Required**. Water content at field capacity, fraction between 0 and 1 [m3/m3].
            - `K_s`: **Required**. Saturated hydraulic conductivity [m/s].
            - `distributionSediment`: Disribution used to split sediment mass into size classes [-].
            - `rusle2015_erodedSediment(nTimeSteps)` : RUSLE2015 data for eroded sediment. Used only for comparison.
            - `usle_C(nTimeSteps)` : Time series of USLE C-factors [-].
            - `usle_C_min(nTimeSteps)` : If `usle_C` not present, estimate C-factor from minimum C. Defaults to 1 if it and `usle_C_av` not present.
            - `usle_C_av(nTimeSteps)` : If `usle_C_min` not present, estimate it from average annual C. If not present, `usle_C_min` defaults to 1.
            - `usle_rsd(nTimeSteps)` : *Defaults to 0*. Residue on surface [kg/ha].
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
                + `depth`: *Defaults to `config%defaultSoilLayerDepth`*. Depth of the soil layer [m].
        * **`SubRiver_{x}_{y}_{s}`**
            - `nInflows` : **Required**. Number of inflow SubRivers.
            - `reachTypes` : **Required**. Type of RiverReaches contained in the SubRiver.
            - **`inflow_{i}`**
                + `gridX`: **Required**. GridCell x reference of inflow to this SubRiver.
                + `gridY`: **Required**. GridCell y reference of inflow to this SubRiver.
                + `subRiver`: **Required**. SubRiver reference of inflow to this SubRiver.
            - **`RiverReach_{x}_{y}_{s}_{r}`**
                + `slope` &#129146; `S`: **Required**. Slope of the RiverReach [m/m].
                + `f_m`: *Defaults to 1 (no meandering)*. Meandering factor such that actual river length = linear river length * f_m [-].
                + `alpha_res`: **Required**. Calibration factor for maximum resuspendable particle size [-]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>7</sub>.
                + `beta_res`: **Required**. Calibration factor for resuspension [s2/kg]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>8</sub>.
                + `spm(nTimeSteps,nSizeClassesSpm)` &#129146; `j_spm_runoff_timeSeries(nTimeSteps,nSizeClassesSpm)` : *Soon to be deprecated*. SPM runoff inflow to the reach [kg/s in data, kg/timestep in model].
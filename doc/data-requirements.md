# Data requirements

List of the parameters required in the input data, along with any defaults. See [here](/doc/config.md) for config file options required. **Required** fields throw an error when they're not included. **Recommended** fields default to something else but this default should be used with caution.

The [convention](/doc/conventions.md) is that input data is parsed in an object's `parseInputData()` method. Format of the below list is `input_data_var_name[dimensions]`&#129146; `internalModelVarName`. If no `internalModelVarName` specified, it is the same of `input_data_var_name`. Required dimensions are given below the data. See [conventions](/doc/conventions.md) for variable naming conventions.

### `global`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `spm_size_classes` | `spmSizeClasses` | s | &#10004; | - | Sizes of the binner SPM size classes. |
| `np_size_classes` | `npSizeClasses` | n | &#10004; | - | Sizes of the binned NP size classes. |
| `spm_particle_densities` | `spmParticleDensities` | f | &#10004; | - | Densities of the different SPM compositional fractions. |
| `T_water` | `T_water` | t | &#10008; | `default_water_temperature` from [config.nml](/doc/config.md) | Temporally-explicit water temperature to be used if none supplied in spatially-explicit data. |

### `Environment` > `GridCell_{x}_{y}`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `size` | `dx` and `dy` | d | &#10008; | `default_grid_size` from [config.nml](/doc/config.md) | Densities of the different SPM compositional fractions. |
| `type` | *no internal variable* | - | &#10004; | - | Integer representing the "type" of GridCell to be simulated. Currently only one type available. Different types will encompass different algorithms and procedures [-]. |
| `n_river_reaches` | `nRiverReaches` | - | &#10008; | 0 | Number of RiverReaches contained within this reach [-]. |
| `reach_types` | - | r | &#10008; | - | *Not currently implemented in model.* Integer array representing the "type" of RiverReaches to be created within this GridCell. Different tpyes will encompass different algorithsm and procedures [-]. |
| `runoff` | `Q_runoff_timeSeries` | t | &#10008; | 0 | Time series of runoff flows [m/s in data file, converted to m/timestep in model]. |
| `precip` | `Q_precip_timeSeries` | t | &#10008; | 0 | Time series of precipitation data [m/s in data file, converted to m/timestep in model]. |
| `evap` | `Q_evap_timeSeries` | t | &#10008; | 0 | Time series of evaporation data [m/s in data file, converted to m/timestep in model]. |
| `quickflow` | `Q_quickflow_timeSeries` | t | &#10008; | 0 | Time series for quickflow data from hydrology model [m/s in data file, converted to m/timestep in model]. |
| `slope` | `slope` | t | &#10004; | - | Average slope of the GridCell [m/m]. |
| `n_river` | `n_river` | - | &#9888; | 0.035 ([natural streams and major rivers](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html)) | Manning's coefficient for the RiverReaches in the GridCell. |
| `T_water` | `T_water_timeSeries` | t | &#9888; | default_water_temperature from [config.nml](/doc/config.md) | Time series of water temperatures in this GridCell [<sup>o</sup>C]. |

### `Environment` > `GridCell_{x}_{y}` > `SoilProfile_{x}_{y}_{p}`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `n_soil_layers` | `nSoilLayers` | - | &#10004; | - | Number of SoilLayers in the SoilProfile [-]. |
| `WC_sat` | `WC_sat` | - | &#10004; | - | Water content at saturation, fraction between 0 and 1 [m3/m3]. |
| `WC_FC` | `WC_FC` | - | &#10004; | - | Water content at field capacity, fraction between 0 and 1 [m3/m3]. |
| `K_s` | `K_s` | - | &#10004; | - | Saturated hydraulic conductivity [m/s]. |
| `distribution_sediment` | `distributionSediment` | s | &#10008; | `default_distribution_sediment` from [config.nml](/doc/config.md) | Distribution used to split sediment mass into size classes [-]. |
| `usle_C` | `usle_C` | t | &#10008; | 1 or estimated from `usle_C_min` and `usle_C_av`, if present | Time series of USLE C-factors [-]. |
| `usle_C_min` | `usle_C_min` | t | &#10008; | 1 | If `usle_C` not present, estimate C-factor from minimum C [-]. |
| `usle_C_av` | `usle_C_av` | t | &#10008; | 1 | If `usle_C` not present, estimate C-factor from average C [-]. |
| `usle_rsd` | `usle_rsd` | t | &#10008; | 0 | Residue on surface [kg/ha]. |
| `usle_K` | `usle_K` | - | &#10004; | - | Soil erodibility factor [t ha h ha-1 MJ-1 mm-1]. |
| `usle_P` | `usle_P` | - | &#10008; | 1 | Support practice factor [-]. |
| `usle_LS` | `usle_LS` | - | &#10004; | - | Topographic factor [-]. |
| `usle_rock` | `usle_rock` | - | &#10008; | 0 | % rock in top of soil profile [-]. |
| `usle_alpha_half` | `usle_alpha_half` | - | &#9888; | 0.33 | Fraction of rainfall falling during half-hour maximum [-]. |
| `usle_area_hru` | `usle_area_hru` | - | &#10004; | - | Area of the HRU corresponding to the GridCell [ha]. |
| `usle_area_sb` | `usle_area_sb` | - | &#10008; | `usle_area_hru` | Area of the subbasin corresponding to the GridCell [km2]. |
| `usle_L_sb` | `usle_L_sb` | - | &#9888; | 50 | Hillslope length for the subbasin [m]. |
| `usle_n_sb` | `usle_n_sb` | - | &#9888; | 0.01 ([fallow land with no residue](http://swat.tamu.edu/media/99192/swat2009-theory.pdf)) | Manning's coefficient for the subbasin [-]. |
| `usle_slp_sb` | `usle_slp_sb` | - | &#9888; | GridCell slope | Slope of the subbasin [m]. |
| `usle_slp_ch` | `usle_slp_ch` | - | &#9888; | GridCell slope | Slope of the channel [km]. |
| `usle_L_ch` | `usle_L_ch` | - | &#10004; | - | The length of the hillslope channel [km]. |

### `Environment` > `GridCell_{x}_{y}` > `SoilProfile_{x}_{y}_{p}` > `SoilLayer_{l}`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `depth` | `depth` | - | &#10008; | `default_soillayer_depth` from [config.nml](/doc/config.md) | Depth of the SoilLayer [m]. |

### `Environment` > `GridCell_{x}_{y}` > `RiverReach_{x}_{y}_{r}`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `alpha_hetero` | `alpha_hetero` | - | &#10008; | `default_alpha_hetero` from [config.nml](/doc/config.md) | Attachment efficiency for modelled NM [-]. |
| `alpha_res` | `alpha_res` | - | &#10004; | - | Calibration factor for maximum resuspendable particle size [-]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>7</sub>. |
| `beta_res` | `beta_res` | - | &#10004; | - | Calibration factor for resuspension [s2/kg]. See [Lazar et al., 2010](http://www.sciencedirect.com/science/article/pii/S0048969710001749?via%3Dihub), parameter a<sub>8</sub>. |
| `f_m` | `f_m` | - | &#10008; | 1 (no meandering) | Meandering factor such that actual river length = linear river length * f_m [-]. |
| `slope` | `slope` | - | &#10004; | - | Slope of the RiverReach [m/m]. *TODO: Should default to the GridCell slope, if not present.* |
| `inflows` | `inflows` | in, riverReachRef | &#10008; | no inflows | An array of references to RiverReaches that inflow to this RiverReach, in the form [x, y, r] (e.g. RiverReach_1_2_3 is specified by [1, 2, 3]). See [spatial structure](/doc/spatial-structure.md) docs. |
| `domain_outflow` | `domainOutflow` | d | &#10008; | no domain outflow | Specify whether this GridCell is the outflow to the model domain by providing a GridCell reference that *would* form the outflow to this cell if it was in the model domain. If present, water, sediment and NM flows are permitted to exit the model domain via the boundary between this cell and the referenced GridCell. See [spatial structure](/doc/spatial-structure.md). |

### `Environment` > `GridCell_{x}_{y}` > `RiverReach_{x}_{y}_{r}` > `BedSediment` and `Environment` > `GridCell_{x}_{y}` > `EstuaryReach_{x}_{y}_{r}` > `BedSediment`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `n_layers` | `nLayers` | - | &#10004; | - | Number of BedSedimentLayers in this BedSediment [-]. |
| `sediment_type` | - | - | &#10004; | - | Integer representing the "type" of BedSediment to be simulated. Currently only one type available. Different types will encompass different algorithms and procedures [-]. |
| `layer_types` | - | l | &#10004; | - | Integer array representing the "type" of each contained BedSedimentLayer to be simulated. Currently only one type available. Different types will encompass different algorithms and procedures [-]. |

### `Environment` > `GridCell_{x}_{y}` > `RiverReach_{x}_{y}_{r}` > `BedSediment` > `Layer_{l}` and `Environment` > `GridCell_{x}_{y}` > `EstuaryReach_{x}_{y}_{r}` > `BedSediment` > `Layer_{l}`

| External name |  Internal name | Dimension | Required | Default | Description |
| --- | --- | --- | --- | --- | --- |
| `capacity` | `C_total` | - | &#10004; | - | Capacity of this layer [m<sup>3</sup>/m<sup>2</sup>]. |
| `initial_mass` | `M_f` | s | &#10004; | - | Initial sediment masses [kg]. |
| `porosity` | `porosity` | - | &#10004; | - | Porosity of this layer [-]. |
| *TODO: Simplify fractional compositions in data file to be 2D array rather than group.* |


## Dimensions

*TODO: Define the dimensions from above.*
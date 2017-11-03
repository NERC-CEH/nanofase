# Data requirements

List of the parameters required in the input data, along with any defaults. Seeing [here](/doc/Config.md) for config file options required. **Required** fields throw an error when they're not included. **Recommended** fields default to something else but this default should be used with caution.

*Note: SubRiver will eventually be removed and GridCells will directly contain RiverReaches, modifying this input file structure somewhat.*

- `Environment`
    + `GridCell_{x}_{y}`
        * `type` : **Required**. Type of GridCell object to be created (only type 1 available currently)
        * `runoff(nTimeSteps)` : *Defaults to 0.* Time series of runoff flows [m3/s - will soon be changed to m/s].
        * `nSubRivers` : **Required**. Number of SubRivers in GridCell
        * `n_river` : **Recommended**, *defaults to 0.035*. Manning's coefficient for the RiverReaches in the GridCell. Defaults to that for [natural streams and major rivers](http://www.engineeringtoolbox.com/mannings-roughness-d_799.html).
        * `rusle2015_erodedSediment(nTimeSteps)` : RUSLE2015 data for eroded sediment. Used only for comparison.
        * `usle_C(nTimeSteps)` : Time series of USLE C-factors [-].
        * `usle_C_min(nTimeSteps)` : If `usle_C` not present, estimate C-factor from minimum C. Defaults to 1 if it and `usle_C_av` not present.
        * `usle_C_av(nTimeSteps)` : If `usle_C_min` not present, estimate it from average annual C. If not present, `usle_C_min` defaults to 1.
        * `usle_rsd(nTimeSteps)` : *Defaults to 0*. Residue on surface [kg/ha].
        * `usle_K` : **Required**. Soil erodibility factor [t ha h ha-1 MJ-1 mm-1].
        * `usle_P` : *Defaults to 1*. Support practice factor [-].
        * `usle_LS` : **Required**. Topographic factor [-].
        * `usle_rock` : *Defaults to 0*. % rock in top of soil profile.
        * `usle_alpha_half` : **Recommended**, *defaults to 0.33*. Fraction of rainfall falling during half-hour maximum [-].
        * `usle_area_hru` : **Required**. Area of the HRU corresponding to the GridCell [ha].
        * `usle_area_sb` : *Defaults to usle_area_hru*. Area of the subbasin corresponding to the GridCell [km2].
        * `usle_L_sb` : **Recommended**, *defaults to 50 m*. Hillslope length for the subbasin [m].
        * `usle_n_sb` : **Recommended**, *defaults to 0.01*. Manning's coefficient for the subbasin. Defaults to that for fallow land with no residue ([SWAT Documentation, pp. 111](http://swat.tamu.edu/media/99192/swat2009-theory.pdf)).
        * `usle_slp_sb` : **Recommended**, *defaults to GridCell slope*. Slope of the subbasin. [m]
        * `usle_slp_ch` : **Recommended**, *defaults to GridCell slope*. Slope of the channel. [km]
        * `usle_L_ch` : **Required**. The length of the hillslope channel [km].
        * `SubRiver_{x}_{y}_{s}`
            - `nInflows` : **Required**. Number of inflow SubRivers.
            - `reachTypes` : **Required**. Type of RiverReaches contained in the SubRiver.
            - `RiverReach_{x}_{y}_{s}_{r}`
                + `slope` : **Required**. Slope of the RiverReach [m/m].
                + `spm(nTimeSteps)` : *Soon to be deprecated*. SPM inflow to the reach.
# Data requirements

List of the parameters required in the input data, along with any defaults. Seeing [here](/doc/Config.md) for config file options required. **Required** fields throw an error when they're not included. **Recommended** fields default to something else but this default should be used with caution.

*Note: SubRiver will eventually be removed and GridCells will directly contain RiverReaches, modifying this input file structure somewhat.*

- `Environment`
    + `GridCell_{x}_{y}`
        * `type` : **Required**. Type of GridCell object to be created (only type 1 available currently)
        * `runoff(nTimeSteps)` : *Defaults to 0.* Time series of runoff flows [m3/s - will soon be changed to m/s].
        * `nSubRivers` : **Required**. Number of SubRivers in GridCell
        * `rusle2015_erodedSediment(nTimeSteps)` : RUSLE2015 data for eroded sediment. Used only for comparison.
        * `usle_C(nTimeSteps)` : Time series of USLE C-factors [-].
        * `usle_C_min(nTimeSteps)` : If `usle_C` not represent, estimate C-factor from minimum C. Defaults to 1 if it and `usle_C_av` not present.
        * `usle_C_av(nTimeSteps)` : If `usle_C_min` not present, estimate it from average annual C. If not present, `usle_C_min` defaults to 1.
        * `usle_rsd(nTimeSteps)` : *Defaults to 0*. Residue on surface [kg/ha].
        * `usle_K` : **Required**. Soil erodibility factor [t ha h ha-1 MJ-1 mm-1].
        * `usle_P` : *Defaults to 1*. Support practice factor [-].
        * `usle_LS` : **Required**. Topographic factor [-].
        * `usle_rock` : *Defaults to 0*. % rock in top of soil profile.
        * `usle_alpha_half` : **Recommended**, *defaults to 0.33*. Fraction of rainfall falling during half-hour maximum.
        * `SubRiver_{x}_{y}_{s}`
            - `nInflows` : **Required**. Number of inflow SubRivers.
            - `reachTypes` : **Required**. Type of RiverReaches contained in the SubRiver.
            - `RiverReach_{x}_{y}_{s}_{r}`
                + `slope` : **Required**. Slope of the RiverReach [m/m].
                + `spm(nTimeSteps)` : *Soon to be deprecated*. SPM inflow to the reach.
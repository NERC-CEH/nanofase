# Data structure

Structure of data output from `Database` objects:

*Work in progress - `Database` objects not currently implemented.*

- global[]
    + `spm_size_classes`
    + `np_size_classes`
    + `grid_cell_type`      Lat/lon or distance-based
    + `grid_cell_size(2)`   May be obsolete if using lat/lon
    + `timestep`
    + `n_timesteps`
    + Environment[]
        * `temperature`
        * GridCell[]
            - `type`
            - `n_SubRivers(:)`
            - `runoff_timeseries(n_timesteps)`
            - SubRiver[]
                + `n_inflows`
                + `reach_types`
                + inflow[]
                    * `x`
                    * `y`
                    * `s`
                + RiverReach[]
                    * `slope`
                    * `spm(n_size_classes, n_timesteps)`
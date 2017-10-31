# Config options

*Work in progress - config.json not currently used. Issues trying to set up JSON-Fortran to read config file.*

A list of the config options available in the config.json file.

- `data`
    + `input_file` : Path to NetCDF input data file, relative to root directory.
    + `output_file` : Path to output data file, relative to root directory.
- `run`
    + `timestep` : Length of each timestep [s].
    + `n_timestep` : How many of these timesteps to run the model for [-].
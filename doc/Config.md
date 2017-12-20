# Config options

The configuration file - specified in Globals.f08, currently `config.nml` - is used to provide configuration options to run the model, such as input and output file paths and model run information (how many time steps and of what length). The file is a [Fortran Namelist](http://owen.sj.ca.us/~rk/howto/slides/f90model/slides/namelist.html) file.


- `data`
    + `inputFile`: Path to NetCDF input data file, relative to root directory.
    + `outputFile`: Path to output data file, relative to root directory.
- `run`
    + `timeStep`: Length of each timestep [s].
    + `nTimeSteps`: How many of these timesteps to run the model for.
    + `epsilon`: Proximity to a value accepted in criteria tests.
- `soil`
    + `defaultSoilLayerDepth`: The default depth of soil layers [m].
- `river`
    + `maxRiverReaches`: Maximum number of RiverReaches within a GridCell.
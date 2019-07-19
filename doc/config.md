# Config options

*Below options massively out of date. Await update in ~Aug 2019.*

The configuration file - specified in [Globals.f90](/src/Globals.f90), currently `config.nml` - is used to provide configuration options to run the model, such as input and output file paths and model run information (how many time steps and of what length). The file is a [Fortran Namelist](http://owen.sj.ca.us/~rk/howto/slides/f90model/slides/namelist.html) file.


- `data`
    + `character(len=100) :: input_file`: Path to NetCDF input data file, relative to root directory.
    + `character(len=100) :: output_file`: Path to the main output data file, relative to root directory.
- `run`
    + `integer :: timestep`: Length of each timestep [s].
    + `integer :: n_timesteps`: How many of these timesteps to run the model for [-].
    + `real(dp) :: epsilon`: Proximity to a value accepted in criteria tests [-].
    + `logical :: error_output`: Should error messages be output to the console?
- `global`
	+ `integer :: default_grid_size`: Default size for GridCells (assuming they are square).
	+ `integer :: default_distribution_sediment(s)`: Default imposed distribution of sediment in binned size classes. Dimension "s" is the number of sediment size classes.
	+ `integer :: default_distribution_np(n)`: Default imposed distribution of NM in binned size classes. Dimension "n" is the number of NM size classes.
- `soil`
    + `real(dp) :: default_soil_layer_depth`: The default depth of soil layers [m].
- `river`
    + `integer :: max_river_reaches`: Maximum number of RiverReaches within a GridCell.
    + `real(dp) :: default_meandering_factor`: The default meandering factor for RiverReaches [-].
    + `real(dp) :: default_water_temperature`: The default water temperature to be used if temporal and/or spatial water temperatures not provided [<sup>o</sup>C].
    + `real(dp) :: default_alpha_hetero`: Default attachment efficiency [-].
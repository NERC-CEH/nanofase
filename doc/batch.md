# Batch runs

*Batch runs are still in the experimental phase - use with caution.*

The model allows for multiple simulations ("chunks") to be chained together, such that the final temporal state of one model simulation provides the intial temporal state of the next. The main advantage of this is when doing model runs over long temporal periods: The model stores all input data in memory, and so long runs may use a significant amount of memory (depending on your geographical setup). Batching runs together means that only the input data for the current chunk is stored in memory at any time. 

In additional to the normal config file (which for batch runs provides config options for the entire batch run), batch runs need a batch config namelist file, which specifies the location of the input data and temporal information of each chunk within the batch run. An example is given at [config/batch_config.example.nml](../config/batch_config.example.nml)

## Running a batch simulation

The model will run in batch mode if a batch config file is passed as the second command line argument, after the normal config file:

```bash
$ ./bin/main /path/to/normal/config.nml /path/to/batch_config.nml
```

where `./bin/main` is the path to the model executable.

### Initial (normal) config file

The initial config file is just the same as a [normal model config file](../config/config.example.nml). Most of the config options are used for the entire batch run, with the exception of the input and constants data paths, start date and number of timesteps. 

### Batch config file

This file defines the number of chunks within the batch, and tells the model where the data for each chunk is, what the start date is, and how many time steps are within each chunk. For example, for a 3-chunk batch run, which uses the same data for each chunk:

```nml
&batch_config
n_chunks = 3
/

&chunks
input_files = "data.example/thames_tio2_2015.nc", "data.example/thames_tio2_2015.nc", "data.example/thames_tio2_2015.nc"
constants_files = "data.example/constants_tio2.nml", "data.example/constants_tio2.nml", "data.example/constants_tio2.nml"
start_dates = "2013-01-01", "2014-01-01", "2015-01-01" 
n_timesteps_per_chunk = 365, 365, 365
/
```

Note the the variables given in this config file overwrite those given in the normal config file. That is, if you specify different input file locations, a different start date or number of timesteps, then the values given in the batch config file will be used.

Also note that the start dates given in the config files (normal or batch) are used to define the chunk start dates, rather than that given in the NetCDF input file (which is only included for information). Because of this, "reusing" the same NetCDF dataset for different temporal periods (as in the example above) is possible. This might be particularly useful to determine time scales over which model output is likely to reach steady state. There are no checks on whether batch runs are contiguous in time.

## Data considerations

To enable compatibility between simulations in the batch run, it is important that the geographical setup for each data file is identical, otherwise the model will fail. In brief, this means that spatial data must be for the same geographical region and have the same grid resolution and coordinate reference system. The temporal period of each simulation *can change* between simulations, as defined by the `n_timesteps_per_chunk` variable in the batch config file.
# Batch runs

*Batch runs are still in the experimental phase - use with caution.*

The model allows for multiple simulations to be chained together, such that the final temporal state of one model simulation provides the intial temporal state of the next. The main advantage to this is when doing model runs over long temporal periods. The model stores all input data in memory, and so long runs may use a significant amount of memory (depending on your geographical setup). Batch runs together means that only the input data for the current batch is stored in memory at any time. 

In additional to the normal config file (which for batch runs provides config options for the entire batch run), batch runs need:
- A *batch config file* for each simulation within the batch run. This specifies the location of input data for this simulation, amongst other things.
- A *batch run file*, which lists all of the simulations to run by their batch config files.

## Running a batch simulation

The model will run in batch mode if a batch run file is passed as the second command line argument, after the normal config file:

```bash
$ ./bin/main /path/to/normal/config.nml /path/to/batch/run/file
```

where `./bin/main` is the path to the model executable.

## Initial (normal) config file

The initial config file is just the same as a [normal model config file](../config/config.example.nml). Most of the config options are used for the entire batch run, with the exception of those included in the individual batch config files.

## Batch config file

Each simulation within the batch needs a separate batch config file, which points to the input data for this simulation and provides temporal information about the batch run. Below is an example [../config/batch_sim.example.nml](../config/batch_sim.example.nml) file:

```nml
&data
input_file = "/path/to/simulation/input/data.nc"
constants_file = "/path/to/simulation/constants/data.nml"
/

&run
n_timesteps = 365
start_date = "2000-01-01"
/
```

## Batch run file

The batch run file is responsible for telling the model (a) that we want to run in batch mode (inferred by the presence of the batch run file in the command line arguments), and (b) the location of the batch config files for each simulation (and the order in which they are to be run in). It is simply a list of these locations, preceeded by a number telling the model how many simulations there are within the batch.

For example, for a two-simulation batch run for batch config files at `config/batch_sim.example.nml` and `config/batch_sim2.example.nml`, the batch run file would look like this:

```
2
config/batch_sim.example.nml
config/batch_sim2.example.nml
```

*Note that the first batch simulation file provided in the batch run file is actually ignored in favour of the normal config file provided as a command line argument.*
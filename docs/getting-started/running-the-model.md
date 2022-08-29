# Runing the model

*Work in progress.*

## Running

The model requires a config file to run. An example is placed at [config.example/config.example.nml](../config.example/config.example.nml). The example is commented and should be relatively self-explanatory. Copy this to a new file and edit as you wish. Make sure the directories you specify for output data and logs exist, otherwise the model will fail. Then pass this config file as the first argument when calling the model executable. For example, if you compiled the model to `./bin/main`:

```shell
$ ./bin/main /path/to/config/file.nml
```

### Input data

The config file is responsible for telling the model where the input data are (via the `&data` group). To compile your own input data for the NanoFASE model, it is highly recommended that you use the [NanoFASE data module](https://github.com/NERC-CEH/nanofase-data). This module is responsible for (amongst other things) compiling multiple spatial and/or temporal input files into the main NetCDF input file required by the model. It is included as a submodule to this repo: [vendor/nanofase-data](../vendor/nanofase-data).

### Output data

Output data is [documented here](users/output.md).

## Batch runs and checkpointing

The model allows for multiple simulations to be chained together into batch runs, and model simulations to be saved and reinstated by use of checkpointing. See the following for more details:
- [](users/batch.md)
- [](users/checkpointing.md)

## Example workflows

A few example workflows are provided in [](users/example-workflows.md).
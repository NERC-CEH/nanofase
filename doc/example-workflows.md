# Example workflows 

This document gives some examples of workflows for compiling input data, running the model and working with output data. Example shell scripts are provided to give an idea of how to implement these workflows, but these will need to be adjusted to work on your system.

## First step: Compile the model

If you haven't already, you need to compile the model to an executable file. This only needs to be done once (unless the model code changes), even if using different data. Instructions are given in the [README.md](../README.md).

## Compiling/editing input data > running the model

- Use the [NanoFASE data module](https://github.com/nerc-ceh/nanofase-data) to compile or edit input data. Refer to this [README.md](https://github.com/NERC-CEH/nanofase-data/blob/develop/README.md) for documentation on how to do this.
- The data compilation will produce two files: A NetCDF file and a Fortran Namelist (.nml) file.
- Create a model config file (example given here: [config.example/config.example.nml](../config.example/config.example.nml)) or update an existing one, ensuring that `&data` > `input_file` points to the NetCDF file, and `&data` > `constants_file` points to the Fortran Namelist file.
- Run the model, as detailed in the [README.md](../README.md).

### Example: updating emissions scenarios

Say you already have input data files in the `data` directory (all paths relative to the repo root):

```
data/
|-- data.nc
|-- constants.nml
|-- new_emissions_areal_pristine_soil.tif
```

You wish to edit the NetCDF file (data.nc) to include a new raster for areal emissions of pristine nanomaterials to soil (new_emissions_areal_pristine_soil.tif). The relevant variable in the NetCDF file is `emissions_areal_pristine_soil` and we can write an edit config file for the NanoFASE data module to do this:

```yaml
input_nc_file: ./data/data.nc
output_nc_file: ./data/data_edited.nc
emissions_areal_soil_pristine:
  type: raster
  path: ./data/new_emissions_areal_pristine_soil.tif
  units: kg/m**2/year
```

Save this to `vendor/nanofase-data/config.edit.yaml`. Now we can run the data compiler, which is provided as a submodule to this repo:

```shell
$ vendor/nanofase-data/nanofase_data.py edit vendor/nanofase-data/config.edit.yaml
```

After this has finished, our `data` folder should have the new NetCDF file:

```
data/
|-- data.nc
|-- data_edited.nc
|-- constants.nml
|-- new_emissions_areal_pristine_soil.tif
```

To run the NanoFASE model, a config file is need (see [README.md](../README.md)). Assuming you already have one located at `config/config.nml`, edit the `&data` group of this file to point to the new data:

```
&data
input_file = "data/data_edited.nc"
constants_file = "data/constants.nml"
output_path = "data/output/"
...
```

Now you're ready to run the model with this updated config file:

```shell
$ make run CONFIG_PATH=config/ CONFIG_FILE=config.nml
```

The output data will be saved at `data/output` (specified in the config file, as per above).

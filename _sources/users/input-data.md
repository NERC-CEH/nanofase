# Input data

The model requires a NetCDF file of spatio(temporal) input data, and a Fortran namelist file for constants. The config file is responsible for telling the model where these input data are located (via the `&data` group). There are two options for generating these input data files:
* *Recommended*: Use the [NanoFASE data module](https://github.com/nerc-ceh/nanofase-data) - see [](nanofase-data). This is a Python library that compiles multiple spatio(temporal) input files into the main NetCDF file, at the same time as deriving secondary variables from these data, which are required by the model.
* Manually creating (or editing a copy of) the NetCDF and namelist files. This is not recommended for compiling new data (though can be useful for editing existing data) due to the requirement for a range of [secondary derived variables](netcdf-namelist-input:secondary-derived-variables) that would have to be calculated manually. If you wish to go down this route, see [](netcdf-namelist-input) for details of the variables required in the NetCDF and constants namelist file.

The complete list of parameters required by the model is givin in [](parameter-reference). Example NetCDF and constants files are given in the [data.example](https://github.com/NERC-CEH/nanofase/tree/develop/data.example) directory.

% TODO draw diagram of data input components
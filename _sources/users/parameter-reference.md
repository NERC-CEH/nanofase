# Model parameter reference

This section is a comprehensive reference for model input parameters, including what file(s) they should be specified in, what their defaults are, and ideas for where to source them from.

For information on how to compile and input these parameters into the model, see [](input-data).

The **Name(dimensions)** column includes the following dimensions for use in the NetCDF file:
* `t`: Time dimension
* `x`, `y`: Spatial dimensions
* `d`: Number of spatial dimensions, always equal to 2
* `w`: Index of inflow to a grid cell, maximum of 7
* `box`: Used to define the bounding box of the grid cells, always equal to 4 to represent each side of the bounding box
* `p`: Index of point source within a grid cell
* `l`: Index representing land use categories

A handful of 1D arrays are present in the constants file. Note that the constants namelist file also requires allocatable array size variables to define the length of these arrays - see [](netcdf-namelist-input:allocatable-array-sizes). These are handled automatically by the NanoFASE data module and *not* included in this reference. The array dimensions used in the constant files are:
* `nm`: NM size classes, length must equal `n_nm_size_classes` in the model config file.
* `spm`: Suspended sediment size classes, length must equal `n_spm_size_classes` in the model config file.
* `fc`: Sediment fractional compositions, length must equal `n_fractional_compositions` in the model config file.
* `soil_l`: Soil layers, length must equal `n_soil_layers` in the model config file.
* `sed_l`: Sediment layers, length must equal `n_sediment_layers` in the model config file.

The **Units** columns gives the internal model units, and therefore those required by the NetCDF and constants namelist. "dt" is the length of the model timestep. For example, if the model has a timestep of one day, then "kg/dt" means "kg/day". The NanoFASE data module is capable of converting units of spatial data (not constants), and therefore any reasonable units can be used for the data input to this - as long as these units are specified in the config file.

The **Specified in** column indicates which file the model parameter should be included in:
* {bdg-primary}`data.nc` The spatio(temporal) NetCDF file input directly to the model.
* {bdg-secondary}`constants.nml` The constants namelist file input directly to the model.
* {bdg-success}`config.yaml` The main config file of the [NanoFASE data module](nanofase-data), which lists spatio(temporal) variables that are compiled into the NetCDF file. Though the data themselves aren't in this file, it provides the path to where the data are.
* {bdg-danger}`constants.yaml` The constants YAML file that the [NanoFASE data module](nanofase-data) converts to the constants namelist file.

Data in the constants file are grouped, and this is denoted in the **Name(dimensions)** column as {bdg-dark-line}`group` `parameter_name`.

The **Potential source** column gives ideas for where the data may be sourced from. These are particularly relevant to European scenarios.

## Environmental parameters

### Meteorological and hydrological parameters

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/met-hydro-parameters.csv
```
````

### Surface water parameters

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/surface-water-parameters.csv
```
````

### Soil and terrestrial parameters

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/soil-terrestrial-parameters.csv
```
````

### Sediment parameters

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/sediment-parameters.csv
```
````

## Nanomaterial parameters

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/nanomaterial-parameters.csv
```
````

## Emissions


````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/emission-parameters.csv
```
````

## Calibration parameters

The calibration parameters relate to sediment dynamics, but are included in a separate table for ease of reference. They can all either be specified as spatial or constant, with the spatial version being used preferentially if both are present.

Generally, the model should be calibrated against observed sediment concentrations using these parameters. For example, for the UK, the [Environment Agency Water Quality Data Archive](https://environment.data.gov.uk/water-quality/view/landing) could be used.

````{div} full-width
```{csv-table} 
:header-rows: 1
:file: parameters/calibration-parameters.csv
```
````

## Secondary derived variables

The spatial variables that are derived by the NanoFASE data module, and therefore required in the NetCDF file but not as input to the data module, are documented here: [](netcdf-namelist-input:secondary-derived-variables).
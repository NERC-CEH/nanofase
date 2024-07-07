# Compiling data with the NanoFASE data module

The *NanoFASE data module* (`nfdata`) is a Python package that is used to compile input data for the model. It is recommended to use this package over and above manually compiling the NetCDF and constants namelist file required by the model, as the data module scripts take care of deriving a variety of [secondary derived variables](netcdf-namelist-input:secondary-derived-variables), amongst other reasons.

The package source code can be [found on GitHub](https://github.com/nerc-ceh/nfdata).

## Getting started

The package is available on [PyPI](https://pypi.org/project/nfdata/) or [Conda](https://anaconda.org/samharrison7/nfdata), and the easiest way to use the package is to install one of these using Pixi, Conda/Mamba or Pip:

`````{tab-set}
````{tab-item} Conda
```console
$ conda install -c samharrison7 nfdata
```
````
````{tab-item} Mamba
```console
$ mamba install -c samharrison7 nfdata
```
````
````{tab-item} Pip
```console
$ pip install nfdata
```
````
````{tab-item} Pixi (global)
```console
$ pixi global install -c samharrison7 nfdata
```
````
````{tab-item} Pixi (project)
```console
$ pixi project channel add samharrison7
$ pixi add nfdata
```
````
`````

## Basic usage

Once installed, the package can be run via the command line using the `nfdata` command:

```
$ nfdata --help
usage: nfdata [-h] [--output OUTPUT] {create,edit,constants} file

Compile or edit data for the NanoFASE model.

positional arguments:
  {create,edit,constants}
                        do you wish to create from scratch, edit the data or create a constants file?
  file                  path to the config file (create/edit tasks) or constants file (constants task)

options:
  -h, --help            show this help message and exit
  --output OUTPUT, -o OUTPUT
                        where to create the new constants file (for constants task)
```

### Creating a new dataset

Specifying the "create" option compiles a new NetCDF dataset and Fortran namelist constant file:

```console
$ nfdata create /path/to/config.create.yaml
```

An annotated example config file is given: [`config.create.example.yaml`](https://github.com/NERC-CEH/nanofase-data/blob/develop/config.create.example.yaml). The file is quite self-explanatory, but a [full description is given below](nanofase-data:config).

The two files will be output to the paths specified in the config file.

### Editing an existing dataset

To edit an existing NetCDF dataset, specify the "edit" option:

```console
$ nfdata edit /path/to/config.edit.yaml
```

An annotated example config file is given: [`config.edit.example.yaml`](https://github.com/NERC-CEH/nanofase-data/blob/develop/config.edit.example.yaml). This is similar (but not identical) in format to the creation config file, except only those variables you with to edit should be specified (all other variables are left as-is). Documentation for the config file is [provided below](nanofase-data:config).

Certain variables can't be edited: `flow_dir`, `is_estuary`. Create a new dataset instead if you wish to change these variables.

The Fortran namelist file cannot be edited using this method and you should instead edit the file directly.

### Only creating a new constants file

To simply convert a constants YAML file to a Fortran namelist file, you can use the `constants` option:

```console
$ nfdata constants /path/to/constants.yaml -o /path/to/constants.nml
```

No config file is required. The location of the newly created constants file is given by the `-o` or `--output` argument.

```{admonition} Tips
:class: tip
- All rasters must be the same CRS as the `flow_dir` raster, and this must be a projected raster. In addition, all rasters except for `land_use` must be the same resolution as `flow_dir`. They can cover a larger geographical region and the module will automatically clip them to the correct size.
- Support for different file types is a bit sporadic at the moment. We suggest sticking the raster files for spatial variables, raster or CSV files for spatiotemporal variables (with 1 file per timestep for raster files) and shapefiles for point sources. You will trigger errors if you use an unsupported file.
- Example input data files are given in [`data.example/`](https://github.com/NERC-CEH/nanofase-data/tree/develop/data.example). Running the model using the example config files uses these data.
```


(nanofase-data:config)=
## Creating a config file

A config file must be provided when running the `nanofase_data.py` script in `create` or `edit` mode. Examples for creation and editing are given:
- [`config.create.example.yaml`](https://github.com/NERC-CEH/nanofase-data/blob/develop/config.create.example.yaml)
- [`config.edit.example.yaml`](https://github.com/NERC-CEH/nanofase-data/blob/develop/config.edit.example.yaml)

The examples are annotated and should be self-explanatory. However, there are a few areas that need further documentation:

### Setup

The `create` and `edit` config files follow a similar layout. A variety of setup data is required and in the examples is placed at the top of the file. This includes file paths to input and output path, and model config info (e.g. timestep info):

```yaml
# Setup
nanomaterial: TiO2                                    # Name of the nanomaterial. Not used in model.
output:
  nc_file: ./data.nc                                  # Where do you want the output NetCDF file to be stored?
  constants_file: ./constants.nml                     # Where do you want the output constants file to be stored?
constants_file: ./data.example/thames_tio2_2015/constants.yaml    # Where is the input constants file?
land_use_config: ./data.example/thames_tio2_2015/land_use.yaml    # Where is the input land use config file?
root_dir: ./data.example/thames_tio2_2015/            # Root dir, can be used in path variables below as <root_dir>
iso3: GBR                                             # iso3 code for country modelled
time:
  n: 365                                              # Number of timesteps to run the model for
  dt: 86400                                           # Length of each timestep in seconds
  start_date: 2015-01-01                              # Start date for the model run
```

```{warning}
The `time` options *do not* clip temporal data with a pre-specified time dimension to this time period. Rather, they impose this time period when compiling the NetCDF file.
```

#### Constants file - `output.constants_file`

The NanoFASE data module generates two files, a NetCDF dataset and a Fortran namelist constants file. The NetCDF dataset holds spatial and/or temporal data, encompassing *most* of the data required by the NanoFASE model. The constants file holds data for variables which are constant in space and time. The main reason for including this as a separate text-based file is to provide an easier way to edit constant variables, using a text editor rather than having to write a script or using NetCDF utilities to do so.

The data module simply converts the YAML constants file provided into a Fortran namelist file. The location of this YAML file should be given in the config file:

```YAML
constants_file: /path/to/constants.yaml
```

Note this conversion only happens in `create` and `constants` mode and there is no utility to edit the Fortran namelist file via the data module. Instead, if you wish to edit the file, you can just use a text editor to do so.

#### `root_dir`

The `root_dir` variable can be used to specify a directory which can be used in the `path` property of each parameter (see [](nanofase-data:parameters)), for example to point to a directory in which all the data are stored. If `<root_dir>` is included in a `path` property, the value of `root_dir` will be substituted. For example:

```yaml
...
root_dir: /path/to/data
...
flow_dir:
  type: raster
  path: <root_dir>flow_dir.tif      # Evaluates to /path/to/data/flow_dir.tif
runoff: 
  type: csv
  path: <root_dir>runoff.csv        # Evaluates to /path/to/data/runoff.csv
```  

(nanofase-data:land-use)=
#### Land use config - `land_use_config`

The module maps between common land use classes (e.g. those provided by [CORINE](https://land.copernicus.eu/pan-european/corine-land-cover)) and the simpler, grouped land use classes used within the NanoFASE model by way of a land use config file. If `land_use_config` is not provided in the config file, [`land_use.default.yaml`](https://github.com/NERC-CEH/nanofase-data/blob/develop/land_use.default.yaml) is used instead - we recommend you use the CORINE land cover map, resampled to the correct CRS and stick with this default. The land use map itself is provided as a raster in by the `land_use` parameter.

For reference, the NanoFASE land use categories are:

```{list-table}
:header-rows: 1

* - Index value `l`
  - Land use category
* - 1
  - `urban_no_soil`
* - 2
  - `urban_parks_leisure`
* - 3
  - `urban_industrial_soil`
* - 4
  - `urban_green_residential`
* - 5
  - `arable`
* - 6
  - `grassland`
* - 7
  - `deciduous`
* - 8
  - `coniferous`
* - 9
  - `heathland`
* - 10
  - `water`
* - 11
  - `desert`
* - 12
  - `other`
```


(nanofase-data:parameters)=
### Parameters

The setup section is followed by a list of parameters, each of which must have at least a `path` and `type` property. If `units` are included, the module will automatically convert them to the correct units on compilation. The units must follow the format used by the [Pint package](https://pint.readthedocs.io/en/stable/) and be in its [default list of units](https://github.com/hgrecco/pint/blob/master/pint/default_en.txt) (which is very extensive, so it probably will be). `source` and `references` can be used to add these attributes to the NetCDF file, but are metadata only and not used by the model.

For example, for the parameter `soil_bulk_density`, we have a GeoTIFF raster file at `<root_dir>soil_bulk_density.tif`, and its units are t/m<sup>3</sup>, which we need the data module to convert to the [required model units of kg/m<sup>3</sup>](netcdf-namelist:soil-bulk-density):

```yaml
soil_bulk_density:
  type: raster
  units: t/m**3
  path: <root_dir>soil_bulk_density.tif
```

A few parameters require additional information:

```{margin} Time-varying emissions
Individual model runs have constant emissions for the whole run. However, individual model runs can be chained together in a multi-year model run, each year having different areal emissions. See [](batch). This is how multi-year simulations with varying areal emissions are currently performed.
```

(nanofase-data:point-emissions)=
#### Point source emissions and temporal profiles

Unlike areal source emissions, which are (currently) constant throughout the model run, point sources can have a temporal profile applied and this makes their input a little more complicated than most variables (though I am working on making it simpler than it currently is).

Point source emissions are provided by a shapefile. Each point within the shapefile should have a number of variables:
  - Source type: A string to categorise this source. This is used to apply different temporal profiles to different sources (currently a maximum of one temporal profile is supported). Named `profile` in [example data](https://github.com/NERC-CEH/nanofase-data/tree/develop/data.example).
  - Value variable: The value for this point source. Named `emission` in [example data](https://github.com/NERC-CEH/nanofase-data/tree/develop/data.example).

The names of these variables (columns) are specified in the config:

```yaml
emissions_point_water_pristine:
  type: shapefile
  value_var: emission                 # The name of the value variable in the shapefile
  path: ...
  source_type_col: profile            # The name of the source type variable in the shapefile
```

**Temporal profiles** for a shapefile can be specified by the `temporal_profile` property. This should point to a CSV file (example given [here](https://github.com/NERC-CEH/nanofase-data/blob/develop/data.example/test-scenario/emissions_temporal-profile_2015.csv)), with `ISO3`, source type and factor columns. The name of the source type and factor columns can be specified in the config file:

```yaml
emissions_point_water_pristine:
  ...
  temporal_profile:
    path: /path/to/temporal_profile.csv
    source_type_col: Emission_source_type       # The name of the column giving the source type
    for_source_type: P2                         # The value of source_type_col in the shapefile for which this temporal profile should apply
    factor_col: Factor                          # Which column gives the temporal factor?
``` 

The source type column is cross-referenced with the `source_type_col` column for the shapefile and only those points with matching source types have this temporal profile applied to them. In the example data, profiles with source type `P2` are given this temporal profile.

Note that for the moment, only daily temporal factors are allowed and temporal profiles are for each year, and thus when the temporal profile CSV file is filtered by ISO3 and source type, it should contain 365-366 rows (depending on whether it is a leap year or not).

```{note}
We appreciate that the way of specifying point source emissions is currently rather awkward. This is likely to be updated to a cleaner (and probably more prescriptive, in terms of column/variable names) interface in the future.
```

### Full parameter schema

The full list of model parameters, including whether they are required by the NanoFASE data module, is given in the [](parameter-reference).
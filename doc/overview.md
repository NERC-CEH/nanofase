# Model overview

*Below description very out of date. Await update in June 2020.*

This is a high-level overview of how the NanoFASE model operates.

## Data input/output
Data input is through a NetCDF data file, whose name is set in the [config file](/doc/config.md) and subsequently stored in the Globals module via the `C%inputFile` variable. The data requirements for the program are listed [here](/doc/data-requirements.md). This file can be generated automatically generated from a JSON file using the Python script located at `vendor\json2netcdf\json2netcdf.py`. For example, `python vendor/json2netcdf/json2netcdf.py data.json data.nc` converts data.json to data.nc, providing Python and the netCDF4 library are installed. See the [json2netcdf documentation](https://github.com/samharrison7/json2netcdf) for more information on how to format the JSON file, and see [this page](/doc/generating-input-file.md) for documentation on how to construct a JSON input file specifically for the NanoFASE model.

The [config file](/doc/config.md) allows for the specification of an output file path, which is stored in `C%outputFile` and used in [main.f90](/src/main.f90).

## Main program execution
The program is executed via [main.f90](src/main.f90), which is responsible for initialising global variables and constants (`call GLOBALS_INIT()`), creating the Environment object (`env%create()`), and updating the Environment object on each timestep (`env%update(t)`).

### Object hierarchy
The model is structured with a hierarchy of objects that mimics the hierarchy of environmental compartments included in the model. A key principle of this hierarchy is that objects lower down the hierarchy are completely independent of their parents. E.g., a `SoilProfile` object knows nothing about the `GridCell` in which it is contained. Any data required from the `GridCell` are passed via procedure parameters.

The following list shows the current hierarchical structure of the model. The symbol &#128461; indicates there may be more than one of the object contained within the parent object:

- `Environment`
    - &#128461; `GridCell`
        - `SoilProfile`
            - &#128461; `SoilLayers`
        - &#128461; `RiverReach`
            - `PointSource`
        	- `Reactor`
            - `BedSediment`
                - &#128461; `BedSedimentLayer`
        - &#128461; `EstuaryReach`
            - `PointSource`
        	- `Reactor`
            - `BedSediment`
                - &#128461; `BedSedimentLayer`
    	- `DiffuseSource`

### `create` and `update`
Most of these environmental compartment objects contain `create` and an `update` methods. `create` methods are called only once and are primarily responsible for allocating memory, getting input data (usually via a `parseInputData` method) and setting up the object hierarchy below that object (i.e., calling its contained objects' `create` methods). `update` methods are called on every time step, and run the object's simulation for that step (e.g., simulating different environmental processes, and calling contained-object `update` methods).

The exception is `BedSediment` objects and below, which are agnostic to the current time step and for which processes (such as deposition and and resuspension) are called separately.

## Data output
Currently, data output is dealt with in a somewhat adhoc fashion in [main.f90](/src/main.f90). Data is usually printed straight to a text/CSV file for further processing via other scripts.
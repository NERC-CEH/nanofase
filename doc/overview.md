# Model overview

This is a high-level overview of how the NanoFASE model operates.

## Data input/output
Data input is through a NetCDF data file, whose name is set in the [config file](/doc/config.md) and subsequently stored in the Globals module via the `C%inputFile` variable (currently "data/data.nc"). The data requirements for the program are listed [here](/doc/data-requirements.md). This file can be generated automatically generated from a JSON file using the Python script located at `vendor\json2netcdf\json2netcdf.py`. For example, `python vendor/json2netcdf/json2netcdf.py data.json data.nc` converts data.json to data.nc, providing Python and the netCDF4 library are installed. See the [json2netcdf documentation](https://github.com/samharrison7/json2netcdf) for more information on how to format the JSON file, and see [data.json](/data/data.json) for an example of the object hierarchy that is required for the NanoFASE model.

The [config file](/doc/config.md) allows for the specification of an output file path, which is stored in `C%outputFile` but not currently used.

## Main program execution
The program is executed via [main.f08](src/main.f08), which is *currently* responsible for initialising global variables and constants (`call GLOBALS_INIT()`), creating the Environment object (`env%create()`), and updating the Environment object on each timestep (`env%update(t)`).

### Object hierarchy
The model is structured with a hierarchy of objects that mimics the hierarchy of environmental compartments included in the model. A key principle of this hierarchy is that objects lower down the hierarchy are completely independent of their parents. E.g., a `SoilProfile` object knows nothing about the `GridCell` in which it is contained. Any data required from the `GridCell` are passed via procedure parameters.

The following list shows the current hierarchical structure of the model. The symbol &#128461; indicates there may be more than one of the object contained within the parent object:

- `Environment`
    - &#128461; `GridCell`
        - `SoilProfile`
            - &#128461; `SoilLayers`
        - &#128461; `SubRiver`
            - &#128461; `RiverReach`
                - `BedSediment`
                    - &#128461; `BedSedimentLayer`

### `create` and `update`
Most of these environmental compartment objects contains a `create` and an `update` methods. `create` methods are called only once and are primarily responsible for allocating memory, getting input data (usually via a `parseInputData` method) and setting up the object hierarchy below that object (i.e., calling its contained objects' `create` methods). `update` methods are called on every time step, and run the object's simulation for that step (e.g., simulating different environmental processes, and calling contained object `update` methods).

The exception is `BedSedment` objects and below, which are agnostic to the current time step and for which processes (such as deposition and and resuspension) are called separately.

## Creation

### Environment%create()
`create()` is responsible for constructing the grid structure (from the data file), in turn creating `GridCell` objects which subsequently create the `SubRiver` objects they contain, and so-on down the object hierarchy. After the grid has been set up, links are made between individual `SubRivers` and their inflows; this operation must be performed separately so that we can ensure *all* `SubRiver` objects have been created before we try to link them via pointers.

### GridCell%create(x, y, isEmpty)
This creates a `GridCell` object with position x, y. It constructs a reference for the `GridCell`, of the format "GridCell\_x\_y". It then calls the `GridCell%parseInputData()` procedure to obtain time-dependent runoff and evapotranspiration data from the data file. These data are then use to create the `SoilProfile` and the collection of `SubRiver` objects that it contains (which are again specified in the data file).

### SoilProfile%create(x, y, p, slope, n_river, area, Q_precip_timeSeries, Q_evap_timeSeries)
This is primarily responsible for obtaining input data (via the `SoilProfile%parseInputData()` procedure) and creating the `SoilLayer`s contained within the `SoilProfile`.

### SoilLayer%create(x, y, p, l, WC_sat, WC_FC, K_s)
This is primarily responsible for obtaining input data (via the `SoilLayer%parseInputData()` procedure).

### SubRiver%create(x, y, s, length, Q_runoff_timeSeries)
This is first responsible for allocating the size of arrays related to SPM, which are all the size of the number of SPM size classes (which is obtained from the data file and provided via a constant, `C%nSizeClassesSpm`). The length and runoff are set via the input parameters, and SPM inflow and discharge (`me%spmIn` and `me%spmOut`) are initialised to 0.

A `SubRiver` character reference is then generated, of the format "SubRiver\_x\_y\_s" (where "s" is an integer representing the index of the `SubRiver` within the GridCell).

**Inflow references:** A list of inflow character references is obtained from the data file, and a temporary `me%inflowRefs` variable filled with them. The purpose of this is that, when the `Environment%create()` method comes to linking `SubRivers` to their inflows, it doesn't have to inquire the data file for a second time, and can simply get a list of inflows to each `SubRiver` from the `me%inflowsRefs` variable.

Each `SubRiver` can contain a collection of RiverReaches, and these are now created and passed length and Qrunoff as the `SubRiver` length and Qrunoff divided by the number of reaches.

### RiverReach%create(x, y, s, r, l, Q_runoff_timeSeries)
In a similar fashion to `SubRiver%create()`, this method first allocates SPM-related arrays, and then generates a RiverReach character reference, of the format "RiverReach\_x\_y\_s\_r". Initial SPM concentrations `me%C_spm` and masses `me%m_spm` (i.e., the mass of SPM currently within the reach) are set to zero, and time-dependent runoff set to that provided by the `Q_runoff_timeSeries` parameter. The slope `me%S` is also obtained from the data file.

The method then checks the data file to see if a river meandering factor `me%f_m` (which defaults to 1) and initial SPM input have been provided. The initial SPM input is simply a mass (not a flux) and `me%m_spm` is updated accordingly. **Note** that the SPM concentrations `me%C_spm` cannot be updated until we know the *volume* of the reach, and the volume is dependent on the inflow rate and thus it is set by `update()`. Thus, before the first time step is simulated, RiverReaches may have a *non-zero SPM mass but zero SPM concentration*.

## Updating (simulating)

The length of the model timestep is set via the [config file](/doc/config.md), and the [main.f08](src/main.f08) file is responsible for looping through the desired timesteps and calling the `update(t)` method on the Environment object.

The choice of method name "update" was chosen to conform to the "Basic Model Interface" standard; see [here](http://csdms.colorado.edu/wiki/BMI_Description) and [here](http://bmi-spec.readthedocs.io/en/latest/). Ultimately, [classModel.f08](src/classModel.f08) (currently only a template) may be utilised to control the model.

### Environment%update(t)
For the moment, the main operation the program performs is routing water and SPM through the grid structure, and generating surface runoff and percolation. The method loops over all `GridCell`s and calls their `update` methods.

A second loop through the grid structure is then needed to "finalise" the update, setting temporary SPM discharge variables to actual SPM discharge variables. This is due to the way that `SubRiver` inflows are specified as pointers to the actual `SubRiver` objects that provide the inflow. If routing was performed for one `SubRiver` and that `SubRiver`'s SPM discharge (`spmOut`) immediately set, then a downstream `SubRiver` (whose routing procedure we called later in the loop) would use this discharge as its inflow, as opposed to the previous timestep's discharge. That is, it would be using discharge from the wrong timestep as its inflow. Thus, we wait until the main routing loop has finished, before finally setting actual SPM discharge variables.

### GridCell%update()
`GridCell`'s routing procedure simply calls the routing procedure on every `SubRiver` it contains.

### SoilProfile%update()
Soil erosion and percolation are calculated by this procedure, by calling `SoilProfile%erode()` and `SoilProfile%percolate()`, respectively. The `percolate` method is responsible for passing quantities of water between individual `SoilLayer`s, depending on the percolation rate they calculate.

### SoilLayer%update()
This method calculates volumes of water percolated and pooled from the current `SoilLayer`, and updates the layer's volume of water accordingly.

### SubRiver%update()
First of all, this method loops through all of the `SubRiver`'s inflows and sums their discharge, forming the inflow (`Qin` and `spmIn`) to the first RiverReach in the `SubRiver`.

Then, we loop through all of the reaches, calling an `update` method on each, providing it with `Qin` and `spmIn` for that reach. This method (detailed below) sets `Qout` and `spmOut` for that RiverReach, and thus provides us the inflow for the downstream RiverReach (the RiverReach in the next loop iteration). This continues until we have obtained the final `Qout` and `spmOut` from the final RiverReach, which will form `Qout` and `spmOut` for the entire `SubRiver`. We also sum the individual SPM masses across all the reaches, to obtain a total SPM mass for the `SubRiver`. As detailed previously, these values are set in temporary variables until of the `SubRiver` routing is complete.

### RiverReach%update()
This is where the actual routing takes place. First off, the `Qin` provided is appended by any runoff for this time step (the time series of runoff data was stored in a state variable `me%Q_runoff_timeSeries` when we created the RiverReach, and we obtain this time step's `me%Q_runoff` from that), to form the total inflow `me%Qin` for the cell. A number of procedures then calculate the dimensions of the RiverReach, using `me%Qin` (amongst other things) as an input; see `calculuateWidth1`, `calculateDepth1`, `calculateVelocity1`, `calculateArea1` and `calculateVolume1` in [classRiverReach1.f08](src/River/RiverReach/classRiverReach1.f08).

The current SPM mass `me%m_spm` is appended to by the inflow and the SPM concentration calculated from this mass and the RiverReach volume that's just been calculated. For each of the SPM size classes, we then calculate a settling rate `me%k_settle` (via the `settling` procedure), which is zero if the temperature-dependent water density exceeds the SPM density, and a resuspension flux `me%j_spm_res` (via the `resuspension` procedure). These are used to remove/add a certain amount of SPM from the reach, and the SPM concentration updated accordingly. Settled/resuspensed SPM are transferred to/from the `BedSediment`.

Finally, we advect the SPM out of the cell: `spmOut = me%Qout*me%rho_spm`, and check that the advected amount is less than total SPM mass before updating the SPM mass and concentration again. If `spmOut > me%m_spm`, then the mass and concentrations are set to zero (i.e., we've removed all of the SPM). If this happens, a warning is output to the console.
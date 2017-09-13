# NanoFASE

Multimedia environmental fate model for engineered nanomaterials.

Project links:
 - [Project website](http://nanofase.eu/).
 - [Facebook](https://www.facebook.com/nanofase/).
 - [Twitter](https://twitter.com/NanoFASE_EU).

[Quick start guide](doc/Quickstart.md)

## Overview of model procedure

This is a high-level overview of how the NanoFASE model operates.

### Data input
Data input is through a NetCDF data file, whose name is set in [Globals.f08](src/Globals.f08) via the `Constants%inputFile` variable (currently data.nc). This file can be generated automatically generated from a JSON file using the Python script located at `vendor\json2netcdf\json2netcdf.py`. For example, `python vendor/json2netcdf/json2netcdf.py data.json data.nc` converts data.json to data.nc, providing Python and the netCDF4 library are installed.

### Flows, fluxes and timesteps
State variables that define inflow or outflows/discharges (e.g., `me%Qin` and `me%spmIn` for RiverReaches) are the inflow/outflow *per timestep*. In contrast, fluxes provided in the datafile should be in SI units, e.g. m3/s for Qin and kg/s for spmIn. These are converted by the program, depending on the length of the timestep specified in the data file. This allows us to easily change the timestep without having to change inputs in the data file.

### Main program execution
The program is executed via [main.f08](src/main.f08), which is *currently* responsible for initialising global variables and constants (`call GLOBALS_INIT()`), creating the Environment object (`env%create()`), and updating the Environment object on each timestep (`env%update(t)`).

### Creation

#### Environment%create()
`create()` is responsible for constructing the grid structure (from the data file), in turn creating GridCell objects which subsequently create the SubRiver objects they contain, and so-on down the object hierarchy. After the grid has been set up, links are made between individual SubRivers and their inflows; this operation must be performed separately so that we can ensure *all* SubRiver objects have been created before we try to link them via pointers.

#### GridCell%create(x, y, isEmpty)
This creates a GridCell object with position x, y. It constructs a reference for the GridCell, of the format "GridCell\_x\_y" and gets runoff data from the data file. It then creates the collection of SubRivers that it contains (which are again specified in the data file). GridCell size and runoff are divided by the number of SubRivers and passed to each SubRiver as length and runoff.

#### SubRiver%create(x, y, s, length, Qrunoff)
This is first responsible for allocating the size of arrays related to SPM, which are all the size of the number of SPM size classes. The length and Qrunoff are set via the input parameters, and SPM inflow and discharge (`me%spmIn` and `me%spmOut`) are initialised to 0.

A SubRiver character reference is then generated, of the format "SubRiver\_x\_y\_s" (where "s" is an integer representing the index of the SubRiver within the GridCell).

**Inflow references:** A list of inflow character references is obtained from the data file, and a temporary `me%inflowRefs` variable filled with them. The purpose of this is that, when the `Environment%create()` method comes to linking SubRivers to their inflows, it doesn't have to inquire the data file for a second time, and can simply get a list of inflows to each SubRiver from the `me%inflowsRefs` variable.

Each SubRiver can contain a collection of RiverReaches, and these are now created and passed length and Qrunoff as the SubRiver length and Qrunoff divided by the number of reaches.

#### RiverReach%create(x, y, s, r, l, Qrunoff)
In a similar fashion to `SubRiver%create()`, this method first allocates SPM-related arrays, and then generates a RiverReach character reference, of the format "RiverReach\_x\_y\_s\_r". Initial SPM densities `me%rho_spm` and masses `me%m_spm` (i.e., the mass of SPM currently within the reach) are set to zero, and runoff set to that provided by the Qrunoff parameter. *Note: This is a hack at the moment: initial_runoff in the data file actually provides runoff for every timestep.* The slope `me%S` is also obtained from the data file.

The method then checks the data file to see if a river meandering factor `me%f_m` and initial SPM input have been provided. The initial SPM input is simply a mass and `me%m_spm` is updated accordingly. **Note** that the SPM densities `me%rho_spm` cannot be updated until we know the *volume* of the reach, and the volume is dependent on the inflow rate and thus it is set by `update()`. Thus, before the first time step is simulated, RiverReaches may have a *non-zero SPM mass but zero SPM density*.

### Simulating (updating)

The length of the model timestep is set via the data file, and the [main.f08](src/main.f08) file is responsible for looping through the desired timesteps and calling the `update(t)` method on the Environment object.

#### Environment%update(t)

For the moment, the main operation the program performs is routing water and SPM through the grid structure. As such, the `update()` method loop through the grid structure and calls the `routing()` procedure on each GridCell.

A second loop through the grid structure is then needed to "finalise" the routing, setting temporary SPM discharge variables to actual SPM discharge variables. This is due to the way that SubRiver inflows are specified as pointers to the actual SubRiver objects that provide the inflow. If routing was performed for one SubRiver and that SubRiver's SPM discharge (`spmOut`) immediately set, then a downstream SubRiver (whose routing procedure we called later in the loop) would use this discharge as its inflow, as opposed to the previous timestep's discharge. That is, it would be using discharge from the wrong timestep as its inflow. Thus, we wait until the main routing loop has finished, before finally setting actual SPM discharge variables.

*Note:* `t` isn't currently used, but might be in the future to obtained time-dependent data from the data file.

#### GridCell%update()

GridCell's routing procedure simply calls the routing procedure on every SubRiver it contains.

#### SubRiver%routing()

First of all, this method loop through all of the SubRiver's inflows and sums their discharge, which forms the inflow (`Qin` and `spmIn`) to the first RiverReach in the SubRiver.

Then, we loop through all of the reaches, calling an `update` method on each, providing it with `Qin` and `spmIn` for that reach. This method (detailed below) sets `Qout` and `spmOut` for that RiverReach, and thus provides us the inflow for the downstream RiverReach (the next RiverReach in the loop iteration). This continues until we have obtained the final `Qout` and `spmOut` from the final RiverReach, which will form `Qout` and `spmOut` for the entire SubRiver. We also sum the individual SPM masses across all the reaches, to obtain a total SPM mass for the SubRiver. As detailed previous, these values are set in temporary variables until of the SubRiver routing is complete.

#### RiverReach%update()

This is where the actual routing takes place. First off, the `Qin` provided is appended by any runoff specified when we created the RiverReach to form the total inflow `me%Qin` for the cell.  *Note: Runoff is a hack at the moment: initial_runoff in the data file actually provides runoff for every timestep.* A number of procedures then calculate the dimensions of the RiverReach, using `me%Qin` (amongst other things) as an input; see `calculuateWidth1`, `calculateDepth1`, `calculateVelocity1`, `calculateArea1` and `calculateVolume1` in [classRiverReach1.f08](src/classRiverReach1.f08).

The current SPM mass `me%m_spm` is appended to by the inflow and the SPM density calculated from this mass and the RiverReach volume that's just been calculated. For each of the SPM size classes, we then calculate a settling velocity and corresponding settling rate, which is zero if the temperature-dependent water density exceeds the SPM density.

This settling rate is then used to remove a certain mass of SPM from the reach (`me%m_spm = me%m_spm - k_settle*me%m_spm`), and the density is updated accordingly. We then check if we've removed all of the sediment (i.e., `me%m_spm < 0`), and if so, set the SPM mass and density to zero.

Finally, we advect the SPM out of the cell: `spmOut = me%Qout*me%rho_spm`, and check that the advected amount is less than total SPM mass before updating the SPM mass and density again. If `spmOut > me%m_spm`, then the mass and densities are set to zero (i.e., we've removed all of the SPM.
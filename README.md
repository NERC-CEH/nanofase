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
Data input is through a NetCDF data file, whose name is set in [Globals.f08](src\Globals.f08) via the `Constants%inputFile` variable (currently data.nc). This file can be generated automatically generated from a JSON file using the Python script located at `vendor\json2netcdf\json2netcdf.py`. For example, `python vendor/json2netcdf/json2netcdf.py data.json data.nc` converts data.json to data.nc, providing Python and the netCDF4 library are installed.

### `main.f08`
The program is executed via main.f08, which is *currently* responsible for initialising global variables and constants (`call GLOBALS_INIT()`), creating the Environment object (`env%create()`), and updating the Environment object on each timestep (`env%update(t)`).

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
In a similar fashion to `SubRiver%create()`, this method first allocates SPM-related arrays, and then generates a RiverReach character reference, of the format "RiverReach\_x\_y\_s\_r". Initial SPM densities `me%rho_spm` and masses `me%m_spm` (i.e., the mass of SPM currently within the reach) are set to zero, and runoff set to that provided by the Qrunoff parameter.

The method then checks the data file to see if a river meandering factor `me%f_m` and initial SPM input have been provided. The initial SPM input is simply a mass and `me%m_spm` is updated accordingly. **Note** that the SPM densities `me%rho_spm` cannot be updated until we know the *volume* of the reach, and the volume is dependent on the inflow rate and thus it is set by `update()`. Thus, before the first time step is simulated, RiverReaches may have a *non-zero SPM mass but zero SPM density*.
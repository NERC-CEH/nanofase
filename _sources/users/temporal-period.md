# Running the model for different temporal periods

The temporal period that the model is run for is defined by the model config (or [batch config](batch.md)) and NetCDF input files:
 - The *start date* of the model run is provided by the `&run > start_date` property in the [model config file](../../config.example/config.example.nml).
 - The length of each time step is given by the `&run > timestep` property in this file, which is given in seconds.
 - The number of time steps the model runs for is given by the `t` dimension in the input NetCDF file.

Hence, the temporal period over which the model runs is the length of the `t` dimension in the NetCDF file, multiplied by the time step length from the config file. The important point here is that the model ignores the `units` attribute of the `t` dimension, which is commonly used to describe the start date of the data. Additionally, there is no check that this `units` attribute is the same as the config `start_date` property. This is intentional: It allows the same NetCDF file to be used to model different time period, which might be useful for ready the model to steady state. The upshot of this is that changing the `start_date` config option doesn't change the data that is being used in the model run, it is simply used to tell the model what the start date of the NetCDF data is.

## What if I only want to run the model for a slice of the NetCDF data's temporal period?

For example, say you have a NetCDF file with data for an entire year with a daily time step (like the [example Thames 2015 data](../../data.example/thames_tio2_2015.nc)), but you only want to run the model for one month of that year - let's say June.

The bad news is that the model doesn't currently provide a way to do this without creating a new NetCDF file. If you were to change the `start_date` config option to `2015-06-01` and `n_timesteps` to `30`, the model would actually use the first 30 time steps it encounters in the data file (for the example data, corresponding to January), but write to the output data that it was performing the calculation for June.

The good news is that there are a variety of very easy methods to slice a NetCDF file down to a given time period:

### Slicing a NetCDF file using command line utility `ncks`

If you have NetCDF installed, you almost certainly have the NetCDF command line utilities installed, including the NetCDF Kitchen Sink `ncks`. Slicing a time dimension is as easy as:

```shell
ncks -d t,start_time,end_time in.nc out.nc
```

where `start_time` and `end_time` are zero-indexed integers (use the flag `-F` if you want one-indexed). So, using our example, to slice June from the [example Thames 2015 data](../../data.example/thames_tio2_2015.nc), we slice between 152 (1 June) and 181 (30 June):

```shell
ncks -d t,152,181 thames_tio2_2015.nc thames_tio2_june2015.nc
```

### Slicing a NetCDF file in Python using `xarray`

Slicing a NetCDF file is also very easy in Python. For example, using the `xarray` library:

```python
import xarray

ds = xarray.open_dataset('thames_tio2_2015.nc')
ds_sliced = ds.sel(t=slice('2015-06-01', '2015-06-30'))
ds_sliced.to_netcdf('thames_tio2_june2015.nc')
```


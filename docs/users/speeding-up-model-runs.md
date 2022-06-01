# Speeding up model runs

If model run times are becoming an issue, there are a number of things you may wish to consider to try and speed things up.

## Compilation optimisation
- Is your compiler using all possible optimisations? If you are using the example Makefile, then compiling using the `make fast` target is a good way of making sure the compiled model is as fast as possible. If you have stability issues with `make fast`, then try `make release` instead.
- You may wish to try autoparellisation, though this might not make any difference (or even slow runs down). Using the example Makefile, you can specify `AUTOPARALLEL=1` and set `N_THREADS` to the number of threads you want to use. For example, to compile the fast target with 16-thread parallelisation: `make fast AUTOPARALLEL=1 N_THREADS=16`. 

## Output data
- Generally, writing to NetCDF files is quicker than writing to CSV, if `netcdf_write_model = 'end'`. In the config file's `&output` group, set `write_csv = .false.` and `write_netcdf = .true.`. Setting `netcdf_write_mode = 'itr'` saves memory, but writes to the NetCDF file on each time step, and so can massively slow things down.
- Play around with the other options in the `&output` group. Generally speaking, setting the `include_*` options to `.false.` will speed things up (as you're writing fewer variables to files). The exception to this is `include_waterbody_breakdown`, which is generally faster when set to `.true.` because aggregating the waterbody variable to grid cell level is done on the fly.

## Size classes and soil/sediment layers
- Suspended particulate matter (SPM) and NM size classes are configurable. Five size classes for each are most commonly used, but consider whether this is really needed for your scenario. In particular, if you are calibrating the sediment dynamics, then you don't need to model NM at all and so setting NM size classes to 1 will significantly speed up the model (`n_nm_size_classes = 1`). Note that the length of arrays such as `nm_size_classes` (diameter of each NM size class) need to match the number of NM size classes.
- The number of sediment and soil layers are also configurable. If you're not interested in one of these compartments, you could specify only one layer (`n_sediment_layers = 1` and `n_soil_layers = 1`).
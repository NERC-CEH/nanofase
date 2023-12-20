# The NanoFASE model

The NanoFASE model is a multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials. The model predicts concentrations of nanomaterials with spatial (~kms) and temporal (~days) resolution in soils, surface waters and sediments, at the catchment scale and larger. See the [project website](http://nanofase.eu/) for information about the EU Horizon 2020 project that led to the development of this model.

:::{caution}
*Currently in active development*
:::

## Quickstart guide

The easiest way to install the model is to use the [Fortran Package Manager](https://fpm.fortran-lang.org/en/install/index.html#install), which can be installed using Conda (`conda install -c conda-forge fpm`). The model also requires a recent version of GFortran, NetCDF Fortran and Git installed (see [](./getting-started/building-the-model.md)).

Clone the code (make sure to `--recurse-submodules`) and use *fpm* to install:

```shell
$ git clone git@github.com:NERC-CEH/nanofase.git --recurse-submodules
$ cd nanofase
$ fpm @install
```

Use the `--prefix <path-to-installation-dir>` flag to install the model into a directory of your choice, e.g. `fpm @install --prefix <path-to-installation-dir>`.

Test the model with the [test scenario](getting-started/test-scenario.md). Presuming the install directory is on your `$PATH`:

```shell
$ mkdir log output
$ nanofase config.example/test-scenario.example.nml
```

Or you can use fpm to run the model, without needing to install it first (i.e. omitting the `fpm @install` step above):

```shell
# Debug version
$ fpm @run -- config.example/test-scenario.example.nml
# Release version with optimisations turned on
$ fpm @release -- config.example/test-scenario.example.nml
```

Take a look at the NetCDF output file using your software of choice. For example, using Python's `xarray` to plot the pristine NM concentration in rivers at the end of the model run (2015-01-11):

```python
>>> import xarray
>>> ds = xarray.open_dataset('output/output.nc')
>>> ds['water__C_nm'].sel(t='2015-01-11').plot()
```

![Test scenario NM concentration in surface waters on 2015-01-11](img/test_scenario_water_C_nm.png)

Next steps:
- Check out [](getting-started/building-the-model.md) and [](getting-started/running-the-model.md) for more build options and basic info on running the model.
- Take a copy of the [test scenario config file](https://github.com/NERC-CEH/nanofase/blob/develop/config.example/test-scenario.example.nml) and start playing around with the config options.
- Run the fuller example scenario of nano-TiO2 in the Thames catchment in 2015, by using the [config.example.nml](https://github.com/NERC-CEH/nanofase/blob/develop/config.example/config.example.nml) file.
- Have a look at the [example input data NetCDF and constants files](https://github.com/NERC-CEH/nanofase/tree/develop/data.example) to see the kind of input data the model needs.

## Acknowledgements

This model has received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement numbers 646002 (NanoFASE), 814572 (NanoSolveIT), 862419 (SAbyNA), 862444 (ASINA) and 731032 (NanoCommons).

# The NanoFASE model

Multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials. The model predicts concentrations of nanomaterials with spatial (~kms) and temporal (~days) resolution in soils, surface waters and sediments, at the catchment scale and larger. See the [project website](http://nanofase.eu/) for information about the EU Horizon 2020 project that led to the development of this model.

:::{caution}
*Currently in active development*
:::

## Quickstart guide

Clone the code (make sure to `--recurse-submodules`), make sure you have a recent version of GFortran, CMake and NetCDF Fortran installed, and build using CMake. From the model root directory:

```shell
$ mkdir build && cd build
$ cmake .. -DCMAKE_BUILD_TYPE=Release
$ make
```

Test the model with the [test scenario](getting-started/test-scenario.md):

```shell
$ cd ..
$ build/nanofase config.example/test-scenario.nml
```

Take a look at the NetCDF output file using your software of choice. For example, using Python's `xarray`:

```python
>>> import xarray
>>> ds = xarray.open_dataset('data/output/output.nc')
>>> ds
```


## Acknowledgements

This model has received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement numbers 646002 (NanoFASE), 814572 (NanoSolveIT), 862419 (SAbyNA), 862444 (ASINA) and 731032 (NanoCommons).
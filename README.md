# NanoFASE model

🏗️ *This model is under active development. Use at your own risk!* 🏗️

Multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials. See the [project website](http://nanofase.eu/) for information about the EU Horizon 2020 project that led to the development of this model.

## Getting the code

Simply clone this repo, making sure to specify `--recurse-submodules` to pull in code for the submodules in the `vendor/` directory:

```shell
$ git clone https://github.com/nerc-ceh/nanofase.git --recurse-submodules
$ cd nanofase
```

If you forget the `--recurse-submodule` flag, then run the following from the repo directory:

```shell
$ git submodule update --init --recursive
```

## Compiling

The model code is regularly compiled using the latest version of GFortran (9.2.1 at the time of writing), and periodically using ifort 18. We recommend using GFortran and one of the following build procedures to avoid complications. The only dependency is NetCDF, which must be installed prior to building (see the Dependencies section below).

### Using `make`

An example makefile ([Makefile.example](./Makefile.example)) is provided. If you already have NetCDF installed, then compiling the model is as simple as making the `bin` directory to place your exectuble in, then running `make`:

```shell
$ cp Makefile.example Makefile
$ mkdir bin
$ make
```

The [example makefile](./Makefile.example) provides targets for debug, release and "fast" compilation. The main make target builds the debug version, whilst the `release` and `fast` targets build optimised release versions (`release` uses `-O3`, `fast` uses `-Ofast -march=native -mtune=native`). Run times can be significantly improved using the `release` and `fast` targets, and so unless you need detailed debugging or profiling, then it is recommended to use one of these.

```shell
# Build the debug version
$ make
# OR, build the release version
$ make release
# OR the fast version, with non-standard optimizations
$ make fast
```

### Using `cmake`

The model can also be built using `cmake`, using the [CMakeLists.txt](./CMakeLists.txt) file. Use the standard `cmake` procedure to compile the model:

```shell
$ mkdir build && cd build
$ cmake ..
$ make
```

Use the `-DCMAKE_BUILD_TYPE=Debug|Release` flag to control whether you want to build the debug (default) or release version.

Note that, whilst `cmake` can be used to compile the model using the `ifort` or `ifx` compilers (e.g. by using the `-DCMAKE_Fortran_COMPILER=ifort` flag), issues can arise when the installed version of NetCDF that the `cmake` script finds isn't a version compiled by an Intel compiler. Therefore, we recommend compiling using the compiler that your NetCDF installation used. As a workaround, you can explicitly point `cmake` to the correct version of NetCDF by declaring `NETCDF_INCLUDES` (include directories) and `NETCDF_LIBRARIES_F90` (link libraries) (e.g. by specifying `-DNETCDF_INCLUDES=.. -DNETCDF_LIBRARIES_F90=..`).

### Dependencies

The only dependency outside of the `vendor` directory (which are compiled from source when the model is compiled, so don't worry about these) is the [NetCDF Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) library, which must already be installed on your system. NetCDF4 is available on most default application repositories, such as those for Ubuntu and Red Hat, and this is the easiest way to install. For example, on Ubuntu:

```shell
$ sudo apt-get install libnetcdf-dev
$ sudo apt-get install libnetcdff-dev
```

If your system has multiple versions of gfortran, make sure that the NetCDF libraries are compiled using the same compiler version. Issues can occur if, for instance, you have an older version of gfortran installed that is run using the `gfortran` command, and version 9 installed that is run using `gfortran-9`. `apt-get` will default to compiling NetCDF using the `gfortran` command, and thus the compiled libraries will not be compatible with NanoFASE model code compiled using `gfortran-9`.

If you're on Windows, it is highly recommended to use Cygwin or a Linux container (e.g. using Docker) and install NetCDF using these. Otherwise, you will likely have to compile NetCDF Fortran from source.

## Running

The model requires a config file to run. An example is placed at [config.example/config.example.nml](./config.example/config.example.nml). The example is commented and should be relatively self-explanatory. Copy this to a new file and edit as you wish. Make sure the directories you specify for output data and logs exist, otherwise the model will fail. Then pass this config file as the first argument when calling the model executable. For example, if you compiled the model to `./bin/main`:

```shell
$ ./bin/main /path/to/config/file.nml
```

### Input data

The config file is responsible for telling the model where the input data are (via the `&data` group). To compile your own input data for the NanoFASE model, it is highly recommended that you use the [NanoFASE data module](https://github.com/NERC-CEH/nanofase-data). This module is responsible for (amongst other things) compiling multiple spatial and/or temporal input files into the main NetCDF input file required by the model. It is included as a submodule to this repo: [vendor/nanofase-data](./vendor/nanofase-data).

### Output data

Output data is [documented here](./doc/output.md).

## Batch runs and checkpointing

The model allows for multiple simulations to be chained together into batch runs, and model simulations to be saved and reinstated by use of checkpointing. See the following for more details:
- [Batch runs](./doc/batch.md)
- [Checkpointing](./doc/checkpointing.md)

## Example workflows

A few example workflows are provided in the [example workflows](./doc/example-workflows.md) doc.

## Acknowledgements

This model has received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement numbers 646002 (NanoFASE), 814572 (NanoSolveIT), 862419 (SAbyNA), 862444 (ASINA) and 731032 (NanoCommons).

# NanoFASE model

Multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials.

Project links:
 - [Project website](http://nanofase.eu/)
 - [Facebook](https://www.facebook.com/nanofase/)
 - [Twitter](https://twitter.com/NanoFASE_EU)

## Getting the code

Simply clone this repo, making sure to specify `--recurse-submodules` to pull in code for the submodules in the `vendor/` directory:

```shell
$ git clone https://github.com/nerc-ceh/nanofase.git --recurse-submodules
```

If you forget the `--recurse-submodule` flag, then run the following from the repo directory:

```shell
$ git submodule update --init --recursive
```

## Compiling

The model code is regularly compiled using the latest version of gfortran (9.2.1 at the time of writing), and periodically using ifort 18. We recommend using gfortran to avoid complications (`sudo apt-get install gcc gfortran`). An example makefile ([Makefile.example](./Makefile.example)) is provided. If you already have NetCDF installed, then compiling the model is as simple as making the `bin` directory to place your exectuble in, then running `make`:

```shell
$ cp Makefile.example Makefile
$ mkdir ./bin
$ make
```

### Debug vs release

The [example makefile](./Makefile.example) provides targets for both debug and release compilation. The main make target builds the debug version, whilst the `release` target builds an optimised release version. Run times can be significantly improved using the `release` target, and so unless you need detailed debugging or profiling, then it is recommended to use the `release` target.

```shell
# Build the debug version
$ make
# Build the release version
$ make release
```

### Dependencies

The only dependency outside of the `vendor` directory (which are compiled from source when the model is compiled, so don't worry about these) is the [NetCDF Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) library, which must already be installed on your system. NetCDF4 is available on most default application repositories, such as those for Ubuntu and Red Hat, and this is the easiest way to install. For example, on Ubuntu:

```shell
$ sudo apt-get install libnetcdf-dev
$ sudo apt-get install libnetcdff-dev
```

If your system has multiple versions of gfortran, make sure that the NetCDF libraries are compiled using the same compiler version. Issues can occur if, for instance, you have an older version of gfortran installed that is run using the `gfortran` command, and version 9 installed that is run using `gfortran-9`. `apt-get` will default to compiling NetCDF using the `gfortran` command, and thus the compiled libraries will not be compatible with NanoFASE model code compiled using `gfortran-9`. Updating your operating system to a recent version (e.g. Ubuntu 18 or 20) should solve these issues.

If you're on Windows and not using Cygwin or MinGW, it's likely you will have to build from source. Good luck.

## Running

The model requires a config file to run. An example is placed at [config.example/config.example.nml](./config.example/config.example.nml). The example is commented and should be relatively self-explanatory. Copy this to a new file and edit as you wish. Make sure the directories you specify for output data and logs exist, otherwise the model will fail. Edit the `CONFIG_PATH` and `CONFIG_FILE` variables in your Makefile to point to your config file. The model can then be run:

```shell
$ make run
```

Alternatively, pass the `CONFIG_PATH` and `CONFIG_FILE` explicility to the Makefile:

```shell
$ make run CONFIG_PATH=/path/to/config/ CONFIG_FILE=file.nml
```

If you wish to run without using `make`, the first command line argument is the path to the config file, which defaults to the root directory:

```shell
$ ./bin/main /path/to/config/file.nml
```

### Input data

The config file is responsible for telling the model where the input data are (via the `&data` group). To compile your own input data for the NanoFASE model, it is highly recommended that you use the [NanoFASE data module](https://github.com/NERC-CEH/nanofase-data). This module is responsible for (amongst other things) compiling multiple spatial and/or temporal input files into the main NetCDF input file required by the model. It is included as a submodule to this repo: [vendor/nanofase-data](./vendor/nanofase-data).

### Output data

Output data is [documented here](./doc/output.md).

## Batch runs and checkpointing

The model allow for multiple simulations to be chained together into batch runs, and model simulations to be saved and reinstated by use of checkpointing. See the following for more details:
- [Batch runs](./doc/batch.md)
- [Checkpointing](./doc/checkpointing.md)

## Example workflows

A few example workflows are provided in the [example workflows](./doc/example-workflows.md) doc.

## NanoFASE CLI tool

The [nanofase.py](./nanofase.py) file is a very simple command line tool for compiling/editing data and compiling/running the model. It is simply a Python wrapper for these operations. It's requirements are the same as the respective operations, e.g. [these Python packages](https://github.com/NERC-CEH/nanofase-data/blob/develop/environment.yaml) for data compilation/editing.

Compiling data:

```shell
$ ./nanofase.py compile-data /path/to/data/compilation/config.yaml
```

Editing data:

```shell
$ ./nanofase.py edit-data /path/to/data/editing/config.yaml
```

Compiling the model:

```shell
$ ./nanofase.py compile-model
```

Running the model

```shell
$ ./nanofase.py run-model /path/to/config.yaml
```

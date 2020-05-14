# NanoFASE model

Multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials.

Project links:
 - [Project website](http://nanofase.eu/)
 - [Facebook](https://www.facebook.com/nanofase/)
 - [Twitter](https://twitter.com/NanoFASE_EU)

## Compiling

The model code is regularly compiled using the latest version of gfortran (9.2.1 at the time of writing), and periodically using ifort 18. We recommend using gfortran to avoid complications. An example makefile ([Makefile.example](./Makefile.example)) is provided. If you already have NetCDF installed, then compiling the model is as simple as making the `bin` directory to place your exectuble in, then running `make`:

```shell
$ cp Makefile.example Makefile
$ mkdir ./bin
$ make
```

### Dependencies

The only dependency outside of the `vendor` directory (which are compiled from source when the model is compiled, so don't worry about these) is the [NetCDF Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) library, which must already be installed on your system. NetCDF4 is available on most default application repositories, such as those for Ubuntu and Red Hat, and this is the easiest way to install. For example, on Ubuntu:

```shell
$ sudo apt-get install libnetcdf-dev
$ sudo apt-get install libnetcdff-dev
```

If you're on Windows and not using Cygwin or MinGW, it's likely you will have to build from source. Good luck.

## Running

The model requires a config file to run. An example is placed at `config/config.example.nml`. The example is commented and should be relatively self-explanatory. Copy this to a new file and edit as you wish. Make sure the directories you specify for output data and logs exist, otherwise the model will fail. Edit the `CONFIG_PATH` and `CONFIG_FILE` variables in your Makefile to point to your config file. The model can then be run:

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

The config file is responsible for telling the model where the input data are (via the `&data` group). To compile your own input data for the NanoFASE model, it is highly recommended that you use the [NanoFASE data module](https://github.com/NERC-CEH/nanofase-data). This module is responsible for (amongst other things) compiling multiple spatial and/or temporal input files into the main NetCDF input file required by the model.

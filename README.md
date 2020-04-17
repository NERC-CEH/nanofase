# NanoFASE model

Multimedia spatiotemporal environmental fate and exposure model for engineered nanomaterials.

Project links:
 - [Project website](http://nanofase.eu/)
 - [Facebook](https://www.facebook.com/nanofase/)
 - [Twitter](https://twitter.com/NanoFASE_EU)

## Compiling and running

The model code is regularly compiled using the latest version of gfortran, and periodically using ifort 18. We recommend using gfortran to avoid complications. An example makefile ([Makefile.example](./Makefile.example)) is provided. If you already have NetCDF installed, then compiling the model is as simple as making the `bin` directory to place your exectuble in, then running `make`:

```shell
$ cp Makefile.example Makefile
$ mkdir ./bin
$ make
```

The program can then be run using

```shell
make run
```

If you wish to run without using `make`, the first command line argument is the path to the config file, which defaults to the root directory:

```shell
./bin/main /path/to/config/file
```

### Dependencies

The only dependency outside of the `vendor` directory (which are compiled from source when the model is compiled) is the [NetCDF Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) library, which must already be installed on your system. NetCDF4 is available on most default application repositories, such as those for Ubuntu and Red Hat, and this is the easiest way to install. For example, on Ubuntu:

```shell
sudo apt-get install libnetcdf-dev
sudo apt-get install libnetcdff-dev
```

If you're on Windows and not using Cygwin or MinGW, it's likely you will have to build from source. Good luck.
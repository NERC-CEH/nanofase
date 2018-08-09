# NanoFASE

Multimedia environmental fate model for engineered nanomaterials.

[**Please read the model conventions!**](doc/conventions.md)

Model documentation:
 - [Model overview](doc/overview.md)
 - [Input data requirements](doc/data-requirements.md)
 - [Config file options](doc/config.md)
 - [Using the DataInterfacer class](doc/data-interfacer.md)
 - [Conventions](doc/conventions.md)

Other documentation:
 - [Git quickstart guide](doc/git-quickstart.md)

Project links:
 - [Project website](http://nanofase.eu/)
 - [Facebook](https://www.facebook.com/nanofase/)
 - [Twitter](https://twitter.com/NanoFASE_EU)

## Compiling and running
The model code is regularly compiled using GFortran 7.3.0 and ifort 18.0.3. An example makefile (Makefile.example) is provided, which can be altered according to your requirements. The compiler is set using the `FC` variable. The path to the config file (config.nml) is set using the `CONFIG_PATH` variable, and defaults to the root directory.

Using the example Makefile, compilation is as simple as:

```shell
make
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


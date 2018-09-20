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
<<<<<<< HEAD
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

## NanoFASE Command Line Interface
The [nanofase.py](./nanofase.py) script provides an interface for running a number of frequently needed tasks related to the model. The script is simply a wrapper for shell commands, which are written to work on Bash terminals (Cygwin, MinGW, Linux etc). A number of these are in turn wrappers for the `make` command and rely on an appropriate Makefile being present (and `make` being installed) - see the [example Makefile](./Makefile.example).

Run `./nanofase.py` without any options to see all available options, but here are a few examples:
- `./nanofase.py compile` will compile the model using the Makefile
- `./nanofase.py run` will run the model using `make run`
- `./nanofase.py compile-run` will compile and then run the model
- `./nanofase.py clean-log` will remove all log files from log/
- `./nanofase.py clean-cache` will remove all cache files from cache/
- `./nanofase.py clean-comp` will run the `make clean` command to remove compilation files
- `./nanofase.py clean` will run all of the three above clean commands
- `./nanofase.py view-log` will output the latest log file to the console
- `./nanofase.py vi-log` will open the latest log file in Vim (providing it is installed)
- `./nanofase.py subl-log` will open the latest log file in Sublime Text (providing it is installed and the `subl` command is in `$PATH`)

### Running the script
Python must be installed to run the script. The script can be run either by `./nanofase.py` or `python nanofase.py`. The former requires the script by executable (`chmod u+x ./nanofase.py`) and your system be capable of interpreting the `#!` shebang to the Python interpretter.

You might wanted to consider adding an alias to save having to type `./nanofase.py` each time you want to run the script. For example, adding `alias nf="/path/to/nanofase/dir/nanofase.py"` will make the script runable by `nf <option>` from any directory.
=======
 - [Twitter](https://twitter.com/NanoFASE_EU)
>>>>>>> 6c5f94e800068f1ee31cf0467c0c59e7a27a8604

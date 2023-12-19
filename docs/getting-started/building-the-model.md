# Building the model

Before running the model, it needs to be compiled into an executable file. There are a number of ways to do this, but all require the following to be installed:
* A modern Fortran compiler, such as GFortran ≥ 7 or Intel Fortran.
* NetCDF Fortran

If you are on Linux or Mac, installing using `apt` or `brew` is easy: `apt install gfortran libnetcdf-dev libnetcdff-dev` or `brew install gcc netcdf`. If you are on Windows, we recommend using Windows Subsystem for Linux (in which case, installation is the same as for Linux using `apt`) or Cygwin (install package `libnetcdf-fortran-devel`).

```{caution}
If your system has multiple versions of GFortran installed, make sure that the NetCDF libraries are compiled using the same compiler version as you compile the NanoFASE model with. Issues can occur if, for instance, you have an older version of GFortran installed that is run using the `gfortran` command, and version 9 installed that is run using `gfortran-9`. `apt` will default to compiling NetCDF using the `gfortran` command, and thus the compiled libraries will not be compatible with NanoFASE model code compiled using `gfortran-9`.
```

## Getting the code

First, get a copy of the code by cloning the repo, making sure to specify `--recurse-submodules` to pull in code for the submodules in the `vendor/` directory:

```shell
$ git clone https://github.com/nerc-ceh/nanofase.git --recurse-submodules
$ cd nanofase
```

If you forget the `--recurse-submodule` flag, then run the following from the repo directory:

```shell
$ git submodule update --init --recursive
```

## Using fpm (Fortran Package Manager)

The easiest way to build the model is use [fpm](https://fpm.fortran-lang.org/) (Fortran Package Manger). You can install this easily using Conda (`conda install -c conda-forge fpm`). To build the model without installing:

```sh
$ fpm @build
```

The model binary will be placed in a subdirectory of the `build` folder. The `fpm @run` command can be used to run this executable (see [](./running-the-model)). Alternatively, you can install the model:

```sh
$ fpm @install
```

Optionally, you can provide an install directory using `--prefix <path-to-installation-dir>`, otherwise the model will install to a default location for your system (e.g. `/home/<user>/.local` on Linux)

## Using CMake

The model can also be built using CMake:

```sh
$ mkdir build && cd build
$ cmake ..
$ make
```

After running these commands, a executable file named `nanofase` (`nanofase.exe` on Windows) will be available in the `build` directory. Use the `-DCMAKE_BUILD_TYPE=Debug|Release` flag to control whether you want to build the debug (default) or release version (e.g. `cmake .. -DCMAKE_BUILD_TYPE=Release` for the release version). Unless you are developing the model, use the release version as the model is significantly faster. 

```{caution}
Note that, whilst CMake can be used to compile the model using the ifort or ifx compilers (e.g. by using the -DCMAKE_Fortran_COMPILER=ifort flag), issues can arise when the installed version of NetCDF that the CMake script finds isn't a version compiled by an Intel compiler (such as those installed by `apt` and `brew`). Therefore, we recommend compiling using the compiler that your NetCDF installation used. As a workaround, you can explicitly point CMake to the correct version of NetCDF by declaring NETCDF_INCLUDES (include directories) and NETCDF_LIBRARIES_F90 (link libraries) (e.g. by specifying -DNETCDF_INCLUDES=.. -DNETCDF_LIBRARIES_F90=..).
```

## Using Make

The model can also be built using `make`, and an [example Makefile](https://github.com/NERC-CEH/nanofase/blob/develop/Makefile.example) is provided to help. This Makefile only uses GFortran compiler flags, and so you will need to extend this if you wish to use with another compiler. To use, simply take a copy of this file and run `make`:

```sh
$ cp Makefile.example
$ mkdir build
$ make
```

The default target builds the model with debugging enabled. To turn on optimisations, build using `make release` (for `-O3` optimisations) or `make fast` (for `-Ofast` optimsations).

The model will be built into the `build` directory by default (a custom directory can be specified with the `$BUILD_DIR` option, e.g. `make BUILD_DIR=makebuild`). An executable file named `nanofase` will now be available in this directory.

## Other build options

We are also working towards supporting the [Fortran Package Manager](https://fpm.fortran-lang.org/) (FPM), but are not quite there yet. See the [fpm.toml](https://github.com/NERC-CEH/nanofase/blob/develop/fpm.toml) file for the work-in-progress FPM manifest file.

The model can also be compiled using Visual Studio with the Intel Fortran integration. See [](../developers/vs-ifort-setup.md) for a guide on compiling using Visual Studio 2017.

# Compiling and running the model with Intel Fortran and Visual Studio

Tested with:
- Intel Fortran 18.0 Update 2 (package 185)
- Visual Studio Community 2017 15.5.6

## Step 1, cloning the model

To get a copy of the latest version of the model from Github, use the `git clone` command:

```shell
$ git clone https://github.com/nerc-ceh/nanofase.git --recurse-submodules
```

This will create a new folder called "nanofase" and clone the model code into there. If you get authentication errors, you can pass your username and password with the URL:

```shell
$ git clone https://username:password@github.com/nerc-ceh/nanofase.git --recurse-submodules
```

The `--recurse-submodules` flat is important! If you forget it, enter the directory and run the following:

```shell
$ cd nanofase
$ git submodule update --init --recursive
```

You may also need to create a couple of new directories, if you use the default config files provided: `log` and `data/output`:

```shell
$ mkdir data/output log
```

## Step 2, creating a Fortran project

Open VS, go to `File > New > Fortran Project from Existing Code`. Fill in the details in the box that pops up as follows. For "Add files to the project from this folder", select the folder you've just cloned the model into. Untick the checkbox to "Create default virtual folders structure" (unless you actually like VS's virtual folders structure), and add `*.nml;` to the end of the "File types to add to the project" text box. Click Next.

Keep the defaults of "Application" and not 32-bit for the next screen. Click Finish.

## Step 3, selecting the correct files to compile

You now need to tag some files that shouldn't be compiled. Locate the following in the Solution Explorer window (`Ctrl+0` if it's not open):
- `src/vendor/feh/ResultFineSediment.f90`
- `vendor/datetime-fortran/tests/datetime_tests.f90`
- `vendor/feh/example/*`
- `vendor/mo_netcdf/examples/*`
- `vendor/mo_netcdf/tests/*`

Select all of these (holding `Ctrl` to select multiple files) then press `Alt+Enter` (or right click and go to `Properties`). Go to the "General" section and change "Exclude File From Build" to "Yes". The little icon by the file in the Solution Explorer should change.

## Step 4, Project properties and linking NetCDF

First off, if you have NetCDF installed, locate the directories containing the lib, bin and include files. If not, they are available for 32-bit Windows in the NanoFASE WP2 NERC Dropbox folder: `Dropbox\NanoFASE WP2 NERC\NetCDF`. In the following, the folder structure in this Dropbox folder is assumed, and `path\to\netcdf\` is the path to that folder.

Go to `Project > <project name> Properties` and change the following options:
- `General > Output directory`: Change to somewhere that you can run exes from without tripping Carbon Black.
- `General > Target directory`: If you have a Carbon Black rule that requires a particular filename convention, use that here.
- `Debugging > Command Arguments`: Add the path to your config file here. Examples are given in `config/` and it's highly recommended you use one of these. `config/calibrate.nml` is the most relevant if you wish to perform calibration, and `config/thames.nml` is the best if you wish to run the model without calibration for the whole of the Thames catchment.
- `Fortran > General > Additional Include Directories`: Set to `path\to\netcdf\NetCDF32\include; path\to\netcdf\NetCDFF\include\ia32`.
- Make sure `Fortran > Preprocessor > Additional Include Directories` is the same.
- `Fortran > Floating Point > Floating-Point Exception Handling`: Set to "Underflow gives 0.0; Abort on other IEEE exceptions (/fpe:0)", because there's something wrong if we're getting FPEs!
- `Linker > General > Additional Library Directories`: Set to `path\to\netcdf\NetCDF32\lib; path\to\netcdf\NetCDFF\lib`.
- `Linker > Input > Additional Dependencies`: Set to `path\to\netcdf\NetCDFF\lib\netcdff.lib`.

## Step 5, Building, data and running

Building (compiling) the project should now work: `Build > Build Solution` (or `Ctrl+Shift+B`), and if it does, you should have a shiny new exe to run. Now you're ready to run the model: `F5` or `Debug > Start Debugging`.

See the [README.md](../README.md) and [example-workflows.md](./example-workflows.md) for examples of how to run the model with different data.
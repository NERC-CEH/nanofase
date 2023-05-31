# Building with Intel Fortran and Visual Studio

Tested with:
- Intel Fortran 18.0 Update 2 (package 185)
- Visual Studio Community 2017 15.5.6

First, grab a copy of the code as per the instructions in [](../getting-started/building-the-model.md).

## Creating a Fortran project in Visual Studio

Open VS, go to `File > New > Fortran Project from Existing Code`. Fill in the details in the box that pops up as follows. For "Add files to the project from this folder", select the folder you've just cloned the model into. Untick the checkbox to "Create default virtual folders structure" (unless you actually like VS's virtual folders structure), and add `*.nml;` to the end of the "File types to add to the project" text box. Click Next.

Keep the defaults for the next screen. Click Finish.

## Don't compile example and test files from other vendors

You now need to tag some files that shouldn't be compiled. Locate the following in the Solution Explorer window (`Ctrl+0` if it's not open):
- `vendor/datetime-fortran/examples/*`
- `vendor/datetime-fortran/tests/*`
- `vendor/feh/example/*`
- `vendor/feh/tests/*`
- `vendor/mo_netcdf/examples/*`
- `vendor/mo_netcdf/tests/*`
- `vendor/spoof/example/*`

Select all of these (holding `Ctrl` to select multiple files) then press `Alt+Enter` (or right click and go to `Properties`). Go to the "General" section and change "Exclude File From Build" to "Yes". The little icon by the file in the Solution Explorer should change.

## Project properties and linking NetCDF

Locate the directories of your NetCDF installation, including lib, bin and include files. 

Go to `Project > <project name> Properties` and change the following options:
- `Debugging > Command Arguments`: Add the path to your config file here. Examples are given in the `config.example` folder.
- `Fortran > General > Additional Include Directories`: Set to the paths where your NetCDF `.h` and NetCDF Fortran `.mod` files are installed.
- Make sure `Fortran > Preprocessor > Additional Include Directories` is the same.
- `Fortran > Floating Point > Floating-Point Exception Handling`: Set to "Underflow gives 0.0; Abort on other IEEE exceptions (/fpe:0)", because there's something wrong if we're getting FPEs!
- `Linker > General > Additional Library Directories`: Set to the paths where your NetCDF and NetCDF Fortran `.lib` files are installed.
- `Linker > Input > Additional Dependencies`: Set to the location of your NetCDF Fortran `.lib` file, which is likely named `netcdff.lib` or similar.

## Building

Building (compiling) the project should now work: `Build > Build Solution` (or `Ctrl+Shift+B`), and if it does, you should have a shiny new exe to run. Now you're ready to run the model: `F5` or `Debug > Start Debugging`.
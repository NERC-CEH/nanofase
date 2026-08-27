# Running the model on JASMIN

The model needs GFortran 7 or later, and JASMIN only has GFortran 4.4 installed by default. GFortran 7.3.1 can be accessed on the `jasmin-sci1-test.ceda.ac.uk` machine using the Developer Toolset `devtoolset-7`. But the version of NetCDF installed was compiled with the default (v4.4) version of GFortran, and thus can't be used with GFortran 7.3.1. In addition, `jasmin-sci1-test` server that the Developer Toolset is hosted on does not have access to LOTUS or SLURM. The only work around for this is to compile NetCDF Fortran from source (using the already installed NetCDF C) on `jasmin-sci1-test`, or install GCC *and* NetCDF Fortran from source for use on the main JASMIN sci servers (and thus have access to LOTUS/SLURM).

Here are brief instructions on how to do this:

## Compiling GCC and GFortran

This is surprisingly easy, but takes *a long time*. These instructions are based on the instructions given by GCC: [https://gcc.gnu.org/wiki/InstallingGCC](https://gcc.gnu.org/wiki/InstallingGCC).

For the following I will assume you download all source code into `~/software_src` and install all software into `~/software`, but you can obviously change these locations to wherever you want (as long as you have permissions for that directory).

First, clone the source code and checkout the v9.2.0 release. The repo is largely and cloning takes ages:

```bash
$ mkdir ~/software_src
$ cd software_src
$ git clone https://github.com/gcc-mirror/gcc
$ cd gcc
$ git checkout releases/gcc-9.2.0
```

You need to download the source code for a few dependencies, and this can be done automatically using the script provided:

```bash
$ ./contrib/download_prerequisites
```

Make object and install directories, e.g. at `~/software/gcc/obj` and `~/software/gcc/install` and move to the object directory:

```bash
$ mkdir ~/sofware/gcc/obj ~/software/gcc/install
$ cd ~/software/gcc/obj
```

Now you need to run the configure script. This needs a couple of config parameters to install correctly on JASMIN. GCC doesn't like you using relative paths:

```bash
$ ~/software_src/gcc/configure --prefix=/home/users/<username>/software/gcc/install --enable-languages=c,fortran --disable-multilib
```

Now make and install. The `make` step takes a *long* time (hours):

```bash
$ make
$ make install
```

## Linking libraries

Now you need to modify the `PATH`, `LD_LIBRARY_PATH` and `LD_RUN_PATH` variables to point to the version of GCC you've just installed. The best way of doing this is to add the following to the bottom of your `~/.bashrc` file:

```bash
export PATH=~/software/gcc/install/bin:$PATH
export LD_LIBRARY_PATH=~/software/gcc/install/lib64:~/software/gcc/install/lib/gcc/x86_64-pc-linux-gnu/9.2.0:~/software/gcc/install/libexec/gcc/x86_64-pc-linux-gnu/9.2.0:$LD_LIBRARY_PATH
export LD_RUN_PATH=~/software/gcc/install/lib64:~/software/gcc/install/lib/gcc/x86_64-pc-linux-gnu/9.2.0:~/software/gcc/install/libexec/gcc/x86_64-pc-linux-gnu/9.2.0:$LD_RUN_PATH
```

Logout and back in to set these variables, and now you can install NetCDFF.

## Compiling NetCDF Fortran

Clone netcdf-fortran from GitHub and checkout to the v4.4.1 release (don't use the latest release, because that requires a later version of NetCDF-C than is already installed):

```bash
$ git clone https://github.com/Unidata/netcdf-fortran
$ cd netcdf-fortran
$ git checkout v4.4.1
```

The [instructions for compiling NetCDF Fortran](https://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html) from Unidata can now be followed, which a few modifications:

Set `NCDIR` to where NetCDF-C is installed, which on JASMIN is `/usr`. Set `FC` to `gfortran`:

```bash
NCDIR=/usr
FC=gfortran
```

Set `NFDIR` to the directory where you want to install NetCDF Fortran. I'll assume this is at `~/software/netcdff`: 

```bash
mkdir ~/software/netcdff
NFDIR=~/software/netcdff
```

Set CPPFLAGS to the directories for NetCDF-C. Note that on JASMIN, the lib directory is `lib64`, not `lib`:

```bash
CPPFLAGS=-I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib64
```

Now you can configure and compile:

```bash
./configure --prefix=${NFDIR}
make check
make install
```

You need to update the `LD_LIBRARY_PATH` variable again, to include the NetCDF Fortran lib directory. Amend the `export LD_LIBRARY_PATH` line in your `~/.bashrc` file:

```
export LD_LIBRARY_PATH=~/software/netcdff/lib:~/software/gcc/install/lib64:~/software/gcc/install/lib/gcc/x86_64-pc-linux-gnu/9.2.0:~/software/gcc/install/libexec/gcc/x86_64-pc-linux-gnu/9.2.0:$LD_LIBRARY_PATH
```

### Issues with Jaspy

Loading Jaspy (e.g via `module load jaspy`) can cause issues. For example, it is likely that your `$PATH` environment variable gets modified to include Jaspy directories first, thus overiding the changes you made to point it towards your `GCC` installation. The easiest way around this is to compile the model *before* loading Jaspy, or after you have unloaded it (`module unload jaspy`).

## Compiling the model

The model Makefile needs to be edited to point to the correct version of NetCDF. Change the variable `NETCDF` to point to the directories you just installed NetCDF Fortran in. For example, if you installed to `~/software/netcdff`, then:

```bash
NETCDF = -I/home/users/<username>/software/netcdff/include -L/home/users/<username>/software/netcdff/lib -lnetcdff -lnetcdf
```

where `<username>` is your JASMIN username. Running the `nf-config` script at `~/software/netcdff/bin/nf-config` will give you this information:

```bash
$ ~/software/netcdff/bin/nf-config --flibs --fflags
```

You should now be able to compile the model using the edited Makefile.

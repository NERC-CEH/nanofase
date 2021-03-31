# Changelog

All notable changes to the model will be documented in this file. Breaking changes (i.e. those that cause changes to the model's interface) are denoted by ⚠️

## [Unreleased]

### Added

- ⚠️ Added option to write to NetCDF file. Specify `&output > write_netcdf = .true.` to use. This requires the compilation of a new Fortran source file, `src/Data/NetCDFOutputModule.f90`, and your compilation process will need ammending. The [example Makefile](./Makefile.example) has been updated accordingly. NetCDF output variables are stored in memory for the entire model run (as opposed to CSV files, which are written iteratively on each time step), which means choosing to write to NetCDF may use significantly more memory. 
- Along with this, added the option for NetCDF output to be chunked when the model is run in batch mode. If `&output > chunk_netcdf = .true.`, then a NetCDF file is written at the end of every chunk in the batch run. If `.false.`, the NetCDF is written at the end of the entire batch. As output variables are stored in memory, for particularly long batch runs, memory usage may be very high for non-chunked NetCDF output. The `nco` command line tools can be used to easily merge chunked NetCDF files after the run, e.g. using [`ncrcat`](http://nco.sourceforge.net/nco.html#ncrcat).

### Changed

- ⚠️ `src/CheckpointModule1.f90` renamed to `src/CheckpointModule.f90`. Your compilation process may need ammending. The [example Makefile](./Makefile.example) has been updated.
- Submodule `vendor/mo_netcdf` updated to latest version to allow the creation of scalar variables.
- Submodule `vendor/json2netcdf` removed.

## [0.0.1] - 2021-03-20

### Added

- Added option to aggregate CSV output for waterbodies at grid cell level, rather than breaking it down to waterbody level. Internal functions for aggregating to grid cell added (e.g. weighted means, fetching outflow reaches). This option can be used by specifying `&output > include_waterbody_breakdown = .false.` in the [model config file](./config.example/config.example.nml). Default is `.true.`.
- This changelog.

[unreleased]: https://github.com/nerc-ceh/nanofase/compare/0.0.1...HEAD
[0.0.1]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.1
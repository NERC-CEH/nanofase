# Changelog

All notable changes to the model will be documented in this file. Breaking changes (i.e. those that cause changes to the model's interface) are denoted by ⚠️. Until v1.0.0, breaking changes might occur between minor versions and patches. Thereafter, semantic versioning will be followed.

## [Unreleased]

### Added

- Documentation for running the model for different time periods.
- Added `output_hash` config option. The value specified here will be appended to output file names (e.g. "output_water.csv" will become "output_water{output_hash}.csv"). Particularly useful when performing parallel model runs to avoid multiple model runs writing to the same file.

### Changed

- Changed "untransformed" to "pristine" in output data nomenclature, to describe NM that hasn't undergone any chemical transformation. This brings the model nomenclature in line with literature conventions.

### Fixed

- Fixed bug where `output_soil__land_use` and `output_soil__bulk_density` attribute of `NetCDFOutput` class weren't being deallocated before being allocated between chunks in a batch run.

## [0.0.2] - 2021-05-21

### Added

- ⚠️ Added option to write to NetCDF file. Specify `&output > write_netcdf = .true.` to use. This requires the compilation of a new Fortran source file, `src/Data/NetCDFOutputModule.f90`, and your compilation process might need ammending. The [example Makefile](./Makefile.example) has been updated accordingly. NetCDF output variables are stored in memory for the entire model run (as opposed to CSV files, which are written iteratively on each time step), which means choosing to write to NetCDF may use significantly more memory. 
- Along with this, added the option `&output > netcdf_write_mode`, to choose when NetCDF files are written to. If `itr`, the NetCDF file is written to on every timestep. If `end`, the NetCDF file is written to at the end of the each chunk (which is the end of the model run, if not running in batch mode). `itr` is slower, but output variables aren't stored in memory for the duration of the run, and so it uses less memory. `end` is faster, but for particularly long chunks or geographically extensive runs, memory use might be prohibitively high.
- Added option to aggregate CSV output for bed sediments at grid cell level (extending functionality added in 0.0.1 to do so for waterbodies). This option can be used by specifying `&output > include_waterbody_breakdown = .false.` in the [model config file](./config.example/config.example.nml)
- Added option to compile with `cmake` using the [CMakeLists.txt](./CMakeLists.txt) file. In theory this should work with GFortran and Intel Fortran, though testing on the latter has been limited.

### Changed

- ⚠️ Renamed files - your compilation process may need ammending. The [example Makefile](./Makefile.example) and [CMakeLists.txt](./CMakeLists.txt) have been updated.
    - `src/CheckpointModule1.f90` → `src/CheckpointModule.f90`
    - `src/DataOutputModule1.f90` → `src/DataOutputModule.f90`
- Submodule `vendor/mo_netcdf` updated to latest version to allow the creation of scalar variables. You might need to `git submodule update`.
- Submodule `vendor/json2netcdf` removed. You might need to remove this folder manually after pulling changes.

## [0.0.1] - 2021-03-20

### Added

- Added option to aggregate CSV output for waterbodies at grid cell level, rather than breaking it down to waterbody level. Internal functions for aggregating to grid cell added (e.g. weighted means, fetching outflow reaches). This option can be used by specifying `&output > include_waterbody_breakdown = .false.` in the [model config file](./config.example/config.example.nml). Default is `.true.`.
- This changelog.

[unreleased]: https://github.com/nerc-ceh/nanofase/compare/0.0.2...HEAD
[0.0.2]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.2
[0.0.1]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.1

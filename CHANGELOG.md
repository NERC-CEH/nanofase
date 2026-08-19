# Changelog

All notable changes to the model will be documented in this file. Breaking changes (i.e. those that cause changes to the model's interface) are denoted by ⚠️. Until v1.0.0, breaking changes might occur between minor versions and patches. Thereafter, semantic versioning will be followed.

## [Unreleased]

## [0.0.7] - 2026-08-19

### Fixed

- Fixed `V_w_b` being carried between iterations in `BedSediment%deposit` (`depositSediment1`). It is assigned only inside the `if (O%A_f(s) > 0 .or. O%A_w(s) > 0)` capacity check but consumed unconditionally when tallying `V_w_tot`, so a layer with no free capacity re-added the previous iteration's value, compounding once per sediment layer and inflating the water volume returned to the water column. It is now reset at the top of each layer iteration. `V_w_tot` only feeds the reporting-only depth adjustment in `depositToBed`, so this makes no difference to model results (verified bit-identical over the Thames scenario), but the previous behaviour was unbounded.
- `water__depth` (and the `depth(m)` CSV column) now report the hydraulic depth implied by the reach volume, `volume / bedArea`, rather than the `me%depth` attribute. `depositToBed` subtracts the water displaced into the bed sediment from `me%depth`, which left the reported depth inconsistent with the reported `water__volume` and `sediment__bed_area` by ~1e-4 relative.
- Fixed an error stop in `depositToBed` when `&sediment > include_bed_sediment = .false.`. `V_water_toDeposit = .dp. depositRslt` sat outside the `includeBedSediment` block, so `depositRslt` was never assigned and asking it for its data aborted the run on the first time step with "Error trying to return 0D data as REAL(DP)". The extraction is now inside the block and `V_water_toDeposit` defaults to zero.

## [0.0.6] - 2026-08-18

### Fixed

- Fixed the grid-cell SPM flux getters `get_j_spm_outflow` and `get_j_spm_inflow` in `GridCellModule`, which summed each reach's `Q%outflow`/`Q%inflow` (water discharge) instead of `j_spm%outflow`/`j_spm%inflow` (sediment mass). As `Q%outflow` is a scalar being added to a size-class array it was broadcast across all size classes, so the aggregated output variables `water__m_spm_outflow` and `water__m_spm_inflow` were reporting exactly `n_spm_size_classes * Q%outflow` in m3, rather than a sediment flux in kg. This only affected the aggregated output (`&output > include_waterbody_breakdown = .false.`); the per-waterbody output path already used `j_spm` and was correct.
- Fixed `dj_spm_inflow` (and the equivalent NM, transformed NM and dissolved terms) leaking between displacements in `EstuaryReachModule`. These are set to the per-displacement share of the reach's inflow before the displacement loop, but on an incoming tide the loop overwrites them with what the reach pushes back upstream. As they were never reset, that overwritten value persisted into every subsequent displacement of the timestep, so the inflow term applied to later displacements depended on whether an earlier displacement happened to be a flood tide. They are now reset at the top of each displacement.
- Fixed an incoming tide contributing no input to `Q_in_total` in `EstuaryReachModule`. The provisional outflow is computed into a local `Q_outflow`, but the `if (Q_outflow > 0)` branch added `me%Q%outflow`, which `emptyFlows()` has zeroed at that point and which is only accumulated later in the displacement loop, so the branch added nothing. It now adds `Q_outflow`. Note this changes estuary results: `Q_in_total` feeds the reach width, depth, velocity and resuspension rate.

## [0.0.5] - 2025-03-15

### Changed

- The `water_temperature` variable in the constants data file has been replaced with `min_water_temperature`, `max_water_temperature` and `min_water_temperature_day_of_year`. Providing `water_temperature` will throw an error.
- ANSI escape character now uses `char(27)` instead of backslash escape sequences, so `-fbackslash` (GFortran) and `-assume bscc` (Intel) compiler options no longer needed.
- ⚠️ The example config files now save output data to `output`, instead of `data/output`.

## [0.0.4] - 2021-12-08

### Added

- Config option to include bank erosion or not: `&water > include_bank_erosion`. Defaults to true.

### Changed

- ⚠️ Model version number is now obtained from the `src/VersionModule.f90` file. This is dynamically updated at build time by the [example Makefile](./Makefile.example) or [CMakeLists.txt](./CMakeLists.txt) file, and if you are not using one of these build processes, should ensure that yours does the same. In short, the output of `git describe --tags` should be used for the `modelVersion` variable in `src/VersionModule.f90`. For example, this can be achieved in a Makefile by the use of `sed`: `sed -i "s/\".*\"/\"${git describe --tags}\"/g" src/VersionModule.f90`. Note that the version number in this file and the [CITATION.cff](./CITATION.cff) file still needs bumping on each release. Build process will need updating.
- ️️️️Lots of file, module and type names have been updated to follow a coherent convention. See the updated [Makefile.example](./Makefile.example).

## [0.0.3] - 2021-08-05

### Added

- Warm up period: You can now run the model before inputting NM using the `&run > warm_up_period` config option, which runs the model for the number of time steps specified. The data that is used is sliced from the start of the dataset (e.g. `warm_up_period = 10` would use the first 10 time steps' data), but no NM is input to the environment. The warm up period must be fewer time steps than the model run (or the first chunk of the run, if running in batch mode).
- Checkpointing now has the ability to save a checkpoint after the warm up period, specified by the `&checkpoint > save_checkpoint_after_warm_up` option. If both `save_checkpoint` and `save_checkpoint_after_warm_up` are specified, the checkpoint at the end of the model run will overwrite the checkpoint file saved after the warm up.
- Documentation for running the model for different time periods.
- Added `output_hash` config option. The value specified here will be appended to output file names (e.g. "output_water.csv" will become "output_water{output_hash}.csv"). Particularly useful when performing parallel model runs to avoid multiple model runs writing to the same file.
- `make fast` target added to example Makefile, which turns on `-Ofast -march=native -mtune=native` flags. Use with caution.
- Config option to not include estuary: `&waterbody > include_estuary`. Defaults to true. If `include_estuary = .false.`, the `is_estuary` NetCDF variable is ignored.

### Changed

- Changed "untransformed" to "pristine" in output data nomenclature, to describe NM that hasn't undergone any chemical transformation. This brings the model nomenclature in line with literature conventions.
- ⚠️ NM transfer between bed sediment layers uses a NxN mass transfer matrix, where N is the number of sediment layers. This tends to end up quite a sparse matrix, and is the biggest bottleneck in the model. To speed things up, CSR storage is now being used for this matrix, requiring the compilation of two new files: `vendor/spoof/sparskit.f90` and `vendor/spoof/Spoof.f90`. Your Makefile might need updating. SPOOF is a new library: SParse matrices in Object-Oriented Fortran, which acts as a modern Fortran interface over the [SPARSKIT library](https://www-users.cse.umn.edu/~saad/software/SPARSKIT/) (so far only CSR and diagonal matrices, and matrix by vector multiplication, are implemented). 

### Fixed

- Fixed bug where `output_soil__land_use` and `output_soil__bulk_density` attribute of `NetCDFOutput` class weren't being deallocated before being allocated between chunks in a batch run.

### Removed

- ️️⚠️ Removed depracated calibration method, including config file `calibrate` group. You will need to remove this from any config files to avoid errors. The `src/classSampleSite.f90` has been removed, and may need removing from your Makefile.

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

[unreleased]: https://github.com/nerc-ceh/nanofase/compare/0.0.7...HEAD
[0.0.7]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.7
[0.0.6]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.6
[0.0.5]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.5
[0.0.4]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.4
[0.0.3]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.3
[0.0.2]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.2
[0.0.1]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.1

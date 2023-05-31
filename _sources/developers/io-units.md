# Input/output units

The following conventions are used when reading and writing from files in the model. It is worth bearing these in mind if extending the model, to make sure you avoid conflicts. Not all units within these ranges are used, but they are given as ranges to ensure future changes to the model aren't breaking changes.

- Config files: `1` to `10`, avoiding `0`, `5` and `6` to avoid conflicts with `stderr`, `stdin` and `stdout`.
- Input data files: Constants namelist files are units `10` to `20`. NetCDF file units are dealt with by the UniData NetCDF-Fortran library.
- Output data files: `100` to `110`.
- Checkpoint files: `500` to `510`.

If you absolutely need to change these variables, they are set in [src/DefaultsModule.f90](../../src/DefaultsModule.f90)
# Changelog

All notable changes to the model will be documented in this file.

## [Unreleased]

## [0.0.1] - 2021-03-20

### Added

- Added option to aggregate CSV output for waterbodies at grid cell level, rather than breaking it down to waterbody level. Internal functions for aggregating to grid cell added (e.g. weighted means, fetching outflow reaches). This option can be used by specifying `&output > include_waterbody_breakdown = .false.` in the [model config file](./config.example/config.example.nml). Default is `.true.`.
- This changelog.

[unreleased]: https://github.com/nerc-ceh/nanofase/compare/0.0.1...HEAD
[0.0.1]: https://github.com/nerc-ceh/nanofase/releases/tag/0.0.1
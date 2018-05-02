# Sources

The NanoFASE model seperates nanomaterial sources into two categories: *Point sources* and *diffuse sources*. The latter represents spatially-averaged depositions over an entire `GridCell` (e.g. from atmospheric deposition), whilst the former provides an input of nanomaterials at a specific location along a waterbody. Thus, `DiffuseSource` objects are contained within `GridCell`s, whilst `PointSource` objects are contained within specific surface water objects (e.g. `RiverReach`, `EstuaryReach`, `LakeSegment` or `SeaSegment`):

- &#128461; `GridCell`
    - `DiffuseSource` - *not currently implemented*
    - &#128461; `RiverReach`
        - `PointSource`
    - &#128461; `EstuaryReach` - *not currently implemented*
        - `PointSource`
    - &#128461; `LakeSegment` - *not currently implemented*
        - `PointSource`
    - &#128461; `SeaSegment` - *not currently implemented*
        - `PointSource`

As is [convention](/doc/conventions.md) for input data, units are SI units (kg).

## `PointSource`

The [input data file](/doc/data-requirements.md) is responsible for specifying inputs of nanomaterials from point sources. Either or both a *fixed mass* or a *time-varying* input can be specified. If one or both are not specified, the model defaults them to zero.

### Fixed mass

A fixed mass source can be provided by the `fixed_mass` field:

```json
"PointSource": {
    "fixed_mass[state][form][n]": 0.001,
    "fixed_mass_frequency": 10
}
```

The dimensions of `fixed_mass` correspond to how [nanomaterial form and state](/doc/nanomaterial-form-and-state.md) are specified within the NanoFASE model.
- `state`: Nanomaterial state (free, bound to solid, heteroaggregated). Length: 2 + number of SPM size classes.
- `form`: Nanomaterial form (core, shell, coating, corona). Length: 4.
- `n`: Number of nanomaterial size classes.

This fixed mass is input to the waterbody every `fixed_mass_frequency` days. So, for the above example, 0.001 kg of nanomaterial (in every state/form/size class) is emitted every 10 days. *(Note that `fixed_mass_frequency` isn't actually a frequency.)*

### Time-varying input

A time-varying input can be specified by the `variable_mass` field:

```json
"PointSorce": {
    "variable_mass[state][form][n][t]" : 0.001
}
```

The extra dimension `t` represents time, and thus has length of the number of time steps the model is run for, allowing a different nanomaterial input to be specified for each time step. Of course, the example above would result in 0.001 kg of nanomaterial (in every state/form/size class) on every time step, and so a fixed mass input with `fixed_mass_frequency = 1` could be used instead.


## `DiffuseSource`

*Diffuse sources aren't currently implemented. Check back soon!*
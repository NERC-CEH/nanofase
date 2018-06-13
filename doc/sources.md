# Sources

The NanoFASE model seperates nanomaterial sources into two categories: *Point sources* and *diffuse sources*. The latter represents spatially-averaged depositions over an entire `GridCell` or portion of a `GridCell` (e.g. from atmospheric deposition to `GridCell`, from sewage sludge to a specific `RiverReach`), whilst the former provides an input of nanomaterials at a specific location along a waterbody. Thus, `DiffuseSource` objects are contained within `GridCell`s or reaches (`RiverReach` and `EstuaryReach`), whilst `PointSource` objects are contained only within surface waterbodies (e.g. `RiverReach`, `EstuaryReach`, `LakeSegment` or `SeaSegment`):

- &#128461; `GridCell`
    - &#128461; `DiffuseSource` - *not currently implemented*
    - &#128461; `RiverReach`
        - &#128461; `PointSource`
    - &#128461; `EstuaryReach` - *not currently implemented*
        - `PointSource`
    - &#128461; `LakeSegment` - *not currently implemented*
        - `PointSource`
    - &#128461; `SeaSegment` - *not currently implemented*
        - `PointSource`

As is [convention](/doc/conventions.md) for input data, units are SI units (kg for point sources, kg/m2 for diffuse sources).

## `PointSource`

The [input data file](/doc/data-requirements.md) is responsible for specifying inputs of nanomaterials from point sources. Either or both a *fixed mass* or a *time-varying* input can be specified. If one or both are not specified, the model defaults them to zero.

### Fixed mass

A fixed mass source can be provided by the `fixed_mass` field:

```json
"RiverReach_1_1_1": {
    "PointSource": {
        "fixed_mass[state][form][n]": 0.001,
        "fixed_mass_frequency": 10
    }
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
"RiverReach_1_1_1": {
    "PointSorce": {
        "variable_mass[state][form][n][t]" : 0.001
    }
}
```

The extra dimension `t` represents time, and thus has length of the number of time steps the model is run for, allowing a different nanomaterial input to be specified for each time step. Of course, the example above would result in 0.001 kg of nanomaterial (in every state/form/size class) on every time step, and so a fixed mass input with `fixed_mass_frequency = 1` could be used instead.

### Multiple `PointSource`s
The `PointSource` group can be suffixed by an integer to allow extra point sources to be specified. The integers must be in sequence, and either `PointSource` *or* `PointSource_1` can be used for the first point source.

```json
"RiverReach_1_1_1": {
    "PointSource_1": { ... },
    "PointSource_2": { ... },
    "PointSource_3": { ... }
}
```


## `DiffuseSource`

Diffuse sources provide a way of inputting spatially-averaged nanomaterial inputs to an entire `GridCell`, for example representing atmospheric deposition. The data structure for such source is simple; the data input file simply needs a time series of nanomaterial inputs (in units of kg/m2):

```json
"GridCell_1_1": {
    "DiffuseSource": {
        "input_mass[state][form][n][t]": 0.001
    }
}
```

The dimensions are the same as for the time-varying input of point sources. Alternatively, a diffuse source can be provided for a specific water body:

```json
"RiverReach_1_1_1": {
    "DiffuseSource": {
        "input_mass[state][form][n][t]": 0.001
    }
}
```

### Multiple `DiffuseSource`s
Similarly to `PointSource`, multiple diffuse sources can be specified in the input file by appending an integer reference to the source's group. This follows the same convention as for `PointSource`.

```json
"GridCell_1_1": {
    "DiffuseSource_1": { ... },
    "DiffuseSource_2": { ... },
    "DiffuseSource_3": { ... }
}
```

# Input size class, state and form

It is likely that available input data will not discretise input masses into size class, state and form fractions, and so care must be taken when structuring the data file to account for this.:
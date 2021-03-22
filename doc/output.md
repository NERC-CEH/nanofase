# Data output

Data output is handled by the [`DataOutput`](../src/Data/DataOutputModule.f90) class and configured in the [model config file](../config/config.example.nml). Output is to a series of text files:

- `summary.md`: A plain-text summary of the model simulation, including compartmental mean PECs. These means are simply a summary of what is output in the following CSV files.
- `output_water.csv`: Output data for every waterbody.
- `output_sediment.csv`: Output data for each waterbody's bed sediment.
- `output_soil.csv`: Output for each soil profile.
<!-- - `output_soil_biota.csv`: Output data for biota in each soil profile. -->
<!-- - `output_water_biota.csv`: Output data for biota in each water body. -->

## Configuration

The `&output` group in the [model config file](../config.example/config.example.nml) is responsible for configuring data output. The following options are available:

- `write_metadata_as_comment`: Should metadata (largely column descriptions) be writen to the header of each CSV file as `#` delimited comments? Top tip: If you turn metadata on and are using Pandas to read the data, then make sure to use the `comment` argument when reading the CSV: `df = pd.read_csv('output_water.csv', comment='#')`. *Defaults to true.* 
- `include_waterbody_breakdown`: For surface waters, should data for each waterbody be output, or should it be aggregated to grid cell level? *Defaults to true.*
- `include_sediment_layer_breakdown`: Should data for each sediment layer be output, or only summaries of the whole sediment profile? *Defaults to true.*
- `include_soil_layer_breakdown`: Should data for each soil layer be output, or only summaries of the whole soil profile? *Defaults to true.*
- `include_soil_state_breakdown`: Should the breakdown of soil NM into free and attached states be output? Bear in mind that outputting a layer and state breakdown may result in particularly large output files. *Defaults to false.*
- `include_sediment_fluxes`: Should the fluxes of sediment from/to waterbodies be included in the output (e.g. soil erosion, bank erosion, deposition/resuspension).

In addition, two options to specific the units for concentrations (kg/m3 or kg/kg) in soils and sediments will soon be available: `soil_pec_units` and `sediment_pec_units`. Watch this space.

## Output files

Column descriptions are given in the file metadata (if `write_metadata_as_comment` is set to `.true.`). For water, each row represents an individual waterbody on a timestep, if `include_waterbody_breakdown` is `.true.`, or aggregated to grid cell level if `.false.`. For soils and sediment, each row represents the entire soil/sediment profile in a grid cell on a particular timestep.

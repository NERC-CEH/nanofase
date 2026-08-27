# Flow direction and DEM conditioning

The model routes water by using a flow direction raster, which uses standard eight-direction (D8) flow direction enconding. In `nfdata`, this flow direction can either be provided by the `flow_dir` parameter, or `nfdata` can attempt to deduce it from a digital elevation model (DEM) provided by the `dem` parameter.

:::{admonition} `flow_dir` or `dem` as a basis for the grid
:class: important
Either the `flow_dir`, or if `flow_dir` isn't present, the `dem` variable, is used as the basis for the grid that defines the NetCDF file output by `nfdata`. That is, parameters such as the CRS, bounds, resolution and mask are obtained from this variable. `nfdata` does not do any reprojection of rasters from other grid systems (therefore, all provided rasters should be the same CRS and resolution), but it does clip rasters to the extent of `flow_dir`/`dem`. This clipping includes masking over nodata values in `flow_dir`/`dem`, and therefore it is one of these variable that decides which grid cells are modelled.
:::

## Directly providing `flow_dir`

```{figure} ../img/flowdir.gif
---
align: right
---
```

If the `flow_dir` variable is present in `nfdata`'s config, this will be used as the flow direction (regardless of whether `dem` is present). This variable should be a raster (openable by [Rasterio](https://rasterio.readthedocs.io/en/latest/index.html)) and have an integer data type with either D8 values or nodata values.

This flow direction _must_ be resolved, otherwise running the model with it will fail. By resolved, we mean that all non-masked (nodata) cells must have a non-zero D8 value (1, 2, 4, 8, 16, 32, 64, 128), and that these values must not result in "pools" where cells flow into each other (e.g. neighbouring cells with the northerly cell having `flow_dir=4` and a southerly cell having `flow_dir=64`).

The `flow_dir` group in the config file must contain a `path`, and optionally a `crs` value. The CRS is usually deduced from the provided raster, but in the case of rasters with ill-defined CRSs, the `crs` option gives the ability to override this. Note that this _does not_ reproject the raster to this CRS, it simply assigns the CRS to the raster.

## Calculating `flow_dir` from a DEM

Alternatively, if the `dem` variable is present and `flow_dir` is not, the model will _attempt_ to deduce the flow direction from this DEM. To achieve this, either a _conditioned_ DEM should be provided, or the config option `condition_dem` must be present to instruct `nfdata` to attempt to condition the DEM.

By _conditioned_, we mean that no pits, depressions or flats should be present in the DEM, as these will prevent a fully-resolved flow direction from being calculated. If an unconditioned DEM is provided (and the `condition_dem` option is not specified), `nfdata` will raise an error. There are [numerous](https://mattbartos.com/pysheds/) [tools](https://richdem.readthedocs.io/) [available](https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html) that can help condition DEMs.

If `condition_dem` is present, `nfdata` will attempt to condition the DEM using [Whitebox Tools' `breach_depressions_least_cost` routine](https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#BreachDepressionsLeastCost). This is a fairly robust tool that is usually able to resolve most DEMs without significant alterations, but it might not work in all cases. `condition_dem` can either be `True`, or be a dict of further config options:

```yaml
condition_dem:
  # If `save_dem_to_path` is set, the conditioned DEM will be saved to this path
  # (even if the conditioning hasn't fully worked). If not, a temporary directory
  # is used and the resulting DEM is deleted after the processing.
  save_dem_to_path: ./dem.tif
  # Additional config options specified here will be passed to `breach_depressions_least_cost`
  # See https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#BreachDepressionsLeastCost
  # If a value for `callback` is provided, it is ignored as we implement our own callback to
  # deal with errors. The only required (by Whitebox) parameter is `dist`, which specifies
  # the maximum search distance for breach paths in cells. The default value used in nfdata is 10000.
  dist: 10000
```

:::{important}
Currently, `nfdata`'s flow direction algorithm assumes that nodata (masked) cells and cells immediately outside of the DEM's bounding box are at a very low elevation, therefore forcing the flow direction from neighbouring cells towards them. Effectively, this means that nodata cells and the region outside of the DEM are treated as waterbodies into which neighbouring cell drain. Therefore, we only recommend to use this flow direction calculation functionality if this is a valid assumption for your scenario, otherwise you should provide a pre-resolved flow direction raster.
:::


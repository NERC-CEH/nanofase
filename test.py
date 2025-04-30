import xarray
ds = xarray.open_dataset('output/output.nc')
ds['water__C_nm'].sel(t='2015-01-11').plot()
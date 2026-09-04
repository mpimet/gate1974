---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.4
  kernelspec:
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

### Converting GATE Dropsonde profiles from NetCDF to zarr

```python
import pathlib

import numpy as np
import xarray as xr
```

```python
root_path="/Users/m300083/Projekte/GATE_v3.2p1"
zarr_file="GATE_DROPSONDES.zarr"
```

```python
def open_dataset(ncfile):
    """Open dropsonde datasets and prepare for concatenation."""
    ds = xr.open_dataset(ncfile).squeeze()

    launch_lon, launch_lat = ds.attrs.get("launch_start_position", "NaN NaN").split()

    return (
        ds.assign(
            launch_lat=((), np.float32(launch_lat), {"units": "degrees_north"}),
            launch_lon=((), np.float32(launch_lon), {"units": "degrees_east"}),
            platform=ds.platform,
            time=ds.flight_time,
        )
        .drop_vars("flight_time")
        .swap_dims(level="time")
    )

def time_dependent(ds):
    """Return time **dependent** subset of a dataset."""
    return ds[[var for var in ds.variables if "time" in ds[var].dims]]


def time_independent(ds):
    """Return time **independent** subset of a dataset."""
    return ds[[var for var in ds.variables if "time" not in ds[var].dims]]
```

**Find all NetCDF files**

```python
root_dir = pathlib.Path(root_path+"/AIRCRAFT")
ncfiles = root_dir.glob("C13*_DROPSONDE/*.nc")
```

**Open all NetCDF files and find the length of the records**

```python
datasets = [open_dataset(ncfile) for ncfile in ncfiles]
lengths = np.array([ds.sizes["time"] for ds in datasets], dtype=np.uint32)
```

**Concatenate along `time`, and merge along (newly created) `sonde` dimension**

```python
ds = xr.merge(
    (
        xr.concat(
            [time_dependent(ds) for ds in datasets],
            dim="time",
            combine_attrs="drop_conflicts",
        ),
        xr.concat(
            [time_independent(ds) for ds in datasets],
            dim="sonde",
            combine_attrs="drop_conflicts",
        ),
    ),
    combine_attrs="drop_conflicts",
).assign(
    times_per_sonde=(
        ("sonde",),
        lengths,
        {
            "long_name": "Number of times per sonde",
            "sample_dimension": "time",
        },
    ),
)
```

**Create Zarr store**

```python
store = ds.chunk(time=2**18).to_zarr(zarr_file, zarr_format=2, mode="w")
```

**Open and plot content of zarr store** 

```python
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

ds = xr.open_dataset(zarr_file, engine="zarr", chunks={}).load()

positions = [ds.attrs['launch_start_position'].split() for ds in datasets]
platforms = [ds.attrs['platform'] for ds in datasets]
positions = [(float(lon), float(lat)) for lon, lat in positions]
```

```python
fig = plt.figure(figsize=(12, 12))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())

ax.coastlines()
ax.set_extent([-40, 0, 0, 40])
gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
gl.xlabels_top = False
gl.ylabels_right = False

from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter

LONGITUDE_FORMATTER = LongitudeFormatter(zero_direction_label=True)
LATITUDE_FORMATTER = LatitudeFormatter()

gl.xformatter = LONGITUDE_FORMATTER
gl.yformatter = LATITUDE_FORMATTER

colors = plt.cm.tab10(np.linspace(0, 1, len(set(platforms))))

gate_A = np.array(
    [
        [-27.0, 6.5],
        [-23.5, 5.0],
        [-20.0, 6.5],
        [-20.0, 10.5],
        [-23.5, 12.0],
        [-27.0, 10.5],
    ]
)

ax.plot(gate_A[:3, 0], gate_A[:3, 1], transform=ccrs.PlateCarree(), color='black', linewidth=1)
ax.plot(gate_A[-3:, 0], gate_A[-3:, 1], transform=ccrs.PlateCarree(), color='black', linewidth=1)
unique_platforms = set(platforms)

# Add map features
ax.add_feature(cfeature.COASTLINE)
ax.add_feature(cfeature.BORDERS, linestyle=':')
ax.add_feature(cfeature.LAND)
ax.add_feature(cfeature.OCEAN, facecolor='white')
ax.add_feature(cfeature.LAKES, alpha=0.5)
ax.add_feature(cfeature.RIVERS)

for i, platform in enumerate(unique_platforms):
    platform_positions = [pos for pos, plat in zip(positions, platforms) if plat == platform]
    lons, lats = zip(*platform_positions)
    ax.scatter(lons, lats, label=platform, color=colors[i], s=5)

legend = ax.legend(loc='upper left')

fig.savefig('dropsonde_positions.png', dpi=300)
plt.show()
```

```python
# Plot temperature profiles
fig, ax = plt.subplots()
start = 0
for length in ds.times_per_sonde.values:
    ds.isel(time=slice(start, start + length)).set_coords("altitude").ta.plot(
        y="altitude", alpha=0.3
    )
    start += length

ylim = ax.set_ylim(0, 12_000)
xlim = ax.set_xlim(220, 320)
```

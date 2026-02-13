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

```python
import platform
import glob

import numpy as np
import xarray as xr
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import cmocean

from cdo import *
```

```python
if platform.node()[:7] == "Lotsawa" or platform.node()[:8] == "w149-176":
    rootpath="/Users/m300083/Projekte/GATE_v3.2/DSHIP/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.2/DSHIP/"
```

```python
cdo = Cdo(tempdir=rootpath+'tmp')
cdo.env = {"SKIP_SAME_TIME": "1"}
```

```python
timeIdx=0
```

```python
path=rootpath+"GATE_AND_COMM_SHIPS"
files = sorted(glob.glob(f"{path}/*.nc"))
```

```python
#print ( path, files )
```

```python
ds = cdo.mergetime(input = files, options = '-r', returnXDataset = True)
```

```python
lat = ds.variables['lat'][:]
lon = ds.variables['lon'][:]
sst = ds.variables['sst'][:,:,:]
```

```python
print ( ds)
```

```python
lon_min = -35
lon_max = -15
lat_min =  0
lat_max = 15

# Extract the data within the desired range
ds_region = ds.sel(lon=slice(lon_min, lon_max), lat=slice(lat_min, lat_max))
```

```python
# Calculate the aspect ratio
aspect_ratio = (lon_max - lon_min) / (lat_max - lat_min)

# Set the figure size based on the aspect ratio
figsize = (12, 12 / aspect_ratio)
```

```python
vMin = ds_region['sst'].min()
vMax = ds_region['sst'].max()
vInt = 20
```

```python
# Create a figure with a map background
fig = plt.figure(figsize=figsize)
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())

date = ds_region.coords['time'][timeIdx].values.astype('datetime64[D]').item()

# Add the data to the plot
contours = ax.contourf(ds_region['lon'], ds_region['lat'], ds_region['sst'].values[timeIdx,:, :],
                       transform=ccrs.PlateCarree(),
                       vmin=vMin,
                       vmax=vMax,
                       levels=vInt,
                       cmap=cmocean.cm.thermal)

# Add a colorbar
cbar = plt.colorbar(contours, orientation='horizontal', pad=0.05, shrink=0.8)
cbar.set_label('sea surface temperature')

# Add map features
ax.add_feature(cfeature.COASTLINE)
ax.add_feature(cfeature.BORDERS, linestyle=':')
ax.add_feature(cfeature.LAND)
ax.add_feature(cfeature.OCEAN, facecolor='white')
ax.add_feature(cfeature.LAKES, alpha=0.5)
ax.add_feature(cfeature.RIVERS)

# Define the polygon
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

# Set grid lines
gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True)
gl.top_labels = False
gl.right_labels = False
gl.xlocator = mticker.MultipleLocator(5)
gl.ylocator = mticker.MultipleLocator(5)

# Set the extent of the plot
ax.set_extent([ds_region['lon'].min(), ds_region['lon'].max(), ds_region['lat'].min(), ds_region['lat'].max()])

title = plt.title(f'GATE sst {date}')
```

```python
# Save the plot as a PNG file
plt.savefig("sst.png")

# Show the plot
plt.show()
```

```python

```

```python

```

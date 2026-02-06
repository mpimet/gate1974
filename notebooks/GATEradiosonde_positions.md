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

import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature

#    'Accent':  A qualitative colormap with 8 distinct colors.
#    'Dark2':   A qualitative colormap with 8 distinct colors.
#    'Paired':  A qualitative colormap with 12 distinct colors.
#    'Pastel1': A qualitative colormap with 9 distinct colors.
#    'Pastel2': A qualitative colormap with 8 distinct colors.
#    'Set1':    A qualitative colormap with 9 distinct colors.
#    'Set2':    A qualitative colormap with 8 distinct colors.
#    'Set3':    A qualitative colormap with 12 distinct colors.
#    'tab10':   A qualitative colormap with 10 distinct colors.
#    'tab20':   A qualitative colormap with 20 distinct colors.
#    'tab20b':  A qualitative colormap with 20 distinct colors.
#    'tab20c':  A qualitative colormap with 20 distinct colors.
```

```python
if platform.node()[:7] == "Lotsawa" :
    rootpath="/Users/m300083/Projekte/GATE_v3.1/RADIOSONDE/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.1/RADIOSONDE/"

METEOR     = glob.glob(rootpath+'METEOR/*.nc')
BIDASSOA   = glob.glob(rootpath+'BIDASSOA/*.nc')
CHARTERER  = glob.glob(rootpath+'CHARTERER/*.nc')
DALLAS     = glob.glob(rootpath+'DALLAS/*.nc')
ENDURER    = glob.glob(rootpath+'ENDURER/*.nc')
GILLISS    = glob.glob(rootpath+'GILLISS/*.nc')
OCEANOGRPR = glob.glob(rootpath+'OCEANOGRPR/*.nc')
QUADRA     = glob.glob(rootpath+'QUADRA/*.nc')
RESEARCHER = glob.glob(rootpath+'RESEARCHER/*.nc')
VANGUARD   = glob.glob(rootpath+'VANGUARD/*.nc')
```

```python
files=BIDASSOA+CHARTERER+DALLAS+ENDURER+GILLISS+METEOR+OCEANOGRPR+QUADRA+RESEARCHER+VANGUARD
#files=METEOR
```

```python
# print ( platform.node()[:7], rootpath, files )
```

```python
datasets = [xr.open_dataset(file) for file in files]

positions = [ds.attrs['launch_end_position'].split() for ds in datasets]
platforms = [ds.attrs['platform'] for ds in datasets]
positions = [(float(lon), float(lat)) for lon, lat in positions]
```

```python
fig = plt.figure(figsize=(12, 12))
ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())

#ax = plt.axes(projection=ccrs.PlateCarree())
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

fig.savefig('radiosonde_positions.png', dpi=300)
plt.show()
```

```python

```

```python

```

```python

```

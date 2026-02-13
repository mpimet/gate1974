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

import hvplot.xarray
from cdo import *

import cartopy.crs as ccrs
import cartopy.feature as cfeature
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from termcolor import colored
```

```python
if platform.node()[:7] == "Lotsawa" or platform.node()[:8] == "d147-123":
    rootpath="/Users/m300083/Projekte/GATE_v3.2/DSHIP/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.2/DSHIP/"

Cdo.env = {"SKIP_SAME_TIME": "1"}
cdo = Cdo(tempdir=rootpath+'tmp')

#path=rootpath+"FAY"; PLATFORM="Fay (1)"; INTERVALL=""
#path=rootpath+"FAYE"; PLATFORM="Fay (2)"; INTERVALL=""
#path=rootpath+"METEOR"; PLATFORM="Meteor"; INTERVALL=""
#path=rootpath+"PLANET"; PLATFORM="Planet"; INTERVALL=""

#path=rootpath+"DALLAS"; PLATFORM="Dallas"; INTERVALL="_3600S"
#path=rootpath+"DALLAS"; PLATFORM="Dallas"; INTERVALL="_1800S"
#path=rootpath+"DALLAS"; PLATFORM="Dallas"; INTERVALL="_0180S"
#path=rootpath+"DALLAS"; PLATFORM="Dallas"; INTERVALL="_0600S"

path=rootpath+"JAMES_M_GILLISS"; PLATFORM="Gilliss"; INTERVALL="_3600S"
#path=rootpath+"JAMES_M_GILLISS"; PLATFORM="Gilliss"; INTERVALL="_1800S"
#path=rootpath+"JAMES_M_GILLISS"; PLATFORM="Gilliss"; INTERVALL="_0600S"
#path=rootpath+"JAMES_M_GILLISS"; PLATFORM="Gilliss"; INTERVALL="_0180S"

#path=rootpath+"RESEARCHER"; PLATFORM="Researcher";  INTERVALL="_3600S"
#path=rootpath+"RESEARCHER"; PLATFORM="Researcher";  INTERVALL="_1800S"
#path=rootpath+"RESEARCHER"; PLATFORM="Researcher";  INTERVALL="_0600S"
#path=rootpath+"RESEARCHER"; PLATFORM="Researcher";  INTERVALL="_0180S"

list=path+"/*"+INTERVALL+".nc"
files = sorted(glob.glob(f"{list}"))
```

```python
data = cdo.mergetime(input = files, options = '-r', returnXDataset = True)
data = data.drop_duplicates(dim='time', keep='first')
```

```python
# time_range = data.sel(time=slice('1974-08-16', '1974-08-17'))
```

```python
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
```

```python
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111, projection=ccrs.PlateCarree())

ax.coastlines()
ax.add_feature(cfeature.OCEAN)
ax.add_feature(cfeature.LAND)
ax.add_feature(cfeature.COASTLINE)
ax.add_feature(cfeature.BORDERS, linestyle='--')

ax.set_extent([-28, -16, 4, 18])

gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                 linewidth=1, color='grey', alpha=0.1, linestyle='-')

gl.top_labels = False
gl.right_labels = False
gl.xformatter = LongitudeFormatter()
gl.yformatter = LatitudeFormatter()

ax.plot(gate_A[:3, 0], gate_A[:3, 1], transform=ccrs.PlateCarree(), color='grey', linewidth=1)
ax.plot(gate_A[-3:, 0], gate_A[-3:, 1], transform=ccrs.PlateCarree(), color='grey', linewidth=1)

try:
    lon = data.variables['longitude'][:]
    lat = data.variables['latitude'][:]
except:
    lon = data.variables['lon'][:]
    lat = data.variables['lat'][:]
    
lon = np.array(lon)
lat = np.array(lat)

plt.scatter(lon, lat, c="black", s=1, alpha=0.8, edgecolors="black")

plt.title(f'{PLATFORM} Positions')
plt.show()
```

```python
print ( data )
```

```python
data.p.plot()
```

```python
try:
    data.ta.plot()
except:
    print(colored("WARNING: temperature ta not available!", color="red", attrs=["bold"])) 
```

```python
data.sst.plot()
```

```python
try:
    data.q.plot()
except:
    print(colored("WARNING: specific humidity q not available!", color="red", attrs=["bold"])) 
```

```python
try:
    da=data.p
    exist=True
except:
    print(colored("WARNING: pressure not available!", color="red", attrs=["bold"])) 
    exist=False

da=data.p
da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
               title=PLATFORM+' Pressure', xlabel='Time',
               ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.ta
    exist=True
except:
    print(colored("WARNING: temperature not available!", color="red", attrs=["bold"])) 
    exist=False

scatter_plot = da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
            title=PLATFORM+' Air Temperature',
            xlabel='Time',
            ylabel=f"{da.name} ({da.attrs.get('units', '')})")

if exist:
    display(scatter_plot)
```

```python
try:
    da=data.sst
    exist=True
except:
    print(colored("WARNING: sst not available!", color="red", attrs=["bold"])) 
    exist=False

da.hvplot.scatter(x='time', s=2, height=400, responsive=True,
               title=PLATFORM+' Sea Surface Temperature', xlabel='Time',
               ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
data.to_zarr(f"{PLATFORM}{INTERVALL}.zarr", mode='w')
```

```python
dzarr = xr.open_zarr(f"{PLATFORM}{INTERVALL}.zarr")
```

```python
print (dzarr)
```

```python

```

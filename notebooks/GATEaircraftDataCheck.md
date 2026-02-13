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
    rootpath="/Users/m300083/Projekte/GATE_v3.2/AIRCRAFT/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.2/AIRCRAFT/"

cdo = Cdo(tempdir=rootpath+'tmp')

# x France DC-7
# x NCAR Elektra
# d NOAA DOD WC-130B (dropsonde, see C130_DROPSONDE)
# x NOAA DC-6 39 Charlie (as NOAA NOAA_DC-6 and C39_CHARLIE, see A/C in ASCII file)
#   Brazil Air Force C-130E
# x UK C-130B (Hercules)
#   USSR Ilyushin 18
# x NCAR Queen Air
# x NASA Convair 990
# x NCAR Sabreline
#
# x: flight data; d: dropsonde
#
#path=rootpath+"NASA_CONVAIR_990_MEANS"; PLATFORM="NASA Convair 990"; INTERVALL=""
#path=rootpath+"NCAR_ELECTRA_MEANS"; PLATFORM="NCAR Elektra"; INTERVALL=""
path=rootpath+"NCAR_SABRE_MEANS"; PLATFORM="NCAR Sabreliner"; INTERVALL=""
#path=rootpath+"NOAA_DC-6_MEANS"; PLATFORM="NOAA DC-6"; INTERVALL=""
#path=rootpath+"NOAA_US-C130_MEANS"; PLATFORM="NOAA C130"; INTERVALL=""
#path=rootpath+"NCAR_QUEEN_AIR_MEANS"; PLATFORM="NCAR Queen Air"; INTERVALL=""

#path=rootpath+"DC-7_CEV"; PLATFORM="DC-7"; INTERVALL="1S"
#path=rootpath+"DC-7_CEV"; PLATFORM="DC-7"; INTERVALL="1M"

#path=rootpath+"UKHERCULES_XV208a"; PLATFORM="UK Hercules XV208"; INTERVALL="_100F"
#path=rootpath+"UKHERCULES_XV208a"; PLATFORM="UK Hercules XV208"; INTERVALL="_001F"
#path=rootpath+"UKHERCULES_XV208b"; PLATFORM="UK Hercules XV208"; INTERVALL="_100F"
#path=rootpath+"UKHERCULES_XV208b"; PLATFORM="UK Hercules XV208"; INTERVALL="_001F"

#path=rootpath+"39_CHARLIE"; PLATFORM="NOAA DC-6 39 Charlie"; INTERVALL=""

files = sorted(glob.glob(f"{path}/*{INTERVALL}.nc"))
```

```python
data = cdo.mergetime(input = files, options = '-r', returnXDataset = True)
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

ax.add_feature(cfeature.OCEAN)
ax.add_feature(cfeature.LAND)
ax.add_feature(cfeature.COASTLINE)
ax.add_feature(cfeature.BORDERS, linestyle='-')

ax.set_extent([-28, -16, 4, 16])
#ax.set_extent([-32, -14, -4, 16])

gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                 linewidth=1, color='grey', alpha=0.1, linestyle='-')

gl.top_labels = False
gl.right_labels = False
gl.xformatter = LongitudeFormatter()
gl.yformatter = LatitudeFormatter()

ax.plot(gate_A[:3, 0], gate_A[:3, 1], transform=ccrs.PlateCarree(), color='grey', linewidth=1)
ax.plot(gate_A[-3:, 0], gate_A[-3:, 1], transform=ccrs.PlateCarree(), color='grey', linewidth=1)

lon = data.variables['lon'][:]
lat = data.variables['lat'][:]
lon = np.array(lon)
lat = np.array(lat)

red_level=20000

mask_200 =  data.p <= red_level
mask_300 = (data.p >  red_level ) & (data.p <= 30000)
mask_400 = (data.p >  30000     ) & (data.p <= 40000)
mask_500 = (data.p >  40000     ) & (data.p <= 50000)
mask_600 =  data.p >  50000

plt.scatter(lon[mask_600], lat[mask_600], c="black" , s=8, alpha=0.3, edgecolors="black")
plt.scatter(lon[mask_500], lat[mask_500], c="blue"  , s=4, alpha=0.4, edgecolors="blue")
plt.scatter(lon[mask_400], lat[mask_400], c="green" , s=4, alpha=0.5, edgecolors="green")
plt.scatter(lon[mask_300], lat[mask_300], c="yellow", s=2, alpha=0.6, edgecolors="yellow")
plt.scatter(lon[mask_200], lat[mask_200], c="red"   , s=1, alpha=0.7, edgecolors="red")

plt.title(f'{PLATFORM} Positions, red dots above {red_level} mbar')
plt.savefig("ac_positions.png")
plt.show()
```

```python
#timerange = slice("1974-08-16T00:00:00", "1974-08-17T00:00:00")
#data.pressure.sel(time=timerange).plot.scatter(s=1,color="black")
#data.temperature1.sel(time=timerange).plot.scatter(s=1,color="black")
```

```python
mark_level = red_level
data.p.plot.scatter(s=1,color="black")
try:
    data.p.where(data.p < mark_level).plot.scatter(s=1,color="red")
except:
    print ("Exception")
    print(colored(f"Note: no pressure above {mark_level} {data.p.attrs.get('units', '')}.", 
          color="red", attrs=["bold"]))

print ()
print ( "Maximum Pressure level is ", data.p.max().values )
print ( "Minimum Pressure level is ", data.p.min().values )
```

```python
try:
    data.ta_2.plot.scatter(s=1,color="black")
except:
    data.ta.plot.scatter(s=1,color="black")
```

```python
try:
    data.altitude.plot.scatter(s=1,color="black")
except:
    print(colored("WARNING: altitude not available!", color="red", attrs=["bold"])) 
```

```python
import math
import xarray as xr
import numpy as np

def calculate_height(pressure_pa):
    """
    Calculate the height in meters for a given pressure in mbar
    for a standard atmosphere.

    Parameters:
    pressure (xarray.DataArray): Pressure in millibars.

    Returns:
    xarray.DataArray: Height in meters.
    """
    # Constants
    R  = 287.058 # Gas constant in J/(kg*K)
    T0 = 288.15  # Standard temperature at sea level in Kelvin
    g  = 9.80665 # Acceleration due to gravity in m/s^2
    P0 = 101325  # Standard pressure at sea level in Pascals

    # Calculate height using the hypsometric equation
    height = (R * T0 / g) * np.log(P0 / pressure_pa)

    # Create a new xarray DataArray with the same coordinates and attributes as the input pressure
    height_da = xr.DataArray(height, dims=pressure_pa.dims, coords=pressure_pa.coords, attrs=pressure_pa.attrs)
    height_da.attrs['units'] = 'meters'
    height_da.attrs['long_name'] = 'Geopotential Height'

    return height_da
```

```python
da=data.p
da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
                  title=PLATFORM+' Pressure',
                  xlabel='Time',
                  ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.z
    exist=True
except:
    print(colored("WARNING: altitude not available!", color="red", attrs=["bold"])) 
    exist=False

scatter_plot = da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
            title=PLATFORM+' Altitude',
            xlabel='Time',
            ylabel=f"{da.name} ({da.attrs.get('units', '')})")

if exist:
    display(scatter_plot)
```

```python
height = calculate_height(data.p)
data['height']=height

da=data.height
da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
               title=PLATFORM+' ISA Geopotential Height ', xlabel='Time',
               ylabel=f"{da.long_name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.ta_2
except:
    da=data.ta

da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
               title=PLATFORM+' temperature', xlabel='Time',
               ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.dew
    exist=True
except:
    print(colored("WARNING: dew point temperature not available!", color="red", attrs=["bold"])) 
    exist=False

scatter_plot = da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
            title=PLATFORM+' Dew Point Temperature',
            xlabel='Time',
            ylabel=f"{da.name} ({da.attrs.get('units', '')})")

if exist:
    display(scatter_plot)
```

```python
try:
    da=data.q
    exist=True
except:
    print(colored("WARNING: specific humidity not available!", color="red", attrs=["bold"]))
    exist=False

scatter_plot = da.hvplot.scatter(x='time', s=1, height=400, responsive=True,
            title=PLATFORM+' Specific Humidity',
            xlabel='Time',
            ylabel=f"{da.name} ({da.attrs.get('units', '')})")

if exist:
    display(scatter_plot)
```

```python
print ( data )
```

```python
data.std_ta.plot.scatter(s=1,color="black")
```

```python

```

```python

```

```python

```

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
import matplotlib.ticker as mticker
from matplotlib.colors import LinearSegmentedColormap

from termcolor import colored
import cartopy.crs as ccrs
```

```python
if platform.node() == "Lotsawa.local":
    rootpath="/Users/m300083/Projekte/GATE_v3.2/AIRCRAFT/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.2/AIRCRAFT/"

path=rootpath+"C130_DROPSONDE"; PLATFORM="C130"; INTERVALL=""
#path=rootpath+"C135_DROPSONDE"; PLATFORM="C135"; INTERVALL=""

files = sorted(glob.glob(f"{path}/*{INTERVALL}.nc"))
print ("Dataset contains ", len(files), "files.")
```

```python
ds = xr.open_dataset(files[10])
```

```python
last_non_nan = ds.lon.dropna('level').size-1

colors = ['green', 'yellow', 'red']
z_normalized = np.linspace(0, 1, ds.lon.size)
cmap = LinearSegmentedColormap.from_list('green_to_red', colors, N=256)

plt.scatter(ds.lon, ds.lat, c=z_normalized, cmap=cmap, s=5, alpha=0.7)
plt.scatter(ds.lon[0,0], ds.lat.values[0,0], color="green", s=20)
plt.scatter(ds.lon[0,last_non_nan], ds.lat.values[0,last_non_nan], color="red", s=20)

plt.show()
```

```python
plt.plot(ds.rh.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.rh.name} ({ds.rh.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds.rh.values, ds.altitude.values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
plt.plot(ds.ta.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.ta.name} ({ds.ta.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds.ta.values, ds.altitude.values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
plt.plot(ds.q.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.q.name} ({ds.q.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds.q.values, ds.altitude.values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
plt.plot(ds.wspd.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.wspd.name} ({ds.wspd.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds.wspd.values, ds.altitude.values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
plt.plot(ds.wdir.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.wdir.name} ({ds.wspd.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds.wdir.values, ds.altitude.values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import interp1d

# Create the figure and axis
fig, ax1 = plt.subplots()

# Plot temperature vs. pressure on the primary y-axis
ax1.plot(ds.ta.values, ds.p.values, marker='.', linestyle='-', markersize=2, color="black")

ax1.set_xlabel(f"{ds.ta.name} ({ds.ta.attrs.get('units')})")
ax1.set_ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})", color='black')
ax1.tick_params('y', colors='black')

# Create a mapping between pressure and altitude
sorted_indices = np.argsort(ds.p.values.flatten())
sorted_pressures = ds.p.values.flatten()[sorted_indices]
sorted_altitudes = ds.altitude.values.flatten()[sorted_indices]

# Create an interpolation function
f = interp1d(sorted_pressures, sorted_altitudes, fill_value="extrapolate")

# Create a secondary y-axis for altitude
ax2 = ax1.twinx()

# Get the tick positions of the primary axis
tick_positions = ax1.get_yticks()

# Calculate the corresponding altitude values using interpolation
altitude_values = f(tick_positions)

# Set the minimum altitude tick to the minimum altitude value
min_altitude = np.min(ds.altitude.values)
altitude_values[altitude_values < min_altitude] = min_altitude

# Remove the lowest altitude tick
altitude_values = altitude_values[:-1]
tick_positions = tick_positions[:-1]

# Set the tick positions on the altitude axis to match the selected altitude values
ax2.set_yticks(altitude_values)

# Format the altitude values as strings for the tick labels
altitude_labels = [f'{alt:.0f} m' for alt in altitude_values]

# Set the y labels
ax2.set_yticklabels(altitude_labels)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color='black')
ax2.tick_params('y', colors='black')

# Create a secondary x-axis for specific humidity
ax3 = ax1.twiny()

# Plot specific humidity vs. pressure on the secondary x-axis
ax3.plot(ds.rh.values, ds.p.values, marker='.', linestyle='-', markersize=4, color="red")

ax3.set_xlabel(f"{ds.rh.long_name} ({ds.rh.attrs.get('units')})", color='red')
ax3.tick_params('x', colors='red')

# Invert the primary y-axis
ax1.invert_yaxis()

plt.show()
```

```python

```

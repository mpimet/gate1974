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
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from termcolor import colored
from cdo import *
```

```python
if platform.node() == "Lotsawa.local":
    rootpath="/Users/m300083/Projekte/GATE_v3.1/RADIOSONDE/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.1/RADIOSONDE/"

cdo = Cdo(tempdir=rootpath+'tmp')

#path=rootpath+"BIDASSOA"; PLATFORM="Bidassoa"; INTERVALL=""
#path=rootpath+"CHARTERER"; PLATFORM="Charterer"; INTERVALL=""
#path=rootpath+"DALLAS"; PLATFORM="Dallas"; INTERVALL=""
#path=rootpath+"ENDURER"; PLATFORM="Endurer"; INTERVALL=""
#path=rootpath+"METEOR"; PLATFORM="Meteor"; INTERVALL=""
#path=rootpath+"OCEANOGRPR"; PLATFORM="Oceanographer"; INTERVALL=""
#path=rootpath+"QUADRA"; PLATFORM="Quadra"; INTERVALL=""
path=rootpath+"RESEARCHER"; PLATFORM="Researcher"; INTERVALL=""
#path=rootpath+"VANGUARD"; PLATFORM="Vanguard"; INTERVALL=""

files = sorted(glob.glob(f"{path}/*{INTERVALL}.nc"))
print ("Dataset contains ", len(files), "files.")
```

```python
ds = xr.open_dataset(files[100])
```

```python
print (ds)
```

```python
plt.plot(ds['ta'].values, ds['p'].values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.ta.name} ({ds.ta.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds['ta'].values, ds['altitude'].values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
ax2.set_ylabel(f"{ds.altitude.name} ({ds.altitude.attrs.get('units')})", color="red")
ax2.tick_params(axis='y', labelcolor="red")

plt.show()
```

```python
plt.plot(ds['q'].values, ds['p'].values, marker='.', linestyle='-', markersize=2, color="black")
plt.xlabel(f"{ds.q.name} ({ds.q.attrs.get('units')})")
plt.ylabel(f"{ds.p.name} ({ds.p.attrs.get('units')})")
plt.gca().invert_yaxis()
plt.grid(True)

# Add secondary y-axis for altitude
ax2 = plt.twinx()
ax2.plot(ds['q'].values, ds['altitude'].values, marker='.', linestyle='-', markersize=2, color="red", alpha=0.7)
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
ax1.plot(ds['ta'].values, ds['p'].values, marker='.', linestyle='-', markersize=2, color="black")

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
ax3.plot(ds['q'].values, ds['p'].values, marker='.', linestyle='-', markersize=4, color="red")

ax3.set_xlabel('Specific Humidity (kg/kg)', color='red')
ax3.tick_params('x', colors='red')

# Invert the primary y-axis
ax1.invert_yaxis()

plt.show()
```

```python

```

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
from pathlib import Path
from tqdm.notebook import tqdm

import xarray as xr
import numpy as np
import pandas as pd

import zarr
```

```python
if platform.node()[:7] == "Lotsawa" or platform.node()[:8] == "d147-123":
    rootpath=Path("/Users/m300083/Projekte/GATE_v3.2p1/AIRCRAFT/")
else:
    rootpath=Path("/work/mh0287/m300083/GATE_v3.2p1/AIRCRAFT/")

instruments = [ "C130_DROPSONDE", "C135_DROPSONDE" ]

ZARR_STORE='GATE_DROPSONDES.zarr'

nc_files = []
for inst in instruments:
    folder = rootpath / inst
    if folder.is_dir():
        nc_files.extend(folder.glob("*.nc"))
    else:
        print(f"⚠️ Folder not found: {folder}")

print(f"✅ Found {len(nc_files)} NetCDF files.")
```

**Read All NetCDF Files and Extract Data**

```python
profiles_data = []

for nc_file in tqdm(nc_files, desc="Processing profiles", total=len(nc_files), leave=True):
    try:
        ds = xr.open_dataset(nc_file)

        # Extract platform
        platform = ds.attrs.get("platform", "unknown")

        # Extract launch positions from global attributes
        launch_start = ds.attrs.get("launch_start_position", "unknown")
        launch_end = ds.attrs.get("launch_end_position", "unknown")

        # Extract time (convert to ISO string)
        time_val = ds.time.values[0]
        time_str = pd.to_datetime(time_val).isoformat()

        # Extract variables (squeeze from (1, N) to (N,))
        profile_data = {
            "platform": platform,
            "launch_start": launch_start,
            "launch_end": launch_end,
            "time": time_str,
            "flight_time": ds.flight_time.values.squeeze(),
            "level": ds.level.values,
            "lat": ds.lat.values.squeeze(),
            "lon": ds.lon.values.squeeze(),
            "p": ds.p.values.squeeze(),
            "altitude": ds.altitude.values.squeeze(),
            "ta": ds.ta.values.squeeze(),
            "dew": ds.dew.values.squeeze(),
            "q": ds.q.values.squeeze(),
            "rh": ds.rh.values.squeeze(),
            "u": ds.u.values.squeeze(),
            "v": ds.v.values.squeeze(),
            "ws": ds.ws.values.squeeze(),
            "wd": ds.wd.values.squeeze(),
        }

        profiles_data.append(profile_data)
        ds.close()

    except Exception as e:
        print(f"❌ Error processing {nc_file}: {e}")
        ds.close()
        continue

print(f"✅ Processed {len(profiles_data)} profiles.")
```

**Flatten All Data into Ragged Arrays**

```python
# Prepare ragged arrays
flat_data = {}
lengths = []

flat_data = {}
lengths = []

# Extract metadata
platforms = []
launch_starts = []
launch_ends = []
times = []

for i, prof in enumerate(profiles_data):
    n_levels = len(prof["level"])
    lengths.append(n_levels)

    # Store metadata
    platforms.append(prof["platform"])
    launch_starts.append(prof["launch_start"])
    launch_ends.append(prof["launch_end"])
    times.append(prof["time"])

    # Flatten variables
    for var_name in ["flight_time","lat","lon","p", "altitude", "ta", "dew", "q", "rh", "u", "v", "ws", "wd"]:
        if var_name not in flat_data:
            flat_data[var_name] = []
        flat_data[var_name].append(prof[var_name])

# Convert to numpy arrays
for var_name in flat_data:
    flat_data[var_name] = np.concatenate(flat_data[var_name])

lengths = np.array(lengths, dtype='int32')

# Save metadata as strings
platforms = np.array(platforms, dtype='U')
launch_starts = np.array(launch_starts, dtype='U')
launch_ends = np.array(launch_ends, dtype='U')
times = np.array(times, dtype='U')
```

**Save to Zarr with Ragged Structure**

```python
store = zarr.DirectoryStore(ZARR_STORE)
root  = zarr.group(store=store, overwrite=True)

# Save flat data arrays
for var_name, data in flat_data.items():
    root.array(
        var_name,
        data=data,
        chunks=(512,),  # the smaller the more memory
        dtype=data.dtype,
        overwrite=True
    )

# Save metadata arrays
root.array('lengths', data=lengths, chunks=(100,), dtype='int32', overwrite=True)
root.array('platform', data=platforms, chunks=(100,), overwrite=True)
root.array('launch_start', data=launch_starts, chunks=(100,), overwrite=True)
root.array('launch_end', data=launch_ends, chunks=(100,), overwrite=True)
root.array('time', data=times, chunks=(100,), overwrite=True)

# Save global attributes
root.attrs['title'] = "GATE radiosonde atmospheric profiles (ragged Zarr)"
root.attrs['description'] = "Combined vertical profiles from multiple vessels, stored with ragged array structure."
root.attrs['source'] = "radiosonde"
root.attrs['conventions'] = "ACDD-1.3, CF-1.12"
root.attrs['license'] = "CC-BY-4.0"
root.attrs['profile_count'] = len(profiles_data)

print("✅ Zarr archive saved to "+ZARR_STORE)
```

**Read Back and Reconstruct Profiles**

```python
# Open Zarr store
root = zarr.open(ZARR_STORE, mode='r')

# Load data
flat_altitude = root['altitude'][:]

flat_lon = root['lon'][:]
flat_lat = root['lat'][:]

flat_p   = root['p'][:]
flat_ta  = root['ta'][:]
flat_q   = root['q'][:]
flat_u   = root['u'][:]
flat_v   = root['v'][:]

flat_rh  = root['rh'][:]
flat_dew = root['dew'][:]

flat_ws  = root['ws'][:]
flat_wd  = root['wd'][:]

lengths = root['lengths'][:]
times = root['time'][:]
profile_launch_start = root['launch_start'][:]
profile_launch_end = root['launch_end'][:]

# Reconstruct profiles
reconstructed_profiles = []
start_idx = 0

for i in range(len(lengths)):
    end_idx = start_idx + lengths[i]

    profile = {
        'platform': platform,
        'time': times[i],
        'launch_start': profile_launch_start[i],
        'launch_end': profile_launch_end[i],
        'level': np.arange(start_idx, end_idx),  # Or use actual level values if stored
        'p': flat_p[start_idx:end_idx],
        'altitude': flat_altitude[start_idx:end_idx],
        'ta': flat_ta[start_idx:end_idx],
        'dew': flat_dew[start_idx:end_idx],
        'q': flat_q[start_idx:end_idx],
        'rh': flat_rh[start_idx:end_idx],
        'u': flat_u[start_idx:end_idx],
        'v': flat_v[start_idx:end_idx],
        'ws': flat_ws[start_idx:end_idx],
        'wd': flat_wd[start_idx:end_idx],
    }

    reconstructed_profiles.append(profile)
    start_idx = end_idx

print(f"✅ Reconstructed {len(reconstructed_profiles)} profiles.")
```

```python

```

```python

```

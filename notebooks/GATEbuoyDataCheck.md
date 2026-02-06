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

import hvplot.xarray
from cdo import *

import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from termcolor import colored
```

```python
if platform.node() == "Lotsawa.local":
    rootpath="/Users/m300083/Projekte/GATE_v3.1/BUOY/"
    %env CDO /opt/homebrew/Caskroom/miniforge/base/envs/plotbox/bin/cdo
else:
    rootpath="/work/mh0287/m300083/GATE_v3.1/BUOY/"

cdo = Cdo(tempdir=rootpath+'tmp')
cdo.env = {"SKIP_SAME_TIME": "1"}

path=rootpath+"METEOR"; PLATFORM="Meteor Buoy"; INTERVALL=""

list=path+"/*"+INTERVALL+".nc"
files = sorted(glob.glob(f"{list}"))
```

```python
data = cdo.mergetime(input = files, options = '-r', returnXDataset = True)
data = data.drop_duplicates(dim='time', keep='first')
```

```python
#data = data.sel(time=slice('1974-06-19', '1974-06-19'))
```

```python
print ( data )
print ()
print ("Covering ", data.time[0].values," to ", data.time[data.time.size-1].values)


```

```python
try:
    data.ws.plot()
except:
    print(colored("WARNING: wind speed ws not available!", color="red", attrs=["bold"])) 
```

```python
try:
    data.wd.plot()
except:
    print(colored("WARNING: wind direction wd not available!", color="red", attrs=["bold"])) 
```

```python
try:
    data.q.plot()
except:
    print(colored("WARNING: specific humidity q not available!", color="red", attrs=["bold"])) 
```

```python
try:
    data.sst.plot()
except:
    print(colored("WARNING: sea surface temperature sst not available!", color="red", attrs=["bold"])) 
```

```python
try:
    data.dbt.plot()
except:
    print(colored("WARNING: dry bulb temperature dbt not available!", color="red", attrs=["bold"])) 
```

```python
try:
    da=data.ws
    exist=True
except:
    print(colored("WARNING: wind speed not available!", color="red", attrs=["bold"])) 
    exist=False

da.hvplot.scatter(x='time', s=3, height=400, responsive=True,
                   title=F"{PLATFORM} {da.long_name}",
                   xlabel='Time',
                   ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.dbt
    exist=True
except:
    print(colored("WARNING: dry bulb temperature not available!", color="red", attrs=["bold"])) 
    exist=False

da=data.dbt
da.hvplot.scatter(x='time', s=3, height=400, responsive=True,
                  title=f"{PLATFORM} {da.long_name}",
                  xlabel='Time',
                  ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.sst
    exist=True
except:
    print(colored("WARNING: sst not available!", color="red", attrs=["bold"])) 
    exist=False

da.hvplot.scatter(x='time', s=3, height=400, responsive=True,
                   title=F"{PLATFORM} {da.long_name}",
                   xlabel='Time',
                   ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
try:
    da=data.q
    exist=True
except:
    print(colored("WARNING: q not available!", color="red", attrs=["bold"])) 
    exist=False

da.hvplot.scatter(x='time', s=3, height=400, responsive=True,
                   title=F"{PLATFORM} {da.long_name}",
                   xlabel='Time',
                   ylabel=f"{da.name} ({da.attrs.get('units', '')})")
```

```python
zarr = data.to_zarr(f"{PLATFORM}{INTERVALL}.zarr", mode='w')
```

```python
dzarr = xr.open_zarr(f"{PLATFORM}{INTERVALL}.zarr")
```

```python
print (dzarr)
```

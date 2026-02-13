<img src="./doc/figures/GATE1974-Logo-small.png" alt="GATE logo" style="float: right; margin: 10px;">

# GATE Data Processing Pipeline

*René Redler, Max Planck Institute for Meteorology, Hamburg*

**Last updated: 13 February 2026**

**Version 3.2**

---

Please see the doc directory for further information. 

Without any guaratee for completeness, GATE_tape_contents.pdf provides a summary of the ASCII file contents and may be of help for identifying further data of interest. 

GATEdataconversion.tex can easily be converted to a pdf document using pdflatex. This documents describes the data processing in some more details with a focus on those data that have been converted.

- [Aircraft data](./fortran/GATEaircraft.md)
- [Buoy data](./fortran/GATEbuoy.md)
- [Dropsonde data](./fortran/GATEdropsonde.md)
- [Radiosondedata](./fortran/GATEradiosonde.md)
- [Buoy data](./fortran/GATEbuoy.md)
- Ship data

---

## 1.) Handling of Initial Tar Files

### `mv-and-expand.sh`

- Creates directories based on the tar file base name

- Moves tar files into their respective directories

### `untar.sh`

- Untars all files within directories created by `mv-and-expand.sh`

## 2.) Sorting Files

### `GATEsort.sh`

**GATE_AND_COMM_SHIPS, Ship and METEOR Radiosonde Data:**

```
GATEsort.sh /<path to>/DATA/GATE/3.00.02.104-3.31.02.101_19740601-19740930
```

**All Other Radiosonde Data:**

```
GATEsort.sh /<path to>/DATA/GATE/3.31.02.101-3.33.02.101_19740601-19740930
```

**Aircraft Data:**

```
GATEsort.sh /<path to>/DATA/GATE/3.36.21.102-3.60.02.105_19740601-19740930

GATEsort.sh /<path to>/DATA/GATE/3.60.02.105-3.64.02.101_19740601-19740930
```

**Dropsonde Data:**

```
GATEsort.sh /<path to>/DATA/GATE/3.64.02.101-3.69.02.104_19740601-19740930
```

## 3.) Processing and Converting ASCII Files

### Fortran Programs

| Program | Description |
| - | - |
| `GATEdropsonde.f90`       | Reads and writes GATE C130 and C130 aircraft dropsonde data |
| `GATEbuoy_meteor.f90`     | Reads and writes GATE Meteor buoy data |
| `GATEradiosonde.f90`      | Reads and writes GATE radiosonde files: CHARTERER, DALLAS, ENDURER, GILLISS, METEOR, OCEANOGRPR, QUADRA, RESEARCHER, VANGUARD (38aca3), BIDASSOA (c598b7) |
| `GATEdship_f61f70.f90`    | GATE_AND_COMM_SHIP SST gridded data |
| `GATEdship_1a7095.f90`    | METEOR, FAY, FAYE, and PLANET DSHIP data |
| `GATEdship_fdfbef.f90`    | JAMES_M_GILLISS, DALLAS, RESEARCHER DSHIP data |
| `GATEaircraft_db67b3.f90` | NCAR_SABRE_MEANS (aircraft NCAR SABRELINER) |
| `GATEaircraft_579bd3.f90` | DC-7_CEV (579bd3 and d30c25, same Fortran format but expressed differently in ASCII files) |
| `GATEaircraft_ced38b.f90` | NOAA_DC-6_MEANS, NOAA_US-C130_MEANS |
| `GATEaircraft_aa9a5e.f90` | NCAR_ELECTRA_MEANS |
| `GATEaircraft_e7f42f.f90` | NASA_CONVAIR_990_MEANS |
| `GATEaircraft_b3d264.f90` | NCAR_QUEEN_AIR_MEANS |
| `GATEaircraft_dab4ec.f90` | UKHERCULES_XV208a |
| `GATEaircraft_96dd74.f90` | UKHERCULES_XV208b |
| `GATEaircraft_28c11d.f90` | 39_CHARLIE |


### c++ Programs

| Program | Description |
| - | - |
| `GATEbuoy_meteor.cpp`   | Reads and writes GATE Meteor buoy data |
| `GATEbuoy_hydro.cpp`    | Reads and writes GATE UK Hydrographic ship buoy data |
| `GATEandCOMM_SHIPS.cpp` | GATE_AND_COMM_SHIP SST gridded data |


### Compilation Instructions for Fortran

- `make -f Makefile.levante` : Compile `GATEradiosonde.f90` and link on Levante

- `make -f Makefile.mpim`    : Compile `GATEradiosonde.f90` and link on MPI-M desktop PCs

- `make -f Makefile.osx`     : Compile `GATEradiosonde.f90` and link on Mac w/ homebrew

- `make clean` : Remove `*.o`, `*.mod`, and `*.x` files

### Compilation Instructions for c++

- `make all`: Compile and link all c++ files on Mac OSX w/ homebrew

- `make clean` : Remove `*.o `and `*.x` files

### Conversion Scripts

- `GATEaircraft.sh` : Launch aircraft data conversion

- `GATEradiosonde.sh` : Launch radiosonde data conversion

- `GATEdship.sh` : Launch DSHIP data conversion

- `GATEbuoy.sh` : Launch buoy data conversion

> **Usage:** Run any script with `-h` flag for help:  
`GATE\<...\>.sh -h`

## Notes

- **DALLAS/fdfbef**: Some files contain data with timestamps earlier than the indicated start of measurements.


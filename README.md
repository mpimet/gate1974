<img src="./doc/figures/GATE1974-Logo-small.png" alt="GATE logo" style="float: right; margin: 10px;">

# GATE Data Processing Pipeline

*René Redler, Max Planck Institute for Meteorology, Hamburg*

**Last updated: 9 February 2026**

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

**GATE\_AND\_COMM\_SHIPS, Ship and METEOR Radiosonde Data:**

```
GATEsort.sh /\<path to>/DATA/GATE/3.00.02.104-3.31.02.101\_19740601-19740930
```

**All Other Radiosonde Data:**

```
GATEsort.sh /\<path to\>/DATA/GATE/3.31.02.101-3.33.02.101\_19740601-19740930
```

**Aircraft Data:**

```
GATEsort.sh /\<path to\>/DATA/GATE/3.36.21.102-3.60.02.105\_19740601-19740930

GATEsort.sh /\<path to\>/DATA/GATE/3.60.02.105-3.64.02.101\_19740601-19740930
```

**Dropsonde Data:**

```
GATEsort.sh /\<path to\>/DATA/GATE/3.64.02.101-3.69.02.104\_19740601-19740930
```

## 3.) Processing and Converting ASCII Files

### Fortran Programs

| Program | Description |
| - | - |
| `GATEdropsonde\_ecf73f.f90` | Reads and writes GATE C130 and C130 aircraft dropsonde data |
| `GATEbuoy\_meteor.f90` | Reads and writes GATE Meteor buoy data |
| `GATEradiosonde\_8db9d2.f90` | Reads and writes GATE Meteor radiosonde files |
| `GATEradiosonde\_38aca3.f90` | Reads and writes GATE radiosonde files: CHARTERER, DALLAS, ENDURER, GILLISS, OCEANOGRPR, QUADRA, RESEARCHER, VANGUARD (all 38aca3), BIDASSOA (c598b7) |
| `GATEdship\_f61f70.f90` | GATE\_AND\_COMM\_SHIP SST gridded data |
| `GATEdship\_1a7095.f90` | METEOR, FAY, FAYE, and PLANET DSHIP data |
| `GATEdship\_fdfbef.f90` | JAMES\_M\_GILLISS, DALLAS, RESEARCHER DSHIP data |
| `GATEaircraft\_db67b3.f90` | NCAR\_SABRE\_MEANS (aircraft NCAR SABRELINER) |
| `GATEaircraft\_579bd3.f90` | DC-7\_CEV (579bd3 and d30c25, same Fortran format but expressed differently in ASCII files) |
| `GATEaircraft\_ced38b.f90` | NOAA\_DC-6\_MEANS, NOAA\_US-C130\_MEANS |
| `GATEaircraft\_aa9a5e.f90` | NCAR\_ELECTRA\_MEANS |
| `GATEaircraft\_e7f42f.f90` | NASA\_CONVAIR\_990\_MEANS |
| `GATEaircraft\_b3d264.f90` | NCAR\_QUEEN\_AIR\_MEANS |
| `GATEaircraft\_dab4ec.f90` | UKHERCULES\_XV208a |
| `GATEaircraft\_96dd74.f90` | UKHERCULES\_XV208b |
| `GATEaircraft\_28c11d.f90` | 39\_CHARLIE |


### c++ Programs

| Program | Description |
| - | - |
| `GATEbuoy\_meteor.cpp` | Reads and writes GATE Meteor buoy data |
| `GATEbuoy\_hydro.cpp` | Reads and writes GATE UK Hydrographic ship buoy data |
| `GATEandCOMM\_SHIPS.cpp` | GATE\_AND\_COMM\_SHIP SST gridded data |


### Compilation Instructions for Fortran

- `make -f Makefile.levante` : Compile `GATEradiosonde.f90` and link on Levante

- `make -f Makefile.mpim`    : Compile `GATEradiosonde.f90` and link on MPI-M desktop PCs

- `make -f Makefile.osx`     : Compile `GATEradiosonde.f90` and link on Mac w/ homebrew

- `make clean` : Remove `\*.o`, `\*.mod`, and `\*.x` files

### Compilation Instructions for c++

- `make all`: Compile and link all c++ files on Mac OSX w/ homebrew

- `make clean` : Remove `\*.o `and `\*.x` files

### Conversion Scripts

- `GATEaircraft.sh` : Launch aircraft data conversion

- `GATEradiosonde.sh` : Launch radiosonde data conversion

- `GATEdship.sh` : Launch DSHIP data conversion

- `GATEbuoy.sh` : Launch buoy data conversion

> **Usage:** Run any script with `-h` flag for help:  
`GATE\<...\>.sh -h`

## Notes

- **DALLAS/fdfbef**: Some files contain data with timestamps earlier than the indicated start of measurements.


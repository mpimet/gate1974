# GATE Aircraft Meteorological Data Processing Program

## Overview

The `GATEaircraft` programs are designed to process and convert meteorological data from GATE aircrafts. They utilize a Fortran module to define data types and utility functions, a main program to handle input files and data processing, and subroutines to convert the data and write it to a NetCDF file. The programs ensure that the processed data is accurately converted and stored in a structured format for further analysis.

## Program Structure

The `GATEaircraft` programs are composed of the following key components:

### GATEaircraft_mod Module

- **Data Types**:
  - `GATE_aircraft_type`: Structure to hold aircraft data, including fields like time, latitude, longitude, pressure, temperature, heading, wind data, and flags.
  - `GATE_metadata_type`: Structure to hold metadata, including aircraft name, measurement times, interval, units, flags, and other metadata.

- **Utility Subroutines**:
  - `standard_deviation`: Calculates the standard deviation of a field.
  - `aircraft_time_converter`: Converts time signals to hours, minutes, and seconds
  - `hercules_time_converter`: Converts time signals to hours, minutes, and seconds for UK Hercules 
  - `dc7_time_converter`: Converts time signals to hours, minutes, and seconds for DC7
  - `charlie_time_converter`: Converts time signals to hours, minutes, and seconds for NOAA Charlie 39.

### Main Program (GATEaircraft)

The main program handles the input file, processes the data, and calls subroutines to perform specific tasks. It includes the following steps:
1. Reads the input file and processes different record types.
2. Calls `convert_data` for processing and converting the data.

### Subroutine (convert_data)

The `convert_data` subroutine processes and converts the input data. It performs the following tasks:
1. Reads the input file and processes the data records.
2. Calls subroutines like `standard_deviation` and `*_time_converter` to process specific data fields.
3. Allocates and manages memory for storing processed data.
4. Filters out invalid data and writes the processed data to an array.

### Subroutine (write_netcdf)

The `write_netcdf` subroutine writes the processed data to a NetCDF file. It includes the following steps:
1. Defines the NetCDF file structure and global attributes.
2. Writes data fields and metadata to the NetCDF file.
3. Filters out invalid data and converts units.
4. Writes the processed data to the NetCDF file.


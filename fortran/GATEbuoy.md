# GATE Buoy Meteorological Data Processing Program

## Overview

The `GATEbuoy_meteor.f90` program is designed to process and convert meteorological data from Meteor buoys collected during the GATE campaigne. It utilizes a Fortran module to define data types and utility functions, a main program to handle input files and data processing, and subroutines to convert the data and write it to a NetCDF file. The program ensures that the processed data is accurately converted and stored in a structured format for further analysis.

## Program Structure

The program is composed of the following key components:

### GATEbuoy_mod Module

- **Data Types**:
  - `GATE_buoy_type`: Structure to hold buoy data, including fields like time, wind speed, wind direction, temperature, humidity, and water temperature.
  - `GATE_metadata_type`: Structure to hold metadata, including ship names, measurement times, interval, units, and buoy positions.

- **Utility Functions**:
  - `date_converter`: Converts a date to year, month, and day.
  - `time_converter`: Converts a time to hours, minutes, and seconds.
  - `date_to_days`: Converts a date to the number of days since January 1, 1974.
  - `is_leap_year`: Checks if a year is a leap year.
  - `days_between`: Calculates the number of days between two dates.

### Main Program (GATEbuoy)

The main program handles the input file, processes the data, and calls subroutines to perform specific tasks. It includes the following steps:
1. Reads the input file and processes different record types.
2. Calls `convert_data` for processing and converting the data.

### Subroutine (convert_data)

The `convert_data` subroutine processes and converts the input data. It performs the following tasks:
1. Reads the input file and processes the data records.
2. Calls `write_netcdf` to write the processed data to a NetCDF file.

### Subroutine (write_netcdf)

The `write_netcdf` subroutine writes the processed data to a NetCDF file. It includes the following steps:
1. Defines the NetCDF file structure.
2. Writes data and metadata to the NetCDF file.


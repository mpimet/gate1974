# Summary of GATEradiosonde_*.f90

The `GATEradiosonde` files contain Fortran code related to processing radiosonde data for the GATE (Global Atmospheric Turbulence Experiment) experiment. The code is organized into several major sections:

1. **Module Definitions**:
    - **GATEradiosonde_mod**: Defines types and subroutines used for processing radiosonde data.
    - **datetime**: Represents date and time.
    - **position**: Represents geographical position.
    - **GATE_radiosonde_type**: Structure to hold radiosonde data.
    - **GATE_metadata_type**: Structure to hold metadata information.

2. **Main Program - GATEradiosonde**:
    - Reads radiosonde data files and processes them based on their type.
    - Calls `convert_data` subroutine for processing radiosonde data.

3. **Subroutine - convert_data**:
    - Converts raw radiosonde data into structured data.
    - Handles different sections of the data file and extracts metadata.
    - Allocates memory for data storage and processes data records.
    - Calls `write_netcdf` subroutine to write processed data to a NetCDF file.

4. **Subroutine - write_netcdf**:
    - Writes processed radiosonde data to a NetCDF file.
    - Defines dimensions, variables, and attributes for the NetCDF file.
    - Handles errors during NetCDF file creation and data writing.

5. **Error Handling**:
    - The code includes error handling for file operations and data processing.

The code uses Fortran 90 features such as modules, derived types, and array handling. It also relies on NetCDF library functions for writing data to NetCDF files.

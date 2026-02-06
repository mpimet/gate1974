import netCDF4 as nc
import argparse
import os
import matplotlib.pyplot as plt

def check_time_axis_increasing(nc_file_path, verbose, plot):
    """
    Checks if the time axis in a netCDF file has increasing values.

    Parameters:
    - nc_file_path: str, path to the netCDF file.
    - plot: bool, whether to plot all time values.
    - verbose: bool, whether to print all time values.
    """
    # Open the netCDF file
    try:
        with nc.Dataset(nc_file_path, 'r') as ds:
            # Find the time variable
            time_var_name = None
            for var_name in ds.variables:
                if 'time' in ds.variables[var_name].dimensions:
                    time_var_name = var_name
                    break
            if time_var_name is None:
                print(f"No time variable found in {nc_file_path}.")
                return

            # Read the time variable
            time_values = ds.variables[time_var_name][:]

            # Check if time values are increasing
            if all(time_values[i] < time_values[i+1] for i in range(len(time_values)-1)):
                print(f"The time axis in {nc_file_path} has increasing values.")
            else:
                print(f"The time axis in {nc_file_path} does not have increasing values.")

            # Print all time values if verbose
            if verbose:
                print(f"Time values in {nc_file_path}:")
                print(time_values)

            # Plot the time values
            if plot:
                plt.figure(figsize=(10, 6))
                plt.plot(time_values)
                plt.title(f"Time values in {os.path.basename(nc_file_path)}")
                plt.xlabel("Index")
                plt.ylabel("Time")
                plt.savefig(f"{os.path.splitext(os.path.basename(nc_file_path))[0]}.png")
                plt.close()
    except FileNotFoundError:
        print(f"File {nc_file_path} not found.")
    except Exception as e:
        print(f"An error occurred: {e}")

def main():
    parser = argparse.ArgumentParser(description='Check if the time axis in netCDF files has increasing values.')
    parser.add_argument('directory', type=str, help='Directory containing netCDF files.')
    parser.add_argument('-p', '--plot', action='store_true', help='Plot all time values.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Print all time values.')
    args = parser.parse_args()

    # Loop over all .nc files in the directory
    for filename in os.listdir(args.directory):
        if filename.endswith(".nc"):
            nc_file_path = os.path.join(args.directory, filename)
            check_time_axis_increasing(nc_file_path, args.verbose, args.plot)

if __name__ == "__main__":
    main()




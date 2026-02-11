#!/usr/bin/bash
##!/opt/homebrew/bin/bash

# Check if Bash version is 4 or later
if [ ${BASH_VERSION:0:1} -lt 4 ]; then
  echo "Error: Bash 4 or later is required"
  echo "Under OSX try /opt/homebrew/bin/bash"
  exit 1
fi

thisdir=$(pwd)

# Define the ship directories with hash
declare -A radiosonde_dirs
radiosonde_dirs[BIDASSOA]="RADIOSONDE/BIDASSOA/c598b7"
radiosonde_dirs[CHARTERER]="RADIOSONDE/CHARTERER/38aca3"
radiosonde_dirs[DALLAS]="RADIOSONDE/DALLAS/38aca3"
radiosonde_dirs[ENDURER]="RADIOSONDE/ENDURER/38aca3"
radiosonde_dirs[GILLISS]="RADIOSONDE/GILLISS/38aca3"
radiosonde_dirs[METEOR]="RADIOSONDE/METEOR/8db9d2"
radiosonde_dirs[OCEANOGRPR]="RADIOSONDE/OCEANOGRPR/38aca3"
radiosonde_dirs[QUADRA]="RADIOSONDE/QUADRA/38aca3"
radiosonde_dirs[RESEARCHER]="RADIOSONDE/RESEARCHER/38aca3"
radiosonde_dirs[VANGUARD]="RADIOSONDE/VANGUARD/38aca3"

# Function to perform the data conversion
process_platform() {
  local ship=$1
  in_path="${inbasedir}/${radiosonde_dirs[$ship]}"
  hash=$(basename ${radiosonde_dirs[$ship]})
 
  out_path="${outbasedir}/${radiosonde_dirs[$ship]}"
  [ ! -d "$out_path" ] && mkdir -p "$out_path"
  cd ${out_path}
  cp ${in_path}/* .
  for infile in `ls -1`; do
    ${thisdir}/GATEradiosonde.x $infile
    mv ${infile}.nc ../.
  done
  [ "$keepFiles" = false ] && rm -r ${out_path}
}

# Function to print help message
print_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -p, --platform <platform>     Specify the platform/ship to process [default all]"
  echo "  -i, --inbasedir <inbasedir>   Specify the base input directory, the path to RADIOSONDE"
  echo "  -o, --outbasedir <outbasedir> Specify the base output directory"
  echo "  -k, --keep                    Keep ASCII files in output directory"
  echo "  -h, --help                    Print this help message"
  echo "Supported ships for -p are:"
  for ship in "${!radiosonde_dirs[@]}"; do
    echo "  $ship"
  done
}

keepFiles=false

# Parse command-line options
while getopts ":hp:i:o:k" opt; do
  case $opt in
    h) print_help; exit 0;;
    p) ship=$OPTARG;;
    i) inbasedir=$OPTARG;;
    o) outbasedir=$OPTARG;;
    k) keepFiles=true;;
    \?) echo "Invalid option: -$OPTARG"; exit 1;;
  esac
done

# Check if the base directory option is provided
if [ -z "$inbasedir" ]; then
  echo "Please provide the base directory option using -b or --basedir"
  print_help
  exit 1
fi

# Check if the output base directory option is provided
if [ -z "$outbasedir" ]; then
  echo "Please provide the output base directory option using -o or --outbasedir"
  print_help
  exit 1
fi

# If ship is provided, check if it's valid
if [ -n "$ship" ]; then
  if [ ! ${radiosonde_dirs[$ship]+_} ]; then
    echo "Invalid ship: $ship"
    echo "Available ships: ${!ship_dirs[@]}"
    exit 1
  fi
  process_platform "$ship"
else
  # Process all ships
  for ship in "${!radiosonde_dirs[@]}"; do
    process_platform "$ship"
  done 
fi


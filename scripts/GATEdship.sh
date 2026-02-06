#!/opt/homebrew/bin/bash
##!/usr/bin/bash

# Check if Bash version is 4 or later
if [ ${BASH_VERSION:0:1} -lt 4 ]; then
  echo "Error: Bash 4 or later is required"
  echo "Under OSX try /opt/homebrew/bin/bash"
  exit 1
fi

thisdir=$(pwd)

# Define the ship directories with hash
declare -A ship_dirs
ship_dirs[GATE_AND_COMM_SHIPS]="DSHIP/GATE_AND_COMM_SHIPS/f61f70"
ship_dirs[METEOR]="DSHIP/METEOR/1a7095"
ship_dirs[FAY]="DSHIP/FAY/1a7095"
ship_dirs[FAYE]="DSHIP/FAYE/1a7095"
ship_dirs[PLANET]="DSHIP/PLANET/1a7095"
ship_dirs[JAMES_M_GILLISS]="DSHIP/JAMES_M_GILLISS/fdfbef"
ship_dirs[DALLAS]="DSHIP/DALLAS/fdfbef"
ship_dirs[RESEARCHER]="DSHIP/RESEARCHER/fdfbef"

# Function to perform the data conversion
process_platform() {
  local ship=$1
  in_path="${inbasedir}/${ship_dirs[$ship]}"
  hash=$(basename ${ship_dirs[$ship]})
  out_path="${outbasedir}/${ship_dirs[$ship]}"
  [ ! -d "$out_path" ] && mkdir -p "$out_path"
  cd ${out_path}
  cp ${in_path}/* .
  for infile in `ls -1`; do
    ${thisdir}/GATEdship_${hash}.x $infile
    mv ${infile}*.nc ../.
  done
  [ "$keepFiles" = false ] && rm -r ${out_path}
}

# Function to print help message
print_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -p, --platform <platform>     Specify the platform/ship to process [default all]"
  echo "  -i, --inbasedir <inbasedir>   Specify the base input directory, the path to DSHIP"
  echo "  -o, --outbasedir <outbasedir> Specify the base output directory"
  echo "  -k, --keep                    Keep ASCII files in output directory"
  echo "  -h, --help                    Print this help message"
  echo "Supported ships for -p are:"
  for ship in "${!ship_dirs[@]}"; do
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
  if [ ! ${ship_dirs[$ship]+_} ]; then
    echo "Invalid ship: $ship"
    echo "Available ships: ${!ship_dirs[@]}"
    exit 1
  fi
  # Process the specified ship
  process_platform "$ship"
else
  # Process all ships
  for ship in "${!ship_dirs[@]}"; do
    process_platform "$ship"
  done
fi

#!/opt/homebrew/bin/bash

# Check if Bash version is 4 or later
if [ ${BASH_VERSION:0:1} -lt 4 ]; then
  echo "Error: Bash 4 or later is required"
  echo "Under OSX try /opt/homebrew/bin/bash"
  exit 1
fi

thisdir=$(pwd)

# Define the aircraft directories with hash
declare -A aircraft_dirs
aircraft_dirs[NCAR_SABRE_MEANS]="AIRCRAFT/NCAR_SABRE_MEANS/db67b3"
aircraft_dirs[NOAA_DC-6_MEANS]="AIRCRAFT/NOAA_DC-6_MEANS/ced38b"
aircraft_dirs[NOAA_US-C130_MEANS]="AIRCRAFT/NOAA_US-C130_MEANS/ced38b"
aircraft_dirs[NCAR_ELECTRA_MEANS]="AIRCRAFT/NCAR_ELECTRA_MEANS/aa9a5e"
aircraft_dirs[NASA_CONVAIR_990_MEANS]="AIRCRAFT/NASA_CONVAIR_990_MEANS/e7f42f"
aircraft_dirs[DC-7_CEV]="AIRCRAFT/DC-7_CEV/579bd3"
aircraft_dirs[NCAR_QUEEN_AIR_MEANS]="AIRCRAFT/NCAR_QUEEN_AIR_MEANS/b3d264"
aircraft_dirs[UKHERCULES_XV208a]="AIRCRAFT/UKHERCULES_XV208/dab4ec"
aircraft_dirs[UKHERCULES_XV208b]="AIRCRAFT/UKHERCULES_XV208/96dd74"
aircraft_dirs[39_CHARLIE]="AIRCRAFT/39_CHARLIE/28c11d"

# Function to perform the data conversion
process_platform() {
  local aircraft=$1
  in_path="${inbasedir}/${aircraft_dirs[$aircraft]}"
  hash=$(basename ${aircraft_dirs[$aircraft]})

  # Additional special case for DC-7_CEV
  if [ "$aircraft" = "DC-7_CEV" ]; then
    out_path="${outbasedir}/AIRCRAFT/${aircraft}/d30c25"
    [ ! -d "$out_path" ] && mkdir -p "$out_path"
    cd ${out_path}
    cp ${in_path}/../d30c25/* .
    for infile in `ls -1`; do
      ${thisdir}/GATEaircraft_${hash}.x $infile
      mv ${infile}*.nc ../.
    done
    [ "$keepFiles" = false ] && rm -r ${out_path}
  fi
  out_path="${outbasedir}//AIRCRAFT/${aircraft}/${hash}"
  [ ! -d "$out_path" ] && mkdir -p "$out_path"
  cd ${out_path}
  cp ${in_path}/* .
  for infile in `ls -1`; do
    ${thisdir}/GATEaircraft_${hash}.x $infile
    mv ${infile}*.nc ../.
  done
  [ "$keepFiles" = false ] && rm -r ${out_path}
}

# Function to print help message
print_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -p, --platform <platform>     Specify the platform/aircraft to process [default all]"
  echo "  -i, --inbasedir <inbasedir>   Specify the base input directory, the path to AIRCRAFT"
  echo "  -o, --outbasedir <outbasedir> Specify the base output directory"
  echo "  -k, --keep                    Keep ASCII files in output directory"
  echo "  -h, --help                    Print this help message"
  echo "Supported aircrafts for -p are:"
  for aircraft in "${!aircraft_dirs[@]}"; do
    echo "  $aircraft"
  done
}

keepFiles=false

# Parse command-line options
while getopts ":hp:i:o:k" opt; do
  case $opt in
    h) print_help; exit 0;;
    p) aircraft=$OPTARG;;
    i) inbasedir=$OPTARG;;
    o) outbasedir=$OPTARG;;
    k) keepFiles=true;;
    \?) echo "Invalid option: -$OPTARG"; exit 1;;
  esac
done

# Check if the base directory option is provided
if [ -z "$inbasedir" ]; then
  echo "Please provide the base directory option using -i or --inbasedir"
  print_help
  exit 1
fi

# Check if the output base directory option is provided
if [ -z "$outbasedir" ]; then
  echo "Please provide the output base directory option using -o or --outbasedir"
  print_help
  exit 1
fi

# If aircraft is provided, check if it's valid
if [ -n "$aircraft" ]; then
  if [ ! ${aircraft_dirs[$aircraft]+_} ]; then
    echo "Invalid aircraft: $aircraft"
    echo "Available aircrafts: ${!aircraft_dirs[@]}"
    exit 1
  fi
  echo $aircraft
  process_platform "$aircraft"
else
  # Process all aircrafts
  for aircraft in "${!aircraft_dirs[@]}"; do
    process_platform "$aircraft"
  done
fi

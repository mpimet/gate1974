##!/opt/homebrew/bin/bash
#!//bin/bash


# Check if Bash version is 4 or later
if [ ${BASH_VERSION:0:1} -lt 4 ]; then
  echo "Error: Bash 4 or later is required"
  echo "Under OSX try /opt/homebrew/bin/bash"
  exit 1
fi

thisdir=$(pwd)

# Define the aircraft directories with hash
declare -A aircraft_dirs
aircraft_dirs[C130_DROPSONDE]="AIRCRAFT/C130_DROPSONDE/ecf73f"
aircraft_dirs[C135_DROPSONDE]="AIRCRAFT/C135_DROPSONDE/ecf73f"

# Function to perform the data conversion
process_platform() {
  local aircraft=$1
  in_path="${inbasedir}/${aircraft_dirs[$aircraft]}"
  hash=$(basename ${aircraft_dirs[$aircraft]})

  out_path="${outbasedir}/AIRCRAFT/${aircraft}/${hash}"
  [ ! -d "$out_path" ] && mkdir -p "$out_path"
  cd ${out_path}
  cp ${in_path}/* .
  for infile in `ls -1`; do
    ${thisdir}/GATEdropsonde.x $infile
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
  echo "Supported dropsondes for -p are:"
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

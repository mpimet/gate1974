#ifndef GATE_WRITE_BUOY_HYDRO_H
#define GATE_WRITE_BUOY_HYDRO_H

#include <netcdf>
#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include "GATEmetadata.h"
#include "GATEbuoy_hydro.h"

void write_netcdf_buoy_hydro(const std::string &infile, 
                  int no_of_measurements, 
                  std::vector<GATE_buoy_type> &dbuoydata,
                  const GATE_metadata_type &metadata);

#endif // GATE_WRITE_BUOY_HYDRO_H


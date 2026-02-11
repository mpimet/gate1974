#ifndef GATE_WRITE_BUOY_METEOR_H
#define GATE_WRITE_BUOY_METEOR_H

#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include <netcdf.h>
#include "GATEmetadata.h"
#include "GATEbuoy_meteor.h"

void write_netcdf_buoy_meteor(
		  const std::string &infile, 
                  int no_of_measurements, 
                  std::vector<GATE_buoy_type> &dbuoydata,
                  const GATE_metadata_type &metadata);

#endif // GATE_WRITE_BUOY_METEOR_H


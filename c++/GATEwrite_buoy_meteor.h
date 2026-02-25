#ifndef GATE_WRITE_BUOY_METEOR_H
#define GATE_WRITE_BUOY_METEOR_H

#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include <netcdf.h>
#include "GATEmetadata.h"
#include "GATEbuoy_meteor.h"

void
write_netcdf_buoy_meteor( std::string const&infile, 
                          int no_of_measurements, 
                          std::vector<GATE_buoy_type> const &dbuoydata,
                          GATE_metadata_type const &metadata);

#endif // GATE_WRITE_BUOY_METEOR_H


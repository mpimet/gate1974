#ifndef GATE_WRITE_COMM_SHIPS_H
#define GATE_WRITE_COMM_SHIPS_H

#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include <netcdf.h>
#include "GATEmetadata.h"
#include "GATEand_comm_ship.h"

void write_netcdf_comm_ships(
                  std::string const& infile, 
                  int nlon,
                  int nlat,
                  GATEand_comm_ship_type const commShipData,
                  GATE_metadata_type const& metadata);

#endif


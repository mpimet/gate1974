#ifndef GATE_WRITE_COMM_SHIPS_H
#define GATE_WRITE_COMM_SHIPS_H

#include <netcdf>
#include <string>
#include <vector>
#include <ctime>
#include <sstream>
#include "GATEmetadata.h"
#include "GATEand_comm_ship.h"

void write_netcdf_comm_ships(
                  const std::string &infile, 
                  int nlon,
		  int nlat,
                  GATEand_comm_ship_type commShipData,
                  const GATE_metadata_type &metadata);

#endif


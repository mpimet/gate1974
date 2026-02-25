#ifndef GATEAND_COMM_SHIP_H
#define GATEAND_COMM_SHIP_H

float const lon_min = -106.0f;
float const lon_max =   62.0f;
float const lat_min =  -22.0f;
float const lat_max =   38.0f;
float const res     =    0.5f;

struct GATEand_comm_ship_type {
    std::vector<int> sst;
};

#endif

#ifndef GATEAND_COMM_SHIP_H
#define GATEAND_COMM_SHIP_H

const float lon_min = -106.0f;
const float lon_max =   62.0f;
const float lat_min =  -22.0f;
const float lat_max =   38.0f;
const float res     =    0.5f;

struct GATEand_comm_ship_type {
    std::vector<int> sst;
};

#endif

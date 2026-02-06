#ifndef GATEMETADATA_H
#define GATEMETADATA_H

#include "GATEdate_time.h"

struct Position {
    int deg;
    int min;
    int sec;
};

struct GATE_metadata_type {
    std::string shipname1;
    std::string shipname2;
    DateTime time_start;
    DateTime time_end;
    DateTime time;
    int interval;
    std::string interval_unit;
    Position lat_start;
    Position lon_start;
    Position lat_end;
    Position lon_end;
    std::string temperature_unit = "kelvin";
    std::string humidity_unit = "kg/kg";
    std::string wind_unit = "m/s";
    std::string wind_dir_unit = "deg";
    std::string pressure_unit = "Pa";
};
#endif

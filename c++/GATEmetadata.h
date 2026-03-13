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
    static constexpr std::string temperature_unit = "kelvin";
    static constexpr std::string humidity_unit = "kg/kg";
    static constexpr std::string wind_unit = "m/s";
    static constexpr std::string wind_dir_unit = "deg";
    static constexpr std::string pressure_unit = "Pa";
};
#endif

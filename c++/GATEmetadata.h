#ifndef GATEMETADATA_H
#define GATEMETADATA_H

#include "GATEdate_time.h"

struct Position {
    int deg;
    int min;
    int sec;
};

struct GATE_metadata_type {

    std::string platform;
    std::string chief_scientist;
    std::string shipname1;
    std::string shipname2;
    std::string aircraftname;
    std::string title;
    std::string summary;
    std::string source;
    std::string keywords;
    std::string featureType;
    std::string instrument;

    static constexpr const char* references     = "https://www.eol.ucar.edu/field_projects/gate";
    static constexpr const char* provider_name  = "René Redler";
    static constexpr const char* provider_id    = "https://orcid.org/0000-0003-3117-3724";
    static constexpr const char* provider_email = "rene.redler@mpimet.mpg.de";
    static constexpr const char* license        = "CC-BY-4.0";
    static constexpr const char* conventions    = "ACDD-1.3, CF-1.12";

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

void set_metadata(int ncid, const GATE_metadata_type& metadata);

#endif

#ifndef GATEBUOY_HYDRO_H
#define GATEBUOY_HYDRO_H

struct GATE_buoy_type {
    int sequence;
    char quality[6];
    int julian_day;
    char time[5];
    int wind_direction;
    float wind_speed;
    float dry_bulb_temperature;
    float sea_level_pressure;

    // for later splitting the quality string
    int val_time;
    int val_wind_speed;
    int val_wind_direction;
    int val_dry_bulb_temperature;
    int val_sea_level_pressure;
    int seconds_of_year;
};

#endif

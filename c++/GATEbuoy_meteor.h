#ifndef GATEBUOY_METEOR_H
#define GATEBUOY_METEOR_H

struct GATE_buoy_type {
    float time;
    float day;
    float wind_speed;
    float val_wind_speed;
    float wind_direction;
    float val_wind_direction;
    float dry_bulb_temp;
    float val_dry_bulb_temp;
    float spec_humidity;
    float val_spec_humidity;
    float water_temperature;
    float val_water_temperature;
};

#endif

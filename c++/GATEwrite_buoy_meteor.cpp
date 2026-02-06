#include <netcdf>
#include <string>
#include <vector>
#include "GATEnetcdf.h"
#include "GATEmetadata.h"
#include "GATEwrite_buoy_meteor.h"

void write_netcdf_buoy_meteor(
		  const std::string &infile,
                  int no_of_measurements,
                  std::vector<GATE_buoy_type> &dbuoydata,
                  const GATE_metadata_type &metadata) {

    std::string outfile = infile + ".nc";
    std::string seconds_since;
    std::string position_str;

    std::ostringstream seconds_since_stream;
    seconds_since_stream << "seconds since " << metadata.time_start.year
                         << '-' << metadata.time_start.month
                         << '-' << metadata.time_start.day
                         << ' ' << 0
                         << ':' << 0
                         << ':' << 0;
    seconds_since = seconds_since_stream.str();

    std::ostringstream position_str_stream;
    position_str_stream << std::fixed;
    position_str_stream.precision(4);

    position_str_stream << metadata.lon_start.deg +
                          (metadata.lon_start.min * 60 + metadata.lon_start.sec) / 3600.0
                           << ' ' <<
                           metadata.lat_start.deg +
                          (metadata.lat_start.min * 60 + metadata.lat_start.sec) / 3600.0;

    position_str = position_str_stream.str();

    int ncid;
    int dimids[2];
    int time_id, measurement_id, timelenght_id;
    int ws_id, wd_id, dbt_id, q_id, sst_id;

    const float fill_value_1   =  -1.0f;
    const float fill_value_999 = 999.9f;

    handle_err(nc_create(outfile.c_str(), NC_CLOBBER, &ncid));

    handle_err(nc_def_dim(ncid, "measurement", 1, &measurement_id));
    handle_err(nc_def_dim(ncid, "time", NC_UNLIMITED, &timelenght_id));

    dimids[0] = timelenght_id;
    dimids[1] = measurement_id;

    handle_err(nc_def_var(ncid, "time", NC_FLOAT, 1, &dimids[0], &time_id));

    handle_err(nc_put_att_text(ncid, time_id, "units", seconds_since.length(), seconds_since.c_str()));
    handle_err(nc_put_att_text(ncid, time_id, "calendar", 19, "proleptic_gregorian"));

    ws_id  = defineVariableAndAttribute<float, 2>(ncid, dimids, "ws",  "wind_speed",              "wind speed",              metadata.wind_unit,        fill_value_1);
    wd_id  = defineVariableAndAttribute<float, 2>(ncid, dimids, "wd",  "wind_from_direction",     "wind from direction",     metadata.wind_dir_unit,    fill_value_1);
    q_id   = defineVariableAndAttribute<float, 2>(ncid, dimids, "q",   "specific_humidity",       "specific humidity",       metadata.humidity_unit,    fill_value_999);
    dbt_id = defineVariableAndAttribute<float, 2>(ncid, dimids, "dbt", "dry_bulb_temperature",    "dry bulb temperature",    metadata.temperature_unit, fill_value_999);
    sst_id = defineVariableAndAttribute<float, 2>(ncid, dimids, "sst", "sea_surface_temperature", "sea surface temperature", metadata.temperature_unit, fill_value_999);

    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "platform", metadata.shipname1.length(), metadata.shipname1.c_str()));
    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "shipname", metadata.shipname2.length(), metadata.shipname2.c_str()));

    std::time_t now = std::time(nullptr);
    std::tm *timeinfo = std::localtime(&now);

    std::ostringstream history_stream;
    history_stream << "Created by Rene Redler, MPI-M on " << (1900 + timeinfo->tm_year) << "-"
                   << (1 + timeinfo->tm_mon) << "-" << timeinfo->tm_mday << " "
                   << timeinfo->tm_hour << ":" << timeinfo->tm_min << ":" << timeinfo->tm_sec
                   << " from data in archive directory 3.36.21.102-3.60.02.105_19740601-19740930.";
    std::string history = history_stream.str();

    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "history", history.length(), history.c_str()));
    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "buoy_start_position", 18, position_str.c_str()));

    std::ostringstream position_end_str_stream;
    position_end_str_stream << std::fixed;
    position_end_str_stream.precision(4);
    position_end_str_stream << metadata.lon_end.deg +
                              (metadata.lon_end.min * 60 + metadata.lon_end.sec) / 3600.0
                               << ' ' <<
                               metadata.lat_end.deg +
                              (metadata.lat_end.min * 60 + metadata.lat_end.sec) / 3600.0;

    std::string position_end_str = position_end_str_stream.str();
    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "buoy_end_position", 18, position_end_str.c_str()));

    handle_err(nc_enddef(ncid));

    size_t start[2] = {0, 0};
    size_t edge[2]  = {static_cast<size_t>(no_of_measurements), 1};

    std::vector<float> measurement_time(no_of_measurements);
    std::vector<float> ws(no_of_measurements);
    std::vector<float> wd(no_of_measurements);
    std::vector<float> q(no_of_measurements);
    std::vector<float> dbt(no_of_measurements);
    std::vector<float> sst(no_of_measurements);

    for (int i = 0; i < no_of_measurements; ++i) {
      auto& rec = dbuoydata[i];
      auto validv = [](auto v1, auto v2) { return (int(v1) == 1 && v2 >= 0.0f); };
      auto validw = [](auto w) { return int(w) == 1; };

      measurement_time[i] = rec.time;

      ws[i]  = validw(rec.val_wind_speed)                               ? rec.wind_speed : -1.0f;
      wd[i]  = validw(rec.val_wind_direction)                           ? rec.wind_direction : -1.0f;
      q[i]   = validv(rec.val_spec_humidity,     rec.spec_humidity)     ? rec.spec_humidity / 1000.0f : 999.9f;
      dbt[i] = validv(rec.val_dry_bulb_temp,     rec.dry_bulb_temp)     ? rec.dry_bulb_temp + 273.15f : 999.9f;
      sst[i] = validv(rec.val_water_temperature, rec.water_temperature) ? rec.water_temperature + 273.15f : 999.9f;
    }
   
    handle_err(nc_put_vara_float(ncid, time_id, start, edge, &measurement_time[0]));

    handle_err(nc_put_vara_float(ncid, ws_id,  start, edge, &ws[0]));
    handle_err(nc_put_vara_float(ncid, wd_id,  start, edge, &wd[0]));
    handle_err(nc_put_vara_float(ncid, q_id,   start, edge, &q[0]));
    handle_err(nc_put_vara_float(ncid, dbt_id, start, edge, &dbt[0]));
    handle_err(nc_put_vara_float(ncid, sst_id, start, edge, &sst[0]));

    handle_err(nc_close(ncid));
}

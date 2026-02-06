#include <netcdf>
#include <string>
#include <vector>
#include "GATEnetcdf.h"
#include "GATEmetadata.h"
#include "GATEwrite_comm_ships.h"

void write_netcdf_comm_ships(
                  const std::string &infile,
                  int nlon,
		  int nlat,
                  GATEand_comm_ship_type commShipData,
                  const GATE_metadata_type &metadata) {

    std::string outfile = infile + ".nc";
    std::string seconds_since;
    std::string position_str;

    std::ostringstream seconds_since_stream;
    seconds_since_stream << "seconds since " << metadata.time.year
                         << '-' << metadata.time.month
                         << '-' << metadata.time.day
                         << ' ' << metadata.time.hour
                         << ':' << metadata.time.minute
                         << ':' << metadata.time.second;
    seconds_since = seconds_since_stream.str();

    int ncid;
    int dimids[3];
    int time_id, lon_id, lat_id;
    int time_dimid, lon_dimid, lat_dimid;
    int sst_id;

    const float fill_value_999 = 999.9f;

    handle_err(nc_create(outfile.c_str(), NC_CLOBBER, &ncid));

    handle_err(nc_def_dim(ncid, "time", 1, &time_dimid));
    handle_err(nc_def_dim(ncid, "lon", nlon, &lon_dimid));
    handle_err(nc_def_dim(ncid, "lat", nlat, &lat_dimid));

    dimids[0] = time_dimid;
    dimids[1] = lat_dimid;
    dimids[2] = lon_dimid;

    handle_err(nc_def_var(ncid, "time", NC_FLOAT, 1, &dimids[0], &time_id));
    handle_err(nc_put_att_text(ncid, time_id, "units", seconds_since.length(), seconds_since.c_str()));
    handle_err(nc_put_att_text(ncid, time_id, "calendar", 19, "proleptic_gregorian"));

    lat_id = defineVariableAndAttribute<float, 1>(ncid, &dimids[1], "lat", "latitude", "latitude", "degrees_north");
    lon_id = defineVariableAndAttribute<float, 1>(ncid, &dimids[2], "lon", "longitude", "longitude", "degrees_east");
    sst_id = defineVariableAndAttribute<float, 3>(ncid, dimids, "sst", "sea_surface_temperature", "sea surface temperature", metadata.temperature_unit, fill_value_999);

    std::time_t now = std::time(nullptr);
    std::tm *timeinfo = std::localtime(&now);

    std::ostringstream history_stream;
    history_stream << "Created by Rene Redler, MPI-M on " << (1900 + timeinfo->tm_year) << "-"
                   << (1 + timeinfo->tm_mon) << "-" << timeinfo->tm_mday << " "
                   << timeinfo->tm_hour << ":" << timeinfo->tm_min << ":" << timeinfo->tm_sec
                   << " from from GATE_AND_COMM_SHIPS mapped sst data in archive directory 3.00.02.104-3.31.02.101_19740601-19740930.";
    std::string history = history_stream.str();

    handle_err(nc_put_att_text(ncid, NC_GLOBAL, "history", history.length(), history.c_str()));

    handle_err(nc_enddef(ncid));

    // Time axis

    float time[1] = {0.0};

    size_t start1d = 0;
    size_t count1d = 1;

    handle_err(nc_put_vara_float(ncid, time_id, &start1d, &count1d, &time[0]));

    // Horizontal axis

    std::vector<float> lon(nlon);
    std::vector<float> lat(nlat);

    for (int i = 0; i < nlon; ++i) {
      lon[i] = lon_min + i * res;
    }

    for (int j = 0; j < nlat; ++j) {
      lat[j] = lat_min + j * res;
    }

    handle_err(nc_put_var_float(ncid, lon_id, lon.data()));
    handle_err(nc_put_var_float(ncid, lat_id, lat.data()));

    // Sea surface temperature

    std::vector<float> sst;
    std::transform(commShipData.sst.begin(), commShipData.sst.end(), std::back_inserter(sst),
		   [fill_value_999](int x) { return (x == 0) ? fill_value_999 : static_cast<float>(x) / 10.0f + 273.15f; });

    size_t start3d[3] = {0, 0, 0};
    size_t count3d[3] = {1, static_cast<size_t>(nlat), static_cast<size_t>(nlon)};

    handle_err(nc_put_vara_float(ncid, sst_id, start3d, count3d, sst.data()));

    handle_err(nc_close(ncid));
}

#ifndef GATE_NETCDF_H
#define GATE_NETCDF_H

#include <netcdf>
#include <iostream>

inline void handle_err(int status) {
    if (status != NC_NOERR) {
        std::cerr << "NetCDF error: " << nc_strerror(status) << std::endl;
        std::exit(1);
    }
}

template<typename T, int NDIMS>
int defineVariableAndAttribute(int ncid,
                               int dimids[NDIMS],
                               const std::string &var_name,
                               const std::string &std_name,
                               const std::string &long_name,
                               const std::string &units,
                               std::optional<T> fill_value = std::nullopt) {
    int var_id;
    int nc_type;
    
    // Map C++ types to NetCDF types
    if constexpr (std::is_same_v<T, float>) {
        nc_type = NC_FLOAT;
    } else if constexpr (std::is_same_v<T, double>) {
        nc_type = NC_DOUBLE;
    } else if constexpr (std::is_same_v<T, int>) {
        nc_type = NC_INT;
    } else {
        static_assert(sizeof(T) == 0, "Unsupported type");
    }

    handle_err(nc_def_var(ncid, var_name.c_str(), nc_type, NDIMS, dimids, &var_id));

    handle_err(nc_put_att_text(ncid, var_id, "standard_name", std_name.length(), std_name.c_str()));
    handle_err(nc_put_att_text(ncid, var_id, "long_name", long_name.length(), long_name.c_str()));
    handle_err(nc_put_att_text(ncid, var_id, "units", units.length(), units.c_str()));
    
    // Only set _FillValue if explicitly provided
    if (fill_value.has_value()) {
        if constexpr (std::is_same_v<T, float>) {
            handle_err(nc_put_att_float(ncid, var_id, "_FillValue", NC_FLOAT, 1, &fill_value.value()));
        } else if constexpr (std::is_same_v<T, double>) {
            handle_err(nc_put_att_double(ncid, var_id, "_FillValue", NC_DOUBLE, 1, &fill_value.value()));
        } else if constexpr (std::is_same_v<T, int>) {
            handle_err(nc_put_att_int(ncid, var_id, "_FillValue", NC_INT, 1, &fill_value.value()));
        }
    }

    return var_id;
}
#endif

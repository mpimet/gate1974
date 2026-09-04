#include "GATEdate_time.h"
#include "GATEnetcdf.h"

// set metadata attributes in a NetCDF file
void set_metadata(int ncid, const GATE_metadata_type& metadata) {
    // Helper to safely write a text attribute
    auto put_att = [&](const std::string& name, const std::string& value) {
        if (!value.empty()) {
            int len = static_cast<int>(value.length());
            handle_err(nc_put_att_text(ncid, NC_GLOBAL, name.c_str(), len, value.c_str()));
        }
    };

    // Set each attribute (same as Fortran)
    put_att("title", metadata.title);
    put_att("summary", metadata.summary);
    put_att("source", metadata.source);
    put_att("keywords", metadata.keywords);
    put_att("featureType", metadata.featureType);
    put_att("platform", metadata.platform);
    put_att("instrument", metadata.instrument);
    put_att("references", metadata.references);
    put_att("creator_name", metadata.chief_scientist);
    put_att("provider_name", metadata.provider_name);
    put_att("provider_email", metadata.provider_email);
    put_att("provider_id", metadata.provider_id);
    put_att("license", metadata.license);
    put_att("conventions", metadata.conventions);
}


#include <iostream>
#include <fstream>
#include <string>

#include <vector>
#include <stdexcept>

#include "GATEmetadata.h"
#include "GATEand_comm_ship.h"
#include "GATEdate_time.h"
#include "GATEwrite_comm_ships.h"

void read_metadata(std::ifstream& file, GATE_metadata_type& metadata);
int convert_data(const std::string& infile);

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <inputfile>" << std::endl;
        return EXIT_FAILURE;
    }

    std::ifstream infile(argv[1]);
    if (!infile.is_open()) {
        std::cerr << "Error opening file: " << argv[1] << std::endl;
        return EXIT_FAILURE;
    }

    std::string line;
    std::getline(infile, line);

    switch (line[0]) {
        case '0':
            std::cout << "Found tape header record type 0 in " << argv[1] << std::endl;
            break;
        case '1':
            std::cout << "Processing " << argv[1] << std::endl;
            convert_data(argv[1]);
            break;
        default:
            std::cerr << argv[1] << " does not contain data" << std::endl;
            break;
    }

    infile.close();
    return EXIT_SUCCESS;
}

void read_metadata(std::ifstream& file, GATE_metadata_type& metadata) {
    std::string line;

    for (int i = 1; i <= 24; ++i) {
        std::getline(file, line);
        switch (i) {
            case 4:
                metadata.time.year   = std::stoi(line.substr( 1, 4));
                metadata.time.month  = std::stoi(line.substr( 5, 2));
                metadata.time.day    = std::stoi(line.substr( 7, 2));
                metadata.time.hour   = std::stoi(line.substr( 9, 2));
                metadata.time.minute = std::stoi(line.substr(11, 2));
                metadata.time.second = std::stoi(line.substr(13, 2));
                break;
        }
        metadata.interval = 1;
        metadata.interval_unit = "D";
    }
    metadata.title           = "GATE mapped sea-surface temperature and salinity data";
    metadata.summary         = "Daily mean mapped data on a 0.5 x 0.5 deg mesh in the region 106W-62E, 22S-38N as part of the "
                               "Global Atmospheric Research Program''s Atlantic Tropical Experiment (GATE, 1974)."
                               "Data were collected from research and commerical vessels.";
    metadata.chief_scientist = "Forida State University";
    metadata.source          = "ship";
    metadata.keywords        = "GATE, sea-surface temperature, sea-surface salnity, oceanography";
    metadata.featureType     = "";
    metadata.platform        = "";
    metadata.instrument      = "various instruments";

}

std::vector<int> parseFixedWidthIntegers(std::string const& input) {
    std::vector<int> result;
    
    // Process the string in chunks of 3 characters
    for (size_t i = 0; i < input.length(); i += 3) {
        // Check if we have enough characters left for a complete 3-character chunk
        if (i + 3 > input.length()) {
            std::cerr << "Warning: Incomplete chunk at position " << i 
                      << ". Skipping." << std::endl;
            break;
        }
        
        // Extract 3 characters
        std::string chunk = input.substr(i, 3);
        
        // Convert to integer using std::stoi - it automatically handles leading spaces
        try {
            int num = std::stoi(chunk);
            result.push_back(num);
        } catch (std::exception const& e) {
            std::cerr << "Error converting chunk '" << chunk << "': " << e.what() << std::endl;
        }
    }
    
    return result;
}

int convert_data(std::string const& infile) {
    std::string line; // Declare the line variable here
    std::ifstream file(infile);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << infile << std::endl;
        return EXIT_FAILURE;
    }

    GATE_metadata_type metadata;
    GATEand_comm_ship_type commShipData;

    int ierror = 0;

    // Read metadata from the file
    read_metadata(file, metadata);

    // Skip next 24 lines
    for (int i = 0; i < 24; ++i) {
        std::getline(file, line);
    }

    // Calculate the number of grid points in each direction
    int const nlon = static_cast<int>((lon_max - lon_min) / res) + 1;
    int const nlat = static_cast<int>((lat_max - lat_min) / res) + 1;

    // Start with data section
    while (ierror == 0) {
        std::string record_lines;
        for (int i = 0; i < 24; ++i) {
            std::getline(file, line);
            if (file.eof()) {
	      ierror = 1;
                break;
            }
            record_lines += line;
        }
        record_lines += "\n";

        if (ierror != 0) break;

        int type_id, records_in_line, records_handled, line_number;

        std::sscanf(record_lines.c_str(), "%1d %4d %10d %5d",
                    &type_id,
                    &records_in_line,
                    &records_handled,
                    &line_number);

        char const *ptr = record_lines.c_str() + 20;
        size_t const chars_to_parse = nlon * 3;
        std::string const limited_ptr(ptr, chars_to_parse);

        std::vector<int> sstBand = parseFixedWidthIntegers(limited_ptr);
        commShipData.sst.insert(commShipData.sst.end(),
                                std::make_move_iterator(sstBand.begin()), 
                                std::make_move_iterator(sstBand.end()));
    }
    if ( commShipData.sst.size() != nlon*nlat ) {
        std::cout << "Number of parsed integers "
                  << commShipData.sst.size() << " does not equal "
                  << nlon*nlat << std::endl;
        return EXIT_FAILURE;
    }
    file.close();

    write_netcdf_comm_ships(infile, nlon, nlat, commShipData, metadata);
    return EXIT_SUCCESS;
}

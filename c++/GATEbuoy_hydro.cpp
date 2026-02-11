#include <iostream>
#include <fstream>
#include <string>

#include "GATEutils.h"
#include "GATEmetadata.h"
#include "GATEbuoy_hydro.h"
#include "GATEdate_time.h"
#include "GATEwrite_buoy_hydro.h"

void read_metadata(std::ifstream& file, GATE_metadata_type& metadata);
void convert_data(const std::string& infile);

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <inputfile>" << std::endl;
        return 1;
    }

    std::ifstream infile(argv[1]);
    if (!infile.is_open()) {
        std::cerr << "Error opening file: " << argv[1] << std::endl;
        return 1;
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
    return 0;
}

void read_metadata(std::ifstream& file, GATE_metadata_type& metadata) {
    std::string line;

    for (int i = 1; i <= 24; ++i) {
        std::getline(file, line);
        switch (i) {
            case 2:
                metadata.shipname1 = line.substr(15, 24);
                break;
            case 3:
                metadata.shipname2 = line.substr(15, 24);
                break;
            case 4:
                metadata.time_start.year   = std::stoi(line.substr(1, 4));
                metadata.time_start.month  = std::stoi(line.substr(5, 2));
                metadata.time_start.day    = std::stoi(line.substr(7, 2));
                metadata.time_start.hour   = std::stoi(line.substr(9, 2));
                metadata.time_start.minute = std::stoi(line.substr(11, 2));
                metadata.time_start.second = std::stoi(line.substr(13, 2));

                metadata.lat_start.deg = std::stoi(line.substr(19, 2));
                metadata.lat_start.min = std::stoi(line.substr(21, 2));
                metadata.lat_start.sec = std::stoi(line.substr(23, 2));
                metadata.lon_start.deg = std::stoi(line.substr(25, 4));
                metadata.lon_start.min = std::stoi(line.substr(29, 2));
                metadata.lon_start.sec = std::stoi(line.substr(31, 2));

                break;
            case 5:
                metadata.time_end.year   = std::stoi(line.substr(1, 4));
                metadata.time_end.month  = std::stoi(line.substr(5, 2));
                metadata.time_end.day    = std::stoi(line.substr(7, 2));
                metadata.time_end.hour   = std::stoi(line.substr(9, 2));
                metadata.time_end.minute = std::stoi(line.substr(11, 2));
                metadata.time_end.second = std::stoi(line.substr(13, 2));

		metadata.lat_end.deg = std::stoi(line.substr(19, 2));
                metadata.lat_end.min = std::stoi(line.substr(21, 2));
                metadata.lat_end.sec = std::stoi(line.substr(23, 2));
                metadata.lon_end.deg = std::stoi(line.substr(25, 4));
                metadata.lon_end.min = std::stoi(line.substr(29, 2));
                metadata.lon_end.sec = std::stoi(line.substr(31, 2));

                break;
            case 7:
                metadata.interval      = std::stoi(line.substr(16, 9));
                metadata.interval_unit = line[25];
                break;
        }
    }
}

void convert_data(const std::string& infile) {
    std::string line; // Declare the line variable here
    std::ifstream file(infile);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << infile << std::endl;
        return;
    }

    GATE_metadata_type metadata;
    std::vector<GATE_buoy_type> buoydata;
    std::vector<GATE_buoy_type> dbuoydata;

    int ierror = 0;
    int no_of_measurement = 0;

    // Read metadata from the file
    read_metadata(file, metadata);

    // Skip next 24 lines
    for (int i = 0; i < 24; ++i) {
        std::getline(file, line);
    }

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

      constexpr int BUOY_DATA_IN_LINE = 46;

      std::vector<GATE_buoy_type> buoydata(BUOY_DATA_IN_LINE);

      std::vector<float> buoy_time[BUOY_DATA_IN_LINE];

      std::sscanf(record_lines.c_str(), "%1d %4d %10d %5d",
                  &type_id,
                  &records_in_line,
                  &records_handled,
                  &line_number);

      const char *ptr = record_lines.c_str() + 20;

      // Derived from Fortran format I4,A5,I4,A4,I4,2F6.2,F8.2
      // const char* const format = "%4d %5c %4d %4c %4i %6f %6f %8f";
      // const int RECORD_WIDTH = calculate_record_width(format);
      //
      // for (int i = 0; i < BUOY_DATA_IN_LINE; i++) {
      // 	std::sscanf(ptr,
      // 		    format,
      // 		    &buoydata[i].sequence,
      // 		    buoydata[i].quality,
      // 		    &buoydata[i].julian_day,
      // 		    buoydata[i].time,
      // 		    &buoydata[i].wind_direction,
      // 		    &buoydata[i].wind_speed,
      // 		    &buoydata[i].dry_bulb_temperature,
      // 		    &buoydata[i].sea_level_pressure);
      // 	std::cout << buoydata[i].sequence   << " "  << buoydata[i].quality << " "
      // 		  << buoydata[i].julian_day << " "  << buoydata[i].time << std::endl;
      //
      // 	ptr += RECORD_WIDTH;
      // }
      //

      for (int i = 0; i < BUOY_DATA_IN_LINE; i++)
        {
	  std::sscanf(ptr, "%4d", &buoydata[i].sequence);
          ptr += 4;

          std::sscanf(ptr, "%5c", buoydata[i].quality);
          buoydata[i].quality[5]='\0'; 
          ptr += 5;

          char temp[4];
          std::sscanf(ptr, "%4c",temp);
          buoydata[i].julian_day = atoi(temp);
          ptr += 4;

          std::sscanf(ptr, "%4c", buoydata[i].time);
          buoydata[i].time[4]='\0'; 
          ptr += 4;

          std::sscanf(ptr, "%4i", &buoydata[i].wind_direction);
          ptr += 4;

          std::sscanf(ptr, "%6f", &buoydata[i].wind_speed);
          ptr += 6;

          std::sscanf(ptr, "%6f", &buoydata[i].dry_bulb_temperature);
          ptr += 6;

          std::sscanf(ptr, "%8f", &buoydata[i].sea_level_pressure);
          ptr += 8;

          buoydata[i].val_time                 = atoi(&buoydata[i].quality[0]);
          buoydata[i].val_wind_direction       = atoi(&buoydata[i].quality[1]);
          buoydata[i].val_wind_speed           = atoi(&buoydata[i].quality[2]);
          buoydata[i].val_dry_bulb_temperature = atoi(&buoydata[i].quality[3]);
          buoydata[i].val_sea_level_pressure   = atoi(&buoydata[i].quality[4]);
      }
      for (int i = 0; i < BUOY_DATA_IN_LINE; ++i) {
        if ( buoydata[i].val_time == 0 ) {

          // Extract hour and minute from time string
          int hours = 0, minutes = 0, seconds = 0;
          sscanf(buoydata[i].time, "%2d%2d", &hours, &minutes);
          int buoyTime = buoydata[i].julian_day * 86400 + hours * 3600 + minutes * 60 + seconds;
          buoydata[i].seconds_of_year = buoyTime;

          if ( no_of_measurement == 0 ) {
            no_of_measurement++;
            dbuoydata.push_back(buoydata[i]);
          }
          if ( no_of_measurement > 0 && buoyTime > dbuoydata[no_of_measurement-1].seconds_of_year ) {
            no_of_measurement++;
            dbuoydata.push_back(buoydata[i]);
          } else {
            std::cout << "WARNING for time: " << buoydata[i].julian_day << " "
                      << hours << ":" << minutes << ":" << seconds << " "
                      << buoyTime << " " << std::endl;
          }
        }
      }
    }

    file.close();

    write_netcdf_buoy_hydro(infile, no_of_measurement, dbuoydata, metadata);
}

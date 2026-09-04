cmake_minimum_required(VERSION 3.14)
project(GATEbuoy LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(NOT DEFINED NETCDF_ROOT)
    message(FATAL_ERROR "NETCDF_ROOT must be set")
endif()

set(NETCDF_INCLUDE_DIR "${NETCDF_ROOT}/include")
set(NETCDF_LIBRARY_DIR "${NETCDF_ROOT}/lib")

include_directories(${NETCDF_INCLUDE_DIR})
link_directories(${NETCDF_LIBRARY_DIR})

set(MAIN_SRCS
    GATEbuoy_meteor.cpp
    GATEbuoy_hydro.cpp
    GATEandCOMM_SHIPS.cpp
)

set(LIB_SRCS
    GATEdate_time.cpp
    GATEmetadata.cpp
    GATEwrite_buoy_meteor.cpp
    GATEwrite_comm_ships.cpp
    GATEwrite_buoy_hydro.cpp
    GATEutils.cpp
)

# Derives executable names from MAIN_SRCS
set(EXECUTABLES "")
foreach(src ${MAIN_SRCS})
    get_filename_component(name ${src} NAME_WE)
    list(APPEND EXECUTABLES ${name})
endforeach()

add_library(gate_lib STATIC ${LIB_SRCS})

# Build executables
foreach(src ${MAIN_SRCS})
    get_filename_component(name ${src} NAME_WE)
    add_executable(${name} ${src})
    target_link_libraries(${name} PRIVATE gate_lib netcdf netcdf-cxx4)
endforeach()

# Cleaning, use derived EXECUTABLES
add_custom_target(distclean
    COMMAND ${CMAKE_COMMAND} -E remove -f
        ${EXECUTABLES}  # ← Automatically derived
        *.o
        *.a
        *.so
        *.dll
        *.exe
        *.dylib
    COMMENT "Remove object files, libraries, and executables"
)

# runs distclean, then removes config files
add_custom_target(realclean
    DEPENDS distclean
    COMMAND ${CMAKE_COMMAND} -E remove -f -r
        CMakeCache.txt
        cmake_install.cmake
        Makefile
        COMMAND ${CMAKE_COMMAND} -E remove_directory CMakeFiles/
    COMMENT "Fully clean build directory"
)

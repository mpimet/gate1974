# Makefile for compiling GATEbuoy_meteor and GATEandCOMM_SHIPS

# Compiler
CXX = g++

# Include and library paths for NetCDF
NETCDF_ROOT=/opt/homebrew
NETCDF_INCLUDE=${NETCDF_ROOT}/include
NETCDF_LIB=${NETCDF_ROOT}/lib

# Compiler flags
CXXFLAGS = -std=c++17 -Wall -Wextra -I${NETCDF_INCLUDE}
LDFLAGS = -L${NETCDF_LIB} -lnetcdf -lnetcdf-cxx4

# Main program source files (contain main())
MAIN_SRCS = GATEbuoy_meteor.cpp GATEbuoy_hydro.cpp GATEandCOMM_SHIPS.cpp

# Library source files (contain functions only)
LIB_SRCS = GATEdate_time.cpp GATEwrite_buoy_meteor.cpp GATEwrite_comm_ships.cpp GATEwrite_buoy_hydro.cpp GATEutils.cpp 

# Object files
MAIN_OBJS = $(MAIN_SRCS:.cpp=.o)
LIB_OBJS = $(LIB_SRCS:.cpp=.o)

# Output executables
EXECUTABLES = $(MAIN_SRCS:.cpp=)

# Explicit dependency: main executables depend on library object files
$(EXECUTABLES): $(LIB_OBJS)

# Default target
all: $(EXECUTABLES)

# Pattern rule for linking executables
%.exe: %.o $(LIB_OBJS)
	$(CXX) $(LDFLAGS) $^ -o $@

# Pattern rule for compiling source files
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean the build
clean:
	rm -f $(MAIN_OBJS) $(LIB_OBJS) $(EXECUTABLES)

.PHONY: all clean


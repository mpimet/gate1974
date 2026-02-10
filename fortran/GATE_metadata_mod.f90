module GATE_metadata_mod

  type :: datetime
     integer :: year
     integer :: month
     integer :: day
     integer :: hour
     integer :: minute
     integer :: second
  end type datetime

  type :: position
     integer :: deg
     integer :: min
     integer :: sec
  end type position

  type :: GATE_metadata_type
     character(len=32) :: platform
     character(len=32) :: shipname1
     character(len=32) :: shipname2
     character(len=32) :: aircraftname

     type (datetime)   :: time
     type (datetime)   :: time_start
     type (datetime)   :: time_end
     type (position)   :: lat_start
     type (position)   :: lon_start
     type (position)   :: lat_end
     type (position)   :: lon_end

     integer           :: interval

     character(len=1)  :: interval_unit
     character(len=2)  :: pressure_unit='Pa'
     character(len=1)  :: altitude_unit='m'
     character(len=6)  :: temperature_unit='kelvin'
     character(len=6)  :: rhov_unit='kg/m-3'
     character(len=5)  :: specific_humidity_unit='kg/kg'
     character(len=3)  :: wind_unit='m/s' 
     character(len=3)  :: wind_dir_unit='deg'
     character(len=6)  :: radiation_unit='W/m**2'
     character(len=7)  :: water_vapour_density='kg/m**3'
  end type GATE_metadata_type

  public :: datetime, position, GATE_metadata_type

end module GATE_metadata_mod

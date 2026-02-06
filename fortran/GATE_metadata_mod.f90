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
     character(len=32) :: aircraftname
     type (datetime)   :: measurement_time_start
     integer           :: interval
     character(len=1)  :: interval_unit
     character(len=2)  :: pressure_unit='Pa'
     character(len=1)  :: altitude_unit='m'
     character(len=6)  :: temperature_unit='kelvin'
     character(len=6)  :: rhov_unit='kg/m-3'
     character(len=2)  :: specific_humidity_unit='kg'
  end type GATE_metadata_type

  public :: datetime, position, GATE_metadata_type

end module GATE_metadata_mod

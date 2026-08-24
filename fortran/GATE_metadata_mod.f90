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

     character(len= 32) :: platform
     character(len=128) :: chief_scientist
     character(len= 32) :: shipname1
     character(len= 32) :: shipname2
     character(len= 32) :: aircraftname
     character(len= 80) :: title
     character(len=300) :: summary
     character(len= 80) :: source
     character(len= 64) :: keywords
     character(len= 32) :: featureType
     character(len= 32) :: instrument
 
     character(len= 64) :: references     = 'https://www.eol.ucar.edu/field_projects/gate'
     character(len= 32) :: provider_name  = 'René Redler'
     character(len= 64) :: provider_id    = 'https://orcid.org/0000-0003-3117-3724'
     character(len= 32) :: provider_email  = 'rene.redler@mpimet.mpg.de'
     character(len= 16) :: license        = 'CC-BY-4.0' 
     character(len= 32) :: conventions    = 'ACDD-1.3, CF-1.12'

     type (datetime)   :: time
     type (datetime)   :: time_start
     type (datetime)   :: time_end
     type (position)   :: lat_start
     type (position)   :: lon_start
     type (position)   :: lat_end
     type (position)   :: lon_end

     integer           :: interval

     character(len=1)  :: altitude_unit='m'
     character(len=1)  :: interval_unit
     character(len=2)  :: pressure_unit='Pa'
     character(len=6)  :: radiation_unit='W/m**2'
     character(len=6)  :: rhov_unit='kg/m-3'
     character(len=1)  :: relative_humidity_unit='%'
     character(len=5)  :: specific_humidity_unit='kg/kg'
     character(len=6)  :: temperature_unit='kelvin'
     character(len=7)  :: water_vapour_density_unit='kg/m**3'
     character(len=3)  :: wind_dir_unit='deg'
     character(len=3)  :: wind_unit='m/s' 
  end type GATE_metadata_type

  public :: datetime, position, GATE_metadata_type

end module GATE_metadata_mod

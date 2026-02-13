#define FILENAME_LENGHT 132
module GATEdropsonde_mod

  use GATE_metadata_mod

  integer, parameter :: no_of_levels_in_line = 19

  type :: GATE_dropsonde_type
     ! do not change the sequence within this type
     integer :: TIME
     real :: PRESSURE
     real :: ALTITUDE
     real :: DEW_POINT
     real :: TEMPERATURE
     real :: RELATIVE_HUMDTY
     real :: SPECIFIC_HUMDTY
     real :: DIRECTION
     real :: SPEED
     real :: WIND_VEL_U_COMP
     real :: WIND_VEL_V_COMP
     real :: LATITUDE
     real :: LONGITUDE
  end type GATE_dropsonde_type

  public :: GATE_dropsonde_type

    contains

      subroutine dropsonde_time_converter(time, hours, minutes, seconds)

      ! The dropsonde time format is given in HHMMSS
      !
      implicit none
      integer, intent(IN)  :: time
      integer, intent(OUT) :: hours, minutes, seconds

      hours   = time / 10000
      minutes = (time - hours * 10000) / 100
      seconds = time - hours * 10000 - minutes * 100

      ! write ( * , * ) "dropsonde time", time, hours, minutes, seconds

    end subroutine dropsonde_time_converter

end module GATEdropsonde_mod

! ----------------------

program GATEdropsonde

  use, intrinsic :: iso_fortran_env, only : iostat_end

  implicit none

  integer :: ierror = 0

  character(len=FILENAME_LENGHT)   :: infile
  character(len=FILENAME_LENGHT)   :: line

  call getarg(1, line)
  infile = TRIM(line)

  write ( * , * ) "Handling ", infile

  open (unit=10, file=infile, status='old', form='formatted', action='read')
  read( 10, '(A80)', iostat=ierror ) line(1:80)

  select case (line(1:1))

  case ( "0" )

     rewind(10)
     read( 10, '(A)', iostat=ierror ) line(1:80)
     read( 10, '(A)', iostat=ierror ) line(1:80)
     write ( * , * )
     write ( * , * ) 'Found tape header record type 0 in'
     write ( * , * ) trim(infile)
     write ( * , * ) 'Type of computer used : ', line(26:37)
     read( 10, '(A)', iostat=ierror ) line(1:80)
     write ( * , * ) 'Translation table:'
     write ( * , * ) line(2:54)
     close (unit=10)

  case ( "1" )
     read( 10, '(A)', iostat=ierror ) line(1:80)
     close (unit=10)
     if ( VERIFY("DROPSONDE",line(16:39)) /= 0 ) then
       write ( * , * )
       write ( * , * ) trim(infile), ' does not contain dropsonde data',line(16:39)
       stop
     end if

     write ( * , * )
     write ( * , * ) 'now processing ', trim(infile)
     call convert_data ( infile )

  case default
     close (unit=10)
     write ( * , * )
     write ( * , * ) trim(infile) , ' does not contain data'
     stop
 
  end select

end program GATEdropsonde

! ----------------------

subroutine convert_data (infile)

  use, intrinsic :: iso_fortran_env, only : iostat_end
  use GATEdropsonde_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile

  ! 5 ints plus no_of_levels_in_line per data line (record)
  integer :: id1, id2, id3, id4
  type (GATE_dropsonde_type) :: tmpdata(no_of_levels_in_line)

  ! array for keeping the whole profile
  type (GATE_dropsonde_type), allocatable :: dropsondedata(:)

  ! launch and field metadata
  type (GATE_metadata_type) :: metadata

  integer :: ierror = 0
  integer :: no_records

  integer :: flightTime
  integer :: hour, minute, second
  integer :: level
  integer :: i

  integer :: dropsondesize, dropsondesize_inc

  character(len=24*80) :: line

  dropsondesize_inc = 128 * no_of_levels_in_line
  dropsondesize     = dropsondesize_inc

  allocate (dropsondedata(dropsondesize))

  open (unit=10, file=trim(infile), status='old', form='formatted', action='read')

  ! file section 1, summary of launch

  do i = 1, 24

     read( 10, 100, iostat=ierror ) line(1:80)

     select case (ierror)

     case ( 0 )

        if ( i == 2 ) then
           metadata%platform = line(16:39)
        end if

        if ( i == 4 ) then
           read(line(2:15),'(i4,5i2)')             &
                metadata%time_start%year,   &
                metadata%time_start%month,  &
                metadata%time_start%day,    &
                metadata%time_start%hour,   &
                metadata%time_start%minute, &
                metadata%time_start%second
           read(line(19:33),'(i3,2i2,i4,2i2)') &
                metadata%lat_start%deg, &
                metadata%lat_start%min, &
                metadata%lat_start%sec, &
                metadata%lon_start%deg, &
                metadata%lon_start%min, &
                metadata%lon_start%sec
           if ( metadata%lat_start%deg > 99  .or. &
                metadata%lat_start%min > 60  .or. &
                metadata%lat_start%sec > 60  .or. & 
                metadata%lon_start%deg > 999 .or. &
                metadata%lon_start%deg > 180 .or. &
                metadata%lon_start%deg < -180 .or. &
                metadata%lat_start%min > 60  .or. &
                metadata%lat_start%sec > 60 ) then
               write ( * , * ) 'Faulty positions in file!'
               stop
           endif

        end if

        if ( i == 5 ) then
           read(line(2:15),'(i4,5i2)')           &
                metadata%time_end%year,   &
                metadata%time_end%month,  &
                metadata%time_end%day,    &
                metadata%time_end%hour,   &
                metadata%time_end%minute, &
                metadata%time_end%second
           read(line(19:33),'(i3,2i2,i4,2i2)')   &
                metadata%lat_end%deg, &
                metadata%lat_end%min, &
                metadata%lat_end%sec, &
                metadata%lon_end%deg, &
                metadata%lon_end%min, &
                metadata%lon_end%sec
        end if

     case ( iostat_end )
        write ( * , * ) 'Unexpectedly reached end of file in section 1!'
        exit
     case default
        write ( * , * ) 'Unexpected error when reading section 1!'
        exit
     end select
  end do

  ! file section 2, metadata of sampled variables

  do i = 1, 48
     read( 10, 100, iostat=ierror ) line(1:80)
     select case (ierror)
     case ( 0 )
        continue
     case ( iostat_end )
        write ( * , * ) 'Unexpectedly reached end of file in section 2!'
        exit
     case default
        write ( * , * ) 'Unexpected error when reading section 2!'
        exit
     end select
  end do

  ! file section 3

  level = 1
  no_records = 0

  do while ( ierror == 0 )

     ! Read data records, 24 lines for 1 record.

     do i = 1, 24
        read( 10, 100, iostat=ierror ) line((i-1)*80+1:i*80)
        ! write ( * , * ) line
        select case (ierror)
        case ( 0 )
           continue
        case ( iostat_end )
           no_records = no_records + 1
           write ( * , * ) 'EOF reached after trying record ', no_records
           exit
        case default
           write ( * , * ) 'Unexpected error when reading section 3!'
           exit
        end select
     end do

     if ( ierror == 0 ) then

        read(line, 110, iostat=ierror ) id1, id2, id3, id4, tmpdata(1:no_of_levels_in_line)

        ! convert dropsonde time signal
        call dropsonde_time_converter(dropsondedata(i)%time, hour, minute, second)
        flightTime = hour*3600 + minute*60 + second

        if ( ierror /= 0 ) write ( * , * ) 'format error in ', trim(line)
        do i = 1, no_of_levels_in_line
           dropsondedata(level) = tmpdata(i)
           call dropsonde_time_converter(tmpdata(i)%time, hour, minute, second)
           flightTime = hour*3600 + minute*60 + second
           dropsondedata(level)%time = flightTime
           level = level + 1
        end do

        no_records = no_records + 1

     end if

  end do ! while-loop

  close (10)

  write ( * , * ) 'Processed ', no_records-1, 'data records for ', level-1, 'levels.'

  call write_netcdf ( infile, level-1, dropsondedata, metadata )

  deallocate ( dropsondedata )

110 format(I1,I4,I10,I5,19(I6,F8.2,F9.1,F8.2,2F8.2,F7.2,F8.2,F7.1,2F8.2,F7.3,F8.3))

100 format(a)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_levels, dropsondedata, metadata )

  use GATEdropsonde_mod
  use GATE_netcdf_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile
  integer,                     intent(in) :: no_of_levels
  type (GATE_dropsonde_type),  intent(in) :: dropsondedata(no_of_levels)
  type (GATE_metadata_type),   intent(in) :: metadata

  character(len=FILENAME_LENGHT) :: outfile
  character(len=18)  :: position_str

  integer, parameter :: ndims = 2
  integer :: ncid
  integer :: dimids(ndims)
  integer :: start(ndims)
  integer :: edge(ndims)

  integer :: launch_time_id
  integer :: level_id, timer_id
  integer :: flight_time_id
  integer :: p_id
  integer :: alt_id
  integer :: ta_id
  integer :: dew_id
  integer :: q_id
  integer :: rh_id
  integer :: u_id
  integer :: v_id
  integer :: wd_id
  integer :: ws_id
  integer :: lat_id, lon_id

  real    :: time(1)
  real    :: position_start_lon, position_start_lat
  real    :: position_end_lon,   position_end_lat

  real    :: p(no_of_levels)
  real    :: altitude(no_of_levels)
  real    :: ta(no_of_levels)
  real    :: dew(no_of_levels)
  real    :: q(no_of_levels)
  real    :: rh(no_of_levels)
  real    :: wdir(no_of_levels)
  real    :: wspd(no_of_levels)
  real    :: u(no_of_levels)
  real    :: v(no_of_levels)
  real    :: lat(no_of_levels)
  real    :: lon(no_of_levels)

  character(len=33)    :: seconds_since

  character(len = 8)   :: clockdate
  character(len = 10)  :: clocktime
  character(len = 5)   :: timezone

  integer              :: values(8)

  character(len = 256) :: history

  integer :: i

  ! some preparation for writing global attributes

  call date_and_time(clockdate, clocktime, timezone, values)

  write (history, "(A,I4,A1,I0.2,A1,I0.2,A1,3(I0.2,A1),A)" ) &
                      "Created by Rene Redler, MPI-M on ",   &
                      values(1), "-", values(2), "-", values(3), " ", &
                      values(5), ":", values(6), ":", values(7), " ", &
                      "from data in archive directory 3.64.02.101-3.69.02.104_19740601-19740930."

  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                        &
       & metadata%time_start%year,   '-',  &
       & metadata%time_start%month,  '-',  &
       & metadata%time_start%day,    ' ',  &
       & metadata%time_start%hour,   ':',  &
       & metadata%time_start%minute, ':',  &
       & metadata%time_start%second

  position_start_lon = metadata%lon_start%deg +      &
       &             ( metadata%lon_start%min * 60 + &
       &               metadata%lon_start%sec ) / 3600.0

  position_start_lat = metadata%lat_start%deg +      &
       &             ( metadata%lat_start%min * 60 + &
       &               metadata%lat_start%sec ) / 3600.0

  position_end_lon = metadata%lon_end%deg +      &
       &           ( metadata%lon_end%min * 60 + &
       &             metadata%lon_end%sec ) / 3600.0

  position_end_lat = metadata%lat_end%deg +      &
       &           ( metadata%lat_end%min * 60 + &
       &             metadata%lat_end%sec ) / 3600.0

  ! start writing

  write ( * , * ) ' writing ', trim(infile)
  write ( outfile, '(A,A3)' ) trim(infile), '.nc'

  call handle_err(nf_create( outfile, NF_CLOBBER, ncid))

  call handle_err(nf_def_dim(ncid, 'level', no_of_levels, level_id))
  call handle_err(nf_def_dim(ncid, 'time', NF_UNLIMITED, timer_id))

  dimids(1) = level_id
  dimids(2) = timer_id

  start(:) = 1
  edge(1)  = no_of_levels
  edge(2)  = 1

  call handle_err(nf_def_var(ncid, "time", NF_FLOAT, 1, dimids(2), launch_time_id))
  call handle_err(nf_def_var(ncid, "flight_time", NF_FLOAT, ndims, dimids, flight_time_id))

  call handle_err(nf_put_att_text(ncid, launch_time_id, 'units', len(seconds_since), seconds_since))
  call handle_err(nf_put_att_text(ncid, launch_time_id, "calendar", 19, "proleptic_gregorian"))

  call handle_err(nf_put_att_text(ncid, flight_time_id, 'units', len(seconds_since), seconds_since))
  call handle_err(nf_put_att_text(ncid, flight_time_id, "calendar", 19, "proleptic_gregorian"))

  lat_id = define_variable_and_attribute_real( &
       ncid, dimids, 'lat', 'latitude', 'latitude', 'degrees_north', 999999.0)

  lon_id = define_variable_and_attribute_real( &
       ncid, dimids, 'lon', 'longitude', 'longitude', 'degrees_east', 999999.0)

  p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'p', 'air_pressure', &
       'air pressure', metadata%pressure_unit, 999999.0)

  alt_id = define_variable_and_attribute_real(ncid, dimids, 'altitude', 'geopotential_height', &
       'geopotential height', metadata%altitude_unit, 999999.0)
  call handle_err(nf_put_att_text(ncid, alt_id, "positive", 2, "up"))

  ta_id = define_variable_and_attribute_real( &
       ncid, dimids, 'ta', 'temperature', 'temperature', metadata%temperature_unit, 9999.0)

  dew_id = define_variable_and_attribute_real( &
       ncid, dimids, 'dew', 'dew_point_temperature', &
       'dew point temperature', metadata%temperature_unit, 9999.0)

  q_id = define_variable_and_attribute_real( &
       ncid, dimids, 'q', 'specific_humidity', 'specific humidity', metadata%specific_humidity_unit, 99.0)

  rh_id = define_variable_and_attribute_real( &
       ncid, dimids, 'rh', 'relative_humidity', 'relative humidity', metadata%relative_humidity_unit, 99.0)

  u_id = define_variable_and_attribute_real( &
       ncid, dimids, 'u', 'eastward_wind', 'eastward wind', metadata%wind_unit, 999.0 )

  v_id = define_variable_and_attribute_real( &
       ncid, dimids, 'v', 'northward_wind', 'northward wind', metadata%wind_unit, 999.0)

  ws_id =  define_variable_and_attribute_real( &
       ncid, dimids, 'ws', 'wind_speed', 'wind speed', metadata%wind_unit, 999.0)

  wd_id =  define_variable_and_attribute_real( &
       ncid, dimids, 'wd', 'wind_from_direction', 'wind from direction', metadata%wind_dir_unit, 999.0)

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "platform", len(trim(adjustl(metadata%platform))), &
       trim(adjustl(metadata%platform))))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_start_lon, ' ', position_start_lat 
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "launch_start_position", 18, position_str))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_end_lon, ' ', position_end_lat 
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "launch_end_position", 18, position_str))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  call handle_err(nf_enddef (ncid))

  time(1) = 0.0

  call handle_err(nf_put_vara(ncid, launch_time_id, start(2), edge(2), time))

  do i = 1, no_of_levels           
     if ( dropsondedata(i)%latitude > 90.0 ) then
        lat(i) = 999999.0
     else   
        lat(i) = dropsondedata(i)%latitude
     endif
     if ( dropsondedata(i)%longitude < -180.0 .or. dropsondedata(i)%longitude > 180.0 ) then
        lon(i) = 999999.0
     else   
        lon(i) = dropsondedata(i)%longitude
     endif
     if ( dropsondedata(i)%pressure > 9999.0 ) then
        p(i) = 999999.0
     else   
        p(i) = dropsondedata(i)%pressure * 100.0
     endif
     if ( dropsondedata(i)%altitude > 10000000.0 ) then
        altitude(i) = 999999.0
     else   
        altitude(i) = dropsondedata(i)%altitude
     endif
     if ( dropsondedata(i)%temperature > 99.0 ) then
        ta(i) = 9999.0
     else   
        ta(i) = dropsondedata(i)%temperature + 273.15
     endif
     if ( dropsondedata(i)%temperature > 99.0 ) then
        dew(i) = 9999.0
     else   
        dew(i) = dropsondedata(i)%temperature + 273.15
     endif
     if ( dropsondedata(i)%specific_humdty > 1000.0 ) then
        q(i) = 99.0
     else   
        q(i) = dropsondedata(i)%specific_humdty / 1000.0
     endif
     if ( dropsondedata(i)%relative_humdty > 101.0 .OR. dropsondedata(i)%relative_humdty < 0.0 ) then
        rh(i) = 99.0
     else   
        rh(i) = dropsondedata(i)%relative_humdty
     endif
     if ( dropsondedata(i)%direction < 0.0 .OR. dropsondedata(i)%direction > 360.0 ) then
        wdir(i) = 999.0
     else   
        wdir(i) = dropsondedata(i)%direction
     endif
     if ( dropsondedata(i)%speed < 0.0 .OR. dropsondedata(i)%speed > 1000.0 ) then
        wspd(i) = 999.9
     else   
        wspd(i) = dropsondedata(i)%speed
     endif
     if ( dropsondedata(i)%wind_vel_u_comp > 100000.0 ) then
        u(i) = 999.0
     else
        u(i) = dropsondedata(i)%wind_vel_u_comp
     endif
     if ( dropsondedata(i)%wind_vel_v_comp > 100000.0 ) then
        v(i) = 999.0
     else
        v(i) = dropsondedata(i)%wind_vel_v_comp
     endif
  enddo
  
  call handle_err(nf_put_vara(ncid, flight_time_id, start, edge, float(dropsondedata(1:no_of_levels)%time)))
  call handle_err(nf_put_vara(ncid, lat_id,         start, edge, lat))
  call handle_err(nf_put_vara(ncid, lon_id,         start, edge, lon))
  call handle_err(nf_put_vara(ncid, p_id,           start, edge, p))
  call handle_err(nf_put_vara(ncid, alt_id,         start, edge, altitude))
  call handle_err(nf_put_vara(ncid, ta_id,          start, edge, ta))
  call handle_err(nf_put_vara(ncid, dew_id,         start, edge, dew))
  call handle_err(nf_put_vara(ncid, q_id,           start, edge, q))
  call handle_err(nf_put_vara(ncid, rh_id,          start, edge, rh))
  call handle_err(nf_put_vara(ncid, u_id,           start, edge, u))
  call handle_err(nf_put_vara(ncid, v_id,           start, edge, v))
  call handle_err(nf_put_vara(ncid, wd_id,          start, edge, wdir))
  call handle_err(nf_put_vara(ncid, ws_id,          start, edge, wspd))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

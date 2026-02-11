#define FILENAME_LENGHT 132
module GATEaircraft_mod

  use GATE_metadata_mod

  integer, parameter :: no_of_records_in_line = 12 ! see format string 110

  type :: GATE_aircraft_type
     ! do not change the sequence within this type
     real    :: TIME
     real    :: LATITUDE
     real    :: LONGITUDE
     real    :: PRESSURE
     real    :: STD_PRESSURE
     real    :: TEMP
     real    :: STD_TEMP
     real    :: DEW_POINT_TEMP
     real    :: STD_DEW_POINT_TEMP
     real    :: U_WIND
     real    :: STD_U_WIND
     real    :: V_WIND
     real    :: STD_V_WIND
     real    :: W_WIND
     real    :: STD_W_WIND
     real    :: HEADING
     real    :: STD_HEADING
     real    :: AIRSPEED
     real    :: PITCH
     real    :: ROLL
     real    :: SHORT_RAD_DOWN
     real    :: SHORT_RAD_UP
     real    :: LONG_RAD_DOWN
     real    :: LONG_RAD_UP
     integer :: QUALITY(17)
  end type GATE_aircraft_type

  public :: GATE_aircraft_type

end module GATEaircraft_mod

! ----------------------

program GATEaircraft

  ! ASCII Input
  !
  ! from 3.36.21.102-3.60.02.105_19740601-19740930

  use, intrinsic :: iso_fortran_env, only : iostat_end

  implicit none

  integer :: i
  integer :: ierror = 0

  character(len=FILENAME_LENGHT)   :: infile
  character(len=FILENAME_LENGHT)   :: line

  call getarg( 1, line )
  infile = trim(line)

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

     rewind(10)
     do i = 1, 8
        read( 10, '(A)', iostat=ierror ) line(1:80)
     enddo
     close (unit=10)
 
     if ( line(2:68) /= "(I1,I4,I10,I5,12(F6.0,F7.3,F8.3,F6.1,F5.2,F5.1,F5.2,F5.1,F5.2,F5.1," ) then
        write ( * , * )
        write ( * , * ) trim(infile), ':'
        write ( * , * ) ' - wrong format description'
        stop
     end if

     write ( * , * )
     write ( * , * ) 'now processing ', trim(infile)
     call convert_data ( infile )

  case default
     close (unit=10)
     write ( * , * )
     write ( * , * ) trim(infile), ' does not contain data'
     stop

  end select

end program GATEaircraft

! ----------------------

subroutine convert_data (infile)

  use, intrinsic :: iso_fortran_env, only : iostat_end
  use GATEaircraft_mod
  use GATEaircraft_time_mod
  use GATE_metadata_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile

  ! 4 ints plus <no_of_records_in_line> measurements per full data line (see format string)

  integer :: type_id          ! file header record type indicator
  integer :: records_in_line  ! number of of records contained in one full data line
  integer :: records_handled  ! number of records allready stored away
  integer :: line_number      ! of one line containing full format string

  type (GATE_aircraft_type) :: flightdata(no_of_records_in_line)

  ! array for keeping the whole profile
  type (GATE_aircraft_type), pointer :: aircraftdata(:)
  type (GATE_aircraft_type), allocatable, target :: aircraftcurrent(:)
  type (GATE_aircraft_type), allocatable, target :: aircrafttempory(:)
  ! measurement and field metadata
  type (GATE_metadata_type) :: metadata

  integer :: ierror = 0
  integer :: iostat = 0
  integer :: no_of_measurement

  integer :: i
  integer :: aircraftsize, aircraftsize_inc

  character(len=24*80) :: line

  integer :: flightTime(no_of_records_in_line), flightTimePrev
  integer :: startTime
  integer :: hour, minute, second
  aircraftsize_inc = 100000 * no_of_records_in_line
  aircraftsize     = aircraftsize_inc

  allocate (aircraftcurrent(aircraftsize))
  aircraftdata => aircraftcurrent

  open (unit=10, file=trim(infile), status='old', form='formatted', action='read')

  ! file section 1, summary of measurement

  do i = 1, 24

     read( 10, 100, iostat=ierror ) line(1:80)

     select case (ierror)

     case ( 0 )

        if ( i == 2 ) then
           metadata%aircraftname = line(16:39)
        end if

        if ( i == 4 ) then
           read(line(2:15),'(i4,5i2)')                  &
                metadata%time_start%year,   &
                metadata%time_start%month,  &
                metadata%time_start%day,    &
                metadata%time_start%hour,   &
                metadata%time_start%minute, &
                metadata%time_start%second
           startTime = metadata%time_start%hour   * 3600 +  &
                       metadata%time_start%minute *   60 +  &
                       metadata%time_start%second
           write ( * , * ) "start date/time is ", &
                metadata%time_start%year,   &
                metadata%time_start%month,  &
                metadata%time_start%day,    &
                metadata%time_start%hour,   &
                metadata%time_start%minute, &
                metadata%time_start%second

        end if

        if ( i == 7 ) then
            read(line(22:26),'(i4,A1)') metadata%interval, metadata%interval_unit
            write ( * , * ) "data sampling interval is ", metadata%interval, metadata%interval_unit
        endif

     case ( iostat_end )
        write ( * , * ) 'Unexpectedly reached end of file in section 1!'
        exit
     case default
        write ( * , * ) 'Unexpected error when reading section 1!'
        exit
     end select
  end do

  ! file section 2, metadata of sampled variables

  do i = 1, 72
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

  no_of_measurement = 0
  flightTimePrev = 0

  do while ( ierror >= 0 )

     ! Read data records, 24 lines for 1 record.

     do i = 1, 24
        read( 10, 100, iostat=ierror ) line((i-1)*80+1:i*80)
        select case (ierror)
        case ( 0 )
           continue
        case ( iostat_end )
           write ( * , * ) 'EOF reached after trying record ', no_of_measurement
           exit
        case default
           write ( * , * ) 'Unexpected error when reading section 3!'
           exit
        end select
     end do

     if ( ierror == 0 ) then

        read(line, 110, iostat=iostat ) type_id, records_in_line, records_handled, line_number, &
                                        flightdata

        if ( ierror /= 0 ) then
           write ( * , * ) 'format error in ', trim(line)
           continue
        endif

        ! Note: this seems to be insane and is not working always.
        ! Thus the aircraftsize_inc is set to a large number 100000
        ! to avoid this.
        if ( no_of_measurement >= aircraftsize ) then
           write ( * , * ) "WARNING: memory is increased!"
           allocate (aircrafttempory(aircraftsize))
           aircrafttempory = aircraftdata
           deallocate(aircraftcurrent)
           aircraftsize=aircraftsize+aircraftsize_inc
           allocate(aircraftcurrent(aircraftsize))
           aircraftcurrent(1:aircraftsize-aircraftsize_inc) = aircrafttempory
           deallocate(aircrafttempory)
           aircraftdata => aircraftcurrent
        end if

        do i = 1, no_of_records_in_line
           ! handle only usable records
           if ( abs(flightdata(i)%latitude)  <  90.0 .AND. &
                abs(flightdata(i)%longitude) < 180.0 .AND. &
                flightdata(i)%time > 0.0 .AND. flightdata(i)%time < 240000.0 ) then

              ! convert aircraft time signal
              call aircraft_time_converter(flightdata(i)%time, hour, minute, second)
              flightTime(i) = hour*3600 + minute*60 + second
              flightdata(i)%time = flightTime(i)
              if ( flightTime(i) < flightTimePrev ) then
                  write ( * , * ) "WARNING for time ", &
                                   aircraftdata(i)%time, hour, minute, second, &
                                   flightTime(i), flightTimePrev
              end if

              ! furthermore skip non-monotonic times
              if ( flightTime(i) > flightTimePrev .and. flightTime(i) > startTime ) then 
                   flightTimePrev = flightTime(i)
                   no_of_measurement = no_of_measurement + 1
                   aircraftdata(no_of_measurement) = flightdata(i)
              end if

           end if
        end do

     end if

  end do ! while-loop

  close (10)

  write ( * , * ) 'Processed ', no_of_measurement, 'data records.'

  if ( no_of_measurement > 0 ) then
     call write_netcdf ( infile, no_of_measurement, aircraftdata, metadata )
  else
      write ( * , * ) 'No NetCDF file to write due to missing data!'
  endif

  deallocate ( aircraftcurrent )

110 format(I1,I4,I10,I5,12(F6.0,F7.3,F8.3,F6.1,F5.2,F5.1,F5.2,F5.1,F5.2,F5.1,                &
                           F5.2,F5.1,F5.2,F5.1,F5.2,F5.1,F5.1,F5.1,F5.1,F5.1,F6.1,F6.1,F6.1, &
                           F6.1,17I1))

100 format(a80)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_measurements, aircraftdata, metadata )

  use GATEaircraft_mod
  use GATE_netcdf_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile
  integer,                        intent(in) :: no_of_measurements
  type (GATE_aircraft_type),      intent(in) :: aircraftdata(no_of_measurements)
  type (GATE_metadata_type),      intent(in) :: metadata

  character(len=FILENAME_LENGHT) :: outfile

  integer, parameter :: ndims = 2
  integer :: ncid
  integer :: dimids(ndims)
  integer :: start(ndims)
  integer :: edge(ndims)

  integer :: measurement_time_id
  integer :: measurement_id, timer_id
  integer :: lat_id, lon_id
  integer :: p_id,   std_p_id
  integer :: ta_id,  std_ta_id
  integer :: dew_id, std_dew_id

  real    :: p      (no_of_measurements)
  real    :: std_p  (no_of_measurements)
  real    :: ta     (no_of_measurements)
  real    :: dew    (no_of_measurements)

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
                      "from data in archive directory 3.36.21.102-3.60.02.105_19740601-19740930."

  ! set seconds_since to beginning of the day
 
  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                 &
       & metadata%time_start%year,   '-',  &
       & metadata%time_start%month,  '-',  &
       & metadata%time_start%day,    ' ',  &
       & 0, ':',                           & 
       & 0, ':',                           &
       & 0

  ! start writing

  write ( * , * ) trim(infile), ':'
  write ( * , * ) seconds_since
  write ( outfile, '(A,A3)' ) trim(infile), '.nc'
  write ( * , * ) " - ", trim(adjustl(metadata%aircraftname))

  call handle_err(nf_create( outfile, NF_CLOBBER, ncid))

  call handle_err(nf_def_dim(ncid, 'measurement', 1, measurement_id))
  call handle_err(nf_def_dim(ncid, 'time', NF_UNLIMITED, timer_id))

  dimids(1) = measurement_id
  dimids(2) = timer_id

  start(:) = 1
  edge(2)  = no_of_measurements
  edge(1)  = 1

  call handle_err(nf_def_var(ncid, "time", NF_FLOAT, 1, dimids(2), measurement_time_id))
  call handle_err(nf_put_att_text(ncid, measurement_time_id, 'units', len(seconds_since), seconds_since))
  call handle_err(nf_put_att_text(ncid, measurement_time_id, "calendar", 19, "proleptic_gregorian"))

  lat_id = define_variable_and_attribute_real( &
       ncid, dimids, 'lat', 'latitude', 'latitude', 'degrees_north', 9999.999)

  lon_id = define_variable_and_attribute_real( &
       ncid, dimids, 'lon', 'longitude', 'longitude', 'degrees_east', 100000.0)

  p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'p', 'air_pressure', &
       'air pressure', metadata%pressure_unit, 999999.9)

  std_p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'std_p', 'standard_deviation_of_air_pressure', &
       'standard deviation of air pressure', metadata%pressure_unit, 9999.99)

  ta_id = define_variable_and_attribute_real( &
       ncid, dimids, 'ta', 'air_temperature', &
       'air temperature', metadata%temperature_unit, 999.9)

  std_ta_id = define_variable_and_attribute_real( &
       ncid, dimids, 'std_ta', 'standard_deviation_of_air_temperature', &
       'standard deviation of air temperature', metadata%temperature_unit, 99.99)

  dew_id = define_variable_and_attribute_real( &
       ncid, dimids, 'dew', 'dew_point_temperature', &
       'dew point temperature', metadata%temperature_unit, 999.9)

  std_dew_id = define_variable_and_attribute_real( &
       ncid, dimids, 'std_dew', 'standard_deviation_of_dew_point_temperature', &
       'standard deviation of dew point temperature', metadata%temperature_unit, 99.99)

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "aircraft", len(trim(adjustl(metadata%aircraftname))), &
       trim(adjustl(metadata%aircraftname))))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  call handle_err(nf_enddef (ncid))

  do i = 1, no_of_measurements

     if ( aircraftdata(i)%pressure > 9999.0 ) then
        p(i) = 999999.9
     else
        p(i) = aircraftdata(i)%pressure * 100.0
     endif

     if ( aircraftdata(i)%std_pressure > 99.90 ) then
        std_p(i) = 9999.99
     else
        std_p(i) = aircraftdata(i)%std_pressure * 100.0
     endif

     if ( aircraftdata(i)%temp > 999.0 ) then
        ta(i) = 999.9
     else
        ta(i) = aircraftdata(i)%temp + 273.15
     endif

     if ( aircraftdata(i)%dew_point_temp > 999.0 ) then
        dew(i) = 999.9
     else
        dew(i) = aircraftdata(i)%dew_point_temp + 273.15
     endif

  enddo

  call handle_err(nf_put_vara(ncid, measurement_time_id, start(2), edge(2), aircraftdata(1:no_of_measurements)%time))

  call handle_err(nf_put_vara(ncid, lat_id,     start, edge, aircraftdata(1:no_of_measurements)%latitude))
  call handle_err(nf_put_vara(ncid, lon_id,     start, edge, aircraftdata(1:no_of_measurements)%longitude))
  call handle_err(nf_put_vara(ncid, p_id,       start, edge, p))
  call handle_err(nf_put_vara(ncid, std_p_id,   start, edge, std_p))
  call handle_err(nf_put_vara(ncid, ta_id,      start, edge, ta))
  call handle_err(nf_put_vara(ncid, std_ta_id,  start, edge, aircraftdata(1:no_of_measurements)%std_temp))
  call handle_err(nf_put_vara(ncid, dew_id,     start, edge, dew))
  call handle_err(nf_put_vara(ncid, std_dew_id, start, edge, aircraftdata(1:no_of_measurements)%std_dew_point_temp))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

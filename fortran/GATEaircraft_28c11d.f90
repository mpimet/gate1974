#define FILENAME_LENGHT 132
module GATEaircraft_mod

  use GATE_metadata_mod

  integer, parameter :: no_of_records_in_line = 4 ! see format string 110

  type :: GATE_aircraft_type
     ! do not change the sequence within this type
     integer :: TIME
     real    :: LATITUDE
     integer :: LAT_FLAG
     real    :: LONGITUDE
     integer :: LON_FLAG
     real    :: PRESSURE
     integer :: P_FLAG
     
     real    :: HEADING
     integer :: HEAD_FLAG
     real    :: EAST_WIND
     integer :: FLAG_EAST_WIND
     real    :: NORTH_WIND
     integer :: FLAG_NORTH_WIND
     
     integer :: W_FLAG
     integer :: V_FLAG
     integer :: U_FLAG
     integer :: RHOV_FLAG
     integer :: T_FLAG
     integer :: W(20)
     integer :: V(20)
     integer :: U(20)
     integer :: RHOV(20)
     integer :: T(20)
  end type GATE_aircraft_type

  public :: GATE_aircraft_type

  contains

    subroutine standard_deviation ( field_size, field, std_field, mean_field )
      integer, intent(in) :: field_size
      integer, intent(in) :: field(field_size)
      real, intent(out)   :: std_field
      real, intent(out)   :: mean_field

      real :: data(field_size)
      real :: variance

      real :: gate_scale11 = 100.0
      real :: gate_scale2  = 50.0

      integer :: i

      do i = 1, field_size
         data(i) = field(i) / gate_scale11 - gate_scale2 
      enddo
      
      mean_field = 0.0
      do i = 1, field_size
        mean_field = mean_field + data(i)
      end do
      mean_field = mean_field / field_size
    
      variance = 0.0
      do i = 1, field_size
        variance = variance + (data(i) - mean_field)*(data(i) - mean_field)
      end do
      variance = variance / field_size
    
      std_field = sqrt(variance)

    end subroutine standard_deviation

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
 
     if ( line(12:62) /= "I1,I4,I10,I5,4(I10,3(F8.0,I1),3(F5.0,I1),5I1,100I4)" ) then
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
  integer :: this_record      ! number of current record

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

  integer :: flightTime, flightTimePrev
  integer :: startTime
  integer :: hour, minute, second

  aircraftsize_inc = 16 * no_of_records_in_line
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

  do i = 1, 24
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

        read(line, 110, iostat=iostat ) type_id, records_in_line, records_handled, this_record, &
                                        flightdata

        if ( ierror /= 0 ) then
           write ( * , * ) 'format error in ', trim(line)
           continue
        endif

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

              ! convert charlie time signal
              call charlie_time_converter(flightdata(i)%time, hour, minute, second)
              flightTime = hour*3600 + minute*60 + second
              flightdata(i)%time = flightTime
              if ( flightTime < flightTimePrev ) then
                  write ( * , * ) "WARNING for time ", &
                                   aircraftdata(i)%time, hour, minute, second, &
                                   flightTime, flightTimePrev
              end if
              no_of_measurement = no_of_measurement + 1
              aircraftdata(no_of_measurement) = flightdata(i)
              ! write ( * , * ) aircraftdata(i)%latitude, aircraftdata(i)%longitude, aircraftdata(i)%pressure
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

110 format(I1,I4,I10,I5,4(I10,3(F8.0,I1),3(F5.0,I1),5I1,100I4))

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
  integer :: p_id
  integer :: ta_id
  integer :: rhov_id
  integer :: std_ta_id
  integer :: std_rhov_id

  real    :: lat (no_of_measurements)
  real    :: lon (no_of_measurements)
  real    :: p   (no_of_measurements)
  real    :: ta  (no_of_measurements)
  real    :: rhov(no_of_measurements)

  real    :: std_ta  (no_of_measurements)
  real    :: std_rhov(no_of_measurements)

  character(len=33)    :: seconds_since

  character(len = 8)   :: clockdate
  character(len = 10)  :: clocktime
  character(len = 5)   :: timezone

  integer              :: values(8)

  character(len = 256) :: history

  integer :: i
  logical :: l_good

  real    :: gate_scale10 = 1000.0
  real    :: gate_scale12 = 10.0
  
  ! some preparation for writing global attributes

  call date_and_time(clockdate, clocktime, timezone, values)

  write (history, "(A,I4,A1,I0.2,A1,I0.2,A1,3(I0.2,A1),A)" ) &
                      "Created by Rene Redler, MPI-M on ",   &
                      values(1), "-", values(2), "-", values(3), " ", &
                      values(5), ":", values(6), ":", values(7), " ", &
                      "from data in archive directory 3.36.21.102-3.60.02.105_19740601-19740930."

  ! set seconds_since to beginning of the day
 
  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                             &
       & metadata%time_start%year,   '-',  &
       & metadata%time_start%month,  '-',  &
       & metadata%time_start%day,    ' ',  &
       & 0,                          ':',  & 
       & 0,                          ':',  &
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
       ncid, dimids, 'lat', 'latitude', 'latitude', 'degrees_north')

  lon_id = define_variable_and_attribute_real( &
       ncid, dimids, 'lon', 'longitude', 'longitude', 'degrees_east')

  p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'p', 'air_pressure', &
       'air pressure', metadata%pressure_unit, 999999.9)

  ta_id = define_variable_and_attribute_real( &
       ncid, dimids, 'ta', 'air_temperature', &
       'air temperature', metadata%temperature_unit, 999999.9)

  std_ta_id = define_variable_and_attribute_real( &
       ncid, dimids, 'std_ta', 'standard_deviation_of_air_temperature', &
       'standard deviation of air temperature', metadata%temperature_unit, 999999.9)

  rhov_id = define_variable_and_attribute_real( &
       ncid, dimids, 'rhov', 'water_vapor_density', &
       'water vapor density', metadata%temperature_unit, 999999.9)

  std_rhov_id = define_variable_and_attribute_real( &
       ncid, dimids, 'std_rhov', 'standard_deviation_of_water_vapor_density', &
       'standard deviation of water vapor density', metadata%temperature_unit, 9999.9)  

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "aircraft", len(trim(adjustl(metadata%aircraftname))), &
       trim(adjustl(metadata%aircraftname))))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  call handle_err(nf_enddef (ncid))

  do i = 1, no_of_measurements

     ! consider only good data

     l_good = .FALSE.

     if ( aircraftdata(i)%lat_flag  < 2 .AND. &
          aircraftdata(i)%lon_flag  < 2 .AND. &
          aircraftdata(i)%p_flag    < 2 .AND. &
          aircraftdata(i)%t_flag    < 2 .AND. &
          aircraftdata(i)%rhov_flag < 2) l_good = .TRUE.
     
     if ( l_good ) then

        lat(i) = aircraftdata(i)%latitude  / gate_scale10
        lon(i) = aircraftdata(i)%longitude / gate_scale10

        p(i) = aircraftdata(i)%pressure    / gate_scale12 * 100.0

        call standard_deviation ( 20, aircraftdata(i)%t,    std_ta(i),   ta(i)   )
        call standard_deviation ( 20, aircraftdata(i)%rhov, std_rhov(i), rhov(i) )

        ta(i)   = ta(i)   + 273.15
        rhov(i) = rhov(i) / 1000.0
        std_ta(i) = std_ta(i)

     else
        p(i)        = 999999.9
        ta(i)       = 999999.9
        rhov(i)     = 999999.9

        std_ta(i)   = 9999.9
        std_rhov(i) = 9999.9
     endif

  enddo

  ! filter out additional obviously wrong data
  do i = 1, no_of_measurements
     if ( p(i) < 40000.0 ) then
        p(i)        = 999999.9
        ta(i)       = 999999.9
        rhov(i)     = 999999.9

        std_ta(i)   = 9999.9
        std_rhov(i) = 9999.9
     endif
  enddo

  call handle_err(nf_put_vara(ncid, measurement_time_id, start(2), edge(2), float(aircraftdata(1:no_of_measurements)%time)))

  call handle_err(nf_put_vara(ncid, lat_id,      start, edge, lat))
  call handle_err(nf_put_vara(ncid, lon_id,      start, edge, lon))
  call handle_err(nf_put_vara(ncid, p_id,        start, edge, p))
  call handle_err(nf_put_vara(ncid, ta_id,       start, edge, ta))
  call handle_err(nf_put_vara(ncid, std_ta_id,   start, edge, std_ta))
  call handle_err(nf_put_vara(ncid, rhov_id,     start, edge, rhov))
  call handle_err(nf_put_vara(ncid, std_rhov_id, start, edge, std_rhov))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

! ----------------------

subroutine handle_err(status)
  implicit none
  include 'netcdf.inc'

  integer, intent(in) :: status

  if (status .ne. nf_noerr) then
     print *, nf_strerror(status)
     stop 'Stopped'
  endif
end subroutine handle_err

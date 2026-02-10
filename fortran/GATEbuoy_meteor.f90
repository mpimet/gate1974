#define FILENAME_LENGHT 132
module GATEbuoy_mod

  use GATE_metadata_mod

  integer, parameter :: no_of_records_in_line = 25 ! see format string 110

  type :: GATE_buoy_type
     ! do not change the sequence within this type
     real :: TIME
     real :: DAY
     real :: WIND_SPEED
     real :: VAL_WIND_SPEED
     real :: WIND_DIRECTION
     real :: VAL_WIND_DIRECTION 
     real :: DRY_BULB_TEMP
     real :: VAL_DRY_BULB_TEMP
     real :: SPEC_HUMIDITY
     real :: VAL_SPEC_HUMIDITY
     real :: WATER_TEMPERATURE
     real :: VAL_WATER_TEMPERATURE
  end type GATE_buoy_type

  public :: GATE_buoy_type

  contains

    subroutine date_converter(date, initial_month, year, month, day)

      ! The date format is given in DD.
      !
      implicit none
      real, intent(IN)     :: date
      integer, intent(IN)  :: initial_month
      integer, intent(OUT) :: year, month, day

      integer, save        :: ctrl_day   = 0
      integer, save        :: ctrl_month = 0

      day   = int(date)
      if ( ctrl_month == 0 ) ctrl_month = initial_month
      if ( day < ctrl_day ) then
         ctrl_month = ctrl_month + 1
      endif

      ctrl_day = day
      month = ctrl_month
      year  = 1974

    end subroutine date_converter

    subroutine time_converter(time, hours, minutes, seconds)

      ! The time format is given in HHMM
      !
      implicit none
      real, intent(IN)     :: time
      integer, intent(OUT) :: hours, minutes, seconds

      hours   = int(time)
      minutes = nint(time*100.0) - hours*100
      seconds = 0
   
    end subroutine time_converter

    function date_to_days(year, month, day) result(days)
        implicit none
        integer, intent(in) :: year, month, day
        integer :: days
        integer, dimension(12) :: month_days = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

        integer :: i, total_days

        total_days = 0

        ! Calculate days since January 1, 1974
        do i = 1974, year - 1
            if (is_leap_year(i)) then
                total_days = total_days + 366
            else
                total_days = total_days + 365
            end if
        end do

        ! Add days for months
        do i = 1, month - 1
            if (i == 2 .and. is_leap_year(year)) then
                total_days = total_days + 29
            else
                total_days = total_days + month_days(i)
            end if
        end do

        ! Add days for the current month
        total_days = total_days + day - 1

        days = total_days

    end function date_to_days

    function is_leap_year(year) result(is_leap)
        implicit none
        integer, intent(in) :: year
        logical :: is_leap

        if (mod(year, 4) == 0) then
            if (mod(year, 100) == 0) then
                if (mod(year, 400) == 0) then
                    is_leap = .true.
                else
                    is_leap = .false.
                end if
            else
                is_leap = .true.
            end if
        else
            is_leap = .false.
        end if

    end function is_leap_year

    function days_between(year1, month1, day1, year2, month2, day2) result(days)
        implicit none
        integer, intent(in) :: year1, month1, day1, year2, month2, day2
        integer :: days

        days = date_to_days(year1, month1, day1) - date_to_days(year2, month2, day2)

    end function days_between

end module GATEbuoy_mod

! ----------------------

program GATEbuoy

  ! ASCII Input
  !
  ! /work/mh0010/GATE/NOAA_data/Sorted_files/
  !     3.36.21.102-3.60.02.105_19740601-19740930/
  !
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
     if ( line(2:59) /= "%I1,I4,I10,I5,25%F10.2,F5.0,F7.1,F4.0,F7.0,F4.0,F9.2,F4.0," .AND. &
          line(2:60) /= "%I1,I4,I10,I5,25%F10.2,F5.0,F7.1,F4.0,F7.0,F4.0,3%F9.2,F4.0" ) then
        write ( * , * )
        write ( * , * ) trim(infile), ':'
        write ( * , * ) ' - wrong format description'
        write ( * , * ) 'Need ', line(2:60)
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

end program GATEbuoy

! ----------------------

subroutine convert_data (infile)

  use, intrinsic :: iso_fortran_env, only : iostat_end
  use GATEbuoy_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile

  ! 4 ints plus no_of_records_in_line measurements per full data line (see format string)

  integer :: type_id          ! file header record type indicator
  integer :: records_in_line  ! number of of records contained in one full data line
  integer :: records_handled  ! number of records allready stored away
  integer :: line_number      ! of one line containing full format string

  type (GATE_buoy_type) :: buoydata(no_of_records_in_line)

  ! array for keeping the whole profile
  type (GATE_buoy_type), pointer :: dbuoydata(:)
  type (GATE_buoy_type), allocatable, target :: dbuoycurrent(:)
  type (GATE_buoy_type), allocatable, target :: dbuoytempory(:)
  ! measurement and field metadata
  type (GATE_metadata_type) :: metadata

  integer :: ierror = 0
  integer :: no_of_measurement

  integer :: buoyTime(no_of_records_in_line), buoyTimePrev
  integer :: startTime
  integer :: days, hour, minute, second
  integer :: year, month, day

  integer :: i
  integer :: dbuoysize, dbuoysize_inc

  character(len=24*80) :: line

  dbuoysize_inc = 1000000
  dbuoysize     = dbuoysize_inc

  allocate (dbuoycurrent(dbuoysize))
  dbuoydata => dbuoycurrent

  open (unit=10, file=trim(infile), status='old', form='formatted', action='read')

  ! file section 1, summary of measurement

  do i = 1, 24

     read( 10, 100, iostat=ierror ) line(1:80)

     select case (ierror)

     case ( 0 )

        if ( i == 2 ) then
           metadata%shipname1 = line(16:39)
        end if

        if ( i == 3 ) then
           metadata%shipname2 = line(16:39)
        end if

        if ( i == 7 ) then
           read(line(17:25),'(I9)')   metadata%interval
           read(line(26:26),'(A1)')   metadata%interval_unit
           write ( * , * ) "Sampling interval ", metadata%interval,metadata%interval_unit 
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

           read(line(20:33),'(i2,2i2,i4,2i2)') &
                metadata%lat_start%deg,   &
                metadata%lat_start%min,   &
                metadata%lat_start%sec,   &
                metadata%lon_start%deg,   &
                metadata%lon_start%min,   &
                metadata%lon_start%sec

           write ( * , * ) "Start of measurement ",     &
                metadata%time_start%year,   &
                metadata%time_start%month,  &
                metadata%time_start%day,    &
                metadata%time_start%hour,   &
                metadata%time_start%minute
        end if

        if ( i == 5 ) then
           read(line(2:15),'(i4,5i2)')                &
                metadata%time_end%year,   &
                metadata%time_end%month,  &
                metadata%time_end%day,    &
                metadata%time_end%hour,   &
                metadata%time_end%minute, &
                metadata%time_end%second
   
          read(line(20:33),'(i2,2i2,i4,2i2)') &
                metadata%lat_end%deg, &
                metadata%lat_end%min, &
                metadata%lat_end%sec, &
                metadata%lon_end%deg, &
                metadata%lon_end%min, &
                metadata%lon_end%sec

          write ( * , * ) "Start of measurement ",   &
                metadata%time_end%year,   &
                metadata%time_end%month,  &
                metadata%time_end%day,    &
                metadata%time_end%hour,   &
                metadata%time_end%minute
        end if

     case ( iostat_end )
        write ( * , * ) 'Unexpectedly reached end of file in section 1!'
        exit
     case default
        write ( * , * ) 'Unexpected error when reading section 1!'
        exit
     end select
  end do

  ! file section 2, metadata of sampled variables, 48 lines

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
  buoyTimePrev = 0

  do while ( ierror == 0 )

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

        read(line, 110, iostat=ierror ) type_id, records_in_line, records_handled, line_number, buoydata

        if ( ierror /= 0 ) write ( * , * ) 'format error in ', trim(line)
        if ( no_of_measurement >= dbuoysize ) then
           allocate (dbuoytempory(dbuoysize))
           dbuoytempory = dbuoydata
           deallocate(dbuoycurrent)
           dbuoysize=dbuoysize+dbuoysize_inc
           allocate(dbuoycurrent(dbuoysize))
           dbuoycurrent(1:dbuoysize-dbuoysize_inc) = dbuoytempory
           deallocate(dbuoytempory)
           dbuoydata => dbuoycurrent
        end if

        do i = 1, no_of_records_in_line
          ! handle only usable records
          if ( abs(buoydata(i)%time) < 99.0 ) then

            ! convert time signal
            call date_converter(buoydata(i)%day, metadata%time_start%month, year, month, day)
            call time_converter(buoydata(i)%time, hour, minute, second)

            days = days_between( year, month, day,                       &
                                 metadata%time_start%year,   &
                                 metadata%time_start%month,  &
                                 metadata%time_start%day)

            buoyTime(i) = days * 86400 + hour*3600 + minute*60 + second

            if ( no_of_measurement > 0 .and. buoyTime(i) <= buoyTimePrev ) then
                write ( * , * ) "WARNING for time ", &
                                int(buoydata(i)%day), int(buoydata(i)%time), days, hour, minute, second, &
                                buoyTime(i), buoyTimePrev
            else
              buoydata(i)%time = float(buoyTime(i))
              buoyTimePrev = buoyTime(i)
              no_of_measurement = no_of_measurement + 1
              dbuoydata(no_of_measurement) = buoydata(i)
            endif
          endif
        end do
     end if

  end do ! while-loop

  close (10)

  write ( * , * ) 'Processed ', no_of_measurement, 'data records.'

  call write_netcdf ( infile, no_of_measurement, dbuoydata, metadata )

  deallocate ( dbuoycurrent )

110 format(I1,I4,I10,I5,25(F10.2,F5.0,F7.1,F4.0,F7.0,F4.0,F9.2,F4.0, &
           F9.2,F4.0,F9.2,F4.0))
100 format(a80)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_measurements, dbuoydata, metadata )

  use GATEbuoy_mod
  use GATE_netcdf_mod
  
  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile
  integer,                        intent(in) :: no_of_measurements
  type (GATE_buoy_type),          intent(inout)  :: dbuoydata(no_of_measurements)
  type (GATE_metadata_type),      intent(in) :: metadata

  character(len=FILENAME_LENGHT) :: outfile
  character(len=18)  :: position_str

  integer, parameter :: ndims = 2
  integer :: ncid
  integer :: dimids(ndims)
  integer :: start(ndims)
  integer :: edge(ndims)

  integer :: measurement_time_id
  integer :: measurement_id, timer_id
  integer :: ws_id, wd_id
  integer :: dbt_id, q_id, sst_id

  real    :: position_start_lon, position_start_lat
  real    :: position_end_lon,   position_end_lat

  real    :: dbulbt(no_of_measurements)
  real    :: sst(no_of_measurements)
  real    :: q(no_of_measurements)
  
  character(len=33) :: seconds_since

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

  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                        &
       & metadata%time_start%year,   '-',  &
       & metadata%time_start%month,  '-',  &
       & metadata%time_start%day,    ' ',  &
       & 0,                                      ':',  &
       & 0,                                      ':',  &
       & 0

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

  write ( * , * ) trim(infile), ':'
  write ( outfile, '(A,A3)' ) trim(infile), '.nc'

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

  wd_id =  define_variable_and_attribute_real( &
       ncid, dimids, 'ws', 'wind_speed', 'wind speed', metadata%wind_unit, -1.0)
  
  ws_id =  define_variable_and_attribute_real( &
       ncid, dimids, 'wd', 'wind_from_direction', 'wind from direction', metadata%wind_dir_unit, -1.0)
  
  dbt_id = define_variable_and_attribute_real( &
       ncid, dimids, 'dbt', 'dry_bulb_temperature', 'dry bulb temperature', metadata%temperature_unit, 999.9)

  q_id = define_variable_and_attribute_real( &
       ncid, dimids, 'q', 'specific_humidity', 'specific humidity', metadata%specific_humidity_unit, 999.0)

  sst_id = define_variable_and_attribute_real( &
       ncid, dimids, 'sst', 'sea_surface_temperature', 'sea surface temperature', metadata%temperature_unit, 999.9)

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "shipname", len(trim(adjustl(metadata%shipname2))), &
       trim(adjustl(metadata%shipname2))))
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "platform", len(trim(adjustl(metadata%shipname1))), &
       trim(adjustl(metadata%shipname1))))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_start_lon, ' ', position_start_lat
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "buoy_start_position", 18, position_str))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_end_lon, ' ', position_end_lat
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "buoy_end_position", 18, position_str))
  
  call handle_err(nf_enddef (ncid))

  write ( * , * ) " - ", trim(adjustl(metadata%shipname2))
  write ( * , * ) " - ", trim(adjustl(metadata%shipname1)), ', sample interval ', metadata%interval
     
  do i = 1, no_of_measurements

     if (int(dbuoydata(i)%val_wind_speed) /= 1 ) then
       dbuoydata(i)%wind_speed = -1.0
     endif

     if (int(dbuoydata(i)%val_wind_direction) /= 1 ) then
       dbuoydata(i)%wind_direction = -1.0      
     endif
     
     if ( dbuoydata(i)%water_temperature < 0.0 .or. int(dbuoydata(i)%val_water_temperature) /= 1 ) then
        sst(i) = 999.9
     else
        sst(i) = dbuoydata(i)%water_temperature+273.15
     endif

     if ( dbuoydata(i)%spec_humidity < 0.0 .or. int(dbuoydata(i)%val_spec_humidity) /= 1 ) then
        q(i) = 999.9
     else
        q(i) = dbuoydata(i)%spec_humidity / 1000.0
     endif

     if ( dbuoydata(i)%dry_bulb_temp < 0.0 .or. int(dbuoydata(i)%val_dry_bulb_temp) /= 1 ) then
        dbulbt(i) = 999.9
     else
        dbulbt(i) = dbuoydata(i)%dry_bulb_temp+273.15
     endif

  enddo
  
  call handle_err(nf_put_vara(ncid, measurement_time_id, start(2), edge(2), dbuoydata(1:no_of_measurements)%time))

  ! Note that we have to convert western longitudes given as postive values in the ASCII file to negative numbers.

  call handle_err(nf_put_vara(ncid, ws_id,  start, edge, dbuoydata(1:no_of_measurements)%wind_speed))
  call handle_err(nf_put_vara(ncid, wd_id,  start, edge, dbuoydata(1:no_of_measurements)%wind_direction))
  call handle_err(nf_put_vara(ncid, q_id,   start, edge, q(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, dbt_id, start, edge, dbulbt(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, sst_id, start, edge, sst(1:no_of_measurements)))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

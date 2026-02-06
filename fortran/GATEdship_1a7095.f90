#define FILENAME_LENGHT 132
module GATEdship_mod

  use GATE_metadata_mod, only : datetime, position

  integer, parameter :: no_of_records_in_line = 23 ! see format string 110

  type :: GATE_dship_type
     ! do not change the sequence within this type
     real :: DATE
     real :: TIME
     real :: LATITUDE
     real :: LONGITUDE
     real :: PRESSURE
     real :: TEMPERATURE 
     real :: WET_BULB_TEMP
     real :: WATER_TEMP
     real :: WIND_DIRECTION
     real :: WINDSPEED
     real :: TOTAL_CLOUD
     real :: LOW_CLOUD
     real :: MIDDLE_CLOUD
     real :: HIGH_CLOUD
     real :: RAIN_DURATION
     real :: RAIN_AMOUNT
  end type GATE_dship_type

  type :: GATE_metadata_type

     character(len=32) :: shipname
     type (datetime)   :: measurement_time_start
     integer           :: interval
     character         :: interval_unit

     character(len=2)  :: pressure_unit='Pa'
     character(len=6)  :: temperature_unit='kelvin'
     character(len=2)  :: rain_unit='mm'
     character(len=3)  :: wind_unit='m/s' 

  end type GATE_metadata_type

  public :: GATE_dship_type, GATE_metadate_type

  contains

    subroutine date_converter(date, year, month, day)

      ! The time format is given in HHMMSST, date is given in YYMMDD
      !
      implicit none
      real, intent(IN)     :: date
      integer, intent(OUT) :: year, month, day

      day   = int(date)/10000
      month = ( int(date) - day * 10000) / 100
      year  = ( int(date) - day * 10000 - month * 100 )

    end subroutine date_converter

    subroutine time_converter(time, hours, minutes, seconds)

      ! The time format is given in HHMMSST, date is given in YYMMDD
      !
      implicit none
      real, intent(IN)     :: time
      integer, intent(OUT) :: hours, minutes, seconds

      hours   = int(time)
      minutes = int((time - hours) * 60)
      seconds = int(((time - hours) * 3600) - (minutes * 60))
   
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

        days = abs(date_to_days(year1, month1, day1) - date_to_days(year2, month2, day2))

    end function days_between

end module GATEdship_mod

! ----------------------

program GATEdship

  ! ASCII Input
  !
  ! /work/mh0010/GATE/NOAA_data/Sorted_files/
  !     3.00.02.104-3.31.02.101_19740601-19740930/
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
 
     if ( line(2:60) /= "%I1,I4,I10,I5,23%F8.0,3F5.1,F7.1,3F5.1,F5.0,F5.1,4F2.0,F4.0" ) then
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

end program GATEdship

! ----------------------

subroutine convert_data (infile)

  use, intrinsic :: iso_fortran_env, only : iostat_end
  use GATEdship_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile

  ! 4 ints plus no_of_records_in_line * 46 measurements per full data line (see format string)

  integer :: type_id          ! file header record type indicator
  integer :: records_in_line  ! number of of records contained in one full data line
  integer :: records_handled  ! number of records allready stored away
  integer :: line_number      ! of one line containing full format string

  type (GATE_dship_type) :: shipdata(no_of_records_in_line)

  ! array for keeping the whole profile
  type (GATE_dship_type), pointer :: dshipdata(:)
  type (GATE_dship_type), allocatable, target :: dshipcurrent(:)
  type (GATE_dship_type), allocatable, target :: dshiptempory(:)
  ! measurement and field metadata
  type (GATE_metadata_type) :: metadata

  integer :: ierror = 0
  integer :: no_of_measurement

  integer :: shipTime(no_of_records_in_line), shipTimePrev
  integer :: startTime
  integer :: days, hour, minute, second
  integer :: year, month, day

  integer :: i
  integer :: dshipsize, dshipsize_inc

  character(len=24*80) :: line

  dshipsize_inc = 1000000
  dshipsize     = dshipsize_inc

  allocate (dshipcurrent(dshipsize))
  dshipdata => dshipcurrent

  open (unit=10, file=trim(infile), status='old', form='formatted', action='read')

  ! file section 1, summary of measurement

  do i = 1, 24

     read( 10, 100, iostat=ierror ) line(1:80)

     select case (ierror)

     case ( 0 )

        if ( i == 2 ) then
           metadata%shipname = line(16:39)
        end if

        if ( i == 7 ) then
           read(line(17:25),'(I9)')   metadata%interval
           read(line(26:26),'(A1)')   metadata%interval_unit
           write ( * , * ) "Sampling interval ", metadata%interval,metadata%interval_unit 
        end if

        if ( i == 4 ) then
           read(line(2:15),'(i4,5i2)')                  &
                metadata%measurement_time_start%year,   &
                metadata%measurement_time_start%month,  &
                metadata%measurement_time_start%day,    &
                metadata%measurement_time_start%hour,   &
                metadata%measurement_time_start%minute, &
                metadata%measurement_time_start%second
           startTime = metadata%measurement_time_start%hour   * 3600 +  &
                       metadata%measurement_time_start%minute *   60 +  &
                       metadata%measurement_time_start%second

           write ( * , * ) "Start of measurement ", metadata%measurement_time_start%year,   &
                metadata%measurement_time_start%month,  &
                metadata%measurement_time_start%day,    &
                metadata%measurement_time_start%hour,   &
                metadata%measurement_time_start%minute
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

  no_of_measurement = 0
  shipTimePrev = 0

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

        read(line, 110, iostat=ierror ) type_id, records_in_line, records_handled, line_number, shipdata

        if ( ierror /= 0 ) write ( * , * ) 'format error in ', trim(line)
        if ( no_of_measurement >= dshipsize ) then
           allocate (dshiptempory(dshipsize))
           dshiptempory = dshipdata
           deallocate(dshipcurrent)
           dshipsize=dshipsize+dshipsize_inc
           allocate(dshipcurrent(dshipsize))
           dshipcurrent(1:dshipsize-dshipsize_inc) = dshiptempory
           deallocate(dshiptempory)
           dshipdata => dshipcurrent
        end if

        do i = 1, no_of_records_in_line
          ! handle only usable records
          if ( abs(shipdata(i)%latitude)  <  90.0 .AND. &
               abs(shipdata(i)%longitude) < 180.0 ) then

            ! convert time signal
            call date_converter(shipdata(i)%date, year, month, day)
            call time_converter(shipdata(i)%time, hour, minute, second)

            days = days_between( metadata%measurement_time_start%year,   &
                                 metadata%measurement_time_start%month,  &
                                 metadata%measurement_time_start%day, year, month, day)

            shipTime(i) = days * 86400 + hour*3600 + minute*60 + second

            if ( no_of_measurement > 0 .and. shipTime(i) <= shipTimePrev ) then
                write ( * , * ) "WARNING for time ", &
                                int(shipdata(i)%date), int(shipdata(i)%time), days, hour, minute, second, &
                                shipTime(i), shipTimePrev
            else
              shipdata(i)%time = float(shipTime(i))
              shipTimePrev = shipTime(i)
              no_of_measurement = no_of_measurement + 1
              dshipdata(no_of_measurement) = shipdata(i)
            endif
          endif
        end do
     end if

  end do ! while-loop

  close (10)

  write ( * , * ) 'Processed ', no_of_measurement, 'data records.'

  call write_netcdf ( infile, no_of_measurement, dshipdata, metadata )

  deallocate ( dshipcurrent )

110 format(I1,I4,I10,I5,23(F8.0,3F5.1,F7.1,3F5.1,F5.0,F5.1,4F2.0,F4.0,1F5.1,8X))
100 format(a80)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_measurements, dshipdata, metadata )

  use GATEdship_mod
  use GATE_utils_mod

  implicit none

  include 'netcdf.inc'

  character(len=FILENAME_LENGHT), intent(in) :: infile
  integer,                        intent(in) :: no_of_measurements
  type (GATE_dship_type),         intent(in) :: dshipdata(no_of_measurements)
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
  integer :: ta_id, p_id, sst_id

  real    :: ta(no_of_measurements)
  real    :: sst(no_of_measurements)
  real    :: p(no_of_measurements)
  
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
                      "from data in archive directory 3.00.02.104-3.31.02.101_19740601-19740930."

  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                        &
       & metadata%measurement_time_start%year,   '-',  &
       & metadata%measurement_time_start%month,  '-',  &
       & metadata%measurement_time_start%day,    ' ',  &
       & metadata%measurement_time_start%hour,   ':',  &
       & metadata%measurement_time_start%minute, ':',  &
       & metadata%measurement_time_start%second

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

  call handle_err(nf_def_var(ncid, "lat", NF_FLOAT, ndims, dimids, lat_id))
  call handle_err(nf_def_var(ncid, "lon", NF_FLOAT, ndims, dimids, lon_id))
  call handle_err(nf_def_var(ncid, "ta",  NF_FLOAT, ndims, dimids, ta_id))
  call handle_err(nf_def_var(ncid, "p",   NF_FLOAT, ndims, dimids, p_id))
  call handle_err(nf_def_var(ncid, "sst", NF_FLOAT, ndims, dimids, sst_id))

  call handle_err(nf_put_att_text(ncid, measurement_time_id, 'units', len(seconds_since), seconds_since))
  call handle_err(nf_put_att_text(ncid, measurement_time_id, "calendar", 19, "proleptic_gregorian"))

  call handle_err(nf_put_att_text(ncid, lat_id, "standard_name", 8, "latitude"))
  call handle_err(nf_put_att_text(ncid, lat_id, "units", 13, "degrees_north"))
  call handle_err(nf_put_att_text(ncid, lon_id, "standard_name", 9, "longitude"))
  call handle_err(nf_put_att_text(ncid, lon_id, "units", 12, "degrees_east"))
  
  call handle_err(nf_put_att_text(ncid, ta_id, "standard_name", 15, "air_temperature"))
  call handle_err(nf_put_att_text(ncid, ta_id, "long_name", 15, "air temperature"))
  call handle_err(nf_put_att_text(ncid, ta_id, "units", len(metadata%temperature_unit), metadata%temperature_unit))
  call handle_err(nf_put_att_real(ncid, ta_id, "_FillValue", NF_REAL, 1, 999.9))

  call handle_err(nf_put_att_text(ncid, p_id, "standard_name", 12, "air_pressure"))
  call handle_err(nf_put_att_text(ncid, p_id, "long_name", 12, "air pressure"))
  call handle_err(nf_put_att_text(ncid, p_id, "units", len(metadata%pressure_unit), metadata%pressure_unit))
  call handle_err(nf_put_att_real(ncid, p_id, "_FillValue", NF_REAL, 1, 99999.9))

  call handle_err(nf_put_att_text(ncid, sst_id, "standard_name", 23, "sea_surface_temperature"))
  call handle_err(nf_put_att_text(ncid, sst_id, "long_name", 23, "sea surface temperature"))
  call handle_err(nf_put_att_text(ncid, sst_id, "units", len(metadata%temperature_unit), metadata%temperature_unit))
  call handle_err(nf_put_att_real(ncid, sst_id, "_FillValue", NF_REAL, 1, 999.9))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "shipname", len(trim(adjustl(metadata%shipname))), &
       trim(adjustl(metadata%shipname))))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  call handle_err(nf_enddef (ncid))

  write ( * , * ) " - ", trim(adjustl(metadata%shipname)), ', sample interval ', metadata%interval

  do i = 1, no_of_measurements
     if ( dshipdata(i)%water_temp > 0.0 .and. dshipdata(i)%water_temp < 50.0 ) then
        sst(i) = dshipdata(i)%water_temp+273.15
     else
        sst(i) = 999.9
     endif
     if ( dshipdata(i)%pressure < 0.0 ) then
        p(i) = 99999.9
     else
        p(i) = dshipdata(i)%pressure*100.0
     endif
     if ( dshipdata(i)%temperature > 0.0 .and.  dshipdata(i)%temperature < 50.0 ) then
        ta(i) = dshipdata(i)%temperature+273.15
     else
        ta(i) = 999.9
     endif
     ! write ( * , * ) dshipdata(i)%time, sst(i), ta(i), p(i)

  enddo
  
  call handle_err(nf_put_vara(ncid, measurement_time_id, start(2), edge(2), dshipdata(1:no_of_measurements)%time))

  ! Note that we have to convert western longitudes given as postive values in the ASCII file to negative numbers.

  call handle_err(nf_put_vara(ncid, lat_id, start, edge, dshipdata(1:no_of_measurements)%latitude))
  call handle_err(nf_put_vara(ncid, lon_id, start, edge, (-1.0)*dshipdata(1:no_of_measurements)%longitude))
  call handle_err(nf_put_vara(ncid, ta_id,  start, edge, ta(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, p_id,   start, edge, p(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, sst_id, start, edge, sst(1:no_of_measurements)))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

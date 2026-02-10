#define FILENAME_LENGHT 132
module GATEdship_mod

  use GATE_metadata_mod

  integer, parameter :: no_of_records_in_line = 6 ! see format string 110

  type :: GATE_dship_type
     ! do not change the sequence within this type
     integer :: DATE
     integer :: TIME
     real    :: LATITUDE
     real    :: LONGITUDE
     real    :: SHIP_SPEED
     integer :: CNTS_SHIP_SPEED
     real    :: SHIP_HEADING
     integer :: CNTS_SHIP_HEAD
     real    :: INC_SOL_RAD
     integer :: CNTS_INC_SOL
     real    :: REFL_SOL_RAD
     integer :: CNTS_REFL_SOLAR
     real    :: NET_RADIATION
     integer :: CNTS_NET_RAD
     real    :: KOLLS_PRESSURE
     integer :: CNTS_KOLLS_PRES
     real    :: ROSE_PRESSURE
     integer :: CNTS_ROSE_PRES
     real    :: SEA_SURF_TEMP
     integer :: CNTS_SURF_TEMP
     real    :: DRY_BULB_TEMP
     integer :: CNTS_DRY_BULB
     real    :: WET_BULB_TEMP1
     integer :: CNTS_WET_BULB1
     real    :: WET_BULB_TEMP2
     integer :: CNTS_WET_BULB2
     real    :: SPEC_HUMIDITY1
     real    :: SPEC_HUMIDITY2
     real    :: DEW_PT_TEMP1
     real    :: DEW_PT_TEMP2
     real    :: WIND_SPEED_BOOM
     integer :: CNTS_W_S_BOOM
     real    :: WIND_SPEED_MAST
     integer :: CNTS_W_S_MAST
     real    :: WIND_DIR_BOOM
     integer :: CNTS_W_D_BOOM
     real    :: WIND_DIR_MAST
     integer :: CNTS_W_D_MAST
     real    :: WIND_U_COM_BOOM
     real    :: WIND_V_COM_BOOM
     real    :: WIND_U_COM_MAST
     real    :: WIND_V_COM_MAST
  end type GATE_dship_type

  public :: GATE_dship_type

  contains

    subroutine date_converter(date, year, month, day)

      ! The time format is given in HHMMSST, date is given in YYMMDD
      !
      implicit none
      integer, intent(IN)  :: date
      integer, intent(OUT) :: year, month, day

      year  = date/10000
      month = ( date - year * 10000) / 100
      day   = ( date - year * 10000 - month * 100 )

    end subroutine date_converter

    subroutine time_converter(time, hours, minutes, seconds)

      ! The time format is given in HHMMSST, date is given in YYMMDD
      !
      implicit none
      integer, intent(IN)  :: time
      integer, intent(OUT) :: hours, minutes, seconds

      hours = time/100000
      minutes = ( time - hours * 100000) / 1000
      seconds = ( time - hours * 100000 - minutes * 1000 ) / 10

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
  !     02_Surface_meteorological_data_different_averages_of_all_meteorological_variables_(ships)

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
 
     if ( line(2:57) /= "(I1,I4,I10,I5,46X,6(I6,I7,2F7.3,F6.1,I6,F6.0,I6,F6.1,I6," ) then
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

  real    :: interval

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
           metadata%shipname1 = line(16:39)
        end if

        if ( i == 7 ) then
           read(line(17:25),'(F9.1)') interval
           read(line(26:26),'(A1)')   metadata%interval_unit
           metadata%interval = int(interval)
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

           write ( * , * ) "Start of measurement ", metadata%time_start%year,   &
                metadata%time_start%month,  &
                metadata%time_start%day,    &
                metadata%time_start%hour,   &
                metadata%time_start%minute
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

  do i = 1, 96
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

        do i = 1, records_in_line
          ! handle only usable records
          if ( abs(shipdata(i)%latitude)  <  90.0 .AND. &
               abs(shipdata(i)%longitude) < 180.0 ) then

            ! convert time signal
            call date_converter(shipdata(i)%date, year, month, day)
            call time_converter(shipdata(i)%time, hour, minute, second)
            days = days_between( metadata%time_start%year,   &
                                 metadata%time_start%month,  &
                                 metadata%time_start%day, year, month, day)

            shipTime(i) = days * 86400 + hour*3600 + minute*60 + second
            ! write ( * , * ) " Time ", shipdata(i)%date, shipdata(i)%time, shipTime(i)

            if ( no_of_measurement > 0 .and. shipTime(i) <= shipTimePrev ) then
                write ( * , * ) "WARNING for time ", &
                                shipdata(i)%date, shipdata(i)%time, hour, minute, second, &
                                shipTime(i), shipTimePrev
            else
              shipdata(i)%time = shipTime(i)
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

110 format(I1,I4,I10,I5,46X,6(I6,I7,2F7.3,F6.1,I6,F6.0,I6,F6.1,I6, &
           12X,4(F6.1,I6),2(F6.2,I6),12X,2(F6.2,I6),4F6.2,         &
           2(F6.1,I6),2(F6.0,I6),4F6.1,30X))
100 format(a80)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_measurements, dshipdata, metadata )

  use GATEdship_mod
  use GATE_netcdf_mod

  implicit none

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
  integer :: rsds_id
  integer :: rlus_id 
  integer :: nrad_id
  integer :: p_id, q_id
  integer :: sst_id

  real    :: p(no_of_measurements)
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
                  "from data in archive directory 3.00.02.104-3.31.02.101_19740601-19740930."

  write ( seconds_since , '(A14,I4,A1,2(I2.2,A1),2(I2.2,A1),I2.2)' ) &
       & 'seconds since ',                        &
       & metadata%time_start%year,   '-',  &
       & metadata%time_start%month,  '-',  &
       & metadata%time_start%day,    ' ',  &
       & metadata%time_start%hour,   ':',  &
       & metadata%time_start%minute, ':',  &
       & metadata%time_start%second

  ! start writing

  write ( * , * ) trim(infile), ':'
  write ( outfile, '(A,A1,I0.4,A4)' ) trim(infile), '_', metadata%interval, 'S.nc'

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

  rsds_id = define_variable_and_attribute_real( &
       ncid, dimids, 'rsds', 'incoming_solar_radiation', 'incoming solar radiation', metadata%radiation_unit, 9.9)

  rlus_id = define_variable_and_attribute_real( &
       ncid, dimids, 'rlus', 'reflective_solar_radiation', 'reflective solar radiation', metadata%radiation_unit, 9.9)

  nrad_id = define_variable_and_attribute_real( &
       ncid, dimids, 'nrad', 'net_radiation', 'net radiation', metadata%radiation_unit, 99.99)
  
  p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'p', 'air_pressure', 'air pressure', metadata%pressure_unit, 99999.9)

  q_id = define_variable_and_attribute_real( &
       ncid, dimids, 'q', 'specific_humidity', 'specific humidity', metadata%specific_humidity_unit, 99999.9)

  sst_id = define_variable_and_attribute_real( &
       ncid, dimids, 'sst', 'sea_surface_temperature', 'sea surface temperature', metadata%temperature_unit, 999.9)

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "shipname1", len(trim(adjustl(metadata%shipname1))), &
       trim(adjustl(metadata%shipname1))))

  call handle_err(nf_put_att_real(ncid, NF_GLOBAL, "Average_interval", NF_REAL, 1, real(metadata%interval)))

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "history", len(trim(history)), history))

  call handle_err(nf_enddef (ncid))

  write ( * , * ) " - ", trim(adjustl(metadata%shipname1)), ', sample interval ', metadata%interval

  do i = 1, no_of_measurements
     if ( dshipdata(i)%sea_surf_temp < 0.0 ) then
        sst(i) = 999.9
     else
        sst(i) = dshipdata(i)%sea_surf_temp+273.15
     endif
     if ( dshipdata(i)%rose_pressure < 0.0 ) then
        p(i) = 99999.9
     else
        p(i) = dshipdata(i)%rose_pressure*100.0
     endif
     if ( dshipdata(i)%spec_humidity1 < 0.0 ) then
        q(i) = 99999.9
     else
        q(i) = dshipdata(i)%spec_humidity1 / 1000.0
     endif
     ! write ( * , * ) dshipdata(i)%time, sst(i), p(i), dshipdata(i)%spec_humidity1, q(i)
  enddo
  
  call handle_err(nf_put_vara(ncid, measurement_time_id, start(2), edge(2), float(dshipdata(1:no_of_measurements)%time)))

  call handle_err(nf_put_vara(ncid, lat_id,  start, edge, dshipdata(1:no_of_measurements)%latitude))
  call handle_err(nf_put_vara(ncid, lon_id,  start, edge, dshipdata(1:no_of_measurements)%longitude))
  call handle_err(nf_put_vara(ncid, rsds_id, start, edge, dshipdata(1:no_of_measurements)%inc_sol_rad))
  call handle_err(nf_put_vara(ncid, rlus_id, start, edge, dshipdata(1:no_of_measurements)%refl_sol_rad))
  call handle_err(nf_put_vara(ncid, nrad_id, start, edge, dshipdata(1:no_of_measurements)%net_radiation))
  call handle_err(nf_put_vara(ncid, p_id,    start, edge, p(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, sst_id,  start, edge, sst(1:no_of_measurements)))
  call handle_err(nf_put_vara(ncid, q_id,    start, edge, q(1:no_of_measurements)))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

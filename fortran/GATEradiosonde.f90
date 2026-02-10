#define FILENAME_LENGHT 132
module GATEradiosonde_mod

  use GATE_metadata_mod

  type :: GATE_radiosonde_type
     ! do not change the sequence within this type
     real :: TIME_AFT_LAUNCH
     real :: PRESSURE
     real :: ALTITUDE
     real :: TEMPERATURE
     real :: TEMP_ERROR
     real :: SPECIFIC_HUMDTY
     real :: HUMDTY_ERROR
     real :: WIND_VEL_U_COMP
     real :: U_COMP_ERROR
     real :: WIND_VEL_V_COMP
     real :: V_COMP_ERROR
  end type GATE_radiosonde_type

  public :: GATE_radiosonde_type, GATE_metadate_type

end module GATEradiosonde_mod

! ----------------------

program GATEradiosonde

  ! ASCII input
  ! 3.31.02.101-3.33.02.101_19740601-19740930
  ! 3.00.02.104-3.31.02.101_19740601-19740930 for METEOR

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
     if ( VERIFY("RADIOSONDE",line(16:39)) /= 0 ) then
       write ( * , * )
       write ( * , * ) trim(infile), ' does not contain radiosonde data',line(16:39)
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

end program GATEradiosonde

! ----------------------

subroutine convert_data (infile)

  use, intrinsic :: iso_fortran_env, only : iostat_end
  use GATEradiosonde_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile

  ! 5 ints plus 46 levels per data line (record)
  integer :: id1, id2, id3, id4, LAUNCH_TIME
  type (GATE_radiosonde_type) :: tmpdata(46)

  ! array for keeping the whole profile
  type (GATE_radiosonde_type), allocatable :: radiosondedata(:)
  ! launch and field metadata
  type (GATE_metadata_type) :: metadata

  integer :: ierror = 0
  integer :: no_records
  integer :: no_of_levels

  integer :: level
  integer :: i

  character(len=24*80) :: line

  open (unit=10, file=trim(infile), status='old', form='formatted', action='read')

  ! file section 1, summary of launch

  do i = 1, 24

     read( 10, 100, iostat=ierror ) line(1:80)

     select case (ierror)

     case ( 0 )

        if ( i == 3 ) then
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

        if ( i == 17 ) read(line(1:80),'(62X,i6)') no_of_levels

     case ( iostat_end )
        write ( * , * ) 'Unexpectedly reached end of file in section 1!'
        exit
     case default
        write ( * , * ) 'Unexpected error when reading section 1!'
        exit
     end select
  end do

  write ( * , * ) trim(infile), ' contains ', no_of_levels, ' levels.'

  allocate ( radiosondedata(no_of_levels) )

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

  level = 1
  no_records = 1

  do while ( ierror == 0 )

     ! Read data records, 24 lines for 1 record.

     do i = 1, 24
        read( 10, 100, iostat=ierror ) line((i-1)*80+1:i*80)
        select case (ierror)
        case ( 0 )
           continue
        case ( iostat_end )
           write ( * , * ) 'EOF reached after trying record ', no_records
           no_records = no_records - 1
           exit
        case default
           write ( * , * ) 'Unexpected error when reading section 3!'
           exit
        end select
     end do

     if ( ierror == 0 ) then

        read(line, 110, iostat=ierror ) id1, id2, id3, id4, launch_time, tmpdata(1:46)
        if ( ierror /= 0 ) write ( * , * ) 'format error in ', trim(line)
        do i = 1, 46
           if ( level <= no_of_levels ) then
              radiosondedata(level) = tmpdata(i)
              ! write ( * , * ) launch_time, level, radiosondedata(level)%altitude
           else
              exit
           end if
           level = level + 1
        end do

        no_records = no_records + 1

     end if

  end do ! while-loop

  close (10)

  write ( * , * ) 'Processed ', no_records, 'data records for ', level-1, 'levels.'

  call write_netcdf ( infile, level-1, radiosondedata, metadata )

  deallocate ( radiosondedata )

110 format(I1,I4,I10,I5,I6,54X,46(F4.0,F5.1,F5.0,F4.1,F2.1,F4.2,F2.2,2(F4.1,F3.1)))
100 format(a)

end subroutine convert_data

! ----------------------

subroutine write_netcdf ( infile, no_of_levels, radiosondedata, metadata )

  use GATEradiosonde_mod
  use GATE_netcdf_mod

  implicit none

  character(len=FILENAME_LENGHT), intent(in) :: infile
  integer,                     intent(in) :: no_of_levels
  type (GATE_radiosonde_type), intent(in) :: radiosondedata(no_of_levels)
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
  integer :: t_id, terr_id
  integer :: q_id, qerr_id
  integer :: u_id, uerr_id
  integer :: v_id, verr_id

  real    :: time(1)
  real    :: position_start_lon, position_start_lat
  real    :: position_end_lon,   position_end_lat

  real    :: p(no_of_levels)
  real    :: ta(no_of_levels)
  real    :: q(no_of_levels)

  integer :: i

  character(len=33) :: seconds_since

  ! some preparation for writing global attributes

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
  call handle_err(nf_put_att_text(ncid, flight_time_id, 'units', len(seconds_since), seconds_since))
  
  p_id = define_variable_and_attribute_real( &
       ncid, dimids, 'p', 'air_pressure', 'air pressure', metadata%pressure_unit, 999999.0)

  alt_id = define_variable_and_attribute_real(ncid, dimids, 'altitude', 'geopotential_height', &
       'geopotential height', metadata%altitude_unit, 99999.0)
  call handle_err(nf_put_att_text(ncid, alt_id, "positive", 2, "up"))

  t_id = define_variable_and_attribute_real( &
       ncid, dimids, 'ta', 'temperature', 'temperature', metadata%temperature_unit, 9999.0)

  terr_id = define_variable_and_attribute_real( &
       ncid, dimids, 'ta_err', 'temperature_error', 'temperature error', metadata%temperature_unit, 9.9 )
  
  q_id = define_variable_and_attribute_real( &
       ncid, dimids, 'q', 'specific_humidity', 'specific humidity', metadata%specific_humidity_unit, 99.0)

  qerr_id = define_variable_and_attribute_real( &
       ncid, dimids, 'q_err', 'specific_humidity_error', 'specific humidity error', metadata%specific_humidity_unit, 99.99)

  u_id = define_variable_and_attribute_real( &
       ncid, dimids, 'u', 'eastward_wind', 'eastward wind', metadata%wind_unit, 999.9 )

  uerr_id = define_variable_and_attribute_real( &
       ncid, dimids, 'u_err', 'eastward_wind',  'eastward wind error', metadata%wind_unit, 99.9 )

  v_id = define_variable_and_attribute_real( &
       ncid, dimids, 'v', 'northward_wind', 'northward wind', metadata%wind_unit, 999.9)

  verr_id = define_variable_and_attribute_real( &
       ncid, dimids, 'v_err', 'northward_wind_error', 'northward wind error', metadata%wind_unit, 99.9)

  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "platform", len(trim(adjustl(metadata%platform))), &
       trim(adjustl(metadata%platform))))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_start_lon, ' ', position_start_lat 
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "launch_start_position", 18, position_str))

  write ( position_str, '(F9.4,A1,F8.4)' ) position_end_lon, ' ', position_end_lat 
  call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "launch_end_position", 18, position_str))

  call handle_err(nf_enddef (ncid))

  time(1) = 0.0

  call handle_err(nf_put_vara(ncid, launch_time_id, start(2), edge(2), time))

  do i = 1, no_of_levels
     if ( radiosondedata(i)%pressure > 9999.0 ) then
        p(i) = 999999.0
     else   
        p(i) = radiosondedata(i)%pressure * 100.0
     endif
     if ( radiosondedata(i)%temperature > 99.0 ) then
        ta(i) = 9999.0
     else   
        ta(i) = radiosondedata(i)%temperature + 273.15
     endif
     if ( radiosondedata(i)%specific_humdty > .98 ) then
        q(i) = 99.0
     else   
        q(i) = radiosondedata(i)%specific_humdty / 1000.0
     endif
  enddo
  
  call handle_err(nf_put_vara(ncid, flight_time_id, start, edge, radiosondedata(1:no_of_levels)%time_aft_launch))
  call handle_err(nf_put_vara(ncid, p_id,           start, edge, p))
  call handle_err(nf_put_vara(ncid, alt_id,         start, edge, radiosondedata(1:no_of_levels)%altitude))
  call handle_err(nf_put_vara(ncid, t_id,           start, edge, ta))
  call handle_err(nf_put_vara(ncid, terr_id,        start, edge, radiosondedata(1:no_of_levels)%temp_error))
  call handle_err(nf_put_vara(ncid, q_id,           start, edge, q))
  call handle_err(nf_put_vara(ncid, qerr_id,        start, edge, radiosondedata(1:no_of_levels)%humdty_error))
  call handle_err(nf_put_vara(ncid, u_id,           start, edge, radiosondedata(1:no_of_levels)%wind_vel_u_comp))
  call handle_err(nf_put_vara(ncid, uerr_id,        start, edge, radiosondedata(1:no_of_levels)%u_comp_error))
  call handle_err(nf_put_vara(ncid, v_id,           start, edge, radiosondedata(1:no_of_levels)%wind_vel_v_comp))
  call handle_err(nf_put_vara(ncid, verr_id,        start, edge, radiosondedata(1:no_of_levels)%v_comp_error))

  call handle_err(nf_close(ncid))

end subroutine write_netcdf

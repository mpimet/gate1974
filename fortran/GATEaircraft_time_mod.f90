module GATEaircraft_time_mod

implicit none

public :: charlie_time_converter, &
          hercules_time_converter, &
          aircraft_time_converter, &
          dc7_time_converter

  contains

    subroutine aircraft_time_converter(time, hours, minutes, seconds)

      ! The aircraft time format is given in HHMMSS
      !
      implicit none
      real, intent(IN)  :: time
      integer, intent(OUT) :: hours, minutes, seconds

      hours = floor(time)/10000
      minutes = ( floor(time) - hours * 10000) / 100
      seconds = 0

    end subroutine aircraft_time_converter

    subroutine charlie_time_converter(time, hours, minutes, seconds)

      ! The Charie time format is given in DDDHHMMSS
      !
      implicit none
      integer, intent(IN)  :: time
      integer, intent(OUT) :: hours, minutes, seconds

      integer              :: days

      days    = time/1000000
      hours   = (time - days*1000000) / 10000
      minutes = (time - days*1000000 - hours * 10000) / 100
      seconds = time - days*1000000 - hours * 10000 - minutes * 100

      ! write ( * , * ) "Charlie time", time, hours, minutes, seconds

    end subroutine charlie_time_converter

    subroutine hercules_time_converter(time, hours, minutes, seconds)

      ! The Hercules time format is given in HHMMSS
      implicit none
      real, intent(IN)  :: time
      integer, intent(OUT) :: hours, minutes, seconds
      hours = floor(time)/10000
      minutes = ( floor(time) - hours * 10000) / 100
      seconds = ( floor(time) - hours * 10000 - minutes * 100 )

    end subroutine hercules_time_converter

    subroutine dc7_time_converter(metadata, n, hours, minutes, seconds)

      use GATE_metadata_mod

      ! The DC-7 time format is ambigous.
      ! 08:02:00 is provided as 820
      ! 09:11:00 is provided as 9110
      ! 09:01:10 is provided as 9110, and other weired issue.
      !
      ! Some cases can be covered but we may have to skip a few first records
      ! due to wrong guesses about the exact time. One could reverse engineer
      ! the intial format based on the start time but ...
      !
      implicit none
      type(GATE_metadata_type), intent(IN) :: metadata
      integer, intent(IN) :: n
      integer, intent(OUT) :: hours, minutes, seconds
      character(LEN=6) :: str
      integer :: i, first_digit

      ! time storage used in next call 
      integer, save :: hh = 0
      integer, save :: mm = 0
      integer, save :: ss = 0

      integer, save :: nlen = 0
      logical       :: flip

      if ( hh == 0 .and. mm == 0 .and. ss == 0) then
         hh = metadata%time_start%hour
         mm = metadata%time_start%minute
         ss = metadata%time_start%second
      endif

      write ( str, '(I6)') n

      do i = 1, LEN_trim(str)
         if (str(i:i) /= ' ') then
            read (str(i:i), '(I1)') first_digit
            exit
         end if
      end do

      nlen = 6 - i + 1

      ! write ( * , * ) "string is ", str, n, nlen

      select case (metadata%interval_unit)

      case ( "M" )

         if ( first_digit >= 3 .and. first_digit <= 9 ) then
            ! hours 3 to 9
            if ( nlen == 3 ) then
               read (str(4:4), *) hours
               read (str(5:5), *) minutes
               read (str(6:6), *) seconds
            else if ( nlen == 4 ) then
               read (str(3:3), *) hours
               read (str(4:5), *) minutes
               read (str(6:6), *) seconds
            endif
         else
            ! hours 10 to 23
            if  ( nlen == 4 ) then
               read (str(3:4), *) hours
               read (str(5:5), *) minutes
               read (str(6:6), *) seconds
            else if ( nlen == 5 ) then
               read (str(2:3), *) hours
               read (str(4:5), *) minutes
               read (str(6:6), *) seconds
            else
               read (str(1:2), *) hours
               read (str(3:4), *) minutes
               read (str(5:6), *) seconds
            endif
         endif

      case ( "S" )

         ! stupid and brute force try to capture most of the exceptions 
         ! We assume here that aircrafts operated only during day time.

         if ( first_digit >= 3 .and. first_digit <= 9 ) then
            ! hours 3 to 9
            if ( nlen == 3 ) then
               read (str(4:4), *) hours
               read (str(5:5), *) minutes
               read (str(6:6), *) seconds
               if ( ( minutes - mm > 1 ) ) then
                 read (str(4:4), *) hours
                 read (str(5:6), *) minutes
                 seconds = 0
               endif
            else if ( nlen == 4 ) then
               flip = .false.
               read (str(3:3), *) hours
               read (str(4:5), *) minutes
               read (str(6:6), *) seconds
               if ( hours == hh ) then
                  if ( ( minutes < mm ) .or. &
                       ( minutes > 59 ) .or. &
                       ( minutes - mm > 1 ) .or. &
                       ( minutes == mm .AND. seconds < ss ) .or. &
                       ( str(4:4) == "0" ) ) then
                    flip = .true.
                  endif
               else
                 ! change of hour or day
                 flip = .true.
               endif
               if ( flip ) then
                  read (str(3:3), *) hours
                  read (str(4:4), *) minutes
                  read (str(5:6), *) seconds
               endif
            else
               read (str(2:2), *) hours
               read (str(3:4), *) minutes
               read (str(5:6), *) seconds
            endif
         else
            ! hours 10 to 23
            if  ( nlen == 4 ) then
               read (str(3:4), *) hours
               read (str(5:5), *) minutes
               read (str(6:6), *) seconds
            else if ( nlen == 5 ) then
               flip = .false.
               read (str(2:3), *) hours
               read (str(4:5), *) minutes
               read (str(6:6), *) seconds
               if ( hours == hh ) then
                  if ( ( minutes < mm ) .or. &
                       ( minutes - mm > 1 ) .or. &
                       ( minutes == mm .AND. seconds < ss ) .or. &
                       ( ss ==  9 ) .or. &
                       ( ss == 10 ) .or. &
                       ( ss == 20 ) .or. &
                       ( ss == 30 ) .or. &
                       ( ss == 40 ) .or. &
                       ( ss == 50 ) .or. &
                       ( str(4:4) == "0" ) ) then
                    flip = .true.
                  endif
               else
                 ! change of hour or day
                 flip = .true.
               endif
               if ( flip ) then
                  read (str(2:3), *) hours
                  read (str(4:4), *) minutes
                  read (str(5:6), *) seconds
                  if ( seconds > 59 ) then
                    read (str(2:3), *) hours
                    read (str(4:5), *) minutes
                    read (str(6:6), *) seconds
                  endif
               endif
            else
               read (str(1:2), *) hours
               read (str(3:4), *) minutes
               read (str(5:6), *) seconds
            endif
         endif

         hh = hours
         mm = minutes
         ss = seconds

      end select

      ! write ( * , * ) n, str, hours, minutes, seconds

    end subroutine dc7_time_converter

end module GATEaircraft_time_mod

module GATE_utils_mod

  public :: handle_err

contains

  subroutine handle_err(status)
    implicit none
    include 'netcdf.inc'

    integer, intent(in) :: status

    if (status .ne. nf_noerr) then
      print *, nf_strerror(status)
      stop 'Stopped'
    endif
  end subroutine handle_err

end module GATE_utils_mod

!
! Fortran module to define NetCDF variables and attributes
!

module GATE_netcdf_mod

  use GATE_metadata_mod, only : GATE_metadata_type

  include 'netcdf.inc'

  integer :: status

  private :: set_attributes, status

  public :: define_variable_and_attribute_double, &
            define_variable_and_attribute_real,   &
            define_variable_and_attribute_int,    &
            set_metadata,                         &
            handle_err

  contains

    ! Error handling subroutine
    subroutine handle_err(status)
      integer, intent(in) :: status
      if (status /= nf_noerr) then
        print *, 'NetCDF error: ', trim(nf_strerror(status))
        stop 1
      end if
    end subroutine handle_err

    subroutine set_attributes(ncid, var_id, std_name, &
         long_name, units)

      integer, intent(in) :: ncid
      integer, intent(in) :: var_id
   
      character(len=*), intent(in) :: std_name
      character(len=*), intent(in) :: long_name
      character(len=*), intent(in) :: units

      ! Set attributes
      status = nf_put_att_text(ncid, var_id, 'standard_name', len_trim(std_name), trim(std_name))
      call handle_err(status)
      status = nf_put_att_text(ncid, var_id, 'long_name', len_trim(long_name), trim(long_name))
      call handle_err(status)
      status = nf_put_att_text(ncid, var_id, 'units', len_trim(units), trim(units))
      call handle_err(status)

    end subroutine set_attributes

    ! Real (float) version
    function define_variable_and_attribute_real(ncid, dimids, var_name, std_name, &
                                                long_name, units, fill_value) &
             result(var_id)

      integer, intent(in) :: ncid
      integer, intent(in) :: dimids(:)
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: std_name
      character(len=*), intent(in) :: long_name
      character(len=*), intent(in) :: units
      real, optional, intent(in)   :: fill_value
      integer :: var_id

      ! Define variable
      call handle_err(nf_def_var(ncid, trim(var_name), nf_float, size(dimids), dimids, var_id))

      ! Set attributes
      call set_attributes(ncid, var_id, std_name, long_name, units)

      ! Set _FillValue if provided
      if (present(fill_value)) then
        call handle_err(nf_put_att_real(ncid, var_id, "_FillValue", NF_REAL, 1, fill_value))
      end if

    end function define_variable_and_attribute_real

    ! Double precision version
    function define_variable_and_attribute_double(ncid, dimids, var_name, std_name, &
                                                  long_name, units, fill_value) &
             result(var_id)

      integer, intent(in) :: ncid
      integer, intent(in) :: dimids(:)
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: std_name
      character(len=*), intent(in) :: long_name
      character(len=*), intent(in) :: units
      double precision, optional, intent(in) :: fill_value
      integer :: var_id

      ! Define variable
      call handle_err(nf_def_var(ncid, trim(var_name), nf_double, size(dimids), dimids, var_id))

      ! Set attributes
      call set_attributes(ncid, var_id, std_name, long_name, units)

      ! Set _FillValue if provided
      if (present(fill_value)) then
        call handle_err(nf_put_att_double(ncid, var_id, "_FillValue", NF_DOUBLE, 1, fill_value))
      end if

    end function define_variable_and_attribute_double

    ! Integer version
    function define_variable_and_attribute_int(ncid, dimids, var_name, std_name, &
                                               long_name, units, fill_value) &
             result(var_id)

      integer, intent(in) :: ncid
      integer, intent(in) :: dimids(:)
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: std_name
      character(len=*), intent(in) :: long_name
      character(len=*), intent(in) :: units
      integer, optional, intent(in) :: fill_value
      integer :: var_id

      ! Define variable
      call handle_err(nf_def_var(ncid, trim(var_name), nf_int, size(dimids), dimids, var_id))

      ! Set attributes
      call set_attributes(ncid, var_id, std_name, long_name, units)

      ! Set _FillValue if provided
      if (present(fill_value)) then
        call handle_err(nf_put_att_int(ncid, var_id, "_FillValue", NF_INT, 1, fill_value))
      end if

    end function define_variable_and_attribute_int

    subroutine set_metadata(ncid, metadata)

      integer, intent(in) :: ncid
      type (GATE_metadata_type), intent(in) :: metadata

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "title", len(trim(adjustl(metadata%title))), &
           trim(adjustl(metadata%title))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "summary", len(trim(adjustl(metadata%summary))), &
           trim(adjustl(metadata%summary))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "source", len(trim(adjustl(metadata%source))), &
           trim(adjustl(metadata%source))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "keywords", len(trim(adjustl(metadata%keywords))), &
           trim(adjustl(metadata%keywords))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "featureType", len(trim(adjustl(metadata%featureType))), &
           trim(adjustl(metadata%featureType))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "platform", len(trim(adjustl(metadata%platform))), &
           trim(adjustl(metadata%platform))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "instrument", len(trim(adjustl(metadata%instrument))), &
           trim(adjustl(metadata%instrument))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "references", len(trim(adjustl(metadata%references))), &
           trim(adjustl(metadata%references))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "creator_name", len(trim(adjustl(metadata%chief_scientist))), &
           trim(adjustl(metadata%chief_scientist))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "provider_name", len(trim(adjustl(metadata%provider_name))), &
           trim(adjustl(metadata%provider_name))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "provider_email", len(trim(adjustl(metadata%provider_email))), &
           trim(adjustl(metadata%provider_email))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "provider_id", len(trim(adjustl(metadata%provider_id))), &
           trim(adjustl(metadata%provider_id))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "license", len(trim(adjustl(metadata%license))), &
           trim(adjustl(metadata%license))))

      call handle_err(nf_put_att_text(ncid, NF_GLOBAL, "conventions", len(trim(adjustl(metadata%conventions))), &
           trim(adjustl(metadata%conventions))))

    end subroutine set_metadata

end module GATE_netcdf_mod



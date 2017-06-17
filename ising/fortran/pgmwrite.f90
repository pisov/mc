subroutine pgmwrite(filename, x, nx, ny)

  implicit none

  character*(*) :: filename
  integer :: nx, ny

  double precision,    dimension(nx, ny) :: x

  double precision,    dimension(nx, ny) :: tmp
  integer, dimension(nx, ny) :: grey

  double precision :: tmin, tmax
  double precision, parameter :: thresh = 255.0

  integer, parameter :: iounit = 10

  integer :: i, j

  tmp(:,:) = x(:,:)

!  Find the max and min absolute values of the array

  tmin = minval(abs(tmp(:,:)))
  tmax = maxval(abs(tmp(:,:)))

!  Scale the values appropriately so the lies between 0 and thresh

  if (tmin .lt. 0 .or. tmax .gt. thresh) then

    tmp(:,:) = int((thresh*((abs(tmp(:,:)-tmin))/(tmax-tmin))) + 0.5)

  else

    tmp(:,:) = int(abs(tmp(:,:)) + 0.5)

  end if

!  Increase the contrast by boosting the lower values

  grey(:,:) = thresh * sqrt(tmp(:,:)/thresh)

  open(unit=iounit, file=filename)

  write(iounit,fmt='(''P2''/''# Written by pgmwrite'')')
  write(iounit,*) nx, ny
  write(iounit,*) int(thresh)
  write(iounit,fmt='(16(i3,'' ''))') ((grey(i,j), i=1,nx), j=1,ny)

  close(unit=iounit)

end subroutine pgmwrite


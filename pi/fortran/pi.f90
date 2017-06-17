program mc 
implicit none

integer(kind = 8) :: Np  ! Number of generated points
integer(kind = 8) :: Nacc! Number of accepted points
integer(kind = 8) :: i   ! Work variable for do loops
double precision  :: pi  ! Accumnulated result
double precision  :: x   ! x - coordinate [0, 1]
double precision  :: y   ! y - coordinate [0, 1]
double precision  :: r2  ! Scuare distance of (x, y) point

write(0,'(A)', advance='NO')'Please enter the number of MC steps Np = '
read(*,*)Np

Nacc = 0
if (Np > 1) then
  call random_seed()
  do i = 1, Np
    call random_number(x)
    call random_number(y)
    r2 = x**2 + y**2
    if ( r2 .le. 1.d0) then
      Nacc = Nacc + 1
    end if
  end do

  pi = 4.d0 * dble(Nacc) / Np

  write(0,'(A20,F15.7)') 'Estimate value Pi = ',pi
else
  write(0,*)'Bad Np value !!!'
  stop
end if

end program mc

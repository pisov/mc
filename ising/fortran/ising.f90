module utils
double precision, parameter :: up = 1.d0
double precision, parameter :: down = -1.d0
contains
function calcEtot(grid, n, J)
  implicit none

  double precision, dimension(0:n+1,0:n+1), intent(in) :: grid
  integer, intent(in) :: n
  double precision, intent(in) :: J

  double precision :: calcEtot

  !
  ! Place to apply the code
  !

end function calcEtot

function calcMagneticMoment(grid, n)
  implicit none

  double precision, dimension(0:n+1,0:n+1), intent(in) :: grid
  integer, intent(in) :: n
  double precision :: calcMagneticMoment

  !
  ! Place to apply the code
  !

end function calcMagneticMoment

subroutine initGrid(grid, n, q)
  implicit none
  double precision, dimension(0:n+1,0:n+1), intent(inout) :: grid
  integer, intent(in) :: n
  double precision, intent(in) :: q

  integer :: i, j
  double precision :: p

  !
  ! Place to apply the code
  !

end subroutine initGrid

subroutine MCstep(grid, n, J)
  implicit none

  double precision, dimension(0:n+1, 0:n+1), intent(inout) :: grid
  integer, intent(in) :: n
  double precision, intent(in) :: J

  integer :: ii, jj
  double precision :: p, q, r

  !
  ! Place to apply the code
  !

end subroutine MCstep

end module utils

program ising
use utils
implicit none

integer(kind=8) :: i, j, MCsteps, wstep, k, wi, norm

integer :: n

double precision, dimension(:,:), allocatable :: grid, buf

double precision :: Jconst, h, T
double precision :: m, varMu, Mu, Chi, Etot, Cb, E
double precision :: p, q, r, tmp

character(len=64) :: filename

write(0,*)'Please enter problem size n = '
read(*,*)n

if (n < 2) then
  write(0,*)'Wrong n = ',n
  stop
end if

write(0,*)'Please enter number of MC steps = '
read(*,*)MCsteps

if (MCsteps < 1) then
  write(0,*)'Wrong MCsteps = ',MCsteps
  stop
end if

wstep = MCsteps / 100 

write(0,*)'Please enter temperature T = '
read(*,*)T

h = 0.d0
Jconst = 1.d0 / T


! Allocate grid array
allocate(grid(0:n+1, 0:n+1))
! Allocate buf array
allocate(buf(n, n))

call random_seed

! Array initialization
call initGrid(grid, n, 0.6d0)

norm = 0
Mu = 0.d0
varMu = 0.d0
Etot = 0.d0
Cb = 0.d0
wi = 0

do k = 1, MCsteps
  grid(0, :)   = grid(n, :)
  grid(n+1, :) = grid(1, :)

  grid(:, 0)   = grid(:, n)
  grid(:, n+1) = grid(:, 1)

  if (k > MCsteps / 2) then
    norm = norm + 1
    m = calcMagneticMoment(grid, n)
    Mu = Mu + m
    varMu  = varMu + m ** 2
    E = calcEtot(grid, n, Jconst) 
    Etot = Etot + E
    Cb = Cb + E ** 2
  end if

  call MCstep(grid, n, Jconst)

  if ( mod(k, wstep).eq.0) then
    write(0,'(A,I15)')'Step: ',k
    write (filename, "(A4,I0.4,A4)") "step", wi, ".pgm"
    buf(1:n, 1:n) = (grid(1:n, 1:n) + 1.d0) * 255.
    call pgmwrite(filename, buf, n, n)
    wi = wi + 1
  end if
end do

Mu = Mu / norm
varMu = sqrt(varMu / norm - Mu ** 2)
Etot = Etot / norm
Cb = sqrt(Cb / norm - Etot ** 2)

write(*,'(6F10.5)')T, Jconst, Mu, varMu, Etot, Cb

deallocate(grid)
deallocate(buf)

end program ising

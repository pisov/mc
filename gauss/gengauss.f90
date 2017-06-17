module functions
implicit none
contains
function NormDist(n)
double precision :: NormDist
integer, intent(in) :: n

integer :: i
double precision, allocatable, dimension(:)  :: x

allocate(x(n))

NormDist = 0.d0

call random_number(x)

NormDist = sum(x) / n - 0.5d0
NormDist = sqrt(dble(n)) * NormDist


end function NormDist

end module functions

program gauss
use functions
implicit none

integer :: i, n, nsamp

nsamp = 10000
n = 10000

call random_seed()

do i = 1, n
  write(*,'(E25.15)') NormDist(nsamp)
end do


end program gauss

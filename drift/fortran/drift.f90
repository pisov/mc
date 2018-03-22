program drift
  implicit none
! Electron mass [Kg]
  double precision, parameter :: xmasse = 9.11d-31
! Nuclon mass [Kg]
  double precision, parameter :: xmassp = 1.67d-27
! Electric field strength [V/m]
  double precision, parameter :: efield = 500.
! Collision frequensy [Hz]
  double precision, parameter :: colf = 1.d9
! Target width [m]
  double precision, parameter :: zend = 0.2
! Number of nuclons in the nucleus [Neon n = 20]
  integer, parameter :: n = 20
! Electric field acceleration [Kg m / s^2]
  double precision, parameter :: acz = efield * 1.6d-19 / xmasse
! Define Pi constant
  double precision :: pi = 3.14159265d0
  
  integer :: i, ielectrons, it_out, itx
  double precision :: vdrift = 0.
  double precision :: norm = 0.
  double precision :: x, y, z, v, vx, vy, vz, acx, acy, dt, ttotal
  double precision :: alpha, beta, phi, theta
  double precision :: xmassn
  double precision :: p, q

! Caclulate the nucleus  mass
  xmassn = n * xmassp

  write(0,'(A)')'Number of electrons ( >= 100)to iterate = '
  read(*,*)ielectrons
  if (ielectrons < 100) then
    write(0, *)'Unsufficient number of electrons'
  end if
  it_out = 100

  acx = 0.
  acy = 0.
  itx = 1
  do i = 1, ielectrons
    if (mod(i, it_out) == 0) then
      write(*,'(A12,I15,A1)')'#"Trajectory', itx,'"'
      write(0,'(A12,I15,A1)')'#"Trajectory', itx,'"'
      itx = itx + 1
    end if
    x = 0.
    y = 0.
    z = 0.
    ttotal = 0.
    vx = 0.
    vy = 0.
    vz = 0.
    ! Write out trajectory
    if (mod(i, it_out) == 0) then
      write(*,'(4F20.10)')ttotal, x, y, z
    end if


!   Caclulate the time period to the next collision
50    call random_number(p)
    dt = - (1. / colf) * log(1. - p)

!   Make one integration step further
    x = x + vx * dt + 0.5 * acx * dt * dt
    y = y + vy * dt + 0.5 * acy * dt * dt
    z = z + vz * dt + 0.5 * acz * dt * dt
    vx = vx + acx * dt
    vy = vy + acy * dt
    vz = vz + acz * dt

! Write out trajectory
    if (mod(i, it_out) == 0) then
      write(*,'(4F20.10)')ttotal, x, y, z
    end if


!   Accumulate total drift time
    ttotal = ttotal + dt

!   Check wheter end of target has been reached
!   if not -> collision
    if (z.lt.zend) then
      v = sqrt(vx*vx+vy*vy+vz*vz)

      beta = atan2(vy,vx)
      alpha = acos(vz/v)

!     Generate collision angles (theta, phi)
      call random_number(p)
      theta = acos(2. * p - 1.)
      call random_number(q)
      phi  = 2 * pi * q

!     Calculate new velocity after elastic collision
      v = v * sqrt(1. - (2.*xmasse/xmassn)*(1.-cos(theta)))

!     Calcluate new direction, i.e. rotate with (theta, phi)
      vx = v*(cos(beta)*cos(alpha)*sin(theta)*cos(phi) +&
     &          cos(beta)*sin(alpha)*cos(theta)-&
     &          sin(beta)*sin(theta)*sin(phi))
      vy = v*(sin(beta)*cos(alpha)*sin(theta)*cos(phi)+&
     &          sin(beta)*sin(alpha)*cos(theta)+&
     &          cos(beta)*sin(theta)*sin(phi))
      vz = v*(-sin(alpha)*sin(theta)*cos(phi)+&
     &          cos(alpha)*cos(theta))
      goto 50
    end if
    if (mod(i, it_out) == 0) then
      write(*,*)
      write(*,*)
    end if
!   Accumulate drift velocity for all electrons
    vdrift = vdrift + z/ttotal
    norm = norm + 1.
  end do
! Average drift velocity for all electrons
  vdrift = vdrift / norm
  write(0,'(A,F20.10,A)')'vdrift = ',vdrift, ' [m/s]'
  write(0,'(A,F20.10,A)')'Ekin   = ',0.5*xmasse*vdrift**2*6.2415097d+18, ' [eV]'
end program drift

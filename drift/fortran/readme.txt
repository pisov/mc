1. Compile

gfortran -O3 drift.f90 -o drift.x

or

make

2. Execute:

./drift.x > trajectory.dat

(ielectrons = 1000)

3. Plot the trajectories

gnuplot plot.gnu

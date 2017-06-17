1. Compile

gfortran -O3 gengauss.f90 -o gengauss.x

or

make

2. Execute

./gengauss.x > gauss.dat

3. Plot the distribution

gnuplot hist.gnu

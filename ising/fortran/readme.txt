1. Compile

gfortran -O3 ising.f90 pgmwrite.f90 -o ising.x

or

make

2. Execute 

[Parameters]
#n = 100
#MCsteps = 100000
#Temperature around citical Tc = 2.2691.. [2/(log(1+sqrt(2))]
#Tc = 2.26918531421302

./ising.x

3. Make GIF animation from generated frames

convert -delay 20 step*.pgm ising.gif

or

./makeanim.sh

4. Then open ising.gif with any browser

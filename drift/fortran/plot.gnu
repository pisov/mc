set xlabel "Z [m]"
set ylabel "X [m]"

set xrange [0:0.2]
set yrange [-0.5:0.5]

set size square 

datafile = 'trajectory.dat'
#stats datafile
STATS_blocks=10
plot for [IDX=1:STATS_blocks] datafile index (IDX-1) u 4:2 w lp ps 0.5 pt 7 notitle
pause -1

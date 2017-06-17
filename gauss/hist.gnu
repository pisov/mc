n=100 #number of intervals
max=1.5 #max value
min=-1.5 #min value
width=(max-min)/n #interval width
#function used to map a value to the intervals
hist(x,width)=width*floor(x/width)+width/2.0
set boxwidth width*0.9

set xlabel "x"
set ylabel "Count"
set xrange [-1.5:1.5]
#count and plot
plot "gauss.dat" u (hist($1,width)):(1.0) smooth freq w boxes lc rgb"green" notitle

pause -1

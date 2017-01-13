set term png

set output "plot4.png"
set xlabel 'n (millions)'
set yrange[0:0.5]
set ylabel 'Time (sec)' tc lt 1
set y2range[0:0.000035]
set y2tics 0.000005 nomirror tc lt 2

plot "results4.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "results4.dat" using ($1/1e6):3 title 'index Speed' axes x1y2

set output "plot8.png"

plot "results8.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "results8.dat" using ($1/1e6):3 title 'index Speed' axes x1y2

set output "plot16.png"

plot "results16.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "results16.dat" using ($1/1e6):3 title 'index Speed' axes x1y2 

set output "plot32.png"

plot "results32.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "results32.dat" using ($1/1e6):3 title 'index Speed' axes x1y2 

set output "plot64.png"

plot "results64.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "results64.dat" using ($1/1e6):3 title 'index Speed' axes x1y2 

set term png

set output "analysis.png"
set xlabel 'n (millions)'
set ylabel 'Time (sec)' tc lt 1
set y2label 'Time (sec)' tc lt 2
set y2tics 0.000005 nomirror tc lt 2

plot "analysisPlot.dat" using ($1/1e6):($2 < 1 ? $2 : 1/0) title 'Concat Speed', \
     "analysisPlot.dat" using ($1/1e6):3 title 'index Speed' axes x1y2



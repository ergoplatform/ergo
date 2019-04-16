set terminal png size 1024,768
set output "emission.png"

set xrange [0:8]
set yrange [0:100000000]

set style line 1 \
    linecolor rgb '#07820f' \
    linetype 1 linewidth 3

set style line 2 \
    linecolor rgb '#0008a8' \
    linetype 1 linewidth 3

set key right center
set key font ",20"

set ytics font ",20"
set xtics font ",20"
set xlabel "Year since genesis block" font ",24"

set ylabel "Ergo" offset -2,0,0 font ",24"
set lmargin 12
set tmargin 2

plot "emission.dat" using 1:2 title 'Treasury part' with lines  linestyle 1, \
     "emission.dat" using 1:3 title 'Monetary base' with lines  linestyle 2
set print "-"

set term png
#set terminal png enhanced size 1280, 1024 font ',20'
set terminal png enhanced font ',20'
set xlabel "Time (years)" font ",20"
set tics font ", 20"
set key font ",20"
set style line 1 lt 20 lw 4 linecolor 7

set output "EmissionCurve.png"
set ylabel "Coins total"  font ",20"

plot 'emission.csv' using 1:2 with lines ls 1 notitle

set output "EmissionRate.png"

set ylabel "Coins per block"  font ",20"

plot 'emission.csv' using 1:3 with lines ls 1 notitle

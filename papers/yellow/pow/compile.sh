#!/usr/bin/env sh

command -v pdflatex && command -v bibtex
if [ "$?" != 0 ]; then
    echo "Command 'pdflatex' or 'bibtex' not exist, both must be installed. For Ubuntu, try:"
    echo "sudo apt install texlive-latex-base texlive-binaries"
    echo
    echo "You may also need to install additional packages like fonts, etc. For Ubuntu, try:"
    echo "sudo apt install texlive-fonts-recommended texlive-science latex-xcolor texlive-latex-extra cm-super"
    echo
    echo "You can install all the TeX additional packages (+2Gb disk space) with: sudo apt install texlive-full"
    exit 1;
fi

rm ErgoPow.pdf
pdflatex ErgoPow.tex
bibtex ErgoPow
pdflatex ErgoPow.tex
pdflatex ErgoPow.tex
rm *.aux
rm *.log

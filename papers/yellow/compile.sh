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

rm -f YellowPaper.pdf
pdflatex -shell-escape YellowPaper
bibtex -shell-escape YellowPaper
pdflatex -shell-escape YellowPaper
pdflatex -shell-escape YellowPaper

if [ $(dirname $0) = "." ]; then
    rm -f *.aux
    rm -f *.log
    rm -f *.blg
    rm -f *.bbl
    rm -f *.out
    rm -f *.toc
    rm -fr _minted-YellowPaper/
fi

#!/usr/bin/env sh

command -v pdflatex && command -v bibtex
if [[ "$?" != 0 ]]; then
    echo "Command 'pdflatex' or 'bibtex' not exist, both must be installed. For Ubuntu, try:"
    echo "sudo apt install texlive-latex-base texlive-binaries texlive-latex-recommended"
    echo
    echo "You may also need to install additional packages like fonts, etc. For Ubuntu, try:"
    echo "sudo apt-get install texlive-fonts-recommended latex-xcolor texlive-latex-extra cm-super"
    exit 1;
fi

rm whitepaper.pdf
pdflatex whitepaper.tex
bibtex whitepaper
pdflatex whitepaper.tex
pdflatex whitepaper.tex
rm *.aux
rm *.log
rm *.bbl
rm *.blg
rm *.out

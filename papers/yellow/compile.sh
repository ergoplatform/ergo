#!/usr/bin/env sh

STOP_FLAG=false

command -v pdflatex
if [ "$?" != 0 ]; then
    echo "Command 'pdflatex' not exist, please install. For Ubuntu, try 'sudo apt install texlive-latex-base'"
    STOP_FLAG=true
fi

command -v bibtex
if [ "$?" != 0 ]; then
    echo "Command 'bibtex' not exist, please install. For Ubuntu, try 'sudo apt install texlive-binaries'"
    STOP_FLAG=true
fi

if [ "$STOP_FLAG" = true ]; then
    echo "Install needed programs to compile TeX."
    echo "You may need to install additional packages like fonts, etc. For Ubuntu, try:"
    echo "sudo apt-get install texlive-fonts-recommended latex-xcolor texlive-latex-extra cm-super"
    exit 1;
fi

rm main.pdf
pdflatex -shell-escape main
bibtex -shell-escape main
pdflatex -shell-escape main
pdflatex -shell-escape main
rm *.aux
rm *.log
rm *.blg
rm *.bbl
rm *.out
rm *.toc
rm -r _minted-main/

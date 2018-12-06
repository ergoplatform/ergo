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

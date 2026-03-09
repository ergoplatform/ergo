#!/bin/bash

# Compile LaTeX document to PDF

# Check if llncs.cls exists, copy if needed
if [ ! -f "llncs.cls" ]; then
    if [ -f "../contractual/llncs.cls" ]; then
        cp ../contractual/llncs.cls .
        echo "Copied llncs.cls from contractual directory"
    else
        echo "Error: llncs.cls not found. Please download LLNCS class file."
        exit 1
    fi
fi

echo "Compiling Input Blocks documentation with TikZ diagrams..."

# Compile LaTeX document (with nonstopmode to continue despite potential warnings)
pdflatex -interaction=nonstopmode main.tex
if [ $? -ne 0 ]; then
    echo "Error during first pdflatex compilation"
    exit 1
fi

bibtex main
if [ $? -ne 0 ]; then
    echo "Error during bibtex compilation"
    exit 1
fi

pdflatex -interaction=nonstopmode main.tex
if [ $? -ne 0 ]; then
    echo "Error during second pdflatex compilation"
    exit 1
fi

pdflatex -interaction=nonstopmode main.tex
if [ $? -ne 0 ]; then
    echo "Error during third pdflatex compilation"
    exit 1
fi

# Clean up auxiliary files
rm -f main.aux main.log main.out main.toc main.bbl main.blg main.lof main.lot

echo "Compilation complete. Output: main.pdf"
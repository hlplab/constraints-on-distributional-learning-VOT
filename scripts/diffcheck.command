#!/bin/bash
cd -- "$(dirname "$BASH_SOURCE")"

# change the paths to your respective tex files
latexdiff --disable-auto-mbox '../output/papers/Cognition-Round1/manuscript-main-file.tex' manuscript-main-file.tex > manuscript_diff.tex
sleep 20
xelatex -interaction=batchmode -halt-on-error manuscript_diff.tex
bibtex manuscript_diff
xelatex -interaction=batchmode -halt-on-error manuscript_diff.tex

mv manuscript_diff.pdf '../output/papers/Cognition-Round2/manuscript_diff.pdf'

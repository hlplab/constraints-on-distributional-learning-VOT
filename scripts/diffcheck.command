#!/bin/bash
cd -- "$(dirname "$BASH_SOURCE")"

# change the paths to your respective tex files
latexdiff ../output/papers/Cognition-Round1/manuscript-main-file.tex manuscript-main-file.tex > manuscript_diff.tex
sleep 20
xelatex manuscript_diff.tex
mv manuscript_diff.pdf ../output/papers/Cognition-Round2/manuscript_diff.pdf

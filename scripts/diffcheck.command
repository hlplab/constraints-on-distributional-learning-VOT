#!/bin/bash
cd -- "$(dirname "$BASH_SOURCE")"

echo "Generating diff between original and revised manuscript..."
latexdiff --disable-auto-mbox '../output/papers/Cognition-Round1/manuscript-main-file.tex' manuscript-main-file.tex > manuscript_diff.tex

echo "Adding table adjustments to fix tables getting cut off..."
# Add table adjustments to the preamble of the diff file
sed -i '' '1s/^/\\usepackage{adjustbox}\n\\let\\oldtabular\\tabular\n\\let\\endoldtabular\\endtabular\n\\renewenvironment{tabular}{\\begin{adjustbox}{max width=\\textwidth}\\begin{oldtabular}}{\\end{oldtabular}\\end{adjustbox}}\n/' manuscript_diff.tex

echo "Running first LaTeX pass..."
xelatex -interaction=nonstopmode manuscript_diff.tex

echo "Running BibTeX..."
bibtex manuscript_diff

echo "Running second LaTeX pass..."
xelatex -interaction=nonstopmode manuscript_diff.tex

echo "Running final LaTeX pass to resolve all references..."
xelatex -interaction=nonstopmode manuscript_diff.tex

echo "Moving PDF to output directory..."
mv manuscript_diff.pdf '../output/papers/Cognition-Round2/manuscript_diff.pdf'

echo "Done! PDF created with properly sized tables and bibliographic references."

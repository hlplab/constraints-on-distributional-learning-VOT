# Set the path to your .bib file (without the .bib extension)
bib_file <- "/Users/mata4004/Desktop/constraints-on-distributional-learning-VOT/tan_jaeger2025_cognition_source_files/latex-stuff/library"

# Check if the .bib file exists
if (!file.exists(paste0(bib_file, ".bib"))) {
  stop(
    "Cannot find the BibTeX file: ", paste0(bib_file, ".bib"),
    ". Please check the path and file name."
  )
}

# Install required LaTeX packages if needed
tryCatch(
  {
    tinytex::tlmgr_install(c("apalike", "natbib", "url"))
  },
  error = function(e) {
    cat("Note: Some package installation issues encountered, but we'll continue.\n")
    cat("The APA style may still work if it's already installed in your system.\n")
  }
)

# Create LaTeX file with APA style
writeLines(
  c(
    "\\documentclass{article}",
    "\\usepackage{natbib}",
    "\\usepackage{url}",
    "\\begin{document}",
    "\\nocite{*}",
    "\\bibliographystyle{apalike}", # APA-like style
    paste0("\\bibliography{", bib_file, "}"),
    "\\end{document}"
  ),
  "converter.tex"
)

# Run pdflatex first to generate the .aux file
tinytex::pdflatex("converter.tex", clean = FALSE)

# Check if the .aux file exists
if (!file.exists("converter.aux")) {
  stop("No .aux file was created. Cannot proceed with BibTeX.")
}

# Run BibTeX manually through system command
system_result <- system("bibtex converter")
if (system_result != 0) {
  warning("BibTeX may have encountered errors. Check the .blg file.")
}

# Check if BibTeX created the .bbl file
if (file.exists("converter.bbl")) {
  cat("Success! The .bbl file was created.\n")
  cat("\nFirst few lines of the .bbl file:\n")
  cat(readLines("converter.bbl", n = 10), sep = "\n")
  cat("\n...(truncated)...\n")

  # Copy the .bbl file to a more permanent location
  file.copy("converter.bbl", "references.bbl", overwrite = TRUE)
  cat("\nThe .bbl file has been copied to 'references.bbl'.\n")
} else {
  cat("Error: .bbl file was not created.\n")
  # Show BibTeX log for debugging
  if (file.exists("converter.blg")) {
    cat("\nBibTeX log contents:\n")
    cat(readLines("converter.blg"), sep = "\n")
  }
}

# Complete the LaTeX processing to get the PDF
tinytex::pdflatex("converter.tex", clean = FALSE)
tinytex::pdflatex("converter.tex")

# Final verification
cat("\nSummary of created files:\n")
cat("- .bbl file created: ", file.exists("converter.bbl"), "\n")
cat("- .pdf file created: ", file.exists("converter.pdf"), "\n")
cat("- Copy saved as: references.bbl\n")

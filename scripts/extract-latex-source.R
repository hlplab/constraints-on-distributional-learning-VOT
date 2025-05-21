# A simple script to extract LaTeX source from an R Markdown document
library(rmarkdown)
library(knitr)

# Path to main Rmd file
rmd_file <- "/Users/mata4004/Desktop/constraints-on-distributional-learning-VOT/scripts/manuscript-main-file.Rmd"

# Output directory
output_dir <- "/Users/mata4004/Desktop/constraints-on-distributional-learning-VOT/latex-source"
dir.create(output_dir, showWarnings = FALSE)

# Check if the main file exists
if (!file.exists(rmd_file)) {
    stop("Cannot find the main Rmd file at: ", rmd_file)
}

# Save original working directory
original_dir <- getwd()

# Set working directory to scripts folder to ensure all relative paths are correct
setwd(dirname(rmd_file))
cat("Working in directory:", getwd(), "\n")

# List files in current directory to help diagnose issues
cat("Files in current directory:\n")
print(list.files())

# Check if child files exist
child_files <- c(
    "section-0-TO-DO.Rmd",
    "section-1-introduction.Rmd",
    "section-2-methods.Rmd",
    "section-3-results-analysis.Rmd",
    "section-4-general-discussion.Rmd",
    "section-5-references.Rmd",
    "supplementary-information.Rmd"
)

for (file in child_files) {
    if (file.exists(file)) {
        cat("Child file exists:", file, "\n")
    } else {
        cat("WARNING: Child file not found:", file, "\n")
    }
}

# Create figures directory
figures_dir <- file.path(output_dir, "figures")
dir.create(figures_dir, showWarnings = FALSE)

# Try using papaja if available, otherwise fall back to pdf_document
output_format <- tryCatch(
    {
        if (requireNamespace("papaja", quietly = TRUE)) {
            cat("Using papaja output format\n")
            papaja::apa6_pdf(keep_tex = TRUE)
        } else {
            cat("Papaja not available, using standard pdf_document\n")
            pdf_document(keep_tex = TRUE)
        }
    },
    error = function(e) {
        cat("Error setting output format:", e$message, "\n")
        cat("Falling back to standard pdf_document\n")
        pdf_document(keep_tex = TRUE)
    }
)

# Set keep_tex in knitr options as well
opts_knit$set(keep_tex = TRUE)

# Render document
cat("Rendering document to LaTeX...\n")

# Try a simpler approach - directly knit to LaTeX
tex_file <- file.path(output_dir, "manuscript.tex")
cat("Attempting direct knitting to:", tex_file, "\n")
tryCatch(
    {
        # Try direct knitting first
        knit2pdf(input = basename(rmd_file), output = tex_file, envir = new.env())
        cat("Direct knitting to PDF successful\n")
    },
    error = function(e) {
        cat("Error with knit2pdf:", e$message, "\n")

        # Try another approach - purl the R code then knit
        cat("Trying with regular knit...\n")
        tryCatch(
            {
                knit(input = basename(rmd_file), output = tex_file)
                cat("Regular knit successful\n")
            },
            error = function(e) {
                cat("Error with regular knit:", e$message, "\n")
                cat("Trying with render...\n")

                # Last try with render
                render(basename(rmd_file),
                    output_format = output_format,
                    output_file = file.path(output_dir, "manuscript.pdf")
                )
            }
        )
    }
)

# Check if LaTeX file was created
latex_file <- file.path(output_dir, "manuscript.tex")
if (file.exists(latex_file)) {
    cat("Success! LaTeX source generated at:", latex_file, "\n")

    # Copy any necessary files for the journal
    # 1. Copy any bibliography files
    bib_dirs <- c(".", "latex-stuff", "../latex-stuff")
    for (dir in bib_dirs) {
        if (dir.exists(dir)) {
            bib_files <- list.files(dir, pattern = "\\.bib$", full.names = TRUE)
            if (length(bib_files) > 0) {
                file.copy(bib_files, output_dir, overwrite = TRUE)
                cat("Copied bibliography files from", dir, "\n")
            }
        }
    }

    # 2. Look for any style files
    for (dir in bib_dirs) {
        if (dir.exists(dir)) {
            style_files <- list.files(dir, pattern = "\\.(cls|sty|csl)$", full.names = TRUE)
            if (length(style_files) > 0) {
                file.copy(style_files, output_dir, overwrite = TRUE)
                cat("Copied style files from", dir, "\n")
            }
        }
    }

    # Create a README file with instructions
    readme_text <- c(
        "LATEX SOURCE FILE",
        "================",
        "",
        "This file contains the LaTeX source for the manuscript",
        "\"Learning to understand an unfamiliar talker: Testing distributional learning as a model of rapid adaptive speech perception\"",
        "",
        "The LaTeX file includes all content from the paper including references.",
        "",
        "For the journal production editor:",
        "1. The manuscript.tex file contains the complete manuscript",
        "2. Bibliography files (*.bib) are included in this directory",
        "3. Style files (*.sty, *.cls) are included in this directory",
        "4. All figures should be in the figures/ directory",
        "5. For any questions, please contact Maryann Tan (maryann.tan@biling.su.se)"
    )
    writeLines(readme_text, file.path(output_dir, "README.txt"))

    cat("README file created\n")
} else {
    cat("Failed to generate LaTeX source. Try manual approach instead:\n")
    cat("1. Open manuscript-main-file.Rmd in RStudio\n")
    cat("2. Click the 'Knit' button and choose 'Knit to PDF'\n")
    cat("3. Find the .tex file in the same directory as the PDF\n")
}

# Return to original directory
setwd(original_dir)

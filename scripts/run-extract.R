# This is a simple script to run the extract-latex-source.R file
# in a fresh R session to avoid package conflicts

# Create a system command to run the extract script in a fresh R session
r_cmd <- paste0(
    'Rscript -e "source(\'',
    "/Users/mata4004/Desktop/constraints-on-distributional-learning-VOT/scripts/extract-latex-source.R",
    '\')"'
)

# Run the command
system(r_cmd)

cat("Script executed in a fresh R session to avoid package conflicts.\n")

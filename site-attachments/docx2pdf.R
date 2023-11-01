## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Create .pdf from .docx to Overwrite LaTeX .pdf]
##' [INIT: 2023-11-01]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

## Pre and post render scripts are run with the main project directory.
## https://quarto.org/docs/projects/scripts.html

library(doconv)

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

doc_docx <- file.path(output, "00-Syllabus.docx")
doc_pdf <- file.path(output, "00-Syllabus.pdf")

docx2pdf(input = doc_docx, output = doc_pdf)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

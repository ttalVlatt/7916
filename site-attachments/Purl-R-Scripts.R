## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Purl .qmd Scripts to Extract .R Files]
##' [INIT: 2023-11-01]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

## Pre and post render scripts are run with the main project directory.
## https://quarto.org/docs/projects/scripts.html

input <- Sys.getenv("QUARTO_PROJECT_INPUT_FILES")

input_list <- stringr::str_split(input, "\n") |>
  unlist() # Flattens list to atomic vector for str_subset

input_qmd <- stringr::str_subset(input_list, ".qmd")

for(i in input_qmd) {
  
  knitr::purl(i, documentation = 0)

}

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

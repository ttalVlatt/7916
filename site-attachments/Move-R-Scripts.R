## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Move Purled R Scripts to Quarto Output Dir]
##' [INIT: 2023-11-01]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

## Pre and post render scripts are run with the main project directory.
## https://quarto.org/docs/projects/scripts.html

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

r_files <- list.files(pattern = ".R$")

dir.create(file.path(output, "R-scripts"))

for(i in r_files) {
  
  file.rename(i, file.path(output, "R-scripts", i))
  
}

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

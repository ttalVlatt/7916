## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Purl .R Scripts from .qmd files]
##' [INIT: 2023-11-01]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

## Pre and post render scripts are run with the main project directory.
## https://quarto.org/docs/projects/scripts.html

qmd_files <- list.files(pattern = ".qmd$")

for(i in qmd_files) {
  
  knitr::purl(i, documentation = 0)
  
}

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

dir.create(output)

dir.create(file.path(output, "r-scripts"))

r_files <- list.files(pattern = ".R$")

for(i in r_files) {
  
  file.rename(i, file.path(output, "r-scripts", i))
  
}
# 
# ## Copy Across Data Folder
# fs::dir_copy("data", file.path(output, "data"), overwrite = T)
# 
# ## Zip R-Scripts and Data folders
# 
# files_to_zip <- dir(file.path(output, "r-scripts"))
# 
# zip(file.path(output, "r-class-scripts.zip"),
#     files = files_to_zip)
# 
# files_to_zip <- dir(file.path(output, "data"))
# 
# zip(file.path(output, "data.zip"),
#     files = files_to_zip)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

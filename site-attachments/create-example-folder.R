## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Create Downloadable Example Folder in Output Directory]
##' [INIT: 2023-12-22]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

library(tidyverse)

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

example <- file.path(output, "example-folder")

dir.create(example)

##'[Create data sub-folder]
fs::dir_copy("data", file.path(example, "data"), overwrite = T)

##'[Copy Lesson R Scripts]
r_scripts <- list.files(file.path(output, "r-scripts"))

## Delete non-lesson scripts from list
r_scripts <- r_scripts |>
  discard(~str_detect(.x, "^_.*")) |>
  discard(~str_detect(.x, "index")) |>
  discard(~str_detect(.x, "00")) |>
  discard(~str_detect(.x, "99")) |>
  discard(~str_detect(.x, "07")) |>
  discard(~str_detect(.x, "14")) |>
  discard(~str_detect(.x, "x-01"))

##'[Add lesn- to all lesson scripts for grouping]

for(i in r_scripts) {
  
  new_name <- paste0("lesn-", i)
  
  file.copy(from = file.path(output, "r-scripts", i),
            to = file.path(example, new_name))
  
}


##'[Create Final Project Sub-Folder]

final <- file.path(example, "reproducible-report")

dir.create(final)

dir.create(file.path(final, "data"))

##'[ZIP Example Folder]

files <- list.files(example, full.names = T, recursive = T)

zip::zip(file.path(output, "example-folder.zip"),
         files = files,
         mode = "mirror",
         root = output)      
           
unlink(example, recursive = T)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Create Downloadable Example Folder in Output Directory]
##' [INIT: 2023-12-22]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## ----------------------------------------------------------------------------
library(tidyverse)

output <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR")

##'[Remove duplicates in output created by Quarto glitch]
dup_files <- list.files(output, pattern = "\\s\\d\\.[a-zA-Z0-9]+$")
dup_folders <- list.files(output, pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(file.path(output, i))
}

for(i in dup_folders) {
  fs::dir_delete(file.path(output, i))
}

##'[Remove duplicates in main created by Quarto glitch]
dup_files <- list.files(pattern = "\\s\\d\\.[a-zA-Z0-9]+$")
dup_folders <- list.files(pattern = "\\s\\d$")

for(i in dup_files) {
  unlink(i)
}

for(i in dup_folders) {
  fs::dir_delete(i)
}

##'[Create example folder]
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
  discard(~str_detect(.x, "syllabus")) |>
  discard(~str_detect(.x, "99")) |>
  discard(~str_detect(.x, "07")) |>
  discard(~str_detect(.x, "14")) |>
  discard(~str_detect(.x, "x-01")) |>
  discard(~str_detect(.x, "x-02")) |>
  discard(~str_detect(.x, "x-04"))

##'[Add lesn- to all lesson scripts for grouping]

for(i in r_scripts) {
  
  # new_name <- paste0("l-", i)
  file.copy(from = file.path(output, "r-scripts", i),
            to = file.path(example, i))
  
}

##'[Copy in R Script Template]
file.copy(from = file.path("site-attachments", "r-script-template.R"),
          to = file.path(example, "r-script-template.R"))

##'[Copy in Syllabus .pdf]
file.copy(from = file.path(output, "syllabus.pdf"),
          to = file.path(example, "syllabus.pdf"))

##'[Create Final Project Sub-Folder]

final <- file.path(example, "reproducible-report")
dir.create(final)
# Copy .gitignore both for function and for placeholder
file.copy(from = ".gitignore", to = file.path(final, ".gitignore"))
dir.create(file.path(final, "data"))
# Copy hd2007 as placeholder to ensure folder is made in zipping
file.copy(from = file.path("data", "hd2007.csv"),
          to = file.path(final, "data", "placeholder.csv"))

##'[ZIP Example Folder]

## To avoid empty files, setwd to the example
setwd(example)
## Then list files from the newly set wd
files <- list.files(full.names = T, recursive = T)
## Then zip from this new wd (the default root)
## Can't just set the root here, as the files need to be listed from the same wd
zip::zip(file.path("..", "EDH-7916.zip"),
         files = files,
         mode = "mirror")
## Reset working directory back to project folder
setwd(file.path("..", ".."))

## Delete unzipped example folder
unlink(example, recursive = T)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

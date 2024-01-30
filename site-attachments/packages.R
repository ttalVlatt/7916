## -----------------------------------------------------------------------------
##
##' [PROJ: 7916]
##' [FILE: Install Any Missing Required Packages]
##' [INIT: Jan 29th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

options(repos = "https://cloud.r-project.org/")

required_packs <- c(
  "tidyverse", "rmarkdown", "knitr", "quarto", "patchwork", "dbplyr", "RSQLite",
  "sf", "tidycensus", "tigris", "estimatr", "gitcreds", "fs", "zip",
  "doconv", "this.path", "tinytex"
)

installed_packs <- installed.packages()

needed_packs <- required_packs[!required_packs %in% installed_packs]
## h/t https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

for(i in needed_packs) {
  install.packages(i)
}

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
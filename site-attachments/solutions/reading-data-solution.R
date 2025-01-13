## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Reading Data Solution]
##' [INIT: January 12 2025]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(readxl) # This is the extra library you need (it's part of tidyverse, but not loaded by default)

## ---------------------------
##' [Q1]
## ---------------------------

data <- read_xlsx("data/r-class-family.xlsx")

## ---------------------------
##' [Q2]
## ---------------------------

data_ipeds <- read_csv("data/hd2023.csv")

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

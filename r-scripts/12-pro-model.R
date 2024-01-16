## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Modeling Basics]
##' [INIT: Jan 16th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)


df <- read_csv(file.path("data", "hsls-small.csv"))

df <- df |>
  select(x1sex, x1race, x1txmtscor, x1paredu, x1ses, x1poverty185) |>
  filter(! if_any(.cols = everything(),
                  .fns = ~ . %in% c(-8, -9))) |>
  mutate(across(.cols = ! c(x1txmtscor, x1ses),
                .fns = ~ as.factor(.)))


t.test(x1txmtscor ~ x1sex, data = df)

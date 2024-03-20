## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling I Solution]
##' [INIT: Jan 28 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

## note to matt

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

## ---------------------------
##' [Input]
## ---------------------------

df <- read_csv(file.path("data", "hsls-small.csv"))

## ---------------------------
##' [Q1]
## ---------------------------

## Part One
df |>
  filter(!x1txmtscor %in% c(-8, -9)) |>
  summarize(mean = mean(x1txmtscor))

## Part Two
math <- df |>
  filter(!x1txmtscor %in% c(-8, -9),
         x1sex != -9) |>
  group_by(x1sex) |>
  summarize(mean = mean(x1txmtscor))

## ---------------------------
##' [Q2]
## ---------------------------

df |>
  filter(x1poverty185 == 1,
         !x1famincome %in% c(-8,-9)) |>
  summarize(med_inc_cat = median(x1famincome))

print("Median income category in 2, which represents Family income > $15,000 and <= $35,000")

## ---------------------------
##' [Q3]
## ---------------------------

## Simplified
df |>
  filter(x4hscompstat %in% c(1,2)) |>
  count(x4hscompstat) |>
  mutate(perc = n / sum(n) * 100)

## Part One
df |>
  filter(x4hscompstat %in% c(1,2)) |>
  summarize(ged = sum(x4hscompstat == 2),
            total = sum(x4hscompstat %in% c(1,2)),
            perc = ged/total*100)

## Part Two
## Simplified
df |>
  filter(x4hscompstat %in% c(1,2)) |>
  group_by(x1region) |>
  count(x4hscompstat) |>
  mutate(perc = n / sum(n) * 100) |>
  filter(x4hscompstat == 1)

df |>
  filter(x4hscompstat %in% c(1,2)) |>
  group_by(x1region) |>
  summarize(ged = sum(x4hscompstat == 2),
            total = sum(x4hscompstat %in% c(1,2)),
            perc = ged/total*100)

## ---------------------------
##' [Q4]
## ---------------------------

## Part One
df |>
  filter(x4evratndclg != -8) |>
  count(x4evratndclg) |>
  mutate(perc = n / sum(n) * 100)


df |>
  filter(x4evratndclg != -8) |>
  summarize(college = sum(x4evratndclg == 1),
            total = sum(x4evratndclg %in% c(0, 1)),
            perc = college/total*100)

## Part Two
df |>
  filter(x4evratndclg != -8,
         !x1famincome %in% c(-8, -9)) |>
  mutate(below_35k = ifelse(x1famincome %in% c(1,2), 1, 0)) |>
  group_by(x1region, below_35k) |>
  count(x4evratndclg) |>
  mutate(perc = n / sum(n) * 100) |>
  filter(x4evratndclg == 1)



df |>
  filter(x4evratndclg != -8,
         !x1famincome %in% c(-8, -9)) |>
  mutate(below_35k = ifelse(x1famincome %in% c(1,2), 1, 0)) |>
  group_by(x1region, below_35k) |>
  summarize(college = sum(x4evratndclg == 1),
            total = sum(x4evratndclg %in% c(0, 1)),
            perc = college/total*100)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

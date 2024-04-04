## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling II Solution]
##' [INIT: Jan 31 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

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

df_q1 <- df |>
  filter(x1txmtscor != -8)

## Easier way

df_sum <- df_q1 |>
  group_by(x1region) |>
  summarize(reg_mean_test = mean(x1txmtscor))

df_q1 |>
  left_join(df_sum, by = "x1region") |>
  mutate(diff = x1txmtscor - reg_mean_test) |>
  group_by(x1region) |>
  summarize(reg_diffs = mean(diff))

## All in one

df_q1 |>
  group_by(x1region) |>
  summarize(reg_mean_test = mean(x1txmtscor)) |>
  ## output is summary, which we want to be the "right" or "y" of the join
  left_join(x = df_q1, by = "x1region") |>
  ## so we specify x = df (original) leaving y open for the piped summary
  mutate(diff = x1txmtscor - reg_mean_test) |>
  group_by(x1region) |>
  summarize(reg_diffs = mean(diff))

## ---------------------------
##' [Q2]
## ---------------------------

df_q2 <- df |>
  filter(x1txmtscor != -8,
         !x1famincome %in% c(-8, -9))

## Easier way

df_sum <- df_q2 |>
  group_by(x1region, x1famincome) |>
  summarize(reg_inc_mean_test = mean(x1txmtscor))

df_q2_easy <- df_q2 |>
  left_join(df_sum, by = c("x1region", "x1famincome"))

## All in one

df_q2_piped <- df_q2 |>
  group_by(x1region, x1famincome) |>
  summarize(reg_inc_mean_test = mean(x1txmtscor)) |>
  left_join(x = df_q2, by = c("x1region", "x1famincome"))

all.equal(df_q2_easy, df_q2_piped)

## ---------------------------
##' [Q3]
## ---------------------------

df_long <- df |>
  select(stu_id, x1paredexpct, x1stuedexpct, x4evratndclg) |>
  pivot_longer(cols = c(x1paredexpct, x1stuedexpct),
               names_to = "exp_type",
               values_to = "exp_value")


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

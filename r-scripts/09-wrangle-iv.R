## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Tidyverse Tricks & SQL]
##' [INIT: Jan 12th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(dbplyr)


df_18_pub <- read_csv(file.path("data", "ipeds-finance", "f1819_f1a_rv.csv"))
df_18_np <- read_csv(file.path("data", "ipeds-finance", "f1819_f2_rv.csv"))
df_18_fp <- read_csv(file.path("data", "ipeds-finance", "f1819_f3_rv.csv"))

df_18 <- bind_rows(df_18_pub, df_18_np, df_18_fp)

df_18 |>
  count(UNITID) |>
  filter(n > 1)


df_18 <- df_18 |>
  select(UNITID,
         F1C011, F1C021, F1C061,
         F2E011, F2E021, F2E051,
         F3E011, F3E02A1, F3E03B1)

print(df_18)


## Split back up into separate files
pub <- df_18 |> filter(!is.na(F1C011)) |>
  ## Rename the variable
  rename(inst_spend = F1C011) |>
  ## Drop the other variables
  select(UNITID, inst_spend)
np <- df_18 |> filter(!is.na(F2E011)) |>
  rename(inst_spend = F2E011) |>
  select(UNITID, inst_spend)
fp <- df_18 |> filter(!is.na(F3E011)) |>
  rename(inst_spend = F3E011) |>
  select(UNITID, inst_spend)
## Re-bind the colleges back up
rebind <- bind_rows(pub, np, fp)


coalesce <- df_18 |>
  mutate(inst_spend = coalesce(F1C011, F2E011, F3E011)) |>
  select(UNITID, inst_spend)

all.equal(rebind, coalesce)

df_18_clean <- df_18 |>
  mutate(inst_spend = coalesce(F1C011, F2E011, F3E011),
         rsch_spend = coalesce(F1C021, F2E021, F3E02A1),
         serv_spend = coalesce(F1C061, F2E051, F3E03B1)) |>
  select(UNITID, inst_spend, rsch_spend, serv_spend)

df_0_inst <- df_18_clean |> filter(inst_spend == 0)
df_0_rsch <- df_18_clean |> filter(rsch_spend == 0)
df_0_serv <- df_18_clean |> filter(serv_spend == 0)
df_0 <- bind_rows(df_0_inst, df_0_rsch, df_0_serv)

## Plus we end up with duplicates
df_0 |>
  count(UNITID) |>
  filter(n > 1)


df_0 <- df_18_clean |>
  filter(if_any(everything(), ~ . == 0)) ## h/t https://stackoverflow.com/questions/69585261/dplyr-if-any-and-numeric-filtering

print(df_0)

df_0 |>
  select(-UNITID) |>
  count(across(everything(), ~ . == 0))

df_18_clean |>
  mutate(highest_cat = case_when(inst_spend > rsch_spend & rsch_spend > serv_spend ~ "inst_rsch_serv",
                                 inst_spend > serv_spend & serv_spend > rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend > inst_spend & inst_spend > serv_spend ~ "rsch_inst_serv",
                                 rsch_spend > serv_spend & serv_spend > inst_spend ~ "rsch_serv_inst",
                                 serv_spend > inst_spend & inst_spend > rsch_spend ~ "serv_inst_rsch",
                                 serv_spend > rsch_spend & rsch_spend > inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ "You missed a condition Matt")) |>
  count(highest_cat)

df_18_clean |>
  mutate(highest_cat = case_when(inst_spend >= rsch_spend & rsch_spend >= serv_spend ~ "inst_rsch_serv",
                                 inst_spend >= serv_spend & serv_spend >= rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend >= inst_spend & inst_spend >= serv_spend ~ "rsch_inst_serv",
                                 rsch_spend >= serv_spend & serv_spend >= inst_spend ~ "rsch_serv_inst",
                                 serv_spend >= inst_spend & inst_spend >= rsch_spend ~ "serv_inst_rsch",
                                 serv_spend >= rsch_spend & rsch_spend >= inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ "You missed a condition Matt")) |>
  count(highest_cat)

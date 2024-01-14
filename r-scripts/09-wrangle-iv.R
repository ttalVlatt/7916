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
library(RSQLite)


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

df <- read_csv(file.path("data", "hsls-small.csv"))

df |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answereed)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp))


microsoft_access <- simulate_access()

db <- memdb_frame(df)



db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answereed)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp)) |>
  show_query()



df_1 <- memdb_frame(read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1980.csv")))
df_2 <- memdb_frame(read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1981.csv")))
df_3 <- memdb_frame(read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1982.csv")))

## change from bind_rows() to union_all() to play nice with dbplyr
## union_all only takes 2 frames at a time, so second pipe needed
union_all(df_1, df_2) |> union_all(df_3) |> show_query()



df_2 <- read_csv(file.path("data", "sch-test", "all-schools.csv"))

db_2 <- memdb_frame(df_2)



## Actually create `df_sum`
df_sum <- db_2 |>
    ## grouping by year so average within each year
    group_by(year) |>
    ## get mean(<score>) for each test
    summarize(math_m = mean(math),
              read_m = mean(read),
              science_m = mean(science)) |>
  show_query()


df_joined <- db_2 |>
    ## pipe into left_join to join with df_sum using "year" as key
    left_join(df_sum, by = "year") |>
  show_query()

df_long <- db_2 |>
    ## cols: current test columns
    ## names_to: where "math", "read", and "science" will go
    ## values_to: where the values in cols will go
    pivot_longer(cols = c("math","read","science"),
                 names_to = "test",
                 values_to = "score") |>
  show_query()

## show
df_long

df_wide <- df_long |>
    ## names_from: values in this column will become new column names
    ## values_from: values in this column will become values in new cols
    pivot_wider(names_from = "test",
                values_from = "score") |>
  show_query()


df_3 <- read_csv(file.path("data", "sch-test", "all-schools-wide.csv"))

db_3 <- memdb_frame(df_3)

df_long <- db_3 |>
    ## NB: contains() looks for "19" in name: if there, it adds it to cols
    pivot_longer(cols = contains("19"),
                 names_to = "test_year",
                 values_to = "score") |>
  show_query()

print(df_long)

## Note: change from DWII,dbplyr can't translate separate, or any stringr commands, so we have to 
## more sophisticated with our pivot_longer
df_long_fix <- db_3 |>
    ## NB: contains() looks for "19" in name: if there, it adds it to cols
    pivot_longer(cols = contains("19"),
                 names_to = c("test", "year"),
                 names_sep = "_",
                 values_to = "score") |>
  show_query()

## show
df_long_fix


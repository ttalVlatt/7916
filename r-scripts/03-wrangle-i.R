################################################################################
##
## <PROJ> EDH7916: Data Wrangling I: Enter the {tidyverse}
## <FILE> dw_one.R 
## <INIT> 20 January 2020
## <AUTH> Benjamin Skinner (GitHub/Twitter: @btskinner)
## <MODI> Matt Capaldi on 12 Jan 2024
##
################################################################################


## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## -----------------------------------------------------------------------------
## Wrangle data
## -----------------------------------------------------------------------------

## ---------------------------
## input
## ---------------------------

## data are CSV, so we use read_csv() from the readr library
df <- read_csv(file.path("data", "hsls-small.csv"))

## Without |>
select(df, x1txmtscor)

## With |>
df |> select(x1txmtscor)

## Without |>
filter(select(df, x1txmtscor), x1txmtscor > 50)

## With |>
df |> select(x1txmtscor) |> filter(x1txmtscor > 50)

## Without |>
mutate(filter(select(df, x1txmtscor), x1txmtscor > 50), square_root = sqrt(x1txmtscor))

## With |>
df |> select(x1txmtscor) |> filter(x1txmtscor > 50) |> mutate(square_root = sqrt(x1txmtscor))

df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

## Without the |>, we could technically break it down step by step assigning
## after each step, but again it's confusing 
temp <- select(df, x1txmtscor)
temp <- filter(temp, x1txmtscor > 50)
temp <- mutate(temp, square_root = sqrt(x1txmtscor))
temp

df_backward_pass <- df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor)) ->
  df_forward_pass

identical(df_backward_pass, df_forward_pass)

df |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

df_small <- df |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

df |> mutate(square_root = sqrt(x1txmtscor))

## -----------------
## mutate
## -----------------

## see unique values for student expectation
df_small |> count(x1stuedexpct)

## see unique values for parental expectation
df_small |> count(x1paredexpct)

df_small <- df_small |>
  mutate(student_exp = ifelse(x1stuedexpct == -8, NA, x1stuedexpct))

print(df_small, n = 26)

df_small <- df_small |>
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct))

print(df_small, n = 26)

df_small |> count(student_exp) 
df_small |> count(parent_exp)

df_small <- df_small |>
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp))

print(df_small, n = 26)

mean(c(5, 6, 4, NA))

mean(c(5, 6, 4, NA), na.rm = T)

df_small <- df_small |>
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp))

print(df_small, n = 26)

## -----------------
## filter
## -----------------

## get summary of our new variable
df_small |> count(high_exp)

## filter out missing values
df_small_cut <- df_small |> filter(!is.na(high_exp))

df_small_cut |> count(high_exp)

## does the original # of rows - current # or rows == NA in count?
nrow(df_small) - nrow(df_small_cut)

## -----------------
## summarize
## -----------------

## get average (without storing)
df_small_cut |> summarize(mean(high_exp))

df_small_cut |> summarize(mean_exp = mean(high_exp))

## get grouped average
df_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp))

## ---------------------------
## output
## ---------------------------

## write with useful name

df_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp)) |>
  write_csv(file.path("data", "region-expects.csv"))


## ---------------------------
## appendix
## ---------------------------

## Let's redo the analysis above, but with a fully chained set of
## functions.

## start with original df
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
  summarize(mean_exp = mean(high_exp)) |>
  ## Write that to a .csv file
  write_csv(file.path("data", "region-expects-chain.csv"))
  


non_chain <- read_csv(file.path("data", "region-expects.csv"))
chain <- read_csv(file.path("data", "region-expects-chain.csv"))
 
all.equal(non_chain, chain)
      

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------

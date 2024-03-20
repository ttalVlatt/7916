## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling III Solution]
##' [INIT: March 18 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(lubridate)

## ---------------------------
##' [Input]
## ---------------------------

df_hd <- read_csv("data/hd2007.csv")
df_mission <- read_csv("data/ic2007mission.csv")


df <- df_hd |>
  left_join(df_mission, by = c("UNITID" = "unitid")) # Could also just rename column to lower case

## ---------------------------
##' [Q1]
## ---------------------------

df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "(^|\\s)dr(a)?(,|\\.|\\s)")) |> # keep anything that after either the start (^) or a " " has "dr" then maybe "a" (for Spanish dra) followed by either "," "." or " " 
  # keep only if there's "dr" maybe an "a" then either 
  group_by(chfnm_lower, STABBR) |> # group by unique chief admin names (due to branch campuses) also group by state, to minimize chance of two different people with the same name being counted as one. Is there a better way to check for branch campuses?
  slice(1) |> # slice the one row of these (to remove duplicates)
  ungroup() |> # remove the grouping as we only wanted slice() to be grouped
  select(chfnm_lower) |> # keep only the chief admin name
  count(chfnm_lower) |> # get counts of the names (should be a column of ones)
  summarize(sum(n)) # count up the number ones

df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "(^|\\s)dr(a)?(,|\\.|\\s)")) |> # keep anything that after either the start (^) or a " " has "dr" then maybe "a" (for Spanish dra) followed by either "," "." or " " 
  # keep only if there's "dr" maybe an "a" then either 
  #group_by(chfnm_lower, STABBR) |> # group by unique chief admin names (due to branch campuses) also group by state, to minimize chance of two different people with the same name being counted as one. Is there a better way to check for branch campuses?
  #slice(1) |> # slice the one row of these (to remove duplicates)
  #ungroup() |> # remove the grouping as we only wanted slice() to be grouped
  distinct(chfnm_lower) |>
  select(chfnm_lower) |> # keep only the chief admin name
  count(chfnm_lower) |> # get counts of the names (should be a column of ones)
  summarize(sum(n)) # count up the number ones

## Below is how I compared our in class regex with Ben's and my final answer

inclass_names <- df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "dr\\.?\\s")) |> # keep anything "dr" maybe a "." then " "
  pull(chfnm_lower)

bens_names <- df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "^dr\\.?[^ew]")) |> # keep anything that starts (^) "dr" maybe a "." then anything but "ew" (to exclude drew or andrew)
  pull(chfnm_lower)

bens_names[!bens_names %in% inclass_names]

matts_names <- df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "(^|\\s)dr(a)?(,|\\.|\\s)")) |> # keep anything that after either the start (^) or a " " has "dr" then maybe "a" (for Spanish dra) followed by either "," "." or " "
  pull(chfnm_lower)

matts_names[!matts_names %in% bens_names]

## The differences between Ben's and my answer get to an important
## research point, technically Ben's answer is what was asked, but in many situations
## you were given that question, they would want "rev. dr" and "rabbi dr" included

## ---------------------------
##' [Q2]
## ---------------------------

df |>
  mutate(chfnm_lower = str_to_lower(CHFNM)) |> # lower case the name
  filter(str_detect(chfnm_lower, "ph\\.?\\s?d\\.?\\s?$")) |> # keep only if there's "dr" maybe a ".", then a space
  group_by(chfnm_lower, STABBR) |> # group by unique chief admin names (due to branch campuses) also group by state to minimize chance of two different people with the same name (ideally, you'd be more sophisticated and check for branch campuses directly)
  slice(1) |> # slice the one row of these (to remove duplicates)
  ungroup() |> # remove the grouping as we only wanted slice() to be grouped
  count(chfnm_lower) |> # get counts of the names (should be a column of ones)
  summarize(sum(n)) # count up the number ones

## ---------------------------
##' [Q3]
## ---------------------------

##'[i]

df |>
  mutate(mission_lower = str_to_lower(mission), # lower case the mission
         instnm_lower = str_to_lower(INSTNM)) |> # lower case the institution name
  filter(str_detect(mission_lower, instnm_lower)) |> # search for the name in the mission
  count(mission_lower) |> # get counts of the names (should be a column of ones)
  summarize(sum(n)) # count up the number ones
  
##'[ii]

df |>
  mutate(mission_lower = str_to_lower(mission)) |> # lower case the mission
  filter(str_detect(mission_lower, "civic")) |> # look for the word "civic
  count(mission_lower) |> # get counts of the names (should be a column of ones)
  summarize(sum(n)) # count up the number ones

##'[iii]

df |>
  mutate(mission_lower = str_to_lower(mission)) |> # lower case mission
  filter(str_detect(mission_lower, "future")) |> # look for the word "future"
  group_by(STABBR) |> # before we count this time, group by state
  count(mission_lower) |> # get counts of the names (should be a column of ones)
  summarize(n = sum(n)) |> # count up the number ones
  arrange(desc(n)) |> # arrange in descending order of the count
  slice_head(n = 3) # keep the top three rows

##'[iv]

df |>
  mutate(mission_lower = str_to_lower(mission)) |> # lower case the mission
  filter(str_detect(mission_lower, "skill")) |> # look for the word skill
  group_by(CONTROL) |> # before we count, group by control
  count(mission_lower) |> # get counts of the names (should be a column of ones)
  summarize(n = sum(n)) |> # count up the number ones
  arrange(desc(n)) # arrange in descending order

## ---------------------------
##' [Q4]
## ---------------------------

df_close <- df |>
  select(UNITID, INSTNM, CLOSEDAT) |> # Keep only the columns we need
  filter(CLOSEDAT != "-2") |> # Get rid of any colleges that haven't closed
  mutate(clean_date = parse_date_time(CLOSEDAT, ## Create clean_date by turning CLOSEDAT into a date_time object
                                      orders = c("mdy", "my"))) ## Try month/day/year format first, then just month/year (assumes 1st of month)

## Print out the 7 that "failed to parse"
df_close |>
  filter(is.na(clean_date))

## After inspection, we can't approximate the 7 that failed to parse accurately, so drop them
df_close <- df_close |>
  drop_na(clean_date)

## Now the clean_date is a lubridate object, we can just treat it like a numeric variable

##'[i]
##' Option One: Use the date written
df_close |>
  mutate(time_from_today = parse_date_time("Feb 1 2020", "mdy") - clean_date) |> # same logic as above, create a date_time object for Feb 1 2020 (format is month/day/year) get the difference from clean_date 
  slice_max(time_from_today) # slice off the row with the most days from today

## Option Two: Use the date/time right now
df_close |>
  mutate(time_from_today = now() - clean_date) |> # now() just gets date/time when run
  slice_max(time_from_today) # slice off the row with the most days from today

##'[ii]
df_close |>
  summarize(answer = max(clean_date) - min(clean_date)) # We can also just use max() and min() like any other variable


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
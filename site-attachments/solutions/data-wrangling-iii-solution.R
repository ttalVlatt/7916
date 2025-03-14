## -----------------------------------------------------------------------------
##
##' [PROJ: EDH7916-Assignment-07]
##' [FILE: Assignment draft]
##' [INIT: 03/10]
##' [AUTH: Jue Wu] 
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

data_hd <- read_csv("data/hd2007.csv") |> 
  rename_all(tolower) 

data_mission <- read_csv("data/ic2007mission.csv")

## ---------------------------
##' [Prep]
## ---------------------------

# 1a
data <- left_join(data_hd, data_mission, by = "unitid")

## ---------------------------
##' [Analysis]
## ---------------------------

# 2a
data |> 
  count(chfnm) |> 
  filter(str_detect(chfnm, "Dr\\.?\\s")) |> # need \\s to exclude Drew, Drahus, Draper, etc 
  arrange(desc(n))

# Dr matches the literal characters "Dr"
# \\.? matches an optional period (the first backslash escapes the second backslash, and the second backslash escapes the period, making it a literal period character; the question mark makes the period optional)
# \\s matches any whitespace character (space, tab, newline, etc.)


# 2b
data |> 
  count(chfnm) |> 
  filter(str_detect(chfnm, "[Pp]\\.?[Hh]\\.?[Dd]\\.?")) 



# 3a
# v1
data |> 
  filter(str_detect(mission, instnm)) |> 
  count(instnm) 

# v2
data |> 
  filter(str_detect(mission, regex(instnm, ignore_case = TRUE))) |> 
  count(instnm)

# v3
data |> 
  mutate(mission_lower = str_to_lower(mission)) |>
  filter(str_detect(mission_lower, str_to_lower(instnm))) |> 
  count(instnm)

# 3b
# v1
data |> 
  filter(str_detect(mission, "civic")) |> 
  count(instnm)

# v2
data |> 
  filter(str_detect(mission, "(?i)civic")) |> 
  count(instnm)

# 3c
# v1
data |> 
  filter(str_detect(mission, "future")) |> 
  group_by(stabbr) |> 
  count() |> 
  arrange(desc(n))

# v2
data |> 
  filter(str_detect(mission, "(?i)future")) |> 
  group_by(stabbr) |> 
  count() |> 
  arrange(desc(n))

# 3d
# v1
data |> 
  filter(str_detect(mission, "skill")) |> 
  group_by(control) |> 
  count() |> 
  arrange(desc(n))

# v2
data |> 
  filter(str_detect(mission, "(?i)skill")) |>  # deal with case-insensitive
  group_by(control) |> 
  count() |> 
  arrange(desc(n))

# v3
data |> 
  mutate(mission_lower = str_to_lower(mission)) |>
  filter(str_detect(mission_lower, "skill")) |> 
  group_by(control) |> 
  count() |> 
  arrange(desc(n))

# 4a
close_data <- data |> 
  filter(closedat != -2) |> 
  mutate(clean_date = parse_date_time(closedat, orders = c("mdy", "my")))

close_data |> 
  filter(is.na(clean_date)) |> 
  select(closedat)

close_data <- close_data |> 
  drop_na(clean_date)

close_data |> 
  filter(clean_date == min(clean_date)) |> 
  select(instnm, clean_date)

# 4b
# v1
march_2025 <- parse_date_time("03-01-2025", orders = "mdy")

close_data |> 
  filter(clean_date == min(clean_date)) |> 
  mutate(interval = interval(min(clean_date), march_2025) |> time_length("month")) |> 
  select(instnm, clean_date, interval)

# v2
min_date <- close_data |> 
  filter(clean_date == min(clean_date)) |> 
  pull(clean_date)

interval(min_date, march_2025) |> time_length("month")

# 4c
close_data |> 
  summarize(diff = max(clean_date) - min(clean_date))

## ---------------------------
##' [Output]
## ---------------------------

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

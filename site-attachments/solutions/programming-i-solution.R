## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Functions & Loops Solution]
##' [INIT: March 18 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##' [EDIT: Jue Wu]
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)


## ---------------------------
##' [Q1]
## ---------------------------


# 1a
files <- list.files("data/sch-test/by-school", 
                    full.names = TRUE)

for(i in files) {
  school <- str_extract(i, "niagara|bend|east|spot")
  year <- str_extract(i, "\\d+")
  name <- paste0("data_", school, year)
  file <- read_csv(i)
  assign(name, file)
}

# 1b
files_niagara_bend <- list.files("data/sch-test/by-school",
                                 full.names = T,
                                 pattern = "niagara|bend")

for(i in files_niagara_bend) {
  school <- str_extract(i, "niagara|bend")
  year <- str_extract(i, "\\d+")
  name <- paste0("data_", school, year)
  file <- read_csv(i)
  assign(name, file)
}

# 1c
files_niagara_bend <- list.files("data/sch-test/by-school",
                                 full.names = T,
                                 pattern = "niagara|bend")

for(i in files_niagara_bend) {
  school <- str_extract(i, "niagara|bend")
  year <- str_extract(i, "\\d+")
  name <- paste0("data_", school, year)
  file <- read_csv(i) |> mutate(file_path = i)
  assign(name, file)
}

# 1d
files_niagara_bend <- list.files("data/sch-test/by-school",
                                 full.names = T,
                                 pattern = "niagara|bend")

data_niagara_bend_bind <- tibble()

for(i in files_niagara_bend) {
  file <- read_csv(i) |> 
    mutate(file_path = i)
  data_niagara_bend_bind <- bind_rows(data_niagara_bend_bind, file)
}

## ---------------------------
##' [Q2]
## ---------------------------

# 2a
data <- haven::read_dta("data/hsls-small.dta")

# 2b
college <- function(id) {
  
  student <- data |> filter(stu_id == id) 
  college <- student |> pull(x4evratndclg) # pull out if the student attend college
  parent_exp <- student |> pull(x1paredexpct) # pull out the parent student expectation
  
  if(is.na(college)) { # if whether they went to college is missing
    paste("We do not know if this student went to college") # print this message
  } else if(college == 1) { # if they went to college
    paste("This student went to college") # paste this message
  } else {
    if(is.na(parent_exp)|parent_exp == 11) {
      paste("This student did not go to college, and their parental expectation is unknown")
    } else if(parent_exp >= 5) {
      paste("This student did not go to college, but their parents expected them to")
    } else if(parent_exp < 5) {
      paste("This student did not go to college, and their parents did not expect them to")
    }
  }
} 

# test
college(10031)

# Optional
college <- function(id) {
  
  student <- data |> filter(stu_id == id) 
  college <- student |> pull(x4evratndclg) # pull out if the student attend college
  parent_exp <- student |> pull(x1paredexpct) # pull out the parent student expectation
  months_gap <- student |> pull(x4hs2psmos) # pull out the months gap
  
  if(is.na(college)) { # if whether they went to college is missing
    paste("We do not know if this student went to college") # print this message
  } else if(college == 1) { 
    if(is.na(months_gap)) {
      paste("This student went to college, but we do not know how long the gap was")
    } else{
      paste("This student went to college, and they had", months_gap, "months between high school and going to college")
    }
  } else {
    if(is.na(parent_exp)|parent_exp == 11) {
      paste("This student did not go to college, and their parental expectation is unknown")
    } else if(parent_exp >= 5) {
      paste("This student did not go to college, but their parents expected them to")
    } else if(parent_exp < 5) {
      paste("This student did not go to college, and their parents did not expect them to")
    }
  }
} 

# test
college(10007)

# Super Optional
college <- function(id) {
  
  student <- data |> filter(stu_id == id) 
  college <- student |> pull(x4evratndclg) # pull out if the student attend college
  parent_exp <- student |> pull(x1paredexpct) # pull out the parent student expectation
  months_gap <- student |> pull(x4hs2psmos) # pull out the months gap
  avg_delay <- mean(data$x4hs2psmos, na.rm = TRUE) # calculate the average delay
  
  if(is.na(college)) { # if whether they went to college is missing
    paste("We do not know if this student went to college") # print this message
  } else if(college == 1) { 
    if(is.na(months_gap)) {
      paste("This student went to college, but we do not know how long the gap was")
    } else{
      if(months_gap == avg_delay) {
        paste("This student went to college, and their gap was the average delay between high school and going to college")
      } else if(months_gap < avg_delay) {
        paste("This student went to college, and their gap was shorter than the average delay")
      } else if(months_gap > avg_delay) {
        paste("This student went to college, and their gap was longer than the average delay")
      }
    }
  } else {
    if(is.na(parent_exp)|parent_exp == 11) {
      paste("This student did not go to college, and their parental expectation is unknown")
    } else if(parent_exp >= 5) {
      paste("This student did not go to college, but their parents expected them to")
    } else if(parent_exp < 5) {
      paste("This student did not go to college, and their parents did not expect them to")
    }
  }
} 

# test
college(10007)

## Test it using a for loop
test_ids <- data |> slice_head(n = 50) |> pull(stu_id)
for(i in test_ids) { print(college(i)) }


##'[Matt's Solution]
##'[Without the optional part]

id <- 10007 # Tip: if you're debugging a function or loop, manually assign something to input values (for a function) or i (for a loop) and then you can test it

did_they_go <- function(id) {
  
  student <- df |> filter(stu_id == id) # pull out the student id (so we can directly use the value)
  college <- student |> pull(x4evratndclg) # pull out if the student attend college
  
  parent <- student |> pull(x1paredexpct) # pull out the parent student expectation
  expect <- if(is.na(parent)|parent == 11) { # if the parent expectation is either NA or 11
    "and we do not know if their parents wanted them to" # assign this text to expect
  } else if(parent >= 5) { # otherwise if the parent expectation is 5 or above
    "and their parents expected them to" # assign this text to expect
  } else if(parent < 5) { # otherwise if the parent expectation is less than 5
    "and their parents did not expect them to" # assign this text to expect
  }
  
  if(is.na(college)) { # if whether they went to college is missing
    paste("We do not know if this student went to college") # print this message
  } else if(college == 1) { # if they went to college
    paste("This student went to college") # paste this message
  } else if(college == 0) { # if they did not go to college
    paste("This student never went to college", expect) # paste this message
    
  }
} 


## Test it using a for loop
test_ids <- df |> slice_head(n = 50) |> pull(stu_id)
for(i in test_ids) { print(did_they_go(i)) }


##'[With the optional part]

did_they_go <- function(id) {
  
  student <- df |> filter(stu_id == id) # pull out the student id (so we can directly use the value)
  college <- student |> pull(x4evratndclg) # pull out if the student attend college
  
  parent <- student |> pull(x1paredexpct) # pull out the parent student expectation
  expect <- if(is.na(parent)|parent == 11) { # if the parent expectation is either NA or 11
    "and we do not know if their parents wanted them to" # assign this text to expect
  } else if(parent >= 5) { # otherwise if the parent expectation is 5 or above
    "and their parents expected them to" # assign this text to expect
  } else if(parent < 5) { # otherwise if the parent expectation is less than 5
    "and their parents did not expect them to" # assign this text to expect
  }
  
  ## NEW PART
  median_delay <- df |> summarize(median = median(x4hs2psmos, na.rm = T)) |> pull(median) # summarize the median completion and pull the value out (notice we start with df not student)
  delay <- student |> pull(x4hs2psmos) # pull out the students months delay
  difference <- delay - median_delay # calculate the difference between the median delay and the students delay
  delay_statement <- if(is.na(delay)) { # if the students delay was NA (note, it doesn't matter if they didn't go to college, as we only use this if they went)
    "and we do not know how long they delayed college" # paste this message
  } else if(delay == 0) { # if the students didn't delay going to college
    "and they did not delay attending college at all" # paste this message
  } else if(difference == 0) { # if the students delay was the median
    paste("and they delayed college by", delay, "months which is the average amount of time") # paste this, using the value delay from above
  } else if(difference < 0) { # if the students delay was below the median
    paste("and they delayed college by", delay, "months which is", abs(difference), "months less than the average") # paste this, using delay and abs(difference) which is the absolute value of the difference (i.e., the value regardless of positive vs negative)
  } else if(difference > 0) { # if the students delay was above the median
    paste("and they delayed college by", delay, "months which is", abs(difference), "months above than the average") # paste this, using delay and abs(difference) which is the absolute value of the difference (i.e., the value regardless of positive vs negative)
  }
  
  if(is.na(college)) { # if whether they went to college is missing
    paste("We do not know if this student went to college") # print this message
  } else if(college == 1) { # if they went to college
    paste("This student went to college", delay_statement) # paste this message NEW added delay_statement
  } else if(college == 0) { # if they did not go to college
    paste("This student never went to college", expect) # paste this message
    
  }
} 


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
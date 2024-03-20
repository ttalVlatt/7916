## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Functions & Loops Solution]
##' [INIT: March 18 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
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

files_to_read <- list.files("data/sch-test/by-school", # look in this folder
                            full.names = TRUE, # we want to keep the full path, not just the file names
                            pattern = "bend|niagara") # only list files that contain either "bend" or "niagara" 

data <- tibble() # create a blank tibble to store out data in

for(i in files_to_read) { # each loop through i becomes an item from the list of file path we created above)
  
  temp_data <- read_csv(i) |> # read in the file i 
    mutate(relative_path = i) # make a new variable relative_path that stores i (remember i is the path to the file, not the file itself)
  
  data <- bind_rows(data, temp_data) # bind data and the temp_data we just read in

}

## ---------------------------
##' [Q2]
## ---------------------------

df <- haven::read_dta("data/hsls-small.dta")

## ---------------------------
##' [Q3]
## ---------------------------

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

## Test it using a for loop
test_ids <- df |> slice_head(n = 50) |> pull(stu_id)
for(i in test_ids) { print(did_they_go(i)) }

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##' [PROJ: Project Title]
##' [FILE: Script Purpose]
##' [INIT: Date]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(haven)
library(quarto)

## ---------------------------
##' [Input]
## ---------------------------

# df_hd <- read_dta("hd2022.dta")
# df_gr <- read_dta("gr2022_pell_ssl.dta")
# 
# df <- left_join(df_hd, df_gr, by = "unitid") |>
#   filter(psgrtype %in% c(1,4),
#          fips == 12) |>
#   select(unitid, instnm, control, pgadjct, pgcmtot) |>
#   mutate(pell_gr_perc = pgcmtot/pgadjct*100) |>
#   drop_na() |>
#   write_dta("df.dta")

df <- read_dta("df.dta")

## ---------------------------
##' [Optional: Make data files for each school]
## ---------------------------

dir.create("college-data")

colleges <- df |> pull(instnm)

for(i in colleges) {
  
  df |>
    filter(instnm == i) |>
    write_dta(paste0("college-data/", i, ".dta"))
  
}

## ---------------------------
##' [Make Reports for Each School]
## ---------------------------

colleges_cut <- colleges[c(38, 39, 68, 91)]

for(i in colleges_cut) {
  
  quarto_render("Template.qmd",
                output_file = paste0(i, ".pdf"), 
                execute_params=list(name = i))
  
}


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
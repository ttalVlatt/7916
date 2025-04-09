## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Modeling Basics Solution]
##' [INIT: January 12 2025]
##' [AUTH: Jue Wu]
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(stargazer)
library(gtsummary)

## ---------------------------
##' [Q1]
## ---------------------------
data <- read_csv("data/hsls-small.csv") |> 
  select(stu_id, x1sex, x1txmtscor, x1paredu, x1poverty185, x4evratndclg, x1famincome)

data <- data |>
  filter(! if_any(.cols = everything(),
                  .fns = ~ . %in% c(-8, -9)))

data <- data |>
  mutate(across(.cols = c(stu_id, x1sex, x1paredu, x1poverty185, x1famincome),
                .fns = ~ factor(.)))

# 1a
t.test(x1txmtscor ~ x1poverty185, data = data)

# 1b
ggplot(data, aes(x = x1poverty185, y = x1txmtscor, fill = x1poverty185)) +
  geom_boxplot() +
  labs(x = "Poverty Status", 
       y = "Math Score", 
       fill = "Poverty Level") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Below 185%", "Above 185%"))

## ---------------------------
##' [Q2]
## ---------------------------

# 2a
logit_reg <- glm(x4evratndclg ~ x1txmtscor + x1sex + x1famincome + x1paredu, data = data)
summary(logit_reg)

# 2b
# stargazer
stargazer(logit_reg, type = "html")

# gtsummary
tbl_regression(logit_reg,
               label = list(x1txmtscor ~ "Math Score",
                            x1sex ~ "Sex",
                            x1famincome ~ "Family Income",
                            x1paredu ~ "Parental Education")) |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) |>
  modify_column_unhide(std.error)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

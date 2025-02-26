library(tidyverse)
library(haven)

data <- read_dta("data/hsls-small.dta")

ggplot(data) +
  geom_histogram(aes(x = x1txmtscor))



## ggsave("math-scores.png")

## install.packages("knitr")

data <- data |> 
  drop_na(x1txmtscor)

data |>
  summarize(mean(x1txmtscor))

library(knitr)

data |>
  summarize(mean(x1txmtscor)) |>
  kable()

data |>
  summarize(mean(x1txmtscor)) |>
  kable(col.names = c("Mean of Math Score"),
        digits = 2)

data |> 
  group_by(as_factor(x1region)) |>
  summarize(mean = mean(x1txmtscor),
            median = median(x1txmtscor),
            min = min(x1txmtscor),
            max = max(x1txmtscor)) |>
  kable(col.names = c("Region", "Mean", "Median", "Min", "Max"),
        digits = 2)





## source("lesson-06-viz-ii.R")

patch

## save(list = c("my_object"),
##      file = "results.Rdata")

## if(file.exists("results.Rdata")) {
##   load("results.Rdata")
## } else {
##   source("a-really-long-script.R")
## }

## install.packages("gtsummary")
## library(gtsummary)

library(gtsummary)

data |>
  select(x1txmtscor, x1region) |>
  tbl_summary()

data |>
  select(x1txmtscor, x1region) |>
  tbl_summary(type = all_continuous() ~ "continuous2",
              statistic = c(all_continuous() ~ c("{mean}",
                                                 "{sd}",
                                                 "{min} to {max}")))

data |>
  select(x1txmtscor, x1region) |>
  tbl_summary(type = all_continuous() ~ "continuous2",
              statistic = c(all_continuous() ~ c("{mean}",
                                                 "{sd}",
                                                 "{min} to {max}"))) |>
  as_kable()

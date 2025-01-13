## ## You should have these packages already, we installed them our first lesson
## ## but if you're not sure, run this code just in case
## install.packages(c("knitr", "rmarkdown", "quarto"))

library(tidyverse)
library(gtsummary)

df <- read_csv(file.path("data", "hsls-small.csv"))

ggplot(data = df) +
  geom_histogram(mapping = aes(x = x1txmtscor))





## source("06-viz-ii.R")

patch



print(df_long)

## Read in using .dta so we have nice labels
df <- haven::read_dta("data/hsls-small.dta") |>
  drop_na(x1txmtscor)

df |>
  summarize(mean(x1txmtscor))

library(knitr)

df |>
  summarize(mean(x1txmtscor)) |>
  kable()

df |>
  summarize(mean(x1txmtscor)) |>
  kable(col.names = c("Mean of Math Score"),
        digits = 2)

df |> 
  group_by(as_factor(x1region), as_factor(x1sex)) |>
  summarize(mean = mean(x1txmtscor),
            median = median(x1txmtscor),
            min = min(x1txmtscor),
            max = max(x1txmtscor)) |>
  kable(col.names = c("Region", "Sex", "Mean", "Median", "Min", "Max"),
        digits = 2,
        caption = "Math Score by Region and Sex")

df |>
  select(x1txmtscor, x1region) |>
  tbl_summary()

df |>
  select(x1txmtscor, x1region) |>
  tbl_summary(type = all_continuous() ~ "continuous2",
              statistic = c(all_continuous() ~ c("{mean}",
                                                 "{sd}",
                                                 "{min} to {max}")))

df |>
  select(x1txmtscor, x1region) |>
  tbl_summary(type = all_continuous() ~ "continuous2",
              statistic = c(all_continuous() ~ c("{mean}",
                                                 "{sd}",
                                                 "{min} to {max}"))) |>
  as_kable()

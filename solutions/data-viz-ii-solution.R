## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Viz II Solution: ggplot2 Challenge]
##' [INIT: 28 January 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

library(tidyverse)

df_plot <- read_csv(file.path("data", "ggplot2-challenge-data.csv"))

## ---------------------------
##' [Q1]
## ---------------------------

## Step one: pivot spending types to long
df_plot <- df_plot |>
  pivot_longer(cols = ends_with("_prop"),
               names_to = "spend_type",
               values_to = "spend_prop") |>
  ## Step two: make institution and spending type labeled factors
  mutate(CONTROL = CONTROL |> factor(levels = c(1,2,3),
                                     labels = c("Public", "Non-Profit", "For-Profit")),
         spend_type = spend_type |> factor(levels = c("inst_prop", "serv_prop", "rsch_prop"),
                                           labels = c("Instruction", "Student Services", "Research")))

## Step two: Plot
ggplot(df_plot) +
  geom_density(aes(x = spend_prop,
                   fill = spend_type),
               alpha = 0.5) +
  facet_wrap(~CONTROL,
             strip.position = "bottom",
             ncol = 2) +
  scale_fill_manual(values = c("#FA4616", "#0021A5", "#22884C")) +
  labs(title = "Ratio of Spending on Instruction, Student Services, and Research",
       subtitle = "Differences Across Public, Non-Profit, and For-Profit Colleges",
       x = "Proportion of Spending",
       fill = "Type of Spending") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.15),   ## h/t https://stackoverflow.com/questions/34022675/display-the-legend-inside-the-graph-when-wrapping-with-ggplot2
        legend.justification = c(0.8, 0.15),
        plot.subtitle = element_text(face = "italic"))

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

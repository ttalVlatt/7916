## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Viz I Solution(s)]
##' [INIT: February 10 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
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

df <- read_csv(file.path("data", "hsls-small.csv")) |>
  mutate(x1ses = ifelse(x1ses %in% c(-8, -9), NA, x1ses),
         x4evratndclg = ifelse(x4evratndclg %in% c(-8, -9), NA, x4evratndclg),
         x1txmtscor = ifelse(x1txmtscor %in% c(-8, -9), NA, x1txmtscor),
         x1poverty185 = ifelse(x1poverty185 %in% c(-8, -9), NA, x1poverty185),
         x1stuedexpct = ifelse(x1stuedexpct %in% c(-8, -9, 11), NA, x1stuedexpct),
         x1paredexpct = ifelse(x1paredexpct %in% c(-8, -9, 11), NA, x1paredexpct)) |>
  drop_na()

## ---------------------------
##' [Q1]
## ---------------------------

## Option One: Filled Histogram/Density Plot

ggplot(df) +
  geom_histogram(aes(x = x1ses,
                     fill = factor(x4evratndclg)))

## or...

ggplot(df) +
  geom_density(aes(x = x1ses,
                     fill = factor(x4evratndclg)))


## Option Two: Box Plot

ggplot(df) +
  geom_boxplot(aes(x = factor(x4evratndclg),
                   y = x1ses,
                   fill = factor(x4evratndclg)))


## Option Three: Jitter Scatter Plot

ggplot(df) +
  geom_point(aes(x = x1ses,
                  y = factor(x4evratndclg),
                  color = factor(x4evratndclg)),
              #position = "jitter",
              size = 0.5,
              alpha = 0.5)

ggplot(df) +
  geom_jitter(aes(x = x1ses,
                  y = factor(x4evratndclg),
                  color = factor(x4evratndclg)),
              size = 0.5,
              alpha = 0.5)

## ---------------------------
##' [Q2]
## ---------------------------

## Note: Just plotting each type separately was okay for the assignment, but it's better if we
## can get everything on one plot, let's look at a few ways of doing that

## Pivot the data long in expectation type

df_long <- df |>
  pivot_longer(cols = c(x1stuedexpct, x1paredexpct),
               names_to = "expect_type",
               values_to = "expect_value")

## Option One: Bar Chart (Treat as Categorical)

## This is skewed so heavily how many people graduated high school
ggplot(df_long) +
  geom_bar(aes(x = factor(x4hscompstat),
               fill = factor(expect_value)),
           color = "white") + 
  facet_wrap(~expect_type) ## We didn't get to this, so I didn't expect anyone to use it

## But we can add position = "fill" to change the bar chart to proportions, which
## makes it a bit better, still not great
ggplot(df_long) +
  geom_bar(aes(x = factor(x4hscompstat),
               fill = factor(expect_value)),
           color = "white",
           position = "fill") + ## https://stackoverflow.com/questions/46984296/proportion-with-ggplot-geom-bar
  facet_wrap(~expect_type)


## Option 2: Density Plot (Treat as Continuous)

ggplot(df_long) +
  geom_boxplot(aes(x = factor(x4hscompstat),
                   y = expect_value,
                   fill = factor(expect_type)))

## Or...

ggplot(df_long) +
  geom_density(aes(x = expect_value,
                      fill = factor(expect_type)),
               alpha = 0.5) +
  facet_wrap(~x4hscompstat)


## Option 3: Wrangle the Data First

df_sum <- df_long |>
  ## Make a new variable if expectation is bachelors or above
  mutate(bach_plus = ifelse(expect_value >= 6, 1, 0)) |>
  group_by(x4hscompstat, expect_type) |>
  ## Get the count of each value grouped by completion status and expect_type
  count(bach_plus)

ggplot(df_sum) +
  geom_col(aes(x = expect_type,
               y = n,
               fill = factor(bach_plus)),
           position = "fill", ## https://stackoverflow.com/questions/46984296/proportion-with-ggplot-geom-bar
           alpha = 0.8) +
  facet_wrap(~x4hscompstat)

## ---------------------------
##' [Q3]
## ---------------------------

## Option One: Scatter Plot with Line

ggplot(df,
       aes(x = x1ses,
           y = x1txmtscor)) +
  geom_point(size = 0.1) + ##'[Using size is a better way to avoid the blob effect]
  geom_smooth(method = "lm")


## Option Two: Heatmap with Line

ggplot(df,
       aes(x = x1ses,
           y = x1txmtscor)) +
  geom_bin_2d() +
  geom_smooth(method = "lm") +
  scale_fill_gradient(high = "navy", low = "skyblue")
## This also gets close to falling in the trap of "looking cool for the sake of it"


## Option Three: Use a Categorical SES Indicator

ggplot(df) +
  geom_boxplot(aes(x = factor(x1poverty185),
                   fill = factor(x1poverty185),
                   y = x1txmtscor))

## Or...

ggplot(df) +
  geom_jitter(aes(x = x1txmtscor,
                  y = factor(x1poverty185),
                  color = factor(x1poverty185)),
              size = 0.5,
              alpha = 0.5)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

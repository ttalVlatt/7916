## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Viz III Solution]
##' [INIT: 02 April 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

## ---------------------------
##' [Q1]
## ---------------------------

## Step one: pivot spending types to long
data_census <- get_acs(geography = "school district (unified)",
                       state = "TX",
                       year = 2021,
                       variables = "DP02_0153PE", # % Households with a computer
                       output = "wide",
                       geometry = TRUE)

## Step two: Change CRS of data_census
data_census <- data_census |>
  st_transform(crs = 4326)

## Step three: Plot the base map
plot_base_map <- ggplot() +
  geom_sf(data = data_census,
          aes(fill = DP02_0153PE),
          color = "black",
          size = 0.1) +
  labs(fill = str_wrap("% Homes with a Computer", 20)) +
  scale_fill_distiller(palette = "Oranges") +
  theme_void()

## Step three: Load IPEDS and transform coordinates using st_as_sf()
data_ipeds <- read_csv("data/mapping-api-data.csv") |>
  st_as_sf(coords = c("LONGITUD", "LATITUDE")) |>
  st_set_crs(4326)

## Step four: Add the points to plot_base_map
plot_base_map +
  geom_sf(data = data_ipeds |> filter(STABBR == "TX",
                                      LCOLELYN == 1), # Plot only colleges with purely digital libraries
          aes(), # No aes() needed as we don't need anything to change with data, we are only plotting schools we are interested in
          size = 9,
          shape = 4,
          stroke = 1.5,
          alpha = 0.8) +
  labs(title = str_wrap("Colleges with Purely-Digitial Libraries vs Household Computer Access", 40)) +
  theme_void()

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

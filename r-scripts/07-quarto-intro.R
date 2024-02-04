## You should have these packages already, we installed them our first lesson
## but if you're not sure, run this code just in case
install.packages(c("knitr", "rmarkdown", "quarto"))

library(tidyverse)

df <- read_csv(file.path("data", "hsls-small.csv"))

ggplot(data = df) +
  geom_histogram(mapping = aes(x = x1txmtscor))





## source("06-viz-ii.R")

patch

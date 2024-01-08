################################################################################
##
## <PROJ> EDH 7916
## <FILE> Setup II: Organizing and Reading Data 
## <INIT> 04 January 2024
## <AUTH> Matt Capaldi (modified from Ben Skinner)
##
################################################################################



library(tidyverse)

df_ipeds <- read_csv(file.path("data", "hd2007.csv"))


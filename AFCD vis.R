library(tidyverse)
library(readr)
library(rfishbase)

##Load AFCD
AFCD <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/AFCD_live.csv")
AFCD_columns = as.data.frame(names(AFCD))

AFCD_long = reshape2::melt(AFCD, id.vars = names(AFCD)[1:25], measure.vars = names(AFCD)[26:length(names(AFCD))]) %>% 
  drop_na(value)

x = maturity("Oreochromis niloticus")

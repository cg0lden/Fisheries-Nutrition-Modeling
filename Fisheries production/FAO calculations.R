library(readr)
library(tidyverse)

##Read and clean FAO production data
FAO_prod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/FAO_global_production.csv")

FAO_prod = reshape2::melt(FAO_prod, id.vars = names(FAO_prod)[1:19], 
                          measure.vars = names(FAO_prod)[20:ncol(FAO_prod)])

groups = FAO_prod %>% 
  filter(variable=="2014") %>% 
  drop_na(value) %>% 
  drop_na(FAOSTAT_group_of_commodities) %>% 
  group_by(FAOSTAT_group_of_commodities, Production_source) %>% 
  summarize(tonnes = sum(value))

ggplot(groups, aes(FAOSTAT_group_of_commodities, tonnes)) +   
  geom_bar(aes(fill = Production_source), position = "dodge", stat="identity") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 12),
        panel.grid = element_blank())
  

        
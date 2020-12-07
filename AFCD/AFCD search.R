#######AFCS search##########
library(tidyverse)
library(readr)
library(rfishbase)

##Load AFCD
AFCD <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/AFCD_live.csv")
AFCD_columns = as.data.frame(names(AFCD))

AFCD_long = reshape2::melt(AFCD, id.vars = names(AFCD)[1:25], measure.vars = names(AFCD)[26:length(names(AFCD))]) %>% 
  drop_na(value)

##Load fish lists
fish_list_Lao <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/fish_list_Lao.csv")
fish_list_P <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/fish_list_P.csv")

#Find english common names

fish_list = unique(fish_list_P$fish)[2:21]
x = common_to_sci(fish_list) %>% 
  filter(Language=="Tagalog")

AFCD_long$value = as.numeric(AFCD_long$value)

AFCD_grouped = AFCD_long %>% 
  filter(variable %in% c("Zinc", "Vitamin.B12", "Fatty.acids.total.n3.polyunsaturated")) %>% 
  group_by(species, Preparation, variable) %>% 
  summarise(value = mean(value))

x = left_join(x, AFCD_grouped, by=c("Species"="species"))

x = x %>% 
  drop_na(value)

x_2 = x %>% 
  group_by(ComName, Preparation, variable) %>% 
  summarise(value = mean(value))

write.csv(x_2, "df.csv", row.names = F)  

y = c("RAVAS","ROHU","BOMBAY","MACKEREL")

y = common_to_sci(y)

y = left_join(y, AFCD_grouped, by=c("Species"="species"))

y = y %>% 
  drop_na(value)

y_2 = y %>% 
  group_by(ComName, Preparation, variable) %>% 
  summarise(value = mean(value))


##First
df_1 = AFCD %>% 
  filter(agrepl("carp", Food.name.in.English, ignore.case = TRUE)
         #,
         #agrepl("Polynemidae", family, ignore.case = TRUE)
         ) %>% 
  select(Food.name.in.English,Preparation, Zinc, Vitamin.B12, Fatty.acids.total.n3.polyunsaturated) %>% 
  gather(key,value, Fatty.acids.total.n3.polyunsaturated, -Preparation, -Food.name.in.English) %>% 
  drop_na(value) %>% 
  group_by(Preparation, key) %>% 
  summarise(value = mean(value))

df_2 = AFCD_long %>% 
  filter(str_detect(variable, "Fatty.acid"),
         str_detect(variable, "n3"),
         agrepl("anchovie", Food.name.in.English, ignore.case = TRUE)
         #,
         #agrepl("freshwater", Food.name.in.English, ignore.case = TRUE)
  ) %>% 
  group_by(Preparation, variable) %>% 
  summarise(value = mean(value, na.rm = FALSE))

write.csv(df, "df.csv", row.names = F)         

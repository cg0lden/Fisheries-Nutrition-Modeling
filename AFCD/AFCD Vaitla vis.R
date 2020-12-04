library(tidyverse)
library(ggpubr)

###read nutrition data from Vaitla et al
load("~/MPA_Nutrition/data/Vaitla_etal_2018_nutrient_data.Rdata")

vaitla = nutrient_preds_long %>% 
  mutate(database = "Vaitla et al") %>% 
  select(nutrient, value_md_fill, database) %>% 
  rename(value = value_md_fill)

##Load AFCD
AFCD <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/AFCD_live.csv")

AFCD = AFCD %>% 
  mutate(vitA = if_else(is.na(Vitamin.A.retinol.activity.equivalent.RAE.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids),
                        Vitamin.A.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids,
                        Vitamin.A.retinol.activity.equivalent.RAE.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids),
         Protein = if_else(is.na(Protein.total.method.of.determination.unknown.or.variable),
                           Protein.total.calculated.from.total.nitrogen,
                           Protein.total.method.of.determination.unknown.or.variable))

AFCD_columns = as.data.frame(names(AFCD))

AFCD_long = reshape2::melt(AFCD, id.vars = names(AFCD)[1:25], measure.vars = names(AFCD)[26:length(names(AFCD))]) %>% 
  drop_na(value)

AFCD_long = AFCD_long %>% 
  filter(variable %in% c("Edible.portion.coefficient", 
                         "Iron.total", 
                         "Zinc",
                         "vitA",
                         "Vitamin.B12",
                         "Fatty.acids.total.n3.polyunsaturated",
                         "Protein")) %>% 
  mutate(variable = recode(variable, 
                           "Edible.portion.coefficient" = "edible",
                           "Iron.total" = "Iron", 
                           "vitA" = "Vitamin A",
                           "Vitamin.B12" = "Vitamin B12",
                           "Fatty.acids.total.n3.polyunsaturated" = "Omega-3 fatty acids"))

AFCD_long$value = as.numeric(AFCD_long$value) 

AFCD = AFCD_long %>% 
  mutate(database = "AFCD") %>% 
  select(variable, value, database) %>% 
  rename(nutrient = variable) %>% 
  filter(!nutrient=="edible")

dat = rbind(AFCD, vaitla)

nutrient_plot=function(nut){
  nutri = dat %>% 
    filter(nutrient == nut)
  ggplot(data=nutri)+
    geom_boxplot(aes(x=database, y=value)) +
    theme_classic()+
    labs(title=nut)
}

p1 = nutrient_plot(nut="Iron")
p2 = nutrient_plot(nut="Zinc")
p3 = nutrient_plot(nut="Omega-3 fatty acids")
p4 = nutrient_plot(nut="Vitamin B12")
p5 = nutrient_plot(nut="Protein")
p6 = nutrient_plot(nut="Vitamin A")

ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)

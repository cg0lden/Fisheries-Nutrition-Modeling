
library(tidyverse)
library(readr)
library(ggpubr)

fw_consump_combined_final <- read_csv("data/fw_consump_combined_final.csv")
MAR_spp_proportions_2010_2014_SAU <- read_csv("data/MAR_spp_proportions_2010_2014_SAU.csv")
AFCD <- read_csv("~/Fisheries-Nutrition-Modeling/AFCD/AFCD_live.csv")
mar_fw_spp_nutrients <- read_csv("data/mar_fw_spp_nutrients.csv") %>% 
  dplyr::select(common_name, nutrient, value)
limiting_nutrient <- read_csv("data/limiting_nutrient.csv")

##Clean freshwater disaggregation data
fw_dis = fw_consump_combined_final %>% 
  rename("iso3c" = "country_code",
         "common_name" = "common_name_simple",
         "production_sector" = "source",
         "tonnes" = "consumption_tons_mid") %>% 
  dplyr::select(iso3c, year, production_sector, common_name, tonnes) %>% 
  mutate(env = "freshwater",
         production_sector = recode(production_sector, 
                                    "Aquaculture (freshwater)" = "aquaculture",
                                    "Aquaculture (brackishwater)" = "aquaculture",
                                    "Capture production" = "capture",
                                    "Import" = "import",
                                    "Recreational" = "capture",
                                    "Freshwater (assumed capture)" = "capture",
                                    "Freshwater (aquaculture)" = "aquaculture"))

#Clean marine disaggregation
MAR_spp_proportions_2010_2014_SAU = MAR_spp_proportions_2010_2014_SAU %>% 
  mutate(env="marine") %>% 
  select(-spp_prop) %>% 
  filter(year=="2014")

all_dis = rbind(fw_dis, MAR_spp_proportions_2010_2014_SAU) %>% 
  group_by(iso3c, year, production_sector, common_name) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  ungroup()

all_dis = left_join(all_dis, mar_fw_spp_nutrients, by="common_name")

all_dis = left_join(all_dis, limiting_nutrient, by="iso3c")

write.csv(all_dis, "mar_fw_spp_nutrients_tonnes.csv", row.names = F)
##Clean AFCD
AFCD = AFCD %>% 
  mutate(vitA = if_else(is.na(Vitamin.A.retinol.activity.equivalent.RAE.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids),
                        Vitamin.A.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids,
                        Vitamin.A.retinol.activity.equivalent.RAE.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids),
         Protein = if_else(is.na(Protein.total.method.of.determination.unknown.or.variable),
                           Protein.total.calculated.from.total.nitrogen,
                           Protein.total.method.of.determination.unknown.or.variable))

AFCD_columns = as.data.frame(names(AFCD))

AFCD_long = reshape2::melt(AFCD, id.vars = names(AFCD)[1:29], measure.vars = names(AFCD)[30:length(names(AFCD))]) %>% 
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
AFCD_long$variable = as.character(AFCD_long$variable)



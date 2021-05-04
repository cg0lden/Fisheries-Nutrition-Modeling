##Assign species proportions to GND model output

library(readr)
library(tidyverse)
library(countrycode)

#Import datasets
QPMARBASE <- read_csv("data/QPMARBASE.csv") %>% mutate(scenario = "base", env = "MAR")
QPMARHIGH <- read_csv("data/QPMARHIGH.csv") %>% mutate(scenario = "high", env = "MAR")
QPINLHIGH <- read_csv("data/QPINLHIGH.csv") %>% mutate(scenario = "high", env = "INL")
QPINLBASE <- read_csv("data/QPINLBASE.csv") %>% mutate(scenario = "base", env = "INL")
FoodConsumptionBase <- read_csv("data/FoodConsBaseRev2.csv") %>% mutate(scenario = "base")
FoodConsumptionScenario <- read_csv("data/FoodConsScenRev2.csv") %>% mutate(scenario = "high")
BaseNutrients <- read_csv("data/NutrientsBaseRev2.csv") %>% mutate(scenario = "base")
ScenarioNutrients <- read_csv("data/NutrientsScenRev2.csv") %>% mutate(scenario = "high")
fw_consump_combined_final <- read_csv("data/consump_fw_all_v2_noHIEScorrection_post.csv")
MAR_spp_proportions_2010_2014_FAO <- read_csv("data/MAR_spp_proportions_2014_FAO.csv")
iso_group <- read_csv("data/iso_group.csv") %>% filter(!group=="EUN")
iso_group_EUN <- read_csv("data/iso_group.csv")
FAO_prod_spp <- read_csv("data/FAO_prod_spp.csv") %>% 
  mutate(ASFIS_species = tolower(ASFIS_species))

##Nutrients
mar_fw_spp_nutrients <- read_csv("data/mar_fw_spp_nutrients_FAO_fw_v2.csv") %>% 
  group_by(common_name, nutrient) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(common_name)

mar_fw_spp_nutrients_categories <- read_csv("data/mar_fw_spp_nutrients_FAO_fw_v2.csv") %>% 
  dplyr::select(common_name, nutrient, category, value) %>% 
  group_by(category, nutrient) %>% 
  summarise(value = mean(value)) %>% 
  drop_na(category)

mar_fw_spp_nutrients_broad_categories <- read_csv("data/mar_fw_spp_nutrients_FAO_fw_v2.csv") %>% 
  dplyr::select(common_name, nutrient, broad_category, value) %>%
  mutate(broad_category = recode(broad_category, 
                                 "freshwater" = "INL",
                                 "Inland waters" = "INL",
                                 "Marine areas" = "MAR")) %>% 
  group_by(broad_category, nutrient) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(broad_category)


##FH stands for total, 
##MLC: molluscs, 
##DEM: demersal fish, 
##OPE, other pelagic fish, 
##OMA: Other marine fish, 
##MUL: mullet, 
##CB: crab, 
##SHP: shrimp and prawn, 
##OCR: crustacean excluding CB and SHP, 
##SAL: Salmon and trout, 
##TIL: Tilapia, 
##CARP: carp, 
##OFWD: other fresh water and diadromous fish (mostly catfish, pangas and eel depending on the country),
##MKF: milk fish, OTH: either small elements to allow an identity for the total and/or others included in the category other aquatic animals. 
##QP: quantity produced in lkt live weight. 


##Clean data
QP_all = rbind(QPMARBASE, QPMARHIGH, QPINLBASE, QPINLHIGH) %>% 
  separate(OUTPUT, c("iso3c", "group", "unit"), "_",remove=F)

QP_all = reshape2::melt(QP_all, id.vars = c(names(QP_all)[1:7], "scenario", "env")) %>% 
  rename("year" = "variable")

QP_all = left_join(QP_all, iso_group, by=c("iso3c" = "group")) %>% 
  mutate(country_code = if_else(is.na(iso3c.y), iso3c, iso3c.y)) %>% 
  select(-iso3c, -iso3c.y) %>% 
  rename("iso3c" = "country_code")

QP_countries = as.data.frame(unique(QP_all$iso3c))

##Remove FHA when duplicated
QP_capture = QP_all %>% 
  filter(group %in% "FHC") %>% 
  select(iso3c, year, env, scenario, group, value)

QP_aquac_spp = QP_all %>% 
  filter(!group %in% c("FHC", "FHA")) %>% 
  group_by(iso3c, year, env, scenario) %>% 
  summarize(value_spp = sum(value)) 

QP_aquac = QP_all %>% 
  filter(!group %in% "FHC",
         group %in% "FHA") %>% 
  select(iso3c, year, env, scenario, value) %>% 
  left_join(QP_aquac_spp, by=c("iso3c", "year", "env", "scenario")) %>% 
  mutate(value_spp = if_else(is.na(value_spp), 0, value_spp),
         delta = value-value_spp) %>% 
  filter(delta>0.1) %>% 
  mutate(group = "FHA") %>% 
  select(-value_spp, -value) %>% 
  rename("value" = "delta")

QP_all_spp = QP_all %>% 
  filter(!group %in% c("FHC", "FHA")) %>% 
  select(iso3c, year, env, scenario, group, value) %>% 
  rbind(QP_capture, QP_aquac)

##Calculate proportions of each group/country
QP_all_total = QP_all_spp %>%
  group_by(iso3c, year, scenario) %>% 
  summarise(value_total = sum(value)) %>% 
  ungroup()

QP_prop = QP_all_spp %>%
  group_by(iso3c, year, group, scenario, env) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

QP_prop = left_join(QP_prop, QP_all_total, by=c("iso3c", "year", "scenario")) 

QP_prop = QP_prop %>% 
  mutate(QP_prop = value/value_total) %>% 
  drop_na(QP_prop) %>% 
  filter(QP_prop>0) %>% 
  select(-value_total, -value) %>% 
  mutate(group = recode(group, "DEMA" = "Demersal Fish",
                        "OPEA" = "Pelagic Fish"))

# #write.csv(QP_countries, "QP_countries.csv", row.names = F)
# QP_groups = as.data.frame(unique(QP_all$group)) %>%
#   rename("common_name" = "unique(QP_all$group)") %>% 
#   filter(!common_name %in% c("FHA", "FHC"))

#write.csv(QP_groups, "data/QP_groups.csv", row.names = F)
##Nutrient output
Base_high_nutrients = rbind (BaseNutrients, ScenarioNutrients)

Base_high_nutrients = Base_high_nutrients %>% 
  rename("output" = "OUTPUT,0",
         "iso3c" = "X2",
         "food_abrev" = "X3",
         "nutrient" = "X4") %>% 
  #separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>% 
  mutate(units = gsub("]", "", units),
         nutrient = recode(nutrient,
                           "CA" = "Calcium",
                           "FE" = "Iron",
                           "MFAT" = "Monounsaturated fatty acids",
                           "O3" = "Omega-3 fatty acids",
                           "PFAT" = "Polyunsaturated fatty acids",
                           "SFAT" = "Saturated fatty acids",
                           "TFAT" = "Fat",
                           "VitA1" = "Vitamin A",
                           "VitB" = "Vitamin B12",
                           "ZN" = "Zinc",
                           "PROT" = "Protein",
                           "DES" = "Calories")) %>% 
  select(-X12)


##Change to long format
NutrientsScen_long = reshape2::melt(Base_high_nutrients, id.vars=c("output", "countries", "products", "elements",     
                                                                   "nutrient_long", "units", "iso3c",        
                                                                   "food_abrev", "nutrient", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("A", "", year)) %>% 
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids", "Calcium"))

##Calculate proportion of aquatic foods for each country in 2017
NutrientsScen_long_terrestrial = NutrientsScen_long %>% 
  filter(year=="2017",
         !products == "Fish",
         scenario=="base") %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(total_terrestrial = sum(value))

NutrientsScen_long_fish = NutrientsScen_long %>% 
  filter(year=="2017",
         products == "Fish",
         scenario=="base") %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(total_aquatic = sum(value))

NutrientsScen_long_all = left_join(NutrientsScen_long_terrestrial, NutrientsScen_long_fish, by=c("iso3c", "nutrient")) %>% 
  drop_na() %>% 
  mutate(total = total_aquatic + total_terrestrial,
         prop_terrestrial = total_terrestrial/total,
         prop_aquatic = total_aquatic/total)

#write.csv(NutrientsScen_long_all, "seafood_nutrient_supply_GND.csv", row.names = F)

##Food Consumption
FC_all = rbind(FoodConsumptionBase, FoodConsumptionScenario) %>%
  rename("output" = "OUTPUT,0",
         "iso3c" = "X5",
         "food_abrev" = "X6") %>%
  #separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>%
  mutate(units = gsub("]", "", units)) %>% 
  select(-X12, -X7)

##Change to long format
FC_all = reshape2::melt(FC_all, id.vars=c("output", "countries", "products", "elements",
                                          "nutrient_long", "units", "iso3c",
                                          "food_abrev","scenario")) %>%
rename("year" = "variable") %>%
mutate(year = gsub("A", "", year))



FC_ASF = FC_all %>% 
  filter(products %in% c("Beef and Veal", "Fish", "Pork", "Poultry", "Sheep"))

##Plot total for all countries
FC_ASF_total = FC_all %>% 
  filter(products %in% c("Beef and Veal", "Fish", "Pork", "Poultry", "Sheep")) %>% 
  group_by(year, products, scenario) %>% 
  summarise(value = mean(value))

FC_ASF_total$year = as.numeric(FC_ASF_total$year)

##Clean freshwater disaggregation data
fw_dis = fw_consump_combined_final %>% 
  rename("iso3c" = "country_code",
         "common_name" = "common_name_simple",
         "production_sector" = "source",
         "tonnes" = "consumption_tons_mid",
         "scientific_name" = "sci_name") %>% 
  mutate(env = "INL",
         genus_cat = "Freshwater fish",
         Production_area = "Inland waters",
         production_sector = recode(production_sector, 
                                    "Aquaculture (freshwater)" = "FHA",
                                    "Aquaculture (brackishwater)" = "FHA",
                                    "Capture production" = "FHC",
                                    "Import" = "FHA",
                                    "Recreational" = "FHC",
                                    "Freshwater (assumed capture)" = "FHC",
                                    "Freshwater (aquaculture)" = "FHA",
                                    "Recreational" = "FHC")) %>% 
  left_join(iso_group_EUN, by="iso3c") %>% 
  dplyr::select(iso3c, year, genus_cat, scientific_name, common_name, production_sector, 
                env, tonnes, group)

fw_dis_capture = fw_dis %>% 
  filter(production_sector=="FHC")

fw_dis_aquac = fw_dis %>% 
  filter(production_sector=="FHA")

#Clean marine disaggregation
mar_dis_import_capture = MAR_spp_proportions_2010_2014_FAO %>% 
  mutate(scientific_name = tolower(scientific_name),
         Genus = tolower(Genus)) %>% 
  filter(production_sector=="import",
         !Genus %in% c("mytilus", "perna", "crassostrea", "litopenaeus", "penaeus", "metapenaeus", "salmo")) %>% 
  mutate(production_sector = "FHC")

mar_dis_import_aquac = MAR_spp_proportions_2010_2014_FAO %>%
  mutate(scientific_name = tolower(scientific_name),
         Genus = tolower(Genus)) %>% 
  filter(production_sector=="import",
         Genus %in% c("mytilus", "perna", "crassostrea", "litopenaeus", "penaeus", "metapenaeus", "salmo")) %>% 
  mutate(production_sector = "FHA")

mar_dis_capture = MAR_spp_proportions_2010_2014_FAO %>% 
  filter(production_sector=="Capture production") %>% 
  rbind(mar_dis_import_capture) %>% 
  mutate(production_sector="FHC",
         common_name = tolower(common_name), 
         env="MAR") %>% 
  left_join(iso_group_EUN, by="iso3c") %>% 
  select(-Genus, -Family, -Order, -spp_prop, -Production_area)

mar_dis_aquac = MAR_spp_proportions_2010_2014_FAO %>% 
  filter(production_sector=="Aquaculture production") %>% 
  rbind(mar_dis_import_aquac) %>% 
  mutate(production_sector="FHA",
         common_name = tolower(common_name), 
         env="MAR") %>% 
  left_join(iso_group_EUN, by="iso3c") %>% 
  select(-Genus, -Family, -Order, -spp_prop, -Production_area)

all_dis = rbind(mar_dis_capture, fw_dis_capture,
                        mar_dis_aquac, fw_dis_aquac) %>% 
  mutate(common_name = tolower(common_name))

all_dis_EUN = all_dis %>%
  filter(group=="EUN")

all_dis = all_dis %>% 
  filter(!group=="EUN")

##Calculate species proportions
spp_total = all_dis %>%
  group_by(iso3c, production_sector, env) %>% 
  summarise(tonnes_total = sum(tonnes))

spp_prop = all_dis %>%  
  group_by(iso3c, production_sector, common_name, env) %>% 
  summarise(tonnes = sum(tonnes))

spp_prop = left_join(spp_prop, spp_total, by=c("iso3c", "production_sector", "env")) 

spp_prop = spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total, -tonnes)

##Calculate species proportions - EUN
spp_total_EUN = all_dis_EUN %>%
  group_by(group, production_sector, env) %>% 
  summarise(tonnes_total = sum(tonnes))

spp_prop_EUN = all_dis_EUN %>%  
  group_by(group, production_sector, common_name, env) %>% 
  summarise(tonnes = sum(tonnes))

spp_prop_EUN = left_join(spp_prop_EUN, spp_total_EUN, by=c("group", "production_sector", "env")) 

spp_prop_EUN = spp_prop_EUN %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total, -tonnes) %>% 
  rename("iso3c" = "group")

##Join both databases
spp_prop = rbind(spp_prop, spp_prop_EUN)

###################################All years
FC_fish = FC_all %>% 
  filter(products == "Fish") %>% 
  rename(consumption = value)

FC_fish = left_join(FC_fish, QP_prop, by=c("iso3c", "year", "scenario")) %>% 
  drop_na(QP_prop) %>% 
  left_join(spp_prop, by=c("iso3c", "group" = "production_sector", "env")) %>% 
  mutate(value2 = consumption*QP_prop,
         value3 = value2*spp_prop,
         pred_consump = if_else(is.na(value3), value2, value3)) %>% 
  select(-value2, -value3)

FC_fish = left_join(FC_fish, mar_fw_spp_nutrients, by="common_name") %>% 
  mutate(nutrient_supply_dis = pred_consump*value*10/365) 

FC_fish_name = FC_fish %>% 
  filter(!is.na(value))

missing_na_cat = FC_fish %>% 
  filter(is.na(value)) %>% 
  select(-nutrient, -value, -nutrient_supply_dis) %>% 
  left_join(mar_fw_spp_nutrients_categories, by=c("group" = "category")) %>% 
  mutate(nutrient_supply_dis = pred_consump*value*10/365) 

FC_fish_cat = missing_na_cat %>% 
  filter(!is.na(value))

missing_na = missing_na_cat %>% 
  filter(is.na(value)) %>% 
  select(-nutrient, -value, -nutrient_supply_dis) %>% 
  left_join(mar_fw_spp_nutrients_broad_categories, by=c("env" = "broad_category")) %>%
  mutate(nutrient_supply_dis = pred_consump*value*10/365) 

FC_fish = rbind(FC_fish_name, FC_fish_cat, missing_na) 

FC_fish = FC_fish %>%
  group_by(iso3c, year, scenario, nutrient) %>% 
  summarise(nutrient_supply_dis = sum(nutrient_supply_dis, na.rm=T)) %>% 
  drop_na(nutrient)

##Subset model output - all years
NutrientsScen_long_all_years = NutrientsScen_long %>% 
  filter(products=="Fish") %>% 
  select(iso3c, year, units, products, scenario, nutrient, value) %>% 
  rename("nutrient_supply" = "value")

##jOIN DATABASES
Nutrients_fish = left_join(NutrientsScen_long_all_years, FC_fish, by=c("iso3c","year", "scenario", "nutrient")) 

Nutrients_2030 = Nutrients_fish %>%
  filter(year==2030,
         nutrient_supply_dis>0)
  
NutrientsScen_long_other = NutrientsScen_long %>% 
  filter(!products=="Fish") %>% 
  rename("nutrient_supply" = "value") %>% 
  dplyr::select(iso3c, year, units, products, scenario, nutrient, nutrient_supply) %>% 
  mutate(nutrient_supply_dis = "NA")

NutrientsScen_long_dis = rbind(NutrientsScen_long_other, Nutrients_fish) 
NutrientsScen_long_dis$nutrient_supply_dis = as.double(NutrientsScen_long_dis$nutrient_supply_dis)
NutrientsScen_long_dis = NutrientsScen_long_dis %>% 
  mutate(nutrient_supply_dis = if_else(is.na(nutrient_supply_dis), nutrient_supply, nutrient_supply_dis)) %>% 
  mutate(nutrient_supply_dis = if_else(nutrient_supply_dis<0, nutrient_supply, nutrient_supply_dis))

NutrientsScen_long_dis_short = NutrientsScen_long_dis %>% 
  group_by(iso3c, year, units, scenario, nutrient) %>% 
  summarise(nutrient_supply_dis = sum(nutrient_supply_dis, na.rm = TRUE),
            nutrient_supply = sum(nutrient_supply, na.rm = TRUE))

NutrientsScen_long_dis_short_2030 = NutrientsScen_long_dis_short %>% 
  filter(year==2030)

write.csv(NutrientsScen_long_dis, "Disaggregated_NutrientScen_FAO_FW.csv", row.names = F)

write.csv(NutrientsScen_long_dis_short, "Disaggregated_NutrientScen_grouped_FAO_fw.csv", row.names = F)
  
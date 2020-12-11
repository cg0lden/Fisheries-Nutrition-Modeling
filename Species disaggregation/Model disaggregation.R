library(readr)
library(tidyverse)

#Import datasets
QPMARBASE <- read_csv("data/QPMARBASE.csv") %>% mutate(scenario = "base", env = "MAR")
QPMARHIGH <- read_csv("data/QPMARHIGH.csv") %>% mutate(scenario = "high", env = "MAR")
QPINLHIGH <- read_csv("data/QPINLHIGH.csv") %>% mutate(scenario = "high", env = "INL")
QPINLBASE <- read_csv("data/QPINLBASE.csv") %>% mutate(scenario = "base", env = "INL")
FoodConsumptionBase <- read_csv("data/FoodConsumptionBase.csv") %>% mutate(scenario = "base")
FoodConsumptionScenario <- read_csv("data/FoodConsumptionScenario.csv") %>% mutate(scenario = "high")
BaseNutrients <- read_csv("data/BaseNutrients.csv") %>% mutate(scenario = "base")
ScenarioNutrients <- read_csv("data/ScenarioNutrients.csv") %>% mutate(scenario = "high")
fw_consump_combined_final <- read_csv("data/fw_consump_combined_final.csv")
MAR_spp_proportions_2010_2014_SAU <- read_csv("data/MAR_spp_proportions_2010_2014_SAU.csv")


##Nutrients
mar_fw_spp_nutrients <- read_csv("data/mar_fw_spp_nutrients.csv") %>% 
  dplyr::select(common_name, nutrient, value)

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

QP_countries = unique(QP_all$iso3c)

##Nutrient output
Base_high_nutrients = rbind (BaseNutrients, ScenarioNutrients)

Base_high_nutrients = Base_high_nutrients %>% 
  rename("output" = "X1",
         "iso3c" = "X6",
         "food_abrev" = "X8") %>% 
  separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
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
  select(-X12, -X11)


##Change to long format
NutrientsScen_long = reshape2::melt(Base_high_nutrients, id.vars=c("output", "countries", "products", "elements",     
                                                                   "nutrient_long", "units", "country_codes", "iso3c",        
                                                                   "product_codes", "food_abrev", "ele_codes", "nutrient",     
                                                                   "total", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("_", "", year)) %>% 
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids"))

##Food Consumption
FC_all = rbind(FoodConsumptionBase, FoodConsumptionScenario) %>% 
  rename("output" = "X1",
         "iso3c" = "X6",
         "food_abrev" = "X8") %>% 
  separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>% 
  mutate(units = gsub("]", "", units)) %>% 
  select(-X12, -X11)


##Change to long format
FC_all = reshape2::melt(FC_all, id.vars=c("output", "countries", "products", "elements",     
                                                                   "nutrient_long", "units", "country_codes", "iso3c",        
                                                                   "product_codes", "food_abrev", "ele_codes", "nutrient",     
                                                                   "total", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("_", "", year))

FC_ASF = FC_all %>% 
  filter(products %in% c("Bovine", "Fish", "Pork", "Poultry", "Ovine"))

##Plot fish consumption by year
ggplot(data = FC_ASF) +
  geom_point(aes(x=year, y=value)) +
  facet_wrap(~products, scales = "free")+
  theme_classic()+
  ylab("Consumption (kg/person/day)")

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

all_dis = rbind(fw_dis, MAR_spp_proportions_2010_2014_SAU)

##Calculate species proportions
spp_total = all_dis %>%
  group_by(iso3c) %>% 
  summarise(tonnes_total = sum(tonnes))

spp_prop = all_dis %>%  
  group_by(iso3c, production_sector, common_name, env) %>% 
  summarise(tonnes = sum(tonnes))

spp_prop = left_join(spp_prop, spp_total, by=c("iso3c")) 

spp_prop = spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total)

##Allocate species to model output
FC_fish = FC_all %>% 
  filter(products == "Fish")

################################Example for 2014
FC_fish_2014 = FC_fish %>% 
  filter(year == "2014",
         scenario=="base") 

##Join databases
FC_fish_2014 = left_join(FC_fish_2014, spp_prop, by=c("iso3c")) %>% 
  mutate(pred_consump = value*spp_prop) %>% 
  rename("fish_kg_person" = "value") %>% 
  dplyr::select(countries, units, iso3c, scenario, common_name, production_sector, env, fish_kg_person, spp_prop, pred_consump)


FC_fish_2014 = left_join(FC_fish_2014, mar_fw_spp_nutrients, by="common_name") %>% 
  mutate(nutrient_supply_dis = pred_consump*value*10/365) %>%
  group_by(iso3c, nutrient) %>% 
  summarise(nutrient_supply_dis = sum(nutrient_supply_dis))

##Subset model output
NutrientsScen_long_2014 = NutrientsScen_long %>% 
  filter(year=="2014",
         scenario=="base",
         products=="Fish") %>% 
  select(iso3c, nutrient, value) %>% 
  rename("nutrient_supply" = "value")

##jOIN DATABASES
Nutrients_all = left_join(NutrientsScen_long_2014, FC_fish_2014, by=c("iso3c", "nutrient")) %>% 
  filter(nutrient_supply_dis>0)

ggplot(data = Nutrients_all) + 
  geom_point(aes(x=nutrient_supply, y=nutrient_supply_dis)) +
  facet_wrap(~nutrient, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Nutrient supply - disaggregation")+
  xlab("Nutrient supply - model") +
  theme_classic()

###################################All years
##Allocate species to model output
FC_fish = FC_all %>% 
  filter(products == "Fish")

##Join databases
FC_fish = left_join(FC_fish, spp_prop, by=c("iso3c")) %>% 
  mutate(pred_consump = value*spp_prop) %>% 
  rename("fish_kg_person" = "value") %>% 
  dplyr::select(countries, year, units, iso3c, scenario, common_name, production_sector, env, fish_kg_person, spp_prop, pred_consump)


FC_fish = left_join(FC_fish, mar_fw_spp_nutrients, by="common_name") %>% 
  mutate(nutrient_supply_dis = pred_consump*value*10/365) %>%
  group_by(iso3c, year, scenario, nutrient) %>% 
  summarise(nutrient_supply_dis = sum(nutrient_supply_dis))


##Subset model output - all years
NutrientsScen_long_all_years = NutrientsScen_long %>% 
  filter(products=="Fish") %>% 
  select(iso3c, year, units, products, scenario, nutrient, value) %>% 
  rename("nutrient_supply" = "value")

##jOIN DATABASES
Nutrients_fish = left_join(NutrientsScen_long_all_years, FC_fish, by=c("iso3c","year", "scenario", "nutrient")) 

Nutrients_2030 = Nutrients_all %>% 
  filter(year==2030)

ggplot(data = Nutrients_2030) + 
  geom_point(aes(x=nutrient_supply, y=nutrient_supply_dis)) +
  facet_wrap(~nutrient, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Nutrient supply - disaggregation")+
  xlab("Nutrient supply - model") +
  theme_classic()+
  labs(title = 2030)


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

write.csv(NutrientsScen_long_dis, "Disaggregated_NutrientScen.csv", row.names = F)

write.csv(NutrientsScen_long_dis_short, "Disaggregated_NutrientScen_grouped.csv", row.names = F)



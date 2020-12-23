library(readr)
library(tidyverse)
library(countrycode)

#Import datasets
QPMARBASE <- read_csv("data/QPMARBASE.csv") %>% mutate(scenario = "base", env = "MAR")
QPMARHIGH <- read_csv("data/QPMARHIGH.csv") %>% mutate(scenario = "high", env = "MAR")
QPINLHIGH <- read_csv("data/QPINLHIGH.csv") %>% mutate(scenario = "high", env = "INL")
QPINLBASE <- read_csv("data/QPINLBASE.csv") %>% mutate(scenario = "base", env = "INL")
FoodConsumptionBase <- read_csv("data/FoodConsumptionBase.csv") %>% mutate(scenario = "base")
FoodConsumptionScenario <- read_csv("data/FoodConsumptionScenario.csv") %>% mutate(scenario = "high")
BaseNutrients <- read_csv("data/BaseNutrients.csv") %>% mutate(scenario = "base")
ScenarioNutrients <- read_csv("data/ScenarioNutrients.csv") %>% mutate(scenario = "high")
fw_consump_combined_final <- read_csv("data/fw_consump_combined_final_wmodelcode.csv")
MAR_spp_proportions_2010_2014_SAU <- read_csv("data/MAR_spp_proportions_2014_SAU.csv")
iso_group <- read_csv("data/iso_group.csv") %>% filter(!group=="EUN")
iso_group_EUN <- read_csv("data/iso_group.csv")
FAO_prod_spp <- read_csv("data/FAO_prod_spp.csv") %>% 
  mutate(ASFIS_species = tolower(ASFIS_species))

##Nutrients
mar_fw_spp_nutrients <- read_csv("data/mar_fw_spp_nutrients.csv") %>% 
  dplyr::select(common_name, nutrient, value)

mar_fw_spp_nutrients_categories <- read_csv("data/mar_fw_spp_nutrients.csv") %>% 
  dplyr::select(common_name, nutrient, category, value) %>% 
  group_by(category, nutrient) %>% 
  summarise(value_cat = mean(value)) %>% 
  drop_na(category)

mar_fw_spp_nutrients_broad_categories <- read_csv("data/mar_fw_spp_nutrients.csv") %>% 
  dplyr::select(common_name, nutrient, broad_category, value) %>% 
  group_by(broad_category, nutrient) %>% 
  summarise(value_cat = mean(value)) %>% 
  drop_na(broad_category) %>% 
  mutate(broad_category = recode(broad_category, 
                                 "freshwater" = "INL",
                                 "marine" = "MAR"))

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

##Calculate proportions of each group/country
QP_all_total = QP_all %>%
  group_by(iso3c, year, scenario) %>% 
  summarise(value_total = sum(value))

QP_prop = QP_all %>%  
  group_by(iso3c, year, group, scenario, env) %>% 
  summarise(value = sum(value))

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
                                                                   "product_codes", "food_abrev","ele_codes",
                                                                   "nutrient", "total", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("_", "", year)) %>% 
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

write.csv(NutrientsScen_long_all, "seafood_nutrient_supply_GND.csv", row.names = F)

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
  filter(products %in% c("Beef and Veal", "Fish", "Pork", "Poultry", "Sheep"))

##Plot fish consumption by year
ggplot(data = FC_ASF) +
  geom_point(aes(x=year, y=value)) +
  facet_wrap(~products, scales = "free")+
  theme_classic()+
  ylab("Consumption (kg/person/day)")

##Plot total for all countries
FC_ASF_total = FC_all %>% 
  filter(products %in% c("Beef and Veal", "Fish", "Pork", "Poultry", "Sheep")) %>% 
  group_by(year, products, scenario) %>% 
  summarise(value = mean(value))

FC_ASF_total$year = as.numeric(FC_ASF_total$year)
ggplot(data = FC_ASF_total) +
  geom_line(aes(x=year, y=value, linetype=scenario)) +
  facet_wrap(~products, scales = "free")+
  theme_classic()+
  ylab("Average Consumption (kg/person/day)")

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

all_dis_import = all_dis %>% 
  filter(production_sector=="import",
         !env == "freshwater",
         !str_detect(common_name, "salmon")) %>% 
  mutate(production_sector = "capture")

all_dis = all_dis %>% 
  filter(!production_sector=="import",
         !production_sector=="aquaculture") %>% 
  rbind(all_dis_import) %>% 
  mutate(production_sector="FHC",
         common_name = tolower(common_name)) %>% 
  left_join(iso_group_EUN, by="iso3c")
  
all_dis_EUN = all_dis %>%
  filter(group=="EUN")

all_dis = all_dis %>% 
  filter(!group=="EUN")

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
  select(-tonnes_total, -tonnes) %>% 
  mutate(env = recode(env, "freshwater" = "INL",
                      "marine" = "MAR"))

##Calculate species proportions - EUN
spp_total_EUN = all_dis_EUN %>%
  group_by(group) %>% 
  summarise(tonnes_total = sum(tonnes))

spp_prop_EUN = all_dis_EUN %>%  
  group_by(group, production_sector, common_name, env) %>% 
  summarise(tonnes = sum(tonnes))

spp_prop_EUN = left_join(spp_prop_EUN, spp_total_EUN, by=c("group")) 

spp_prop_EUN = spp_prop_EUN %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total, -tonnes) %>% 
  mutate(env = recode(env, "freshwater" = "INL",
                      "marine" = "MAR")) %>% 
  rename("iso3c" = "group")

##Join both databases
spp_prop = rbind(spp_prop, spp_prop_EUN)

###################################All years
##Allocate species to model output
FC_fish = FC_all %>% 
  filter(products == "Fish")

FC_fish = left_join(FC_fish, QP_prop, by=c("iso3c", "year", "scenario")) %>% 
  drop_na(QP_prop) %>% 
  left_join(spp_prop, by=c("iso3c", "group" = "production_sector", "env")) %>% 
  mutate(value2 = value*QP_prop,
         value3 = value2*spp_prop,
         pred_consump = if_else(is.na(value3), value2, value3)) %>% 
  select(-value, -value2, -value3) %>% 
  filter(!group=="FHA")

FC_fish = left_join(FC_fish, mar_fw_spp_nutrients, by="common_name") %>%
  left_join(mar_fw_spp_nutrients_categories, by=c("group" = "category")) %>% 
  mutate(nutrient = if_else(is.na(nutrient.x), nutrient.y, nutrient.x),
         nut_value = if_else(is.na(value), value_cat, value),
         nutrient_supply_dis = pred_consump*nut_value*10/365) %>% 
  select(iso3c, year, scenario, nutrient, env, pred_consump, nutrient_supply_dis)

missing_na = FC_fish %>% 
  filter(is.na(nutrient_supply_dis)) %>% 
  left_join(mar_fw_spp_nutrients_broad_categories, by=c("env" = "broad_category")) %>%
  mutate(nutrient_supply_dis = pred_consump*value_cat*10/365) %>% 
  rename("nutrient" = "nutrient.y") %>% 
  select(-nutrient.x, -value_cat)

FC_fish = rbind(FC_fish, missing_na) %>%
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

##Calculate proportion of aquatic foods for each country in 2017
NutrientsScen_long_terrestrial = NutrientsScen_long_dis %>% 
  filter(year=="2017",
         !products == "Fish",
         scenario=="base") %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(total_terrestrial = sum(nutrient_supply),
            total_terrestrial_disaggregated = sum(nutrient_supply_dis))

NutrientsScen_long_fish = NutrientsScen_long_dis %>% 
  filter(year=="2017",
         products == "Fish",
         scenario=="base") %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(total_aquatic = sum(nutrient_supply),
            total_aquatic_disaggregated = sum(nutrient_supply_dis))

NutrientsScen_long_all = left_join(NutrientsScen_long_terrestrial, NutrientsScen_long_fish, by=c("iso3c", "nutrient")) %>% 
  drop_na() %>% 
  mutate(total = total_aquatic + total_terrestrial,
         prop_terrestrial = total_terrestrial/total,
         prop_aquatic = total_aquatic/total,
         total_disaggregated = total_aquatic_disaggregated + total_terrestrial_disaggregated,
         prop_terrestrial_disaggregated = total_terrestrial_disaggregated/total_disaggregated,
         prop_aquatic_disaggregated = total_aquatic_disaggregated/total_disaggregated) %>% 
  select(iso3c, nutrient, total_terrestrial, total_aquatic, 
          total, prop_terrestrial, prop_aquatic, total_terrestrial_disaggregated, total_aquatic_disaggregated, 
         total_disaggregated, prop_terrestrial_disaggregated, prop_aquatic_disaggregated)

write.csv(NutrientsScen_long_all, "seafood_nutrient_supply_GND.csv", row.names = F)

################################Example for 2014
##Allocate species to model output
FC_fish_2014 = FC_all %>% 
  filter(products == "Fish") %>% 
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
  filter(nutrient_supply_dis>0) %>% 
  mutate(nutrient_supply_diff = nutrient_supply_dis-nutrient_supply)

ggplot(data = Nutrients_all) +
  geom_point(aes(x=nutrient_supply, y=nutrient_supply_dis)) +
  facet_wrap(~nutrient, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Nutrient supply - disaggregation")+
  xlab("Nutrient supply - model") +
  theme_classic()

##Map differences
Nutrients_diff = Nutrients_all %>% 
  dplyr::select(iso3c, nutrient, nutrient_supply_diff)

# World polygons from the maps package
# World
# world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Plot data (all)
################################################################################

# # Add expanded data to SF
# data_all_sf <- world %>% 
#   left_join(Nutrients_diff, by=c("gu_a3"="iso3c"))



# # Plot data
# g <- ggplot(data_all_sf) +
#   facet_wrap(~ nutrient) +
#   geom_sf(mapping=aes(fill=nutrient_supply_diff), lwd=0.1, color="grey30") +
#   # Legend
#   scale_fill_gradient2(name="% difference in 2030\nmodel output vs disaggregated values", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Crop out Antarctica
#   coord_sf(y=c(-55, NA)) +
#   # Theme
#   theme_bw() +
#   theme(legend.position = "bottom",
#         axis.text=element_blank(),
#         axis.title=element_blank(),
#         legend.text=element_text(size=6),
#         legend.title=element_text(size=8),
#         strip.text=element_text(size=6),
#         plot.title=element_text(size=8),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"))
# g
# 
# # Export plot
# ggsave(g, filename=file.path(plotdir, "figure_perc_food_diff_all.pdf"), 
#        width=8.5, height=11, units="in", dpi=600)


#############Plotting
NutrientsScen_long_2030 = NutrientsScen_long %>% 
  filter(nutrient=="Calcium",
         iso3c=="CHN")
NutrientsScen_long_2030$year=as.character(NutrientsScen_long_2030$year)
NutrientsScen_long_2030$year=as.numeric(NutrientsScen_long_2030$year)

ggplot(data=NutrientsScen_long_2030, aes(x=year, y=value, colour = scenario)) +
  geom_line()+
  #geom_smooth()+
  facet_wrap(~products, scales = "free") +
  theme_classic()+
  labs(title="China, Calcium")
  
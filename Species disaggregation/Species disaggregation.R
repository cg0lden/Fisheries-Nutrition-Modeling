#########Species disaggregation for BFA paper###########

library(readr)
library(tidyverse)
QPMARBASE <- read_csv("Species disaggregation/QPMARBASE.csv")
QPMARHIGH <- read_csv("Species disaggregation/QPMARHIGH.csv")

##FH stands for total, 
##MLC: molluscs, 
##DEM: demersal fish, 
##OPE, other pelagic fish, 
##OMA: Other marine fish, 
##MUL: mullet, CB: crab, 
##SHP: shrimp and prawn, 
##OCR: crustacean excluding CB and SHP, 
##SAL: Salmon and trout, 
##TIL: Tilapia, 
##CARP: carp, 
##OFWD: other fresh water and diadromous fish (mostly catfish, pangas and eel depending on the country),
##MKF: milk fish, OTH: either small elements to allow an identity for the total and/or others included in the category other aquatic animals. 
##QP: quantity produced in lkt live weight. 

fo_base = reshape2::melt(QPMARBASE, id.vars = names(QPMARBASE)[1:8], 
                             measure.vars = names(QPMARBASE)[9:ncol(QPMARBASE)])

fo_high = reshape2::melt(QPMARHIGH, id.vars = names(QPMARHIGH)[1:8], 
                         measure.vars = names(QPMARHIGH)[9:ncol(QPMARHIGH)])

fo_base = fo_base %>% 
  rename(year = variable) %>% 
  mutate(scenario = "base")

fo_high = fo_high %>% 
  rename(year = variable) %>% 
  mutate(scenario = "high")

fo_marine = rbind(fo_base, fo_high)
fo_marine$year = as.character(fo_marine$year)
fo_marine$year = as.numeric(fo_marine$year)

fo_marine_totals = fo_marine %>% 
  filter(group %in% c("FHA", "FHC"))

fo_capture = fo_marine %>% 
  filter(group %in% c("FHC"))

##plot data
ggplot(data = fo_marine_totals)+
  geom_line(aes(x=year, y=value, colour=source, linetype=scenario))+
  theme_classic()+
  facet_wrap(~iso3c)

##plot for individual countries
plot_countries = function(iso){
  country_data = fo_marine_totals %>% 
    filter(iso3c==iso)
  
  ggplot(data = country_data)+
    geom_line(aes(x=year, y=value, colour=source, linetype=scenario))+
    theme_classic()
}

##List of countries
## "AFL" "AFN" "AFS" "ANL" "ARG" "ASA" "ASC" "ASL" "AUS"
## "BRA" "CAN" "CHE" "CHL" "CHN" "COL" "EGY" "ETH" "EUE"
## "EUN" "GBR" "IDN" "IND" "IRN" "ISR" "JPN" "KAZ" "KOR"
## "MEX" "MYS" "NEO" "NGA" "NOR" "NZL" "OCE" "PAK" "PER"
## "PHL" "PRY" "RUS" "SAC" "SAU" "THA" "TUR" "UKR" "USA"
## "VNM" "ZAF"
plot_countries(iso="BRA")

##Plot total
fo_total = fo_marine_totals %>% 
  group_by(source, scenario, year) %>% 
  summarise(value = sum(value))

ggplot(data = fo_total)+
  geom_line(aes(x=year, y=value, colour=source, linetype=scenario))+
  theme_classic()

##############Import and clean FAO data
##########Import Aquaculture data####################

##Read and clean FAO production data
FAO_prod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/FAO_global_production.csv")

FAO_prod = reshape2::melt(FAO_prod, id.vars = names(FAO_prod)[1:19], 
                          measure.vars = names(FAO_prod)[20:ncol(FAO_prod)])

FAO_prod = FAO_prod %>% 
  rename(year = variable, tonnes = value) %>% 
  drop_na(tonnes) %>% 
  filter(!FAOSTAT_group_of_commodities %in% c("Aquatic Mammals"))

##Create Genus category
FAO_prod = FAO_prod %>%
  mutate(genus_cat = case_when(#Cephalopods
    FAOSTAT_group_of_commodities=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    FAOSTAT_group_of_commodities=="Demersal Marine Fish" ~ "Demersal Fish",
    #Pelagic fish
    FAOSTAT_group_of_commodities=="Pelagic Marine Fish" ~ "Pelagic Fish",
    #Crustaceans
    FAOSTAT_group_of_commodities=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    FAOSTAT_group_of_commodities=="Marine Fish NEI" ~ "Marine Fish; Other",
    #Moluscs; Other
    FAOSTAT_group_of_commodities=="Molluscs excl. Cephalopods" ~ "Moluscs; Other",
    #Freshwater
    FAOSTAT_group_of_commodities=="Freshwater and Diadromous Fish" ~ "Freshwater Fish",
    #Aquatic Plants
    FAOSTAT_group_of_commodities=="Aquatic Plants" ~ "Aquatic Plants",
    #Other aquatic animals (Aquatic Animals; Others)
    FAOSTAT_group_of_commodities=="Aquatic Animals NEI" ~ "Aquatic Animals; Others"))

##Remove freshwater species
FAO_prod = FAO_prod %>% 
  filter(!genus_cat %in% c("Freshwater Fish", "Aquatic Plants", "Aquatic Animals; Others"),
         !ISSCAAP_group %in% c("Pearls, mother-of-pearl, shells",
                               "Freshwater crustaceans",
                               "Freshwater molluscs"))

FAO_prod$year = as.character(FAO_prod$year)
FAO_prod$year = as.numeric(FAO_prod$year)

FAO_prod = FAO_prod %>% 
  filter(year>2009)

FAO_aquac = FAO_prod %>% 
  filter(Production_source=="Aquaculture production")

##Read and clean FAO food balance import/export data
FAO_commod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/food_balance_FAO_2020.csv")

FAO_commod = reshape2::melt(FAO_commod, id.vars = names(FAO_commod)[1:5], 
                            measure.vars = names(FAO_commod)[6:ncol(FAO_commod)])

FAO_commod = FAO_commod %>% 
  rename(year = variable, tonnes = value, species_group = FAOSTAT_group) %>% 
  drop_na(tonnes)

##Create Genus category
FAO_commod = FAO_commod %>%
  mutate(genus_cat = case_when(#Cephalopods
    species_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    species_group=="Demersal fish" ~ "Demersal Fish",
    #Pelagic fish
    species_group=="Pelagic fish" ~ "Pelagic Fish",
    #Crustaceans
    species_group=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    species_group=="Marine fish nei" ~ "Marine Fish; Other",
    #Moluscs; Other
    species_group=="Molluscs excl. cephalopods" ~ "Moluscs; Other",
    #Freshwater
    species_group=="Freshwater & diadromous fish" ~ "Freshwater Fish",
    #Other aquatic animals (Aquatic Animals; Others)
    species_group=="Aquatic animals nei" ~ "Aquatic Animals; Others"))

FAO_commod = FAO_commod %>% 
  filter(!genus_cat %in% c("Freshwater Fish"))

FAO_commod$year = as.character(FAO_commod$year)
FAO_commod$year = as.numeric(FAO_commod$year)

FAO_commod = FAO_commod %>% 
  filter(year>2009)

FAO_export = FAO_commod %>% filter(Element=="Food exports")
FAO_import = FAO_commod %>% filter(Element=="Food imports")

###SAU data
##Read SAU data
SAU = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2010_2014.csv")

country_code_SAU <- read_csv("Fisheries production/country_code_SAU.csv")

SAU = left_join(SAU, country_code_SAU, by=c("fishing_entity"="country_name"))

SAU = SAU %>%
  mutate(genus_cat = case_when(
    #Cephalopods
    functional_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    functional_group=="Small demersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium demersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large demersals (>=90 cm)" ~ "Demersal Fish", 
    functional_group=="Small bathydemersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium bathydemersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large bathydemersals (>=90 cm)" ~ "Demersal Fish",
    functional_group=="Small to medium flatfishes (<90 cm)" ~ "Demersal Fish",
    functional_group=="Large flatfishes (>=90 cm)" ~ "Demersal Fish",
    #Pelagic fish
    functional_group=="Small pelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium pelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large pelagics (>=90 cm)" ~ "Pelagic Fish", 
    functional_group=="Small benthopelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium benthopelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large benthopelagics (>=90 cm)" ~ "Pelagic Fish",
    functional_group=="Small bathypelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium bathypelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large bathypelagics (>=90 cm)" ~ "Pelagic Fish",
    #Crustaceans
    functional_group=="Shrimps" ~ "Crustaceans",
    functional_group=="Lobsters, crabs" ~ "Crustaceans",
    functional_group=="Krill" ~ "Crustaceans",
    #Reef fish
    functional_group=="Small reef assoc. fish (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium reef assoc. fish (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large reef assoc. fish (>=90 cm)" ~ "Demersal Fish", 
    functional_group=="Small to medium rays (<90 cm)" ~ "Demersal Fish",
    #Sharks and rays
    functional_group=="Large rays (>=90 cm)" ~ "Demersal Fish",
    functional_group=="Small to medium sharks (<90 cm)" ~ "Demersal Fish",
    functional_group=="Large sharks (>=90 cm)" ~ "Demersal Fish",
    #Moluscs; Other
    functional_group=="Other demersal invertebrates" ~ "Moluscs; Other",
    functional_group=="Jellyfish" ~ "Aquatic Animals; Others"))


#########################Option 2 - use a mix of food balance sheets and FAO/SAU production data
##Steps
#1) Calculate proportion of species within each category
#2) Remove net exports (exports - imports) based food balance sheets and species proportions  
#3) Apply these proportions to estimated consumption 
#4) Multiply by the average nutrient composition of each group

##Considerations
#
####################Merge SAU and aquaculture data###########
commercial_catch = SAU %>% 
  filter(fishing_sector %in% c("Industrial", "Artisanal"),
         !end_use_type %in% c("Fishmeal and fish oil", "Discard"))

commercial_catch = commercial_catch %>%
  rename(country = fishing_entity,
         iso3c = country_ISO) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, fishing_sector, 
                end_use_type, tonnes) %>%
  mutate(production_sector = "capture")

commercial_catch_agg = commercial_catch %>%
  group_by(country, iso3c, year, genus_cat, scientific_name,
           common_name, production_sector) %>%
  summarize(tonnes = sum(tonnes))


###Proportion of artisanal catch retained in the country
commercial_catch_ggroup = commercial_catch %>% 
  group_by(iso3c, genus_cat, year, production_sector) %>% 
  summarize(tonnes_prod = sum(tonnes))

FAO_aquac = FAO_aquac %>%
  mutate(production_sector = "aquaculture") %>% 
  rename(common_name = ASFIS_species) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, 
                tonnes)

FAO_aquac$tonnes = as.numeric(FAO_aquac$tonnes)
FAO_aquac_agg = FAO_aquac %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, production_sector) %>% 
  summarize(tonnes = sum(tonnes))

FAO_aquac_agg = FAO_aquac_agg %>% 
  filter(year<2015)

total_commercial_prod = rbind(commercial_catch_agg, FAO_aquac_agg)

total_commercial_prod = as.data.frame(total_commercial_prod)

#Percent of production in each new category per country and production source
countries = unique(total_commercial_prod$iso3c)
genus = unique(total_commercial_prod$genus_cat)
years = unique(total_commercial_prod$year)

for(k in 1:length(years)){
  for(j in 1:length(countries)){
    for(i in 1:length(genus)){
      x = total_commercial_prod %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i],
               year == years[k])
      total_tonnes = sum(x$tonnes)
      x = x %>% 
        mutate(prop_catch = tonnes/total_tonnes)
      y = FAO_export %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i],
               year == years[k])
      y = y$tonnes[1]
      #print(sau_countries[j])
      #print(genus_categories[i])
      #print(y)
      x = x %>% 
        mutate(pred_exp_catch = prop_catch*y)
      x$pred_exp_catch[is.na(x$pred_exp_catch)]=0
      x = x %>% 
        mutate(pred_consumed_catch = tonnes - pred_exp_catch)
      if(j==1&i==1&k==1){
        total_consump = x}else{
          total_consump = rbind(total_consump, x)}
    }
  }
}

##############Set negatives to zero#############
total_consump = total_consump %>% 
  mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
         negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))

total_consumption = total_consump %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

#########Add imported fish#############

##Predict reexport
pred_reexport = total_consump %>% 
  group_by(year, iso3c, genus_cat) %>% 
  summarize(reexport = sum(negative_values)) 

##Remove reexport from imports
FAO_import = FAO_import %>% 
  filter(year %in% years)
FAO_import = left_join(FAO_import, pred_reexport, by=c("year", "iso3c", "genus_cat"))

##Add imports
FAO_pred_import = FAO_import %>% 
  mutate(pred_import = tonnes + reexport,
         pred_consumption = if_else(pred_import<0,0,pred_import),
         scientific_name = "NA",
         production_sector = "Imports",
         common_name = "NA") %>%
  rename(country=Country) %>% 
  drop_na(pred_consumption) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

total_consumption = rbind(total_consumption, FAO_pred_import)

##Add recreational and subsistance
consumption_catch = SAU %>% 
  filter(!fishing_sector %in% c("Industrial", "Artisanal"),
         !end_use_type %in% c("Fishmeal and fish oil", "Discard")) %>% 
  rename(country = fishing_entity,
         iso3c = country_ISO,
         pred_consumption = tonnes) %>% 
  mutate(production_sector = "capture") %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

total_consumption = rbind(total_consumption, consumption_catch)

####Calculate species proportions for each sector (capture and aquaculture)

##Aquaculture
aquac_spp_total = total_consumption %>% 
  filter(production_sector=="aquaculture") %>% 
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

aquac_spp_prop = total_consumption %>% 
  filter(production_sector=="aquaculture") %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes = sum(pred_consumption))

aquac_spp_prop = left_join(aquac_spp_prop, aquac_spp_total, by=c("iso3c", "year")) 

aquac_spp_prop = aquac_spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(iso3c, year, scientific_name, spp_prop)

##Capture
capture_spp_total = total_consumption %>% 
  filter(production_sector=="capture") %>% 
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

capture_spp_prop = total_consumption %>% 
  filter(production_sector=="capture") %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes = sum(pred_consumption))

capture_spp_prop = left_join(capture_spp_prop, capture_spp_total, by=c("iso3c", "year")) 

capture_spp_prop = capture_spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0)

##Save the results
#Aquaculture
write.csv(aquac_spp_prop, "MAR_aquaculture_spp_proportions_2010_2014_SAU.csv", row.names = FALSE)
#Capture
write.csv(capture_spp_prop, "MAR_capture_spp_proportions_2010_2014_SAU.csv", row.names = FALSE)




########################Now, lets do this using only FAO data############################################
###Fisrt, seperate marine production

FAO = FAO_prod %>% 
  filter(Production_source_detailed %in% c("Capture production", "Aquaculture production (marine)"),
         Production_area %in% c("Marine areas"))

#clean data
FAO = FAO %>% 
  select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, tonnes)  

############Use SAU to predict fishmeal coversion 

#First, calculate fishmeal production by species, year and country
fishmeal_catch = SAU %>% 
  filter(end_use_type %in% c("Fishmeal and fish oil")) %>%
  rename(country = fishing_entity,
         iso3c = country_ISO) %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes_fishmeal = sum(tonnes)) %>% 
  drop_na(iso3c)

##Next, calculate total catch for these species
total_catch = SAU %>% 
  rename(country = fishing_entity,
         iso3c = country_ISO) %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  drop_na(iso3c)

##Then, merge the two datasets
fishmeal_catch = left_join(fishmeal_catch, total_catch, by=c("iso3c", "year", "scientific_name"))

##Now calculate the proportion of catch for fishmeal and fishoil
fishmeal_catch = fishmeal_catch %>% 
  mutate(prop_fishmeal = tonnes_fishmeal/tonnes) %>% 
  select(iso3c, year, scientific_name, prop_fishmeal)

##############Apply proportions to FAO data
FAO = left_join(FAO, fishmeal_catch, by=c("iso3c", "year", "scientific_name"))

FAO = FAO %>% 
  mutate(prop_fishmeal = if_else(is.na(prop_fishmeal), 1, 1-prop_fishmeal)) %>% 
  rename(total_tonnes = tonnes) %>% 
  mutate(tonnes = total_tonnes*prop_fishmeal)

#Percent of production in each new category per country and production source
countries = unique(FAO$iso3c)
genus = unique(FAO$genus_cat)
years = unique(FAO$year)

for(k in 1:length(years)){
  for(j in 1:length(countries)){
    for(i in 1:length(genus)){
      x = FAO %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i],
               year == years[k])
      total_tonnes = sum(x$tonnes)
      x = x %>% 
        mutate(prop_catch = tonnes/total_tonnes)
      y = FAO_export %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i],
               year == years[k])
      y = y$tonnes[1]
      #print(sau_countries[j])
      #print(genus_categories[i])
      #print(y)
      x = x %>% 
        mutate(pred_exp_catch = prop_catch*y)
      x$pred_exp_catch[is.na(x$pred_exp_catch)]=0
      x = x %>% 
        mutate(pred_consumed_catch = tonnes - pred_exp_catch)
      if(j==1&i==1&k==1){
        total_consump_FAO = x}else{
          total_consump_FAO = rbind(total_consump_FAO, x)}
    }
  }
}

##############Set negatives to zero#############
total_consump_FAO = total_consump_FAO %>% 
  mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
         negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))

total_consumption_FAO = total_consump_FAO %>%
  select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, pred_consumption)
########Add imported fish#############
FAO_export = FAO_commod %>% filter(Element=="Food exports")
FAO_import = FAO_commod %>% filter(Element=="Food imports")

##Predict reexport
pred_reexport = total_consump_FAO %>% 
  group_by(year, iso3c, genus_cat) %>% 
  summarize(reexport = sum(negative_values)) 

##Remove reexport from imports
FAO_import = FAO_import %>% 
  filter(year %in% years)

FAO_import = left_join(FAO_import, pred_reexport, by=c("year", "iso3c", "genus_cat"))

##Add imports
FAO_pred_import = FAO_import %>% 
  mutate(pred_import = tonnes + reexport,
         pred_consumption = if_else(pred_import<0,0,pred_import),
         scientific_name = "NA",
         Production_source = "Imports",
         ASFIS_species = "NA") %>%
  rename(country=Country) %>% 
  drop_na(pred_consumption) %>% 
  dplyr::select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, pred_consumption)

total_consumption_FAO = rbind(total_consumption_FAO, FAO_pred_import)

####Calculate species proportions for each sector (capture and aquaculture)

##Aquaculture
aquac_spp_total_FAO = total_consumption_FAO %>% 
  filter(Production_source=="Aquaculture production") %>% 
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

aquac_spp_prop_FAO = total_consumption_FAO %>% 
  filter(Production_source=="Aquaculture production") %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes = sum(pred_consumption))

aquac_spp_prop_FAO = left_join(aquac_spp_prop_FAO, aquac_spp_total_FAO, by=c("iso3c", "year")) 

aquac_spp_prop_FAO = aquac_spp_prop_FAO %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0)

##Capture
capture_spp_total_FAO = total_consumption_FAO %>% 
  filter(Production_source=="Capture production") %>% 
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

capture_spp_prop_FAO = total_consumption_FAO %>% 
  filter(Production_source=="Capture production") %>% 
  group_by(iso3c, year, scientific_name) %>% 
  summarise(tonnes = sum(pred_consumption))

capture_spp_prop_FAO = left_join(capture_spp_prop_FAO, capture_spp_total_FAO, by=c("iso3c", "year")) 

capture_spp_prop_FAO = capture_spp_prop_FAO %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0)

##Save the results
#Aquaculture
write.csv(aquac_spp_prop_FAO, "MAR_aquaculture_spp_proportions_2010_2017_FAO.csv", row.names = FALSE)
#Capture
write.csv(capture_spp_prop_FAO, "MAR_capture_spp_proportions_2010_2017_FAO.csv", row.names = FALSE)


##Now, let's apply this to the consumption data 



###Species disaggregation - Capture fisheries
##Option 1 - use food balance sheets and group averages
##Steps
#1) Calculate proportion of capture vs aquaculture in each category
#2) Remove predicted aquaculture production in each group (assuming all products are exported  in the same proportion) 
#3) Calculate group proportions of total consumption
#4) Apply these proportions to estimated consumption 
#5) Multiply by the average nutrient composition of each group

##Considerations
#Hard to figure out how much is exported of capture vs aquaculture in each category. 
#It might be that all the aquaculture production is being exported
#Only problematic for categories with capture and aquaculture production


##Calculate proportion of consumption within each group and year
prop_genus_cat = FAO_commod %>% 
  filter(element == "Total food supply",
         year>2009) %>% 
  group_by(year, iso3c, genus_cat) %>% 
  summarise(tonnes = sum(tonnes))

total_genus_cat = FAO_commod %>% 
  filter(element == "Total food supply",
         year>2009) %>% 
  group_by(year, iso3c) %>% 
  summarise(tonnes_total = sum(tonnes))

prop_genus_cat = left_join(prop_genus_cat, total_genus_cat, by=c("year", "iso3c"))

prop_genus_cat = prop_genus_cat %>% 
  mutate(prop = tonnes/tonnes_total)

##Next, calculate proportion of aquaculture vs capture for each group and year
prop_genus_cat_source = FAO_prod %>% 
  filter(year>2009) %>% 
  group_by(year, iso3c, genus_cat, Production_source) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  drop_na(genus_cat)

total_genus_cat_source = FAO_prod %>% 
  filter(year>2009) %>% 
  group_by(year, iso3c, genus_cat) %>% 
  summarise(tonnes_total = sum(tonnes))

prop_genus_cat_source = left_join(prop_genus_cat_source, total_genus_cat_source, by=c("year", "iso3c", "genus_cat"))

prop_genus_cat_source = prop_genus_cat_source %>% 
  mutate(prop = tonnes/tonnes_total)


##Option 3 - use species value
##Steps
#1) Calculate fish price quantiles for each country and year
#2) Assume that a certain proportion of species are being eported based on their value (high value species going to wealthy countries)
#3) Estimate production retained domestically
#4) Apply these proportions to estimated consumption 
#5) Multiply by the average nutrient composition of each group




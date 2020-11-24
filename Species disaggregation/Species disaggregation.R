#########Species disaggregation for BFA paper###########

library(readr)
library(tidyverse)
QPMARBASE <- read_csv("Species disaggregation/QPMARBASE.csv")
QPMARHIGH <- read_csv("Species disaggregation/QPMARHIGH.csv")

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

##Read and clean FAO food balance import/export data
FAO_commod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/food_balance_FAO.csv")

FAO_commod = reshape2::melt(FAO_commod, id.vars = names(FAO_commod)[1:5], 
                            measure.vars = names(FAO_commod)[6:ncol(FAO_commod)])

FAO_commod = FAO_commod %>% 
  rename(year = variable, tonnes = value) %>% 
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

#FAO_export = FAO_commod %>% filter(element=="Exports")
#FAO_import = FAO_commod %>% filter(element=="Imports")


###SAU data
##Read SAU data
SAU = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2010_2014.csv")

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

###Species disaggregation - Capture fisheries
##Option 1 - use food balance sheets and group averages

##First calculate proportion of consumption within each group and year
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



##Option 2 - use a mix of food balance sheets and FAO production data

##Option 3 - use SAU data


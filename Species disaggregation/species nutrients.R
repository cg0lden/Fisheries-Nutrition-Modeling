library(readr)
library(tidyverse)
library(rfishbase)
library(fuzzyjoin)

##load data
fw_consump_combined_final <- read_csv("data/fw_consump_combined_final.csv")
MAR_spp_proportions_2010_2014_SAU <- read_csv("data/MAR_spp_proportions_2010_2014_SAU.csv")

##Read SAU data
SAU = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2010_2014.csv")

country_code_SAU <- read_csv("Species disaggregation/countries_ISO.csv")

SAU = left_join(SAU, country_code_SAU, by=c("fishing_entity"="missing_countries"))

##Load fishbase data to store taxa information for all species
fish_taxa <- read_csv("data/fish_taxa.csv")
family_info = fish_taxa %>% 
  dplyr::select(Genus, Family, Order) %>% 
  distinct(Genus, .keep_all = TRUE)

##Seaflife
seaflife_table = sealifebase %>% 
  select(Genus, Family, Order) %>% 
  distinct(Genus, .keep_all = TRUE)

taxa_table = rbind(family_info, seaflife_table) %>% 
  distinct(Genus, .keep_all = TRUE)

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


##Get unique species
fw_spp = fw_consump_combined_final %>% 
  rename("iso3c" = "country_code",
         "common_name" = "common_name_simple",
         "production_sector" = "source",
         "tonnes" = "consumption_tons_mid",
         "scientific_name" = "sci_name") %>% 
  dplyr::select(common_name, scientific_name, family, order)%>% 
  distinct(common_name, scientific_name, .keep_all = TRUE) %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  mutate(category = "freshwater")

mar_spp = MAR_spp_proportions_2010_2014_SAU %>% 
  select(common_name) %>% 
  distinct(common_name, .keep_all = TRUE)

mar_spp_SAU = SAU %>%
  select(common_name, scientific_name, functional_group) %>% 
  distinct(common_name, .keep_all = TRUE) %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE)

mar_spp_SAU = left_join(mar_spp_SAU, taxa_table, by=c("genus" = "Genus")) %>% 
  rename("family" = "Family",
         "order" = "Order",
         "category" = "functional_group")

mar_spp = left_join(mar_spp, mar_spp_SAU, by="common_name")

all_spp = rbind(mar_spp, fw_spp) %>% 
  distinct(common_name, .keep_all = TRUE) 
  
all_spp = all_spp %>%
  mutate(Iron = "Iron",
         Zinc = "Zinc",
         "Vitamin A" = "Vitamin A",
         "Vitamin B12" = "Vitamin B-12",
         "Omega-3 fatty acids" = "Omega-3 fatty acids",
         Protein = "Protein",
         edible = "edible")

all_spp = reshape2::melt(all_spp, id.vars = c("scientific_name", "common_name", "genus", "spp", "category", "family", "order"),
                            measure.vars = c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12",
                                             "Omega-3 fatty acids", "edible"))
all_spp = all_spp %>%
  rename(nutrient = variable) %>%
  select(-value)

#write.csv(all_spp, "fw_mar_spp.csv", row.names = FALSE)
######calculate average for species scientific names, genus and family#####################

#calculate mean values for species
afcd_spp = AFCD_long %>%
  group_by(species, variable) %>% 
  summarize(value = mean(value))

#calculate mean values for genus
afcd_genus = AFCD_long %>%
  group_by(genus, variable) %>% 
  summarize(value = mean(value))

#calculate mean values for family
afcd_family = AFCD_long %>%
  group_by(family, variable) %>% 
  summarize(value = mean(value))

#calculate mean values for order
afcd_order = AFCD_long %>%
  group_by(order, variable) %>% 
  summarize(value = mean(value))

#calculate mean values for common_name
#calculate mean values for order
afcd_comm_name = AFCD_long %>%
  group_by(Food.name.in.English, variable) %>% 
  summarize(value = mean(value))

afcd_comm_name$Food.name.in.English<- gsub("[()]","",afcd_comm_name$Food.name.in.English)
afcd_comm_name$Food.name.in.English<- gsub("*","",afcd_comm_name$Food.name.in.English)

afcd_name_1 = AFCD_long %>% 
  select(Food.name.in.English, variable, value) %>% 
  separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
  separate(common_name_1, c("common_name","procc_2", "prep_2"), ",", remove=FALSE) %>% 
  group_by(common_name, variable) %>% 
  summarize(value = mean(value))

afcd_name_2 = AFCD_long %>% 
  select(Food.name.in.English, variable, value) %>% 
  separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
  separate(common_name_1, c("common_name_2","procc_2", "prep_2"), ",", remove=FALSE) %>%
  separate(common_name_2, c("common_name","common_name_3", "prep_3"), " ", remove=FALSE) %>% 
  group_by(common_name, variable) %>% 
  summarize(value = mean(value))

afcd_name_3 = AFCD_long %>% 
  select(Food.name.in.English, variable, value) %>% 
  separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
  separate(common_name_1, c("common_name_2","procc_2", "prep_2"), ",", remove=FALSE) %>%
  separate(common_name_2, c("common_name_3","common_name", "prep_3"), " ", remove=FALSE) %>% 
  group_by(common_name, variable) %>% 
  summarize(value = mean(value))

afcd_name = rbind(afcd_name_1, afcd_name_2, afcd_name_3) %>%
  drop_na(common_name)

afcd_name$common_name<- gsub("[()]","",afcd_name$common_name)
afcd_name$common_name<- gsub("*","",afcd_name$common_name)


##########################Fill nutritional data by species scientific name 
#join databases
sau_nutrition = left_join(all_spp, afcd_spp, by=c("scientific_name" = "species", "nutrient" = "variable"))

sau_nutrition$value[sau_nutrition$value_md_fill==0]=NA

#find NAs and remove lobsters and crabs
missing = sau_nutrition %>% 
  filter(is.na(value)) %>% 
  select(-value)

sau_nutrition = sau_nutrition %>% 
  filter(!is.na(value))

###############Fill missing with genus #######################

#Join datasets 
missing = left_join(missing, afcd_genus, by=c("genus" = "genus", "nutrient"="variable"))
missing_genus = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_genus)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)


###############Fill missing with family #######################

#Join datasets
missing = left_join(missing, afcd_family, by=c("scientific_name" = "family", "nutrient"="variable"))
missing_family1 = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

#Join datasets
missing = left_join(missing, afcd_family, by=c("family" = "family", "nutrient"="variable"))
missing_family2 = missing %>% 
  filter(!is.na(value))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family2)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

###############Fill missing with common name #######################

#Join datasets
missing = left_join(missing, afcd_comm_name, by=c("common_name" = "Food.name.in.English", "nutrient"="variable"))
missing_name1 = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_name1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

##Fuzzy join
#Join datasets
afcd_comm_name$variable = as.character(afcd_comm_name$variable)
missing = regex_left_join(missing, afcd_comm_name, by=c("common_name" = "Food.name.in.English", "nutrient"="variable")) %>% 
  group_by(scientific_name, common_name, genus, spp, category, family, order, nutrient) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

missing_name2 = missing %>% 
 filter(!is.na(value))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_name2)

##Seperate remaining missing values
missing = missing %>% 
 filter(is.na(value)) %>% 
 dplyr::select(-value)

###############Fill missing with order #######################

#Join datasets
missing = left_join(missing, afcd_order, by=c("order" = "order", "nutrient"="variable"))
missing_order = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_order)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

########################## Fill missing data by category #############################

######calculate mean values for each category from compiled data
unique_cat = sau_nutrition %>% 
  group_by(category, nutrient) %>% 
  summarize(value = mean(value))

#Join datasets
missing = left_join(missing, unique_cat, by=c("category", "nutrient"))
missing_cat = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_cat)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

sau_nutrition = sau_nutrition %>% 
  filter(!nutrient=="edible")

write.csv(sau_nutrition, "data/mar_fw_spp_nutrients.csv", row.names = F)

  
library(readr)
library(tidyverse)
library(rfishbase)
library(fuzzyjoin)

##load data
fw_consump_combined_final <- read_csv("data/fw_consump_combined_final_wmodelcode.csv")
MAR_spp_proportions_2010_2014_SAU <- read_csv("data/MAR_spp_proportions_2014_SAU.csv")

##Read SAU data
SAU = read_csv("~/Google Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2010_2014.csv")

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
                         "Protein", 
                         "Calcium")) %>% 
  mutate(variable = recode(variable, 
                           "Edible.portion.coefficient" = "edible",
                           "Iron.total" = "Iron", 
                           "vitA" = "Vitamin A",
                           "Vitamin.B12" = "Vitamin B12",
                           "Fatty.acids.total.n3.polyunsaturated" = "Omega-3 fatty acids"))

AFCD_long$value = as.numeric(AFCD_long$value) 
AFCD_long$variable = as.character(AFCD_long$variable)

AFCD_long_raw = AFCD_long %>% 
  filter(Preparation %in% c("Raw", "raw", "r", "crudo", "cruda", "cru", "crua", "crudas")) 

AFCD_long_other = AFCD_long %>% 
  filter(!Preparation %in% c("Raw", "raw", "r", "crudo", "cruda", "cru", "crua", "crudas"))

##Get unique species
fw_spp = fw_consump_combined_final %>% 
  rename("iso3c" = "country_code",
         "common_name" = "common_name_simple",
         "production_sector" = "source",
         "tonnes" = "consumption_tons_mid",
         "scientific_name" = "sci_name") %>% 
  mutate(category = if_else(is.na(model_code), "freshwater", model_code)) %>% 
  dplyr::select(common_name, scientific_name, family, order, category)%>% 
  distinct(common_name, scientific_name, .keep_all = TRUE) %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  mutate(broad_category = "freshwater")


mar_spp = MAR_spp_proportions_2010_2014_SAU %>% 
  select(common_name) %>% 
  distinct(common_name, .keep_all = TRUE)

mar_spp_SAU = SAU %>%
  select(common_name, scientific_name, functional_group) %>% 
  distinct(common_name, .keep_all = TRUE) %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE)

mar_spp_SAU = left_join(mar_spp_SAU, taxa_table, by=c("genus" = "Genus")) %>% 
  rename("family" = "Family",
         "order" = "Order") %>% 
  select(-functional_group)

fao_prod_taxa_classification_20201216 <- read_csv("data/fao_prod_taxa_classification_20201216.csv") %>% 
  rename("scientific_name" = "SciName",
         "common_name" = "CommonName",
         "genus" = "Genus",
         "family" = "Family",
         "order" = "Order") %>% 
  mutate(spp="NA") %>% 
  select(common_name, scientific_name, genus, spp, family, order)

all_spp_mar = rbind(mar_spp_SAU, fao_prod_taxa_classification_20201216) %>% 
  distinct(common_name, .keep_all = TRUE)
  
mar_spp = left_join(mar_spp, all_spp_mar, by="common_name") %>% 
  mutate(broad_category = "marine")

all_spp_categories <- read_csv("data/all_spp_categories.csv")

mar_spp = left_join(mar_spp, all_spp_categories, by="scientific_name") %>% 
  rename("category" = "genus_cat")

all_spp = rbind(mar_spp, fw_spp) %>% 
  distinct(common_name, scientific_name, .keep_all = TRUE) 

##Insert Species groups
QP_groups <- read_csv("data/QP_groups.csv") %>% 
  mutate(scientific_name = "NA",
         genus = "NA",
         spp="NA",
         family = "NA",
         order = "NA",
         broad_category = "freshwater")

all_spp = rbind(all_spp, QP_groups)

all_spp = all_spp %>%
  mutate(Iron = "Iron",
         Zinc = "Zinc",
         "Vitamin A" = "Vitamin A",
         "Vitamin B12" = "Vitamin B-12",
         "Omega-3 fatty acids" = "Omega-3 fatty acids",
         Protein = "Protein",
         Calcium = "Calcium")

all_spp = reshape2::melt(all_spp, id.vars = c("scientific_name", "common_name", "genus", "spp", "category", "family", "order", "broad_category"),
                            measure.vars = c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12",
                                             "Omega-3 fatty acids", "Calcium"))
all_spp = all_spp %>%
  rename(nutrient = variable) %>%
  select(-value) %>% 
  mutate(scientific_name = tolower(scientific_name),
         common_name = tolower(common_name),
         genus = tolower(genus),
         family = tolower(family),
         order = tolower(order))

#write.csv(all_spp, "fw_mar_spp.csv", row.names = FALSE)


######calculate average for species scientific names, genus and family#####################

###########raw##############
#calculate mean values for species
afcd_spp_raw = AFCD_long_raw %>%
  group_by(species, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(species) %>% 
  mutate(species = tolower(species))

#calculate mean values for genus
afcd_genus_raw = AFCD_long_raw %>%
  group_by(genus, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(genus) %>% 
  mutate(genus = tolower(genus))

#calculate mean values for family
afcd_family_raw = AFCD_long_raw %>%
  group_by(family, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(family) %>% 
  mutate(family = tolower(family))

#calculate mean values for order
afcd_order_raw = AFCD_long_raw %>%
  group_by(order, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(order) %>% 
  mutate(order = tolower(order))

#calculate mean values for common_name
#calculate mean values for order
afcd_comm_name_raw = AFCD_long_raw %>%
  group_by(Food.name.in.English, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() 

afcd_comm_name_raw$Food.name.in.English<- gsub("[()]","",afcd_comm_name_raw$Food.name.in.English)
afcd_comm_name_raw$Food.name.in.English<- gsub("*","",afcd_comm_name_raw$Food.name.in.English)

###########cooked/frozen##############
#calculate mean values for species
afcd_spp = AFCD_long_other %>%
  group_by(species, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(species) %>% 
  mutate(species = tolower(species))

#calculate mean values for genus
afcd_genus = AFCD_long_other %>%
  group_by(genus, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(genus) %>% 
  mutate(genus = tolower(genus))

#calculate mean values for family
afcd_family = AFCD_long_other %>%
  group_by(family, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(family) %>% 
  mutate(family = tolower(family))

#calculate mean values for order
afcd_order = AFCD_long_other %>%
  group_by(order, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(order) %>% 
  mutate(order = tolower(order))

#calculate mean values for common_name
#calculate mean values for order
afcd_comm_name = AFCD_long_other %>%
  group_by(Food.name.in.English, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup()

afcd_comm_name$Food.name.in.English<- gsub("[()]","",afcd_comm_name$Food.name.in.English)
afcd_comm_name$Food.name.in.English<- gsub("*","",afcd_comm_name$Food.name.in.English)


# afcd_name_1 = AFCD_long %>% 
#   select(Food.name.in.English, variable, value) %>% 
#   separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
#   separate(common_name_1, c("common_name","procc_2", "prep_2"), ",", remove=FALSE) %>% 
#   group_by(common_name, variable) %>% 
#   summarize(value = mean(value))
# 
# afcd_name_2 = AFCD_long %>% 
#   select(Food.name.in.English, variable, value) %>% 
#   separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
#   separate(common_name_1, c("common_name_2","procc_2", "prep_2"), ",", remove=FALSE) %>%
#   separate(common_name_2, c("common_name","common_name_3", "prep_3"), " ", remove=FALSE) %>% 
#   group_by(common_name, variable) %>% 
#   summarize(value = mean(value))
# 
# afcd_name_3 = AFCD_long %>% 
#   select(Food.name.in.English, variable, value) %>% 
#   separate(Food.name.in.English, c("common_name_1","procc", "prep"), ";", remove=FALSE) %>% 
#   separate(common_name_1, c("common_name_2","procc_2", "prep_2"), ",", remove=FALSE) %>%
#   separate(common_name_2, c("common_name_3","common_name", "prep_3"), " ", remove=FALSE) %>% 
#   group_by(common_name, variable) %>% 
#   summarize(value = mean(value))
# 
# afcd_name = rbind(afcd_name_1, afcd_name_2, afcd_name_3) %>%
#   drop_na(common_name)
# 
# afcd_name$common_name<- gsub("[()]","",afcd_name$common_name)
# afcd_name$common_name<- gsub("*","",afcd_name$common_name)
# 

##########################Fill nutritional data by species scientific name 
#join databases
sau_nutrition = left_join(all_spp, afcd_spp_raw, by=c("scientific_name" = "species", "nutrient" = "variable"))

sau_nutrition$value[sau_nutrition$value_md_fill==0]=NA

#find NAs
missing = sau_nutrition %>% 
  filter(is.na(value)) %>% 
  select(-value)

sau_nutrition = sau_nutrition %>% 
  filter(!is.na(value))

#join databases
missing = left_join(missing, afcd_spp, by=c("scientific_name" = "species", "nutrient" = "variable"))

missing_spp = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_spp)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

###############Fill missing with genus #######################
#Join datasets 
missing = left_join(missing, afcd_genus_raw, by=c("genus" = "genus", "nutrient"="variable"))
missing_genus = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_genus)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

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
missing = left_join(missing, afcd_family_raw, by=c("scientific_name" = "family", "nutrient"="variable"))
missing_family1 = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

#Join datasets
missing = left_join(missing, afcd_family_raw, by=c("family" = "family", "nutrient"="variable"))
missing_family2 = missing %>% 
  filter(!is.na(value))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family2)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

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
missing = left_join(missing, afcd_comm_name_raw, by=c("common_name" = "Food.name.in.English", "nutrient"="variable"))
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
afcd_comm_name_raw$variable = as.character(afcd_comm_name_raw$variable)
missing = regex_left_join(missing, afcd_comm_name_raw, by=c("common_name" = "Food.name.in.English", "nutrient"="variable")) %>% 
  group_by(scientific_name, common_name, genus, spp, category, broad_category, family, order, nutrient) %>% 
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
  group_by(scientific_name, common_name, genus, spp, category, broad_category, family, order, nutrient) %>% 
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
missing = left_join(missing, afcd_order_raw, by=c("order" = "order", "nutrient"="variable"))
missing_order = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_order)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

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

########################## Fill missing data by broad category #############################

######calculate mean values for each category from compiled data
unique_cat = sau_nutrition %>% 
  group_by(broad_category, nutrient) %>% 
  summarize(value = mean(value))

#Join datasets
missing = left_join(missing, unique_cat, by=c("broad_category", "nutrient"))
missing_bcat = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_bcat)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

write.csv(sau_nutrition, "data/mar_fw_spp_nutrients.csv", row.names = F)

  
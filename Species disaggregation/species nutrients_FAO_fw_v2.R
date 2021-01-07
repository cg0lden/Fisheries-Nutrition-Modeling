library(readr)
library(tidyverse)
library(rfishbase)
library(fuzzyjoin)

##load data
fw_consump_combined_final <- read_csv("data/consump_fw_all_v2_noHIEScorrection_post.csv")
MAR_spp_proportions_2010_2014_FAO <- read_csv("data/MAR_spp_proportions_2014_FAO.csv")

all_spp = MAR_spp_proportions_2010_2014_FAO %>% 
  rename(category = genus_cat, 
         genus = Genus,
         family = Family,
         order = Order,
         broad_category = Production_area) %>% 
  mutate(scientific_name = tolower(scientific_name),
         common_name = tolower(common_name),
         genus = tolower(genus),
         family = tolower(family),
         order = tolower(order)) %>% 
  distinct(common_name, scientific_name, .keep_all = TRUE) %>% 
  select(-iso3c, -year, -tonnes, -spp_prop) 

##Insert Species groups
QP_groups <- read_csv("data/QP_groups.csv") %>% 
  mutate(scientific_name = "NA",
         genus = "NA",
         family = "NA",
         order = "NA",
         production_sector = "Aquaculture production",
         broad_category = "NA")

##Add freshwater species
fw_spp = fw_consump_combined_final %>% 
  rename("iso3c" = "country_code",
         "common_name" = "common_name_simple",
         "production_sector" = "source",
         "tonnes" = "consumption_tons_mid",
         "scientific_name" = "sci_name",
         "production_sector" = "source") %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  mutate(category = if_else(is.na(model_code), "Inland waters", model_code),
         scientific_name = tolower(scientific_name),
         common_name = tolower(common_name),
         genus = tolower(genus),
         family = tolower(family),
         order = tolower(order),
         broad_category = "freshwater") %>% 
  dplyr::select(common_name, scientific_name, family, order, category, 
                broad_category, production_sector, genus) %>% 
  distinct(common_name, scientific_name, .keep_all = TRUE)

all_spp = rbind(all_spp, QP_groups, fw_spp)


all_spp = all_spp %>%
  mutate(Iron = "Iron",
         Zinc = "Zinc",
         "Vitamin A" = "Vitamin A",
         "Vitamin B12" = "Vitamin B-12",
         "Omega-3 fatty acids" = "Omega-3 fatty acids",
         Protein = "Protein",
         Calcium = "Calcium")

all_spp = reshape2::melt(all_spp, id.vars = c("scientific_name", "common_name", "genus", "category", "family", "order", "production_sector", "broad_category"),
                         measure.vars = c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12",
                                          "Omega-3 fatty acids", "Calcium"))
all_spp = all_spp %>%
  rename(nutrient = variable) %>%
  select(-value)

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

######calculate average for species scientific names, genus and family#####################
#calculate mean values for species
afcd_spp = AFCD_long %>%
  group_by(species, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(species) %>% 
  mutate(species = tolower(species))

#calculate mean values for genus
afcd_genus = AFCD_long %>%
  group_by(genus, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(genus) %>% 
  mutate(genus = tolower(genus))

#calculate mean values for family
afcd_family = AFCD_long %>%
  group_by(family, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(family) %>% 
  mutate(family = tolower(family))

#calculate mean values for order
afcd_order = AFCD_long %>%
  group_by(order, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup() %>% 
  drop_na(order) %>% 
  mutate(order = tolower(order))

#calculate mean values for common_name
afcd_comm_name = AFCD_long %>%
  group_by(Food.name.in.English, variable) %>% 
  summarize(value = mean(value)) %>% 
  ungroup()

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

#find NAs
missing = sau_nutrition %>% 
  filter(is.na(value)) %>% 
  select(-value)

sau_nutrition = sau_nutrition %>% 
  filter(!is.na(value))

missing_spp = sau_nutrition
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
  group_by(scientific_name, common_name, genus, category, production_sector, broad_category, family, order, nutrient) %>% 
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

write.csv(sau_nutrition, "data/mar_fw_spp_nutrients_FAO_fw_v2.csv", row.names = F)


####Stats for all nutrients
fill_spp = missing_spp %>% mutate(taxa_fill = "Scientific name")
fill_family = rbind(missing_family1, missing_family2) %>% mutate(taxa_fill = "Family")
fill_genus = missing_genus %>% mutate(taxa_fill = "Genus")  
fill_name = rbind(missing_name1, missing_name2) %>% mutate(taxa_fill = "Common name")
fill_order = missing_order %>% mutate(taxa_fill = "Order")
fill_cat = rbind(missing_cat, missing_bcat) %>% mutate(taxa_fill = "GND Category")
fill_all = rbind(fill_spp, fill_family, fill_genus,
                 fill_name, fill_order, fill_cat)

fill_stats = fill_all %>% 
  group_by(taxa_fill, nutrient) %>% 
  count()
fill_stats$taxa_fill = factor(fill_stats$taxa_fill, 
                              levels= c("Scientific name", "Genus", "Family", 
                                        "Common name", "Order", "GND Category"))
ggplot(data=fill_stats)+
  geom_tile(aes(x=nutrient, y=taxa_fill, fill=n)) +
  geom_text(aes(x=nutrient, y=taxa_fill, label = n)) +
  labs(title="FAO/freshwater (2,143 unique species)")

fill_stats_wide = fill_stats %>% 
  spread(key=nutrient, value = n) %>% 
  mutate(Total = Calcium + Iron + `Omega-3 fatty acids` + 
           Protein + `Vitamin A` + `Vitamin B12` + Zinc)

write.csv(fill_stats_wide, "fill_stats.csv", row.names = F)



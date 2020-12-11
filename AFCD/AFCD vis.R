library(tidyverse)
library(readr)
library(rfishbase)
library(fuzzyjoin)

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
AFCD_long$variable = as.character(AFCD_long$variable)

AFCD_n3 = AFCD_long %>% 
  filter(variable=="Omega-3 fatty acids") %>% 
  group_by(Preparation) %>% 
  summarise(value = mean(value))

ggplot(data=AFCD_n3) +
  geom_point(aes(x=Preparation, y=value)) +
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

FAO_aquac = FAO_prod %>% 
  filter(year==2014,
         Production_source=="Aquaculture production",
         Production_area=="Marine areas")

aquac_total = FAO_aquac %>% 
  group_by(scientific_name, ASFIS_species, genus_cat) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  rename(common_name = ASFIS_species,
         category = genus_cat) %>% 
  mutate(source = "Aquaculture")

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

SAU_total = SAU %>% 
  filter(year==2014) %>% 
  group_by(scientific_name, common_name, functional_group) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  filter(!scientific_name %in% c("Miscellaneous marine crustaceans",
                                "Miscellaneous aquatic invertebrates",
                                "Miscellaneous diadromous fishes",
                                "Marine finfishes not identified",
                                "Marine fishes not identified",
                                "Marine pelagic fishes not identified")) %>% 
  arrange(desc(tonnes)) %>% 
  rename(category = functional_group) %>% 
  mutate(source = "Capture")

##Load fishbase data to store taxa information for all species
fish_taxa = load_taxa()
family_info = fish_taxa %>% 
  dplyr::select(Genus, Family, Order) %>% 
  distinct(Genus, .keep_all = TRUE)

##Seaflife
seaflife_table = sealifebase %>% 
  select(Genus, Family, Order) %>% 
  distinct(Genus, .keep_all = TRUE)

taxa_table = rbind(family_info, seaflife_table) %>% 
  distinct(Genus, .keep_all = TRUE)

#get unique species name from SAU (capture) and FAO (aquaculture)

unique_spp = rbind(SAU_total, aquac_total)
unique_spp = unique_spp %>%
  mutate(Iron = "Iron",
         Zinc = "Zinc",
         "Vitamin A" = "Vitamin A",
         "Vitamin B12" = "Vitamin B-12",
         "Omega-3 fatty acids" = "Omega-3 fatty acids",
         Protein = "Protein",
         edible = "edible")

unique_spp = reshape2::melt(unique_spp, id.vars = c("scientific_name", "common_name", "category", "tonnes", "source"),
                                measure.vars = c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12",
                                                 "Omega-3 fatty acids", "edible"))
unique_spp = unique_spp %>%
  rename(nutrient = variable) %>%
  select(-value)

unique_spp = unique_spp %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE)

unique_spp = left_join(unique_spp, taxa_table, by=c("genus" = "Genus"))

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
sau_nutrition = left_join(unique_spp, afcd_spp, by=c("scientific_name" = "species", "nutrient" = "variable"))

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
missing = left_join(missing, afcd_family, by=c("Family" = "family", "nutrient"="variable"))
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
missing = left_join(missing, afcd_name, by=c("common_name" = "common_name", "nutrient"="variable"))
missing_name1 = missing %>% 
  filter(!is.na(value)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_name1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value)) %>% 
  dplyr::select(-value)

# #Join datasets
# afcd_name$variable = as.character(afcd_name$variable)
# missing = fuzzy_left_join(missing, afcd_name, match_fun = str_detect, by=c("common_name" = "common_name", "nutrient"="variable"))
# missing_family2 = missing %>% 
#   filter(!is.na(value))
# 
# ##include filled values
# sau_nutrition = rbind(sau_nutrition, missing_family2)
# 
# ##Seperate remaining missing values
# missing = missing %>% 
#   filter(is.na(value)) %>% 
#   dplyr::select(-value)

###############Fill missing with order #######################

#Join datasets
missing = left_join(missing, afcd_order, by=c("Order" = "order", "nutrient"="variable"))
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

max.values = as.data.frame(c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12",
  "Omega-3 fatty acids"))
colnames(max.values) = "nutrient"
max.values$max.values = NA
max.values$nutrient = as.character(max.values$nutrient)

##Remove outlier
outlier.vitA = max(sau_nutrition$value[sau_nutrition$nutrient=="Vitamin A"])
sau_nutrition = sau_nutrition %>% 
  filter(!value==outlier.vitA)

outlier.iron = max(sau_nutrition$value[sau_nutrition$nutrient=="Iron"])
sau_nutrition = sau_nutrition %>% 
  filter(!value==outlier.iron)

outlier.zinc = max(sau_nutrition$value[sau_nutrition$nutrient=="Zinc"])
sau_nutrition = sau_nutrition %>% 
  filter(!value==outlier.zinc)

outlier.b12 = max(sau_nutrition$value[sau_nutrition$nutrient=="Vitamin B12"])
sau_nutrition = sau_nutrition %>% 
  filter(!value==outlier.b12)

#Calculate max values
for(n in 1:nrow(max.values)){
  max.values$max.values[n]=max(sau_nutrition$value[sau_nutrition$nutrient==max.values$nutrient[n]])
}

sau_nutrition = left_join(sau_nutrition, max.values, by="nutrient")

sau_nutrition = sau_nutrition %>% 
  mutate(stand_value = value/max.values)

#####Make plots
aquac_nutrition = sau_nutrition %>% 
  filter(source=="Aquaculture")

ggplot(data=aquac_nutrition) +
  geom_point(aes(x=tonnes, y=stand_value)) +
  facet_wrap(~nutrient)+
  labs(title="Aquaculture")

capture_nutrition = sau_nutrition %>% 
  filter(source=="Capture")

ggplot(data=capture_nutrition) +
  geom_point(aes(x=tonnes, y=stand_value)) +
  facet_wrap(~nutrient)+
  labs(title="Capture")


##single plots
plot.single = function(nut){
  sau_nut = sau_nutrition %>% 
    filter(nutrient==nut)

ggplot(data=sau_nut) +
  geom_point(aes(x=value, y=tonnes, colour=source))+
  ylab("Concentration")+
  xlab("Production (tonnes)")+
  theme_classic()
}
plot.single(nut="Iron")







# ########
# AF = rnorm(5000, mean=10, sd=5)
# AF = AF[AF>0]
# AF = as.data.frame(AF)
# AF$ASF = "Aquatic Foods"
# AF$source = "Available"
# colnames(AF) = c("value", "ASF", "source")
# 
# RM = rnorm(100, mean=10, sd=0.5)
# RM = RM[RM>0]
# RM = as.data.frame(RM)
# RM$ASF = "Red Meat"
# RM$source = "Available"
# colnames(RM) = c("value", "ASF", "source")
# 
# PO = rnorm(30, mean=12, sd=0.3)
# PO = PO[PO>0]
# PO = as.data.frame(PO)
# PO$ASF = "Poultry"
# PO$source = "Available"
# colnames(PO) = c("value", "ASF", "source")
# 
# dat = rbind(AF, RM, PO)
# 
# AF = rnorm(1000, mean=10, sd=2)
# AF = AF[AF>0]
# AF = as.data.frame(AF)
# AF$ASF = "Aquatic Foods"
# AF$source = "Consumed"
# colnames(AF) = c("value", "ASF", "source")
# 
# RM = rnorm(10, mean=10, sd=0.1)
# RM = RM[RM>0]
# RM = as.data.frame(RM)
# RM$ASF = "Red Meat"
# RM$source = "Consumed"
# colnames(RM) = c("value", "ASF", "source")
# 
# PO = rnorm(5, mean=12, sd=0.1)
# PO = PO[PO>0]
# PO = as.data.frame(PO)
# PO$ASF = "Poultry"
# PO$source = "Consumed"
# colnames(PO) = c("value", "ASF", "source")
# 
# dat = rbind(dat, AF, RM, PO)
# 
# AF = rnorm(1000, mean=10, sd=1)
# AF = AF[AF>0]
# AF = as.data.frame(AF)
# AF$ASF = "Aquatic Plants"
# AF$source = "Consumed"
# colnames(AF) = c("value", "ASF", "source")
# 
# RM = rnorm(1000, mean=20, sd=3)
# RM = RM[RM>0]
# RM = as.data.frame(RM)
# RM$ASF = "etc"
# RM$source = "Consumed"
# colnames(RM) = c("value", "ASF", "source")
# 
# PO = rnorm(1000, mean=15, sd=10)
# PO = PO[PO>0]
# PO = as.data.frame(PO)
# PO$ASF = "Crustaceans"
# PO$source = "Consumed"
# colnames(PO) = c("value", "ASF", "source")
# dat2 = rbind(AF, RM, PO)
# 
# 
# p1 = ggplot(data = dat)+
#   geom_point(aes(x=ASF, y=value))+
#   facet_wrap(~source, dir="v")+
#   theme_classic()+
#   ylab("Nutrient A")
# 
# p2 = ggplot(data=dat2)+
#   geom_point(aes(x=ASF, y=value))+
#   theme_classic()+
#   ylab("Nutrient A")
# 

#################################
#Other calculations

##Find top 10 ouliers in the data
variables = unique(AFCD_long$variable)[2:7]

for(n in 1:length(variables)){
  outliers = AFCD_long %>% 
    filter(variable == variables[n]) %>% 
    arrange(desc(value))
  outliers = outliers[1:10,]
  if(n==1){
    all_outliers = outliers
  }else{
    all_outliers = rbind(all_outliers, outliers)
  }
}

write.csv(all_outliers, "outliers.csv", row.names = F)

#########Species disaggregation for BFA paper###########

library(readr)
library(tidyverse)

##############Clean FAO data
##########Aquaculture data####################

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

##Seperate diadromous fish
FAO_prod_diad =  FAO_prod %>% 
  filter(ISSCAAP_division =="Diadromous fishes",
         year %in% c("2014"))
  

##Remove freshwater species
FAO_prod_MAR = FAO_prod %>% 
  filter(Production_area=="Marine areas",
         year %in% c("2014"))

FAO_prod_MAR$year = as.character(FAO_prod_MAR$year)
FAO_prod_MAR$year = as.numeric(FAO_prod_MAR$year)

##Read and clean FAO food balance import/export data

FAO_commod = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/food_balance_FAO_2020.csv")

#FAO_commod = read_csv("~/Google Drive/MPAs and Human Nutrition/Data/FAO/food_balance_FAO.csv")


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
  filter(year==2014)

FAO_export = FAO_commod %>% filter(Element=="Food exports")
FAO_import = FAO_commod %>% filter(Element=="Food imports")

FB_export = FAO_export %>% 
  group_by(iso3c) %>%
  summarise(tonnes_FB = sum(tonnes))

FB_import = FAO_import %>% 
  group_by(iso3c) %>%
  summarise(tonnes_FB = sum(tonnes))

##Read FAO import/export data
FAO_trade = read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/FAO/FAO_commodities_quantity.csv")

FAO_trade = reshape2::melt(FAO_trade, id.vars = names(FAO_trade)[1:17], 
                           measure.vars = names(FAO_trade)[18:ncol(FAO_trade)]) %>% 
  drop_na(value) %>% 
  rename(tonnes = value,
         year = variable)

#Convert to live weight
FAO_trade_fillets = FAO_trade %>% 
  filter(str_detect(commodity, fixed('fillet', ignore_case=TRUE))) %>% 
  mutate(tonnes = tonnes*2)

FAO_trade_whole = FAO_trade %>% 
  filter(!str_detect(commodity, fixed('fillet', ignore_case=TRUE)))


FAO_trade = rbind(FAO_trade_fillets, FAO_trade_whole)

##Seperate diadromous export total
diad_export = FAO_trade %>%
  filter(ISSCAAP_division=="Diadromous fishes",
         Trade_flow=="Exports",
         year %in% c("2014")) %>% 
  group_by(iso3c) %>% 
  summarise(export = sum(tonnes)) %>% 
  ungroup()

##Read and clean import/export data
all_spp_categories <- FAO_prod %>% 
  select(scientific_name, genus_cat, Production_area, ISSCAAP_division) %>% 
  rbind(read_csv("data/all_spp_categories.csv") %>% mutate(Production_area="Marine areas", ISSCAAP_division="Marine fishes")) %>% 
  distinct(scientific_name, .keep_all = TRUE) %>% 
  mutate(scientific_name = tolower(scientific_name))

fao_prod_taxa_classification_20201216 <- read_csv("data/fao_prod_taxa_classification_20201216.csv") %>% 
  select(SciName, CommonName, Genus, Family, Order, Class, Saltwater01, Fresh01) %>% 
  left_join(all_spp_categories, by=c("SciName" = "scientific_name"))

BFA_nutrition_species_trade <- read_csv("data/BFA_nutrition_species_trade.csv") %>% 
  left_join(fao_prod_taxa_classification_20201216, by=c("species" = "SciName"))

##Seperate Diadromous import
BFA_nutrition_species_trade_diad = BFA_nutrition_species_trade %>% 
  filter(ISSCAAP_division=="Diadromous fishes")

##Remove Freshwater species based on FAO
##Remove all fish with "freshwater" in the common name and tht is certainly not a saltwater species
BFA_nutrition_species_trade = BFA_nutrition_species_trade %>% 
  filter(!ISSCAAP_division=="Diadromous fishes",
         Production_area=="Marine areas")

##Imports
BFA_imports = BFA_nutrition_species_trade %>% 
  filter(flow=="imports") %>% 
  select(-X1, -flow) %>% 
  rename("quantity" = "quantity_t_live_weight",
         "country" = "country_name") %>% 
  filter(quantity>0.001)

BFA_imports_total = BFA_imports %>% 
  group_by(iso3c) %>%
  summarise(tonnes = sum(quantity))

BFA_imports = BFA_imports %>% 
  left_join(BFA_imports_total, by="iso3c") %>% 
  left_join(FB_import, by="iso3c") %>% 
  mutate(spp_prop = quantity/tonnes,
         imports = spp_prop*tonnes_FB) %>% 
  rename("common_name" = "CommonName",
         "scientific_name" = "species") %>% 
  select(country, iso3c, scientific_name, common_name, Genus, Family, Order, genus_cat, Production_area, imports)

##Imports - Diadromous
BFA_imports_diad = BFA_nutrition_species_trade_diad %>% 
  filter(flow=="imports") %>% 
  select(-X1, -flow) %>% 
  rename("quantity" = "quantity_t_live_weight",
         "country" = "country_name") %>% 
  filter(quantity>0.001)

BFA_imports_total_diad = BFA_imports_diad %>% 
  group_by(iso3c) %>%
  summarise(tonnes = sum(quantity))

BFA_imports_diad = BFA_imports_diad %>% 
  left_join(BFA_imports_total, by="iso3c") %>% 
  left_join(FB_import, by="iso3c") %>% 
  mutate(spp_prop = quantity/tonnes,
         imports = spp_prop*tonnes_FB) %>% 
  rename("common_name" = "CommonName",
         "scientific_name" = "species") %>% 
  select(country, iso3c, scientific_name, common_name, Genus, Family, Order, genus_cat, Production_area, imports)

BFA_imports_all = rbind(BFA_imports_diad, BFA_imports)

#########################Option 2 - use a mix of food balance sheets and FAO/SAU production data
##Steps
#1) Calculate proportion of species within each category
#2) Remove net exports (exports - imports) based food balance sheets and species proportions  
#3) Apply these proportions to estimated consumption 
#4) Multiply by the average nutrient composition of each group

####################aggregate and clean data###########
FAO_prod_MAR = FAO_prod_MAR %>% 
  rename(common_name = ASFIS_species,
         production_sector = Production_source,
         Family = Species_Family,
         Order = Species_Order) %>% 
  separate(scientific_name, c("Genus", "spp"), " ", remove=FALSE) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, 
                Genus, Family, Order, production_sector, Production_area, tonnes)

FAO_prod_MAR$tonnes = as.numeric(FAO_prod_MAR$tonnes)
FAO_prod_MAR = FAO_prod_MAR %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, Genus, Family, Order, production_sector, Production_area) %>% 
  summarize(tonnes = sum(tonnes)) %>% 
  ungroup()

total_commercial_prod = FAO_prod_MAR

#Percent of production in each new category per country and production source
countries = unique(total_commercial_prod$iso3c)
genus = unique(total_commercial_prod$genus_cat)

  for(j in 1:length(countries)){
    for(i in 1:length(genus)){
      x = total_commercial_prod %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i])
      total_tonnes = sum(x$tonnes)
      x = x %>% 
        mutate(prop_catch = tonnes/total_tonnes)
      y = FAO_export %>% 
        filter(iso3c == countries[j],
               genus_cat == genus[i])
      y = y$tonnes[1]
      #print(sau_countries[j])
      #print(genus_categories[i])
      #print(y)
      x = x %>% 
        mutate(pred_exp_catch = prop_catch*y)
      x$pred_exp_catch[is.na(x$pred_exp_catch)]=0
      x = x %>% 
        mutate(pred_consumed_catch = tonnes - pred_exp_catch)
      if(j==1&i==1){
        total_consump = x}else{
          total_consump = rbind(total_consump, x)}
    }
  }



##plot negative values
# ggplot(data=total_consump)+
#   geom_point(aes(x=iso3c, y=pred_consumed_catch))+
#   theme_classic()

total_consump = total_consump %>% 
  mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
         negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))

total_consumption = total_consump %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, 
                Genus, Family, Order,production_sector, Production_area, pred_consumption)

# ggplot(data=total_consumption)+
#   geom_point(aes(x=iso3c, y=pred_consumption))+
#   theme_classic()

#########Add imported fish#############

##Calculate predicted imoprt consumption by spp
FAO_import_consump = BFA_imports_all %>%
  mutate(production_sector = "import",
         year = 2014) %>%
  rename("tonnes" = "imports") %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, 
                Genus, Family, Order, production_sector, Production_area, tonnes)

FAO_import_consump$tonnes = as.numeric(FAO_import_consump$tonnes)
FAO_import_consump = FAO_import_consump %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, Genus, Family, Order, production_sector, Production_area) %>% 
  summarize(pred_consumption = sum(tonnes)) %>% 
  ungroup()

total_consumption = rbind(total_consumption, FAO_import_consump)

###################Now, lets do this for diadromous (production - exports)

FAO_prod_diad = FAO_prod_diad %>% 
  rename(common_name = ASFIS_species,
         production_sector = Production_source,
         Family = Species_Family,
         Order = Species_Order) %>% 
  separate(scientific_name, c("Genus", "spp"), " ", remove=FALSE) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, 
                Genus, Family, Order, production_sector, Production_area, tonnes)

FAO_prod_diad$tonnes = as.numeric(FAO_prod_diad$tonnes)
FAO_prod_diad = FAO_prod_diad %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, Genus, Family, Order, production_sector, Production_area) %>% 
  summarize(tonnes = sum(tonnes)) %>% 
  ungroup()

spp_total_diad = FAO_prod_diad %>%
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(tonnes))

total_consumption_diad = left_join(FAO_prod_diad, spp_total_diad, by=c("iso3c", "year")) %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total) %>% 
  left_join(diad_export, by="iso3c") %>% 
  mutate(export = if_else(is.na(export), 0, export),
         pred_export = export*spp_prop,
         pred_consumption = tonnes-pred_export,
         pred_consumption = if_else(pred_consumption<0,0,pred_consumption)) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, 
                Genus, Family, Order,production_sector, Production_area, pred_consumption) %>% 
  filter(pred_consumption>0)

total_consumption = rbind(total_consumption, total_consumption_diad)

###Total (including imports)
spp_total = total_consumption %>%
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

spp_prop = total_consumption %>%
  group_by(iso3c, year, genus_cat, scientific_name, 
           common_name, Genus, Family, Order, production_sector, Production_area) %>% 
  summarise(tonnes = sum(pred_consumption))

spp_prop = left_join(spp_prop, spp_total, by=c("iso3c", "year")) 

spp_prop = spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>%
  select(-tonnes_total)


##Save the results
#Total
write.csv(spp_prop, "data/MAR_spp_proportions_2014_FAO.csv", row.names = FALSE)

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


FAO_prod$year = as.character(FAO_prod$year)
FAO_prod$year = as.numeric(FAO_prod$year)

FAO_prod = FAO_prod %>% 
  filter(year==2014)

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
                            measure.vars = names(FAO_trade)[18:ncol(FAO_trade)])

FAO_trade = FAO_trade %>% 
  rename(year = variable, tonnes = value) %>% 
  drop_na(tonnes) %>%
  mutate(genus_cat = case_when(#Cephalopods
    FAOSTAT_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    FAOSTAT_group=="Demersal fish" ~ "Demersal Fish",
    FAOSTAT_group=="Demersal" ~ "Demersal Fish",
    FAOSTAT_group=="Demersal frozen" ~ "Demersal Fish",
    #Pelagic fish
    FAOSTAT_group=="Pelagic fish" ~ "Pelagic Fish",
    FAOSTAT_group=="Pelagic" ~ "Pelagic Fish",
    #Crustaceans
    FAOSTAT_group=="Crustaceans" ~ "Crustaceans",
    #Marine Fish; Other
    FAOSTAT_group=="Marine fish nei" ~ "Marine Fish; Other",
    #Moluscs; Other
    FAOSTAT_group=="Molluscs excl. cephalopods" ~ "Moluscs; Other",
    FAOSTAT_group=="Molluscs excl. ceph." ~ "Moluscs; Other",
    #Freshwater
    FAOSTAT_group=="Freshwater & diadromous fish" ~ "Freshwater Fish",
    FAOSTAT_group=="Freshwater & diadromous" ~ "Freshwater Fish",
    #Other aquatic animals (Aquatic Animals; Others)
    FAOSTAT_group=="Aquatic animals nei" ~ "Aquatic Animals; Others"))

#Convert to live weight
FAO_trade_fillets = FAO_trade %>% 
  filter(str_detect(commodity, fixed('fillet', ignore_case=TRUE))) %>% 
  mutate(tonnes = tonnes*2)

FAO_trade_whole = FAO_trade %>% 
  filter(!str_detect(commodity, fixed('fillet', ignore_case=TRUE)))


FAO_trade = rbind(FAO_trade_fillets, FAO_trade_whole)

##Imports
FAO_trade_import = FAO_trade %>% 
  filter(Trade_flow=="Imports",
         year %in% c("2014"))

##Clean names
FAO_trade_import = FAO_trade_import %>% 
  separate(Harmonized_group, c("common_name_2", "name_2", "name_3", "name_4"), ",", remove=FALSE) 

FAO_trade_import = FAO_trade_import %>% 
  separate(common_name_2, c("common_name","scientific_name"), '\\(', remove=FALSE) 

FAO_trade_import$scientific_name<- gsub(")","",FAO_trade_import$scientific_name)
FAO_trade_import$common_name<- gsub("Other ","",FAO_trade_import$common_name)

FAO_trade_import = FAO_trade_import %>% 
  select(iso3c, country, commodity, genus_cat, common_name, scientific_name, year, tonnes)

FAO_import_agg = FAO_trade_import %>% 
  group_by(common_name, scientific_name , genus_cat, iso3c, country, year) %>% 
  summarise(tonnes = sum(tonnes))

##Calculate proportion of spp in each category, country and year
#First calculate total in each category, country and year
FAO_import_agg_total = FAO_import_agg %>% 
  group_by(genus_cat, iso3c, year) %>% 
  summarise(tonnes_total = sum(tonnes))

##Merge files

FAO_import_agg = left_join(FAO_import_agg, FAO_import_agg_total, by=c("genus_cat", "iso3c", "year"))

FAO_import_agg = FAO_import_agg %>% 
  mutate(spp_prop = tonnes/tonnes_total)

FAO_import_final = FAO_import_agg %>% 
  select(genus_cat, iso3c, country,  year, common_name, scientific_name, tonnes, spp_prop)

FAO_import_final$year = as.character(FAO_import_final$year)

FAO_import_final = FAO_import_final %>% 
  filter(year==2014)

##Read and clean import/export data
fao_prod_taxa_classification_20201216 <- read_csv("data/fao_prod_taxa_classification_20201216.csv") %>% 
  select(SciName, CommonName, Saltwater01)

BFA_nutrition_species_trade <- read_csv("data/BFA_nutrition_species_trade.csv") %>% 
  left_join(fao_prod_taxa_classification_20201216, by=c("species" = "SciName"))

##Exports
BFA_exports = BFA_nutrition_species_trade %>% 
  filter(flow=="exports") %>% 
  select(-X1, -flow) %>% 
  rename("quantity" = "quantity_t_live_weight",
         "country" = "country_name")

BFA_exports_total = BFA_exports %>% 
  group_by(iso3c) %>%
  summarise(tonnes = sum(quantity))

BFA_exports = BFA_exports %>% 
  left_join(BFA_exports_total, by="iso3c") %>% 
  left_join(FB_export, by="iso3c") %>% 
  mutate(spp_prop = quantity/tonnes,
         exports = spp_prop*tonnes_FB) %>% 
  rename("common_name" = "CommonName",
         "scientific_name" = "species") %>% 
  select(country, iso3c, scientific_name, common_name, exports)

##Imports
BFA_imports = BFA_nutrition_species_trade %>% 
  filter(flow=="imports") %>% 
  select(-X1, -flow) %>% 
  rename("quantity" = "quantity_t_live_weight",
         "country" = "country_name")

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
  select(country, iso3c, scientific_name, common_name, imports)

#########################Option 2 - use a mix of food balance sheets and FAO/SAU production data
##Steps
#1) Calculate proportion of species within each category
#2) Remove net exports (exports - imports) based food balance sheets and species proportions  
#3) Apply these proportions to estimated consumption 
#4) Multiply by the average nutrient composition of each group

##Considerations
#
####################Merge SAU and aquaculture data###########
commercial_catch = FAO_prod %>%
  rename(country = fishing_entity) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, fishing_sector, 
                end_use_type, tonnes) %>%
  mutate(production_sector = "capture")

commercial_catch_agg = commercial_catch %>%
  group_by(country, iso3c, year, genus_cat, scientific_name,
           common_name, production_sector) %>%
  summarize(tonnes = sum(tonnes))


###Proportion of artisanal catch retained in the country 
##(assumed that all artisanal catch is part of the international seafood trade)
commercial_catch_ggroup = commercial_catch %>% 
  group_by(iso3c, genus_cat, year, production_sector) %>% 
  summarize(tonnes_prod = sum(tonnes))

FAO_prod = FAO_prod %>%
  mutate(production_sector = "aquaculture") %>% 
  rename(common_name = ASFIS_species) %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, 
                tonnes)

FAO_aquac$tonnes = as.numeric(FAO_aquac$tonnes)
FAO_prod_agg = FAO_prod %>% 
  group_by(country, iso3c, year, genus_cat, scientific_name, 
           common_name, production_sector) %>% 
  summarize(tonnes = sum(tonnes))

total_commercial_prod = FAO_prod_agg

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

##############Set negatives to zero#############
total_consump = total_consump %>% 
  mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
         negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))

total_consumption = total_consump %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

# ggplot(data=total_consumption)+
#   geom_point(aes(x=iso3c, y=pred_consumption))+
#   theme_classic()

##Add recreational and subsistance
consumption_catch = SAU %>% 
  filter(!fishing_sector %in% c("Industrial", "Artisanal"),
         !end_use_type %in% c("Fishmeal and fish oil", "Discard")) %>% 
  rename(country = fishing_entity,
         pred_consumption = tonnes) %>% 
  mutate(production_sector = "capture") %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, pred_consumption)

total_consumption = rbind(total_consumption, consumption_catch)

total_consumption = total_consumption %>% 
  select(iso3c, year, genus_cat, common_name, scientific_name, production_sector, pred_consumption)

#########Add imported fish#############

##Calculate predicted imoprt consumption by spp
FAO_import_consump = BFA_imports %>%
  mutate(production_sector = "import",
         year = 2014,
         genus_cat = "NA") %>%
  rename("tonnes" = "imports") %>% 
  dplyr::select(country, iso3c, year, genus_cat, scientific_name, common_name, production_sector, 
                tonnes)

FAO_import_consump$tonnes = as.numeric(FAO_import_consump$tonnes)
FAO_import_consump = FAO_import_consump %>% 
  group_by(iso3c, year, genus_cat, scientific_name, 
           common_name, production_sector) %>% 
  summarize(pred_consumption = sum(tonnes)) %>% 
  ungroup()

total_consumption = rbind(total_consumption, FAO_import_consump)

###################Now, lets do this for salmon(production + imports - exports)

##Salmon capture from SAU
SAU_salmon = SAU_salmon %>% 
  group_by(iso3c, scientific_name, common_name, genus_cat) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  mutate(source = "capture") %>% 
  ungroup()

##Salmon Aquaculture production from FAO
Salmon_aquac = Salmon_aquac %>% 
  rename(common_name = ASFIS_species) %>% 
  group_by(iso3c, scientific_name, common_name, genus_cat) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  mutate(source = "aquaculture") %>% 
  ungroup()

##Salmon Import
Salmon_import_final = Salmon_import %>%
  mutate(genus_cat = "Freshwater Fish",
         common_name = "Salmon", 
         scientific_name = "NA",
         source = "import") %>% 
  group_by(iso3c, scientific_name, common_name, genus_cat, source) %>%
  summarise(tonnes = sum(tonnes)) %>% 
  ungroup()

##Bind all production
salmon_prod = rbind(SAU_salmon, Salmon_aquac, Salmon_import_final)

##Salmon production by source (capture, aquaculture, imports)
salmon_prod = salmon_prod %>% 
  group_by(iso3c, source) %>% 
  summarise(tonnes=sum(tonnes)) %>% 
  ungroup()

##Total Salmon production by country and year
salmon_prod_total = salmon_prod %>% 
  group_by(iso3c) %>% 
  summarise(tonnes_total=sum(tonnes)) %>% 
  ungroup()

##Calculate proportion from each source
salmon_prod = left_join(salmon_prod, salmon_prod_total, by=c("iso3c")) %>% 
  mutate(prop_source = tonnes/tonnes_total)

##Calculate Salmon export per country and year
Salmon_export = Salmon_export %>% 
  group_by(iso3c) %>% 
  summarise(tonnes_export = sum(tonnes)) %>% 
  ungroup()

salmon_prod_exp = full_join(salmon_prod, Salmon_export, by=c("iso3c"))
  
salmon_prod_exp$prop_source[is.na(salmon_prod_exp$prop_source)] = 1
salmon_prod_exp$tonnes[is.na(salmon_prod_exp$tonnes)] = 0

salmon_prod_exp = salmon_prod_exp %>% 
  mutate(pred_export = prop_source*tonnes_export,
         pred_consumption = tonnes-pred_export)


##Set negatives to zero
salmon_prod_exp$pred_consumption[salmon_prod_exp$pred_consumption<0] = 0

salmon_consumption = salmon_prod_exp %>% 
  rename("production_sector" = "source") %>% 
  mutate(year=2014,
         genus_cat = "Freshwater Fish",
         scientific_name="NA",
         common_name = "Salmon") %>% 
  dplyr::select(iso3c, year, genus_cat, scientific_name, 
                common_name, production_sector, pred_consumption)
#salmon_consumption = as.data.frame(salmon_consumption)

total_consumption = rbind(total_consumption, salmon_consumption)

####Calculate species proportions for each sector (capture, aquaculture and total (including import))

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

###Total (including imports)
spp_total = total_consumption %>%
  group_by(iso3c, year) %>% 
  summarise(tonnes_total = sum(pred_consumption))

spp_prop = total_consumption %>%  
  group_by(iso3c, year, production_sector, common_name) %>% 
  summarise(tonnes = sum(pred_consumption))

spp_prop = left_join(spp_prop, spp_total, by=c("iso3c", "year")) 

spp_prop = spp_prop %>% 
  mutate(spp_prop = tonnes/tonnes_total) %>% 
  drop_na(spp_prop) %>% 
  filter(spp_prop>0) %>% 
  select(-tonnes_total)


##Save the results
#Total
write.csv(spp_prop, "data/MAR_spp_proportions_2014_SAU.csv", row.names = FALSE)
#Aquaculture
write.csv(aquac_spp_prop, "MAR_aquaculture_spp_proportions_2010_2014_SAU.csv", row.names = FALSE)
#Capture
write.csv(capture_spp_prop, "MAR_capture_spp_proportions_2010_2014_SAU.csv", row.names = FALSE)


##Compare food balance vs predicted consumption
FAO_commod_total = FAO_commod %>% 
  filter(Element=="Total food supply",
         year %in% c("2010", "2011", "2012", "2013", "2014")) %>% 
  group_by(iso3c, year) %>% 
  summarise(tonnes_FB = sum(tonnes))

FAO_commod_total$year = as.character(FAO_commod_total$year)
FAO_commod_total = left_join(FAO_commod_total, spp_total, by=c("iso3c", "year"))

FAO_commod_total = FAO_commod_total %>% 
  filter(year=="2014")

ggplot(FAO_commod_total, aes(x=tonnes_FB, y=tonnes_total, label = iso3c)) +
  geom_point() +
  theme_classic() +
  ylab("Predicted Seafood Consumption (tonnes)") +
  xlab("FB Seafood Consumption (tonnes)") +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(check_overlap = TRUE, nudge_y = 1000000)
####Disaggregate imports










# ########################Now, lets do this using only FAO data############################################
# ###Fisrt, seperate marine production
# 
# FAO = FAO_prod %>% 
#   filter(Production_source_detailed %in% c("Capture production", "Aquaculture production (marine)"),
#          Production_area %in% c("Marine areas"))
# 
# #clean data
# FAO = FAO %>% 
#   select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, tonnes)  
# 
# ############Use SAU to predict fishmeal coversion 
# 
# #First, calculate fishmeal production by species, year and country
# fishmeal_catch = SAU %>% 
#   filter(end_use_type %in% c("Fishmeal and fish oil")) %>%
#   rename(country = fishing_entity,
#          iso3c = country_ISO) %>% 
#   group_by(iso3c, year, scientific_name) %>% 
#   summarise(tonnes_fishmeal = sum(tonnes)) %>% 
#   drop_na(iso3c)
# 
# ##Next, calculate total catch for these species
# total_catch = SAU %>% 
#   rename(country = fishing_entity,
#          iso3c = country_ISO) %>% 
#   group_by(iso3c, year, scientific_name) %>% 
#   summarise(tonnes = sum(tonnes)) %>% 
#   drop_na(iso3c)
# 
# ##Then, merge the two datasets
# fishmeal_catch = left_join(fishmeal_catch, total_catch, by=c("iso3c", "year", "scientific_name"))
# 
# ##Now calculate the proportion of catch for fishmeal and fishoil
# fishmeal_catch = fishmeal_catch %>% 
#   mutate(prop_fishmeal = tonnes_fishmeal/tonnes) %>% 
#   select(iso3c, year, scientific_name, prop_fishmeal)
# 
# ##############Apply proportions to FAO data
# FAO = left_join(FAO, fishmeal_catch, by=c("iso3c", "year", "scientific_name"))
# 
# FAO = FAO %>% 
#   mutate(prop_fishmeal = if_else(is.na(prop_fishmeal), 1, 1-prop_fishmeal)) %>% 
#   rename(total_tonnes = tonnes) %>% 
#   mutate(tonnes = total_tonnes*prop_fishmeal)
# 
# #Percent of production in each new category per country and production source
# countries = unique(FAO$iso3c)
# genus = unique(FAO$genus_cat)
# years = unique(FAO$year)
# 
# for(k in 1:length(years)){
#   for(j in 1:length(countries)){
#     for(i in 1:length(genus)){
#       x = FAO %>% 
#         filter(iso3c == countries[j],
#                genus_cat == genus[i],
#                year == years[k])
#       total_tonnes = sum(x$tonnes)
#       x = x %>% 
#         mutate(prop_catch = tonnes/total_tonnes)
#       y = FAO_export %>% 
#         filter(iso3c == countries[j],
#                genus_cat == genus[i],
#                year == years[k])
#       y = y$tonnes[1]
#       #print(sau_countries[j])
#       #print(genus_categories[i])
#       #print(y)
#       x = x %>% 
#         mutate(pred_exp_catch = prop_catch*y)
#       x$pred_exp_catch[is.na(x$pred_exp_catch)]=0
#       x = x %>% 
#         mutate(pred_consumed_catch = tonnes - pred_exp_catch)
#       if(j==1&i==1&k==1){
#         total_consump_FAO = x}else{
#           total_consump_FAO = rbind(total_consump_FAO, x)}
#     }
#   }
# }
# 
# ##############Set negatives to zero#############
# total_consump_FAO = total_consump_FAO %>% 
#   mutate(pred_consumption = if_else(pred_consumed_catch<0,0,pred_consumed_catch),
#          negative_values = if_else(pred_consumed_catch<0,pred_consumed_catch,0))
# 
# total_consumption_FAO = total_consump_FAO %>%
#   select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, pred_consumption)
# ########Add imported fish#############
# FAO_export = FAO_commod %>% filter(Element=="Food exports")
# FAO_import = FAO_commod %>% filter(Element=="Food imports")
# 
# ##Predict reexport
# pred_reexport = total_consump_FAO %>% 
#   group_by(year, iso3c, genus_cat) %>% 
#   summarize(reexport = sum(negative_values)) 
# 
# ##Remove reexport from imports
# FAO_import = FAO_import %>% 
#   filter(year %in% years)
# 
# FAO_import = left_join(FAO_import, pred_reexport, by=c("year", "iso3c", "genus_cat"))
# 
# ##Add imports
# FAO_pred_import = FAO_import %>% 
#   mutate(pred_import = tonnes + reexport,
#          pred_consumption = if_else(pred_import<0,0,pred_import),
#          scientific_name = "NA",
#          Production_source = "Imports",
#          ASFIS_species = "NA") %>%
#   rename(country=Country) %>% 
#   drop_na(pred_consumption) %>% 
#   dplyr::select(country, iso3c, year, scientific_name, ASFIS_species, Production_source, genus_cat, pred_consumption)
# 
# total_consumption_FAO = rbind(total_consumption_FAO, FAO_pred_import)
# 
# ####Calculate species proportions for each sector (capture and aquaculture)
# 
# ##Aquaculture
# aquac_spp_total_FAO = total_consumption_FAO %>% 
#   filter(Production_source=="Aquaculture production") %>% 
#   group_by(iso3c, year) %>% 
#   summarise(tonnes_total = sum(pred_consumption))
# 
# aquac_spp_prop_FAO = total_consumption_FAO %>% 
#   filter(Production_source=="Aquaculture production") %>% 
#   group_by(iso3c, year, scientific_name) %>% 
#   summarise(tonnes = sum(pred_consumption))
# 
# aquac_spp_prop_FAO = left_join(aquac_spp_prop_FAO, aquac_spp_total_FAO, by=c("iso3c", "year")) 
# 
# aquac_spp_prop_FAO = aquac_spp_prop_FAO %>% 
#   mutate(spp_prop = tonnes/tonnes_total) %>% 
#   drop_na(spp_prop) %>% 
#   filter(spp_prop>0)
# 
# ##Capture
# capture_spp_total_FAO = total_consumption_FAO %>% 
#   filter(Production_source=="Capture production") %>% 
#   group_by(iso3c, year) %>% 
#   summarise(tonnes_total = sum(pred_consumption))
# 
# capture_spp_prop_FAO = total_consumption_FAO %>% 
#   filter(Production_source=="Capture production") %>% 
#   group_by(iso3c, year, scientific_name) %>% 
#   summarise(tonnes = sum(pred_consumption))
# 
# capture_spp_prop_FAO = left_join(capture_spp_prop_FAO, capture_spp_total_FAO, by=c("iso3c", "year")) 
# 
# capture_spp_prop_FAO = capture_spp_prop_FAO %>% 
#   mutate(spp_prop = tonnes/tonnes_total) %>% 
#   drop_na(spp_prop) %>% 
#   filter(spp_prop>0)
# 
# 
# ##Save the results
# #Aquaculture
# write.csv(aquac_spp_prop_FAO, "MAR_aquaculture_spp_proportions_2010_2017_FAO.csv", row.names = FALSE)
# #Capture
# write.csv(capture_spp_prop_FAO, "MAR_capture_spp_proportions_2010_2017_FAO.csv", row.names = FALSE)
# 
# 
# ##Com
# 
# 
# 

# ##Now, let's apply this to the consumption data 
# 
# 
# 
# ###Species disaggregation - Capture fisheries
# 
# ##Option 1 - use food balance sheets and group averages
# ##Steps
# #1) Calculate proportion of capture vs aquaculture in each category
# #2) Remove predicted aquaculture production in each group (assuming all products are exported  in the same proportion) 
# #3) Calculate group proportions of total consumption
# #4) Apply these proportions to estimated consumption 
# #5) Multiply by the average nutrient composition of each group
# 
# ##Considerations
# #Hard to figure out how much is exported of capture vs aquaculture in each category. 
# #It might be that all the aquaculture production is being exported
# #Only problematic for categories with capture and aquaculture production
# 
# 
# ##Calculate proportion of consumption within each group and year
# prop_genus_cat = FAO_commod %>% 
#   filter(element == "Total food supply",
#          year>2009) %>% 
#   group_by(year, iso3c, genus_cat) %>% 
#   summarise(tonnes = sum(tonnes))
# 
# total_genus_cat = FAO_commod %>% 
#   filter(element == "Total food supply",
#          year>2009) %>% 
#   group_by(year, iso3c) %>% 
#   summarise(tonnes_total = sum(tonnes))
# 
# prop_genus_cat = left_join(prop_genus_cat, total_genus_cat, by=c("year", "iso3c"))
# 
# prop_genus_cat = prop_genus_cat %>% 
#   mutate(prop = tonnes/tonnes_total)
# 
# ##Next, calculate proportion of aquaculture vs capture for each group and year
# prop_genus_cat_source = FAO_prod %>% 
#   filter(year>2009) %>% 
#   group_by(year, iso3c, genus_cat, Production_source) %>% 
#   summarise(tonnes = sum(tonnes)) %>% 
#   drop_na(genus_cat)
# 
# total_genus_cat_source = FAO_prod %>% 
#   filter(year>2009) %>% 
#   group_by(year, iso3c, genus_cat) %>% 
#   summarise(tonnes_total = sum(tonnes))
# 
# prop_genus_cat_source = left_join(prop_genus_cat_source, total_genus_cat_source, by=c("year", "iso3c", "genus_cat"))
# 
# prop_genus_cat_source = prop_genus_cat_source %>% 
#   mutate(prop = tonnes/tonnes_total)
# 
# 
# ##Option 3 - use species value
# ##Steps
# #1) Calculate fish price quantiles for each country and year
# #2) Assume that a certain proportion of species are being eported based on their value (high value species going to wealthy countries)
# #3) Estimate production retained domestically
# #4) Apply these proportions to estimated consumption 
# #5) Multiply by the average nutrient composition of each group
# 
# ##


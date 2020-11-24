###Species gaps

library(readr)
library(tidyverse)
library(rfishbase)

mar_afcd_gaps_with_landings <- read_csv("mar_afcd_gaps_with_landings.csv")

fish_taxa = load_taxa() 
fish_taxa = fish_taxa %>% 
  mutate(Species = tolower(Species),
         Genus = tolower(Genus),
         Family = tolower(Family),
         Order = tolower(Order),
         Class = tolower(Class))

mar_afcd_gaps_with_landings = left_join(mar_afcd_gaps_with_landings, fish_taxa, by=c("mar_taxa_name" = "Species"))

##From those with data wich families are well represented?
count_genus = mar_afcd_gaps_with_landings %>% 
  filter(missing==0) %>% 
  drop_na(Genus) %>% 
  count(Genus) %>% 
  rename(taxa = Genus)

##From those with data wich families are well represented?
count_family = mar_afcd_gaps_with_landings %>% 
  filter(missing==0) %>% 
  drop_na(Family) %>% 
  count(Family) %>% 
  rename(taxa = Family)
  
##From those with data wich orders are well represented?
count_order = mar_afcd_gaps_with_landings %>% 
  filter(missing==0) %>% 
  drop_na(Order) %>% 
  count(Order) %>% 
  rename(taxa = Order)

count_class = mar_afcd_gaps_with_landings %>% 
  filter(missing==0) %>% 
  drop_na(Class) %>% 
  count(Class) %>% 
  rename(taxa = Class)

taxa_count = rbind(count_genus, count_family, count_order, count_class) %>% 
  mutate(taxa = tolower(taxa))

###Which families we have data for?
genus_match = as.data.frame(unique(mar_afcd_gaps_with_landings$afcd_match_genus)) %>% 
  mutate(match = "genus") %>% 
  rename(taxa = "unique(mar_afcd_gaps_with_landings$afcd_match_genus)")

family_match = as.data.frame(unique(mar_afcd_gaps_with_landings$afcd_match_family)) %>% 
  mutate(match = "family") %>% 
  rename(taxa = "unique(mar_afcd_gaps_with_landings$afcd_match_family)")

order_match = as.data.frame(unique(mar_afcd_gaps_with_landings$afcd_match_order)) %>% 
  mutate(match = "order") %>% 
  rename(taxa = "unique(mar_afcd_gaps_with_landings$afcd_match_order)")

taxa_match = rbind(genus_match, family_match, order_match)

taxa_match = full_join(taxa_match, taxa_count, by="taxa")

###prioritizing
missing_afcd = mar_afcd_gaps_with_landings %>% 
  filter(missing==1) %>% 
  select(mar_taxa_name, tonnes, Genus, Family, Order)

missing_afcd = left_join(missing_afcd, taxa_match, by=c("mar_taxa_name" = "taxa"))

missing_afcd_match = missing_afcd %>% 
  filter(!is.na(n)) %>% 
  select(mar_taxa_name, tonnes, n)

write.csv(missing_afcd_match, "gaps_match.csv", row.names = FALSE)
##########
###Which families we have data for?
species_match = as.data.frame(unique(fish_taxa$Species)) %>% 
  mutate(match = "species") %>% 
  rename(taxa = "unique(fish_taxa$Species)")

genus_match = as.data.frame(unique(fish_taxa$Genus)) %>% 
  mutate(match = "genus") %>% 
  rename(taxa = "unique(fish_taxa$Genus)")

family_match = as.data.frame(unique(fish_taxa$Family)) %>% 
  mutate(match = "family") %>% 
  rename(taxa = "unique(fish_taxa$Family)")

order_match = as.data.frame(unique(fish_taxa$Order)) %>% 
  mutate(match = "order") %>% 
  rename(taxa = "unique(fish_taxa$Order)")

class_match = as.data.frame(unique(fish_taxa$Class)) %>% 
  mutate(match = "class") %>% 
  rename(taxa = "unique(fish_taxa$Class)")

taxa = rbind(species_match, genus_match, family_match, order_match, class_match)

##################

missing_afcd = mar_afcd_gaps_with_landings %>% 
  filter(missing==1)

missing_afcd = left_join(missing_afcd, taxa, by=c("mar_taxa_name" = "taxa"))

missing_afcd = missing_afcd %>% 
  select(mar_taxa_name, match, tonnes, Genus, Family, Order, Class)

missing_afcd = left_join(missing_afcd, count_genus, by=c("Genus"="taxa"))
missing_afcd = left_join(missing_afcd, count_family, by=c("Family"="taxa"))
missing_afcd = left_join(missing_afcd, count_order, by=c("Order"="taxa"))
missing_afcd = left_join(missing_afcd, count_class, by=c("Class"="taxa"))

missing_afcd = missing_afcd %>% 
  mutate(priority = if_else(is.na()))

write.csv(missing_afcd, "mar_afcd_gaps_priorities.csv", row.names = FALSE)

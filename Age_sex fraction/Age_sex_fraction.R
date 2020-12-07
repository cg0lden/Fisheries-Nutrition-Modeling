library(tidyverse)
library(readr)
library(rfishbase)
library(fuzzyjoin)

##Load GENuS data
genus_nutrient_supplies_by_age_sex_2011 <- readRDS("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/Nutrition Data/Human Dietary and Nutrition Data/GENuS Data/clean/genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds")

# plot_age_sex = function(nut, iso, gender){
#   genus_iso = genus_nutrient_supplies_by_age_sex_2011 %>% 
#     filter(iso3_use==iso,
#            nutrient==nut,
#            sex==gender) %>% 
#     mutate(Fraction = value_med/mean(value_med))
#   
#   ggplot(data=genus_iso) +
#     geom_point(aes(x=age_range, y=Fraction))+
#     theme_pubr()+
#     labs(title=paste(iso, nut, gender, sep=" - "))+
#     theme(axis.text.x = element_text(angle = 90))
# }
# 
# p1 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Males")  
# p2 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Females")  
# p3 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Children") 
# p4 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Males")  
# p5 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Females")  
# p6 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Children") 
# p7 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Males")  
# p8 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Females")  
# p9 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Children")
# 
# ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)
# 
# 
# p1 = plot_age_sex(nut="Iron", iso="BRA", gender="Males")  
# p2 = plot_age_sex(nut="Iron", iso="BRA", gender="Females")  
# p3 = plot_age_sex(nut="Iron", iso="BRA", gender="Children") 
# p4 = plot_age_sex(nut="Iron", iso="IND", gender="Males")  
# p5 = plot_age_sex(nut="Iron", iso="IND", gender="Females")  
# p6 = plot_age_sex(nut="Iron", iso="IND", gender="Children") 
# p7 = plot_age_sex(nut="Iron", iso="USA", gender="Males")  
# p8 = plot_age_sex(nut="Iron", iso="USA", gender="Females")  
# p9 = plot_age_sex(nut="Iron", iso="USA", gender="Children")
#  
# ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)

##Convert children into male/female

genus_children_male = genus_nutrient_supplies_by_age_sex_2011 %>% 
  filter(sex=="Children") %>% 
  mutate(sex = recode(sex, "Children" = "Males"))

genus_children_female = genus_nutrient_supplies_by_age_sex_2011 %>% 
  filter(sex=="Children") %>% 
  mutate(sex = recode(sex, "Children" = "Females"))

genus_nutrient = genus_nutrient_supplies_by_age_sex_2011 %>% 
  filter(!sex=="Children")

genus_nutrient = rbind(genus_nutrient, genus_children_male, genus_children_female)

nutrients = unique(genus_nutrient$nutrient)
countries = unique(genus_nutrient$iso3_use)
genders = unique(genus_nutrient$sex)

for(n in 1:length(countries)){
  for(k in 1:length(nutrients)){
    for(j in 1:length(genders)){
      genus_iso = genus_nutrient %>% 
        filter(iso3_use==countries[n],
               nutrient==nutrients[k],
               sex==genders[j]) %>% 
        mutate(fraction_med = value_med/mean(value_med),
               fraction_high = value_hi/mean(value_hi),
               fraction_low = value_lo/mean(value_lo))
      if(n==1&k==1&j==1){
        age_sex_fraction = genus_iso
      }else{
        age_sex_fraction = rbind(age_sex_fraction, genus_iso)
      }
    }
  }
}

#write.csv(age_sex_fraction, "data/age_sex_fraction_average_male_female.csv", row.names = F)

##Read model output data
BaseNutrients <- read_csv("data/BaseNutrients.csv") %>% 
  mutate(scenario = "base")
ScenarioNutrients <- read_csv("data/ScenarioNutrients.csv") %>% 
  mutate(scenario = "high")

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
                           "VitB" = "Vitamin B6",
                           "ZN" = "Zinc",
                           "PROT" = "Protein",
                           "DES" = "Calories")) %>% 
  select(-X12, -X11)
  

##Change to long format
NutrientsScen_long = reshape2::melt(Base_high_nutrients, id.vars=c("output", "countries", "products", "elements",     
                                                                   "nutrient_long", "units", "country_codes", "iso3c",        
                                                                   "product_codes", "food_abrev", "ele_codes", "nutrient",     
                                                                   "total", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("_", "", year))

##Now join databases
age_sex_fraction_clean = age_sex_fraction %>% 
  select(iso3_use, nutrient, age_range, sex, fraction_med) %>% 
  drop_na(fraction_med)

NutrientsScen_long = left_join(NutrientsScen_long, age_sex_fraction_clean, by=c("iso3c"="iso3_use", "nutrient"="nutrient"))
  
NutrientsScen_long = NutrientsScen_long %>% 
  mutate(pred_value = fraction_med*value)

#write.csv(NutrientsScen_long, "data/NutrientsScen_age_sex.csv", row.names = F)

##Kenia example
kenia = NutrientsScen_long %>% 
  filter(iso3c == "KEN")

write.csv(kenia, "data/NutrientsScen_age_sex_kenia.csv", row.names = F)

###Find missing countries

# missing_countries = NutrientsScen_long %>% 
#   filter(!nutrient %in% c("Omega-3 fatty acids", "DES", "VitA2"),
#          is.na(fraction_med))
# 
# AglinkCosimo2020countriesregions <- read_csv("Age_sex fraction/AglinkCosimo2020countriesregions.csv")
# x = as.data.frame(unique(missing_countries$iso3c))
# 
# x = left_join(x, AglinkCosimo2020countriesregions, by=c("unique(missing_countries$iso3c)"="Code"))
# 
# unique(x$`Country name`)

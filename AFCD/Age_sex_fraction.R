library(tidyverse)
library(readr)
library(rfishbase)
library(fuzzyjoin)

##Load GENuS data
genus_nutrient_supplies_by_age_sex_2011 <- readRDS("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/Data/Nutrition Data/Human Dietary and Nutrition Data/GENuS Data/clean/genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds")

plot_age_sex = function(nut, iso, gender){
  genus_iso = genus_nutrient_supplies_by_age_sex_2011 %>% 
    filter(iso3_use==iso,
           nutrient==nut,
           sex==gender) %>% 
    mutate(Fraction = value_med/mean(value_med))
  
  ggplot(data=genus_iso) +
    geom_point(aes(x=age_range, y=Fraction))+
    theme_pubr()+
    labs(title=paste(iso, nut, gender, sep=" - "))+
    theme(axis.text.x = element_text(angle = 90))
}

p1 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Males")  
p2 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Females")  
p3 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="BRA", gender="Children") 
p4 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Males")  
p5 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Females")  
p6 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="IND", gender="Children") 
p7 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Males")  
p8 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Females")  
p9 = plot_age_sex(nut="Polyunsaturated fatty acids", iso="USA", gender="Children")

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)


p1 = plot_age_sex(nut="Iron", iso="BRA", gender="Males")  
p2 = plot_age_sex(nut="Iron", iso="BRA", gender="Females")  
p3 = plot_age_sex(nut="Iron", iso="BRA", gender="Children") 
p4 = plot_age_sex(nut="Iron", iso="IND", gender="Males")  
p5 = plot_age_sex(nut="Iron", iso="IND", gender="Females")  
p6 = plot_age_sex(nut="Iron", iso="IND", gender="Children") 
p7 = plot_age_sex(nut="Iron", iso="USA", gender="Males")  
p8 = plot_age_sex(nut="Iron", iso="USA", gender="Females")  
p9 = plot_age_sex(nut="Iron", iso="USA", gender="Children")
 
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)

nutrients = unique(genus_nutrient_supplies_by_age_sex_2011$nutrient)
countries = unique(genus_nutrient_supplies_by_age_sex_2011$iso3_use)
genders = unique(genus_nutrient_supplies_by_age_sex_2011$sex)

for(n in 1:length(countries)){
  for(k in 1:length(nutrients)){
    for(j in 1:length(genders)){
      genus_iso = genus_nutrient_supplies_by_age_sex_2011 %>% 
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

write.csv(age_sex_fraction, "age_sex_fraction_average.csv", row.names = F)

###Compare disaggregation methods

Disaggregated_NutrientScen_grouped_FAO_fw <- read_csv("Disaggregated_NutrientScen_grouped_FAO_fishmeal.csv") %>% 
  rename(nutrient_supply_dis_FAO_fw = nutrient_supply_dis,
         nutrient_supply_FAO_fw = nutrient_supply)
Disaggregated_NutrientScen_grouped_FAO <- read_csv("Disaggregated_NutrientScen_grouped_FAO.csv") %>% 
  rename(nutrient_supply_dis_FAO = nutrient_supply_dis,
         nutrient_supply_FAO = nutrient_supply)
Disaggregated_NutrientScen_grouped <- read_csv("Disaggregated_NutrientScen_grouped.csv")


Disaggregated_all = left_join(Disaggregated_NutrientScen_grouped, Disaggregated_NutrientScen_grouped_FAO,
                              by=c("iso3c", "year", "units", "scenario", "nutrient")) %>% 
  left_join(Disaggregated_NutrientScen_grouped_FAO_fw, by=c("iso3c", "year", "units", "scenario", "nutrient")) %>% 
  filter(year==2030)


ggplot(data = Disaggregated_all) +
  geom_point(aes(x=nutrient_supply_dis, y=nutrient_supply_dis_FAO_fw)) +
  facet_wrap(~nutrient, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Nutrient supply - SAU disaggregation")+
  ylab("Nutrient supply - FAO/freshwater disaggregation") +
  theme_classic()+
  labs(title = 2030)

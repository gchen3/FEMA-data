library(tidyverse)

#### Merge MIT and GeoCorr data ####

setwd("C:/Users/Gang Chen/Dropbox/Gang Chen/Social vulnerability and RB/New data for PAR/")

load("Congress committee/senate_FEMA_committee.RData")
load("Congress committee/house_FEMA_committee.RData")
load("Geocorr/county_year_district.RData")

#### County political influence on FEMA committee data
names(county_year_district)
names(house_FEMA_committee)
names(senate_FEMA_committee)

head(county_year_district)
head(house_FEMA_committee)
head(senate_FEMA_committee)

county_FEMA_committee <- county_year_district %>%
  dplyr::left_join(house_FEMA_committee, by = join_by(district == district, state == state_name, year == year)) %>%
  dplyr::left_join(senate_FEMA_committee, by = join_by(year == year, state == state)) %>%
  mutate(house_homeland = ifelse(is.na(house_homeland), 0, house_homeland),
         house_budget = ifelse(is.na(house_budget), 0, house_budget),
         senate_budget = ifelse(is.na(senate_budget), 0, senate_budget),
         senate_homeland = ifelse(is.na(senate_homeland), 0, senate_homeland)) %>%
  group_by(year, state, county_name) %>%
  summarise(house_homeland = sum(house_homeland), 
            house_budget = sum(house_budget),
            senate_budget=sum(senate_budget), 
            senate_homeland=sum(senate_homeland))
  

names(county_FEMA_committee)
head(county_FEMA_committee)

save(county_FEMA_committee, file="county_FEMA_committee.RData")

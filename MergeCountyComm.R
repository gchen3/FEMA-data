library(tidyverse)

#### Merge MIT and GeoCorr data ####

senate_FEMA_committee <- readRDS("senate_FEMA_committee.RData")
house_FEMA_committee <- readRDS("house_FEMA_committee_long.RData")
county_year_district <- readRDS("county_year_district_long.RData")

#### County political influence on FEMA committee data
names(county_year_district)
names(house_FEMA_committee)
names(senate_FEMA_committee)

head(county_year_district)
head(house_FEMA_committee)
head(senate_FEMA_committee)

county_FEMA_committee <- county_year_district %>%
  dplyr::left_join(house_FEMA_committee, by = join_by(district == cd, state == state_name, year == year)) %>%
  dplyr::left_join(senate_FEMA_committee, by = join_by(year == year, state == state)) %>%
  mutate(house_homeland = ifelse(is.na(house_homeland), 0, house_homeland),
         house_budget = ifelse(is.na(house_budget), 0, house_budget),
         senate_budget = ifelse(is.na(senate_budget), 0, senate_budget),
         senate_homeland = ifelse(is.na(senate_homeland), 0, senate_homeland)) %>%
  group_by(year, state, county) %>%
  summarise(house_homeland = sum(house_homeland), 
            house_budget = sum(house_budget),
            senate_budget=sum(senate_budget), 
            senate_homeland=sum(senate_homeland))
  

names(county_FEMA_committee)
head(county_FEMA_committee)

save(county_FEMA_committee, file="county_FEMA_committee.RData")

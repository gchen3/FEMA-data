library(tidyverse)

#### Load and clean GeoCorr data ####
setwd("C:/Users/Gang Chen/Dropbox/Gang Chen/Social vulnerability and RB/New data for PAR/Geocorr")
geocorr2000 <- read_csv("geocorr2000_20MAR1513225.csv", skip=1) 
geocorr2014 <- read_csv("geocorr2014_2407908347.csv", skip=1)
geocorr2018 <- read_csv("geocorr2018_2407903800.csv", skip=1)

names(geocorr2000) <- str_replace_all(names(geocorr2000), c(" " = "_")) %>% tolower()
names(geocorr2014) <- str_replace_all(names(geocorr2014), c(" " = "_")) %>% tolower()
names(geocorr2018) <- str_replace_all(names(geocorr2018), c(" " = "_")) %>% tolower()

names(geocorr2000)
names(geocorr2014)
names(geocorr2018)

geocorr_all <- geocorr2000 %>%
  rename("session_108" = "108th_congressional_district_(2002)", 
         "session_109" = "cong_district_-_109th_(2004)",
         "state" = "state_postal_code") %>%
  select(session_108, session_109, state, county, cntyname) %>%
  dplyr::full_join(geocorr2018, by = join_by(county == county_code), relationship = "many-to-many") %>%
  rename("session_111" = "111th_congressional_district",
         "session_113" = "113th_congressional_district",
         "session_114" = "114th_congressional_district",
         "session_115" = "115th_congressional_district",
         "session_116" = "116th_congressional_district") %>%
  select(starts_with("session"), state, county, cntyname, state_abbreviation, county_name, -state_abbreviation, -cntyname)
  
county_year_district <- geocorr_all %>%
  gather(key = "session", value = "district", "session_108":"session_116") %>%
  arrange(county) %>%
  mutate(
      year1 = case_when (
        session == "session_108" ~ 2003,
        session == "session_109" ~ 2005,
        session == "session_111" ~ 2009,
        session == "session_113" ~ 2013,
        session == "session_114" ~ 2015,
        session == "session_115" ~ 2017,
        session == "session_116" ~ 2019)) %>%
    mutate(
      year2 = case_when (
      session == "session_108" ~ 2004,
      session == "session_109" ~ 2006,
      session == "session_111" ~ 2010,
      session == "session_113" ~ 2014,
      session == "session_114" ~ 2016,
      session == "session_115" ~ 2018,
      session == "session_116" ~ 2020)) %>%
    mutate(
      year3 = case_when (
      session == "session_109" ~ 2007,
      session == "session_111" ~ 2011)) %>%
    mutate(
      year4 = case_when (
      session == "session_109" ~ 2008,
      session == "session_111" ~ 2012)) %>%
  gather(key = "session", value = "year", year1:year4) %>%
  arrange(year, state, county, district) %>%
  distinct() %>%
  mutate(district=as.numeric(district)) %>%
  select(-session)
  
  

# Note That:
# Congressional District Maps
# The congressional district map suites include three map types 
# (national, state-based, and congressional district-based) that depict the congressional districts 
# in effect for these congressional sessions.
# 
# The congressional district boundaries are the same for the 109th, 
# 110th, 111th, and 112th Congresses of the United States, except for in Georgia and Texas where 
# there were changes between the 109th and 110th Congresses.

save(county_year_district, file="county_year_district.RData")

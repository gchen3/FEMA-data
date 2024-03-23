library(readxl)
library(tidyverse)
library(Matrix) 
library(dplyr)


#### Clean House data ####

filepath <- "C:/Users/Gang Chen/Dropbox/Gang Chen/Social vulnerability and RB/New data for PAR/Congress committee"

setwd(filepath)

house_assign <- read_excel(paste0(filepath, "/house_assignments_103-115-3.xls"))
house_member <- read_excel(paste0(filepath, "/house_members_103-115-2.xlsx"), skip=1)

names(house_assign)<- str_replace_all(names(house_assign), c(" " = "_")) %>% tolower()
names(house_member)<- str_replace_all(names(house_member), c(" " = "_")) %>% tolower()

names(house_assign)
names(house_member)

house_FEMA_committee <-  house_assign %>%
  filter(congress>=108)  %>%
  dplyr::rename("id"="id_#") %>%
  select(congress, committee_code, id, name, committee_name, state, cd, state_name) %>%
  filter(committee_code==251 | committee_code==115) %>%       ## 251=Homeland Security; 115=budget committee
  tidyr::spread(key = "committee_code", value = "committee_code") %>%
  dplyr::rename("homeland"="251", "budget"="115") %>%
  mutate(
  homeland_com = case_when(homeland == "251" ~ 1, 
                           TRUE ~ 0))           %>%
  mutate(
    budget_com = case_when(budget == "115" ~ 1, 
                           TRUE ~ 0))     %>%
  select(-homeland, -budget) %>%
  mutate(
    year1 = case_when (
      congress == "108" ~ 2003,
      congress == "109" ~ 2005,
      congress == "110" ~ 2007,
      congress == "111" ~ 2009,
      congress == "112" ~ 2011,
      congress == "113" ~ 2013,
      congress == "114" ~ 2015,
      congress == "115" ~ 2017,
      congress == "116" ~ 2019))%>%
  mutate(
    year2 = case_when (
      congress == "108" ~ 2004,
      congress == "109" ~ 2006,
      congress == "110" ~ 2008,
      congress == "111" ~ 2010,
      congress == "112" ~ 2012,
      congress == "113" ~ 2014,
      congress == "114" ~ 2016,
      congress == "115" ~ 2018,
      congress == "116" ~ 2020)) %>%
  gather(key = "congress", value = "year", year1:year2)  %>%
  select(-congress) %>%
  select(year, state_name, cd, homeland_com, budget_com, committee_name, name, id) %>%
  arrange(year, cd, committee_name, homeland_com, budget_com) %>%
  mutate(district = as.numeric(cd)) %>%
  group_by(year, state_name, district) %>%
  summarise(house_homeland=sum(homeland_com), house_budget=sum(budget_com))
  
#### the same district has multiple lines if they have more than one rep serving in more than one committee

head(house_FEMA_committee)
view(house_FEMA_committee)

save(house_FEMA_committee, file="house_FEMA_committee.RData")

####Clean Senate Data####

senate_assign <- read_excel(paste0(filepath, "/senate_assignments_103-115-3.xls"))
senate_member <- read_excel(paste0(filepath, "/senators_103-115-2.xls"), skip=1)

names(senate_member)<- str_replace_all(names(senate_member), c(" " = "_")) %>% tolower()
names(senate_assign)<- str_replace_all(names(senate_assign), c(" " = "_")) %>% tolower()

names(senate_member)
names(senate_assign)

senate_FEMA_committee <- senate_assign %>%
  select(congress, committee_code, "id_#", name, state_code, district, state_name) %>%
  dplyr::rename("id" = "id_#", state=state_name) %>%
  filter(committee_code==344 | committee_code==316) %>%       ## 344 =Homeland Security and governmental affairs; 316=budget committee
  arrange(congress, committee_code, id) %>%
  distinct() %>%
  tidyr::spread(key = committee_code, value = name) %>%
  dplyr::rename("homeland" = "344", "budget" ="316") %>%
  mutate(
    homeland_committee = case_when (
      is.na(homeland) ~ 0,
      TRUE ~ 1),
    budget_committee = case_when (
      is.na(budget) ~ 0,
      TRUE ~ 1))            %>%
  select(-homeland, -budget) %>%
  filter(congress>=108)  %>%
  mutate(
    year1 = case_when (
      congress == "108" ~ 2003,
      congress == "109" ~ 2005,
      congress == "110" ~ 2007,
      congress == "111" ~ 2009,
      congress == "112" ~ 2011,
      congress == "113" ~ 2013,
      congress == "114" ~ 2015,
      congress == "115" ~ 2017,
      congress == "116" ~ 2019))%>%
  mutate(
    year2 = case_when (
      congress == "108" ~ 2004,
      congress == "109" ~ 2006,
      congress == "110" ~ 2008,
      congress == "111" ~ 2010,
      congress == "112" ~ 2012,
      congress == "113" ~ 2014,
      congress == "114" ~ 2016,
      congress == "115" ~ 2018,
      congress == "116" ~ 2020)) %>%
  gather(key = "congress", value = "year", year1:year2)%>%
  arrange(year, state, district, homeland_committee, budget_committee) %>%
  select(-congress, -state_code, -district) %>%
  group_by(year, state) %>%
  summarise(senate_budget=sum(budget_committee), senate_homeland=sum(homeland_committee))


head(senate_FEMA_committee)
view(senate_FEMA_committee)

save(senate_FEMA_committee, file="senate_FEMA_committee.RData")






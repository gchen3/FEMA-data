library(tidyverse)
library(readxl)

filepath <- "C:/Users/Gang Chen/Dropbox/Gang Chen/Social vulnerability and RB/New data for PAR/ASPEP/"

setwd(filepath)

aspep_2016 <- read.csv("aspep_counties_1993_2016.csv")

aspep_county <- aspep_2016 %>%
  filter(year>2005) %>%
  select(state, county_name, year, government_function, full_time_employees)

for (i in 17:22) {
 assign(paste0("emp",i), read_excel(paste0(i,"emp",".xlsx")))
 }
emp17 <- select(emp17, -"Part-time Hours", -"Part-time Hours Data Flag", -"Full-time Equivalent Employees") %>%
  mutate(year=2017)
emp18 <- select(emp18, -"Part-time Hours", -"Part-time Hours Data Flag", -"Full-time Equivalent Employees") %>%
  mutate(year=2018)
emp19 <- emp19 %>% mutate(year=2019)
emp20 <- emp20 %>% mutate(year=2020)
emp21 <- emp21 %>% mutate(year=2021)
emp22 <- emp22 %>% mutate(year=2022)

emp_new <- rbind(emp17, emp18, emp19, emp20, emp21, emp22)
names(emp_new) <- str_replace_all(names(emp_new), " ", "_") %>% tolower()

emp_full_years <- emp_new %>%
  rename(full_time_employees = "full-time_employees") %>%
  filter(type_of_government == "County") %>%
  select(state, county_name, year, government_function, full_time_employees) %>%
  rename(gov_func = government_function,
         full_time = "full_time_employees") %>%
  tidyr::spread(key = gov_func, value = full_time) %>%
  rename("fiscal" = "Financial Administration",
          "total" = "Total - All Government Employment Functions") %>%
  mutate(per_fiscal = fiscal/total)

save(emp_full_years, file="emp_full_years.RData")


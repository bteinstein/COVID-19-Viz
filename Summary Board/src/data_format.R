library(tidyverse)

# get updated data
source("Summary Board/src/get_data.R")


############ Top Five Country by higest number of active cases ######
daily_top_three_cc_country <- 
  data_covid %>% 
  rename(country = `Country/Region`) %>% 
  select(country, date, confirmed,iso2c) %>% 
  filter(iso2c != "Cruise ship") %>% 
  group_by(date, country) %>% 
  summarise(c_confirmed = sum(confirmed)) %>%
  ungroup() %>% 
  group_by(date) %>% 
  arrange(desc(c_confirmed), .by_group = T) %>% 
  top_n(3) %>% 
  ungroup() %>% 
  left_join(data_covid %>% 
              select(`Country/Region`, iso2c) %>% 
              mutate(c_code = tolower(iso2c)) %>% 
              rename(country = `Country/Region`) %>% 
              distinct())

daily_top_four_cc_country <- 
  data_covid %>% 
  rename(country = `Country/Region`) %>% 
  select(country, date, confirmed,iso2c) %>% 
  filter(iso2c != "Cruise ship") %>% 
  group_by(date, country) %>% 
  summarise(c_confirmed = sum(confirmed)) %>%
  ungroup() %>% 
  group_by(date) %>% 
  arrange(desc(c_confirmed), .by_group = T) %>% 
  top_n(4) %>% 
  ungroup() %>% 
  left_join(data_covid %>% 
              select(`Country/Region`, iso2c) %>% 
              mutate(c_code = tolower(iso2c)) %>% 
              rename(country = `Country/Region`) %>% 
              distinct())

daily_top_four_cc_country_mod <- 
  data_covid %>% 
  rename(country = `Country/Region`) %>% 
  select(country, date, confirmed,iso2c) %>% 
  filter(iso2c != "Cruise ship") %>% 
  group_by(date, country) %>% 
  summarise(c_confirmed = sum(confirmed)) %>%
  ungroup() %>% 
  group_by(date) %>% 
  arrange(desc(c_confirmed), .by_group = T) %>% 
  top_n(4) %>% 
  slice(1:4) %>% 
  ungroup() 

############ Daily Summary #########
data_daily <-   data_covid %>% 
  group_by(date) %>% 
  summarise(Infected = sum(confirmed),
            Recovered = sum(recovered),
            Deceased  = sum(deceased),
            Active    = sum(active)) 

data_dsummary <- data_daily %>% 
  pivot_longer(names_to = "status", 
               cols = c(Infected, Recovered, Deceased, Active)) 

########## Country Summery ############
daily_csummary <- 
  data_covid %>% 
  rename(country = `Country/Region`) %>% 
  select(country, date, confirmed,iso2c) %>% 
  filter(iso2c != "Cruise ship") %>% 
  group_by(date, country) %>% 
  summarise(c_confirmed = sum(confirmed)) %>%
  ungroup()


rm(data_covid)

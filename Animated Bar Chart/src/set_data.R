library(dplyr)

#Load clean data
confirmed_cases <- rio::import('./data/cln_confirm_c_df.csv')
recovered_cases <- rio::import('./data/cln_recovered_df.csv')

# recovery_cases <- rio::import('./data/')
# Converting Wide to Long
lng_confirmed_cases <- pivot_longer(data = confirmed_cases %>% 
                                      filter(!Country %in% c("Diamond Princess", "MS Zaandam" ) ), # Dropping "Diamond Princess" "MS Zaandam"  
                                    cols = c(-Country, -iso2c, -long, -lat, -income, -region),
                                    names_to = "Days",
                                    values_to = "Confirmed Cases" ) %>% 
  mutate(Date = as.Date(Days, "%m/%d/%y"))


lng_recovered_cases <- pivot_longer(data = recovered_cases %>% 
                                      filter(!Country %in% c("Diamond Princess", "MS Zaandam" ) ), # Dropping "Diamond Princess" "MS Zaandam"  
                                    cols = c(-Country, -iso2c, -long, -lat, -income, -region),
                                    names_to = "Days",
                                    values_to = "Recovered Cases" ) %>% 
  mutate(Date = as.Date(Days, "%m/%d/%y"))

# Feature cleaning
p_data <- lng_confirmed_cases %>% filter(`Confirmed Cases` > 1)
r_data <- lng_recovered_cases %>% filter(`Recovered Cases` > 1)



df = p_data %>% 
  group_by(Date) %>% 
  arrange(Date, desc(`Confirmed Cases`)) %>% 
  mutate(order = 1:n()) %>% 
  filter(order <= 10) %>% 
  mutate(Date2 = Date, 
         c_code = tolower(iso2c))

df_f = df %>% select(Date, Country, `Confirmed Cases`, c_code,iso2c, order) %>% 
        rename(cc = `Confirmed Cases`)

cum_cc <- lng_confirmed_cases %>% 
  group_by(Date) %>% 
  summarise(cc = sum(`Confirmed Cases`)) %>% 
  rename(dt = Date)

cum_rc <- lng_recovered_cases %>% 
  group_by(Date) %>% 
  summarise(rc = sum(`Recovered Cases`)) %>% 
  rename(dt = Date)

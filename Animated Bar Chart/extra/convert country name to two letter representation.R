library(rvest)
library(dplyr)


webpage <- read_html("https://www.iban.com/country-codes")

country_code <- webpage %>% 
  html_nodes("table") %>% 
  html_table() %>% .[[1]]

# some data cleaning
county_code %>% 
  mutate(Country= trimws(gsub(pattern = "\\(.*\\)", replacement = '', x = Country)),
         code = tolower(`Alpha-2 code`)) %>% 
  select(Country, code) 

readr::write_csv(x = country_code, path = "./data/country_code.csv")
%>% 
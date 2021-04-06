# Scrapping data from Web About corona virus

## Trying various sources

# Github Repository of Johns Hopkins CSSE
# CSSE COVID-19 Dataset - Time Series Data
csse_raw_url <-  'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_'
c_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
d_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
r_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  
  
confirm_c_df = readr::read_csv(c_url) # confirmed cases
deaths_df <-readr::read_csv(d_url) # death cases
recovered_df <-readr::read_csv(r_url) # recovered cases

# confirm_c_df = readr::read_csv(paste0(csse_raw_url,'confirmed_global.csv')) # confirmed cases
# deaths_df <-readr::read_csv(paste0(csse_raw_url,'deaths_global.csv')) # confirmed cases
# recovered_df <-readr::read_csv(paste0(csse_raw_url,'recovered_global.csv')) # confirmed cases
# recovered_df <-readr::read_csv(paste0(csse_raw_url,'recovered_global.csv')) # confirmed cases


# names(confirm_c_df)

# Cleaning 
library(wbstats)
library(tidyverse)

list_of_country <- unique(confirm_c_df$`Country/Region`)
# list_of_country
# total country with confirm cases
length(list_of_country)

# Country info from world bank
library(wbstats)
wbcntr <- wbstats::wbcountries()

# quick function 
`%nin%` = Negate(`%in%`)
sum(list_of_country %nin% wbcntr$country)


cntr_names <-  data.frame(
  stringsAsFactors = FALSE,
  Country = c("Bahamas","Brunei",
              "Congo (Brazzaville)","Congo (Kinshasa)","Diamond Princess",
              "Czechia","Egypt","Gambia","Holy See","Iran",
              "Korea, South","Kyrgyzstan","Russia","Saint Lucia",
              "Saint Vincent and the Grenadines","Slovakia","Taiwan*","US",
              "Venezuela","Syria","Laos","Saint Kitts and Nevis",
              "Burma","MS Zaandam"),
  Country_Name = c("Bahamas, The",
                   "Brunei Darussalam","Congo, Rep.",
                   "Congo, Dem. Rep.","Diamond Princess","Czech Republic",
                   "Egypt, Arab Rep.","Gambia, The","Vatican City",
                   "Iran, Islamic Rep.","Korea, Rep.",
                   "Kyrgyz Republic","Russian Federation","St. Lucia",
                   "St. Vincent and the Grenadines","Slovak Republic",
                   "Taiwan, China","United States","Venezuela, RB",
                   "Syrian Arab Republic","Lao PDR",
                   "St. Kitts and Nevis","Myanmar","MS Zaandam")
)




# [1] 24 ----- Name miss-match
### 1. Confirmed Cases
confirm_c_df$Country = confirm_c_df$`Country/Region`

confirm_c_df$Country[which(confirm_c_df$Country %in% cntr_names$Country)] <- 
  cntr_names$Country_Name[which( cntr_names$Country %in% confirm_c_df$`Country/Region` )]

### 2. Death
deaths_df$Country = deaths_df$`Country/Region`

deaths_df$Country[which(deaths_df$Country %in% cntr_names$Country)] <- 
  cntr_names$Country_Name[which( cntr_names$Country %in% deaths_df$`Country/Region` )]

### 3. Recovery
recovered_df$Country = recovered_df$`Country/Region`

recovered_df$Country[which(recovered_df$Country %in% cntr_names$Country)] <- 
  cntr_names$Country_Name[which( cntr_names$Country %in% recovered_df$`Country/Region` )]


###############################################################
sum(confirm_c_df$Country %nin% wbcntr$country)
# sum(cntr_names$Country_Name %nin% wbcntr$country)
confirm_c_df$Country[which(confirm_c_df$Country %nin% wbcntr$country)]
confirm_c_df$`Country/Region`[which(confirm_c_df$Country %nin% wbcntr$country)]
#"Diamond Princess" "Holy See"         "MS Zaandam"   

length(unique(confirm_c_df$Country))
length(unique(confirm_c_df$`Country/Region`))
##################################################################



## Data Aggregation
cln_confirm_c_df <- (
  confirm_c_df %>% 
    select(-c(Long, Lat, `Province/State`,`Country/Region`))   %>% 
    group_by(Country) %>% 
    summarise_all(list(sum))  %>% 
    ungroup()
) %>% left_join(wbcntr %>% select(country,iso2c, long, lat, income, region),
                by = c('Country' = 'country')) %>% 
  select(Country,iso2c, long, lat, income, region, everything())
head(cln_confirm_c_df)


cln_confirm_c_df$Country[is.na(cln_confirm_c_df$iso2c)]
cln_confirm_c_df$iso2c[cln_confirm_c_df$Country == 'Vatican City'] = 'VA'

write_csv(cln_confirm_c_df, path = "./data/cln_confirm_c_df.csv")
########
cln_deaths_df <- (
  deaths_df %>% 
    select(-c(Long, Lat, `Province/State`,`Country/Region`))   %>% 
    group_by(Country) %>% 
    summarise_all(list(sum))  %>% 
    ungroup()
) %>% left_join(wbcntr %>% select(country,iso2c, long, lat, income, region),
                by = c('Country' = 'country')) %>% 
  select(Country,iso2c, long, lat, income, region, everything())
head(cln_deaths_df)


cln_deaths_df$Country[is.na(cln_deaths_df$iso2c)]
cln_deaths_df$iso2c[cln_deaths_df$Country == 'Vatican City'] = 'VA'

write_csv(cln_deaths_df, path = "./data/cln_deaths_df.csv")

######
cln_recovered_df <- (
  recovered_df %>% 
    select(-c(Long, Lat, `Province/State`,`Country/Region`))   %>% 
    group_by(Country) %>% 
    summarise_all(list(sum))  %>% 
    ungroup()
) %>% left_join(wbcntr %>% select(country,iso2c, long, lat, income, region),
                by = c('Country' = 'country')) %>% 
  select(Country,iso2c, long, lat, income, region, everything())
head(cln_recovered_df)


cln_recovered_df$Country[is.na(cln_recovered_df$iso2c)]
cln_recovered_df$iso2c[cln_recovered_df$Country == 'Vatican City'] = 'VA'

write_csv(cln_recovered_df, path = "./data/cln_recovered_df.csv")

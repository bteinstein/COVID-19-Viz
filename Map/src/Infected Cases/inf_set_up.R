# Libraries
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(extrafont)
loadfonts(device = "win", quiet = T)


# Load Data
# get updated data
source("Summary Board/src/data_format.R")
rm(daily_top_four_cc_country, daily_top_three_cc_country)

# Countries Name normalization
world_map_d <- map_data("world")

# Align country name
covid_cntr <- unique(daily_csummary$country)
wrd_contr <- unique(world_map_d$region)

`%nin%` = Negate(`%in%`)
sum(covid_cntr %nin% wrd_contr)
covid_cntr[covid_cntr %nin% wrd_contr]

daily_csummary$country <- plyr::revalue(
  x = daily_csummary$country, 
  replace = c(
    "Antigua and Barbuda" = "Barbuda", 
    "Burma" = "Myanmar",  
    "Cabo Verde" = "Cape Verde",  
    "Congo (Brazzaville)" = "Republic of Congo",  
    "Congo (Kinshasa)" = "Democratic Republic of the Congo",  
    "Cote d'Ivoire" = "Ivory Coast",  
    "Czechia" = "Czech Republic",  
    "Eswatini" = "Swaziland",  
    "Holy See" = "Vatican",  
    "Korea, South" = "South Korea",  
    "North Macedonia" = "Macedonia",  
    "Saint Kitts and Nevis" = "Nevis",  
    "Saint Vincent and the Grenadines" = "Grenadines",  
    "Trinidad and Tobago" = "Tobago",  
    "United Kingdom" = "UK",  
    "US" = "USA",  
    "West Bank and Gaza" = "Palestine" 
  ) 
)

daily_top_four_cc_country_mod$country <- plyr::revalue(
  x = daily_top_four_cc_country_mod$country, 
  replace = c( 
    "Korea, South" = "South Korea",  
    "US" = "USA" 
  ) 
)

rm(covid_cntr, wrd_contr, `%nin%`)

########## Data Marging ##############
df_map <- right_join(daily_csummary, world_map_d, by = c("country"="region"))
df_map <- df_map[is.na(df_map$c_confirmed)==FALSE, ]


cnames <- aggregate(cbind(long, lat) ~ region, data=world_map_d, FUN=mean) %>% 
  semi_join(daily_csummary, by = c('region'='country'))

cnames[cnames$region == 'USA', 2:3] <- c(-100.03200, 38)
cnames[cnames$region == 'Italy', c('long','lat')] = c(16, 46)
cnames[cnames$region == 'Spain', 'lat'] = 40 
cnames[cnames$region == 'Germany',c('long','lat')] = c(10.3, 53.5 )
cnames[cnames$region == 'South Korea', c('long','lat')] = c(129, 38)
cnames[cnames$region == 'Japan', 'lat'] = 30
cnames[cnames$region == 'France', c('long','lat')] = c(-5, 46.7)


d_label <- daily_top_four_cc_country_mod %>% 
  left_join(cnames, by = c("country"="region")) %>% 
  mutate(label = paste0(country,"\n",format(c_confirmed, nsmall=0, big.mark=",")))



##################### LEGEND ###############
c_levels <- c(0,   10,   100,   500,  1000,   10000,
              50000, 100000, 200000, 300000, 400000, 700000)
c_labels <- c('0 - 10',    '11 - 100',   '101 - 500',   
              '501 - 1000', '1001 - 10000', '10001 - 50000', 
              '50001 - 100000', '100001 - 200000',  
              '200001 - 300000', '300001 - 400000', '400001 - 700000')

df_map$confirmed <- factor(
  cut(df_map$c_confirmed, c_levels, include.lowest = T),
  labels = c_labels
)


########### Clean #############
rm(cnames, daily_top_four_cc_country_mod, daily_csummary) 

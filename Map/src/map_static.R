{
  library(tidyverse)
library(gganimate)
library(ggthemes)
library(RColorBrewer)
library(viridis)
# remotes::install_github("thomasp85/transformr")

# get updated data
source("Summary Board/src/data_format.R")
rm(daily_top_four_cc_country, daily_top_three_cc_country, data_daily, data_dsummary)

# Check Data Set
head(daily_csummary)

# Some legend
# something about the gradient
quantile(daily_csummary$c_confirmed, seq(0, 1, .10))

# Align country name
covid_cntr <- unique(daily_csummary$country)

world_map_d <- map_data("world")
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

}


{
# Merge dataset
df.map1 <- right_join(daily_csummary, world_map_d, by = c("country"="region"))
df.map1$c_confirmed[is.na(df.map1$c_confirmed)] = 0




#df.map2 <- merge(world_map_d, daily_csummary,  by.y = "country",by.x = "region")
}


# length(unique(df.map1$country))
# [1] 252
# > length(unique(df.map2$country))
# [1] 182

# sum(is.na(df.map1$c_confirmed))  # 10945
# sum(is.na(df.map2$c_confirmed))  # 0

df.map1[is.na(df.map1$c_confirmed==0), ]


# df.map1$country[df.map1$country[ %nin% wrd_contr] 
# l_color = c("#350f5c","#5e2045","#f28751","#ffb253","#feca50","#feff5c")
# 
# # colo_s <- c("black","#01032a","#f6bb31","#942919","#b3200b","#d5260b","#F03B20","#f64b31")
# # colo_s <- c("black","#01032a","#662900","#942919","#b3200b","#d5260b","#F03B20","#ff0000")
# "#fcbba1"





# First Plot
p = ggplot(data = world_map_d,aes(x = long, y = lat, group = group) ) + 
  geom_polygon(fill="black", color="black") +
  # geom_polygon(data = df.map1 %>% filter(date=="2020-02-11"),
  geom_polygon(data = df.map1 %>% filter(date < "2020-01-25"),
               aes(fill = confirmed, group = group),
               color = "black") +
  expand_limits(x = c(min(df.map1$long), max(df.map1$long)), 
                y = c(min(df.map1$lat), max(df.map1$lat))) +
  # scale_fill_gradientn(colours =colo_s, limits = c(0,600000), na.value = "#f8c7b7") +
  scale_fill_manual(values = colo_s,
                    name = "Confirmed Cases")+
  theme_hc(style = "darkunica") + 
  theme(#plot.background = element_rect(fill = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom") +
  xlab(" ") + ylab(" ") +
  labs(title = "COVID-19 World Pandemic",
       subtitle = "Cumulative Confirmed Cases {frame_time}") #+
# transition_time(date)

p


map2 <- ggplot(data = df.map1 %>% filter(date=="2020-04-05")) +
  geom_polygon(aes(x = long, y = lat, fill = c_confirmed, group = group),
               color = "grey50") +
  expand_limits(x = c(min(df.map1$long), max(df.map1$long)), 
                y = c(min(df.map1$lat), max(df.map1$lat))) +
  scale_fill_viridis(option = "magma", 
                     name = "Confirmed Cases",
                     limits = c(0,600000),
                     direction = 1,end = 0.85, 
                     na.value = "#f8c7b7",
                     labels = scales::,
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = FALSE,
                       title.position = 'top',
                       title.hjust = 0.5,
                       title.vjust = 0.5
                     )) + 
  # theme_hc() + 
  theme_void( )+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # panel.background = element_rect(fill="black"),
        legend.position = "bottom") +
  xlab(" ") + ylab(" ") +
  labs(title = "COVID-19 World Pandemic") #+#, 
# subtitle = "Cumulative Confirmed Cases {frame_time}") #+
# transition_time(date)


map2


library(tidyverse)
# library(gganimate)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(extrafont)
library(showtext)
font_import()
loadfonts(device = "win", quiet = T)
loadfonts(device = "pdf", quiet = T)
loadfonts(device = "postscript", quiet = T)

## 
font.add(family = 'Letter Gothic Std', )

# get updated data
source("Summary Board/src/data_format.R")
rm(daily_top_four_cc_country, daily_top_three_cc_country)


#######################  PREP DATA  ===== df_map
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

# Merge dataset
df_map <- right_join(daily_csummary, world_map_d, by = c("country"="region"))
# df_map$c_confirmed[is.na(df_map1$c_confirmed)] = 0 
df_map <- df_map[is.na(df_map$c_confirmed)==FALSE, ]


cnames <- aggregate(cbind(long, lat) ~ region, data=world_map_d, FUN=mean) %>% 
  semi_join(daily_csummary, by = c('region'='country'))

cnames[cnames$region == 'USA', 2:3] <- c(-100.03200, 38)

d_label <- daily_top_four_cc_country_mod %>% 
  left_join(cnames, by = c("country"="region")) %>% 
  mutate(label = paste0(country,"\n",c_confirmed))

##################### LEGEND
c_levels <- c(0,   10,   100,   500,  1000,   10000,
                      50000, 100000, 200000, 300000, 400000, 700000)
c_labels <- c('0 - 10',    '11 - 100',   '101 - 500',   '501 - 1000',
              '1001 - 10000', '10001 - 50000', '50001 - 100000', '100001 - 200000',
              '200001 - 300000', '300001 - 400000', '400001 - 700000')

colfunc <- colorRampPalette(c('#ffffff', '#cfa35c','#512888')) # base on collor interpolation
c_colors <- heat.colors(11)[11:1] #colfunc(11)   High to Low

names(c_colors) <- c_labels

df_map$confirmed <- factor(
  cut(df_map$c_confirmed, c_levels, include.lowest = T),
  labels = c_labels
)

# Check
summary(df_map$confirmed)
summary(df_map$c_confirmed)

sum(between(unique(daily_csummary$c_confirmed),left =0, right =10 ))
max(df_map$c_confirmed)

###################### MAP 
date_range = unique(daily_csummary$date)
df = df_map[df_map$date == date_range[81], ]

################################ Discrete #######################################################
{
  # c_colors_f <- heat.colors(11)[11:1]
  c_colors  <- colorRampPalette(brewer.pal(9,"Purples"),space = "Lab")(11)
  
  p = ggplot() +
  geom_map(data = world_map_d,
           aes(map_id = region),
           fill=c_colors[1], 
           map = world_map_d) +
  expand_limits(x = df$long, y = df$lat)
# p +theme_dark()


p2 <- p + 
  geom_polygon(data = df, 
               aes(x = long, y = lat, 
                   fill = confirmed, group = group), 
               color = "grey50")+
  labs(title= "Novel\nCoronavirus\n(COVID-19)", 
       x='',y='') + 
  scale_fill_manual(element_blank(),
                    values = c_colors, drop=FALSE, 
                    guide = guide_legend(
                      label.position = "bottom",
                      direction = "vertical",
                      barheight = unit(2, units = "mm"),
                      barwidth = unit(100, units = "mm"),
                      draw.ulim = FALSE,
                      title.position = 'top',
                      title.hjust = 0.5,
                      title.vjust = 0.5))


###
p2 + theme(
  plot.title = element_text(colour = 'white'),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  
  legend.position = c(0.1, 0.2),  # legend.position = 'bottom',
  legend.background = element_blank() ,
  legend.key.size = unit(10,'pt') ,
  legend.key.width  = unit(10,'pt'),
  legend.text =  element_text(size = rel(.9), face = "bold", color = "#d4d0d9"),
  legend.key = element_rect(fill = "black", color = 'black'),
  
  panel.background = element_rect(fill = 'black'),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = 'black')
  # plot.background = element_rect(fill = 'black')

  )

}




################################## Continous #####################################################
# map1 <- 
  
p = ggplot() +
  geom_map(data = world_map_d,
           aes(map_id = region),
           fill='#F0F921FF', 
           map = world_map_d) +
  expand_limits(x = df$long, y = df$lat)

p + 
  geom_polygon(data = df, aes(x = long, y = lat, 
                              fill = c_confirmed, group = group), 
               color = "black") + 
  scale_fill_gradientn(colours = heat.colors(5)[5:1])
                       
                       
                       
                       
  scale_fill_viridis(option = "magma",
                     na.value = "#0D0887FF",
                     direction = -1,
                     end = 0.8,
                     labels = scales::comma,
                     limits = c(0,700000),
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = FALSE,
                       title.position = 'top',
                       title.hjust = 0.5,
                       title.vjust = 0.5)
  ) + 
  theme_void() + 
  theme(axis.text.x = element_blank(), 
        axis.text.y =   element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  xlab(" ") + ylab(" ") 







#+
  scale_fill_viridis(option = "magma", direction = -1, 
                     name = "Risk of Coup Attempt in 2020",
                     labels = scales::percent,
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = FALSE,
                       title.position = 'top',
                       title.hjust = 0.5,
                       title.vjust = 0.5,
                       discrete == T
                     )) #+ 
  theme_hc() + theme(axis.text.x = element_blank(), 
                     axis.text.y =   element_blank(), 
                     axis.ticks = element_blank(),
                     legend.position = "bottom") +
  xlab(" ") + ylab(" ") +
  labs(title = "Forecasted Risk of Military Coup Attempt in 2020",
       subtitle = "Risk forecast based on 70+ indicators")
#implicitly print map
map1
cnames <- aggregate(cbind(long, lat) ~ region, data=world_map_d, FUN=mean) %>% 
            semi_join(daily_csummary, by = c('region'='country'))

cnames[cnames$region == 'USA', 2:3] <- c(-100.03200, 38)

d_label <- daily_top_four_cc_country_mod %>% 
  left_join(cnames, by = c("country"="region")) %>% 
  mutate(label = paste0(country,"\n",format(c_confirmed, nsmall=0, big.mark=",")))


################## BASE #######################

z = 
ggplot() +
  geom_map(data = world_map_d,
           aes(map_id = region),
           fill=c_colors[1], 
           map = world_map_d) +
  # xlim(c(-205, 208))  +
  expand_limits(x = df$long, y = df$lat)

dt = date_range[81]
d_cum  =  format(data_daily$Infected[data_daily$date == dt], 
                 nsmall=0, big.mark=",")

z1 <- z + 
  geom_text(aes(x = -220, y= 35,
                label = "Novel\nCoronaVirus\n(COVID-19)"), 
            size = 10.5, hjust = 'left', nudge_x = -25, lineheight = 0.79,
            color = 'white', fontface = "bold", family = "Segoe UI")+ 
  geom_text(aes(x = -220, y= 10,
                label = "Total\nConfirmed Cases"), 
            size = 9, hjust = 'left', nudge_x = -25,lineheight = 0.79,
            color = '#b4b3c0') + 
  geom_text(aes(x = -220, y= -10,
                label = paste0(dt)), 
            size = 8.2, hjust = 'left', nudge_x = -25,
            color = '#1704a5', fontface = "bold", family = "Segoe UI") + 
  geom_text(aes(x = -220, y= -20,
                label = d_cum ), 
            size = 12, hjust = 'left', nudge_x = -25,
            color = 'white', fontface = "bold", family = "Segoe UI") + 
  geom_text(aes(x = -220, y= -35,
                label = "DATA SOURCE:" ), 
            size = 4, hjust = 'left', nudge_x = -25,
            color = '#1b06b7', fontface = "bold", family = "Segoe UI")  + 
  geom_text(aes(x = -220, y= -40,
                label = "Johns Hopkins CSSE" ), 
            size = 3.5, hjust = 'left', nudge_x = -25,
            color = '#a2a1ad', fontface = "bold", family = "Segoe UI")  + 
  geom_text(aes(x = -220, y= -43,
                label = "https://github.com/CSSEGISandData/COVID-19" ), 
            size = 2.7, hjust = 'left', nudge_x = -25,
            color = '#a2a1ad', fontface = "bold", family = "Segoe UI")   + 
  geom_text(aes(x = -220, y= -50,
                label = "VISUALIZATION:" ), 
            size = 4, hjust = 'left', nudge_x = -25,
            color = '#1b06b7', fontface = "bold", family = "Segoe UI")  +
  geom_text(aes(x = -220, y= -55,
                label = "Babatunde Adebayo" ), 
            size = 3.5, hjust = 'left', nudge_x = -25,
            color = '#a2a1ad', fontface = "bold", family = "Segoe UI")  +
  geom_text(aes(x = -220, y= -58,
                label = "https://github.com/bteinstein" ), 
            size = 3, hjust = 'left', nudge_x = -25,
            color = '#a2a1ad', fontface = "bold", family = "Segoe UI") +
  cust_theme + gl +
  theme(plot.margin  = unit(c(t = 2, r = 2, b = 2, l = 2), "pt"))

################# FILLeR ###########################
z1 + 
  geom_polygon(data = df, aes(x = long, y = lat, 
                   fill = confirmed, group = group), 
               color = "grey50") + 
  geom_text(data=d_label %>% filter(date == date_range[81]), 
            aes(long, lat, label = label), 
            fontface = "bold", color = "white", size=2) +
  scale_fill_manual(element_blank(), values = c_colors, drop=FALSE) +
  cust_theme + gl




################## GUIDE - LEGEND ###################
gl <- guides(fill = guide_legend(
  nrow = 3,
  label.position = "right",
  direction = "horizontal",
  barheight = unit(2, units = "mm"),
  barwidth = unit(100, units = "mm"),
  draw.ulim = FALSE,
  title.position = 'top',
  title.hjust = 0.5
))




################# THEME #########################
cust_theme <- theme(
  plot.title = element_text(colour = 'white'),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  
  legend.position = c(0.73, 0.03),  # legend.position = 'bottom',
  legend.background = element_blank() ,
  legend.key.size = unit(11,'pt') ,
  legend.key.width  = unit(11,'pt'),
  legend.text =  element_text(size = rel(1.05), face = "bold", color = "#756e6e"),
  legend.key = element_rect(fill = "black", color = 'black'),
  
  panel.background = element_rect(fill = 'black'),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = 'black', color = "black")
  # plot.background = element_rect(fill = 'black')
  
)
  
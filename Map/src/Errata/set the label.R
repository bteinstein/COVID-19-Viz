i = 85
dt = date_range[i]
df = df_map[df_map$date == dt, ]
d_cum  =  format(data_daily$Infected[data_daily$date == dt], 
                 nsmall=0, big.mark=",")

if (data_daily$Infected[data_daily$date == dt] > 1000000) {
  d_col = l_col = "#3f48cc"
}


z <- ggplot() +
  geom_map(data = world_map_d,
           aes(map_id = region),
           fill=c_colors[1], 
           map = world_map_d) +
  expand_limits(x = df$long, y = df$lat)


dd = d_label
dd[dd$country == 'Italy', 'long'] = 16 
dd[dd$country == 'Italy', 'lat'] = 46 
dd[dd$country == 'Spain', 'lat'] = 40 
dd[dd$country == 'Germany', 'lat'] = 53.5
dd[dd$country == 'Germany', 'long'] = 10.3
dd[dd$country == 'South Korea', 'lat'] = 38
dd[dd$country == 'South Korea', 'long'] = 129
dd[dd$country == 'Japan', 'lat'] = 30
dd[dd$country == 'France', 'long'] = -5
dd[dd$country == 'France', 'lat'] = 46.7
c(-5, 46.7)

z + 
  geom_text(data=dd,
            aes(long, lat, label = label), color = "gold",
            lineheight = 0.79,
            fontface = "bold",  size = 3.5,
            family = "Segoe UI") 


dd[dd$country == 'Japan', ]

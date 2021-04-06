rm(list = ls())
library(magick)
source('C:/Users/btein/Workplace/Visualization and Animation/COVID-19-Viz/Map/src/Infected Cases/inf_set_up.R', echo = F)

#### Some Plot Parameters  ####
# c_colors <- c("#363638","#484849","#5d5d60",
#               "#b5caf0","#b6c6e2","#97b1d6",
#               "#7c9fcd","#5b8cc1","#3d7db7","#0070af","#0063a6")

### "#313133",

c_colors <- c("#363638","#78787c", ###  #5d5d60",
              "#b5caf0","#a6b9dd","#b6c6e2","#97b1d6",
              "#7c9fcd","#5b8cc1","#3d7db7","#0070af","#0063a6")

date_range = unique(df_map$date)

c_blc <- "#202023"

for (i in 1:length(date_range)) {
  #### Set up  ####
  dt = date_range[i]
  df = df_map[df_map$date == dt, ]
  d_label_sub = d_label %>% filter(date == dt)
  
  d_cum  =  format(data_daily$Infected[data_daily$date == dt], 
                   nsmall=0, big.mark=",")
  
  data_ovr_label_col <- "white"  #"#2d39ef" ##  "#0b87f0" 
  d_cum_col <- "white"
  
  if (data_daily$Infected[data_daily$date == dt] > 1000000)  d_cum_col <- "#2d39ef"
  # if (any(d_label_sub$c_confirmed > 100000))  data_ovr_label_col <- "#00c0ff" 
  #### Base Plot  ####
  z <- ggplot() +
    geom_map(data = world_map_d,
             aes(map_id = region),
             fill=c_colors[1], 
             map = world_map_d) +
    expand_limits(x = df$long, y = df$lat)
  
  
  #### Fill and Legend  ####
  z1 <-  z + 
    geom_polygon(data = df, aes(x = long, y = lat, 
                                fill = confirmed, group = group), 
                 color = "#191919") +
    geom_text(data= d_label_sub,
              aes(long, lat, label = label), color = data_ovr_label_col,
              lineheight = 0.79,
              fontface = "bold",  size = 3.5,
              family = "Segoe UI") +
    scale_fill_manual(element_blank(), values = c_colors, drop=FALSE)
    
  
  #### Annotations  ####
  z2 <- z1 + 
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
              color = '#7699ff', fontface = "bold", family = "Segoe UI") + 
    geom_text(aes(x = -220, y= -20,
                  label = d_cum ), 
              size = 12, hjust = 'left', nudge_x = -25,
              color = d_cum_col, fontface = "bold", family = "Segoe UI") + 
    geom_text(aes(x = -220, y= -35,
                  label = "DATA SOURCE:" ), 
              size = 4.2, hjust = 'left', nudge_x = -25,
              color = '#537efc', fontface = "bold", family = "Segoe UI")  + 
    geom_text(aes(x = -220, y= -40,
                  label = "Johns Hopkins CSSE" ), 
              size = 3.7, hjust = 'left', nudge_x = -25,
              color = '#a2a1ad', fontface = "bold", family = "Segoe UI")  + 
    geom_text(aes(x = -220, y= -43,
                  label = "https://github.com/CSSEGISandData/COVID-19" ), 
              size = 2.9, hjust = 'left', nudge_x = -25,
              color = '#a2a1ad', fontface = "bold", family = "Segoe UI")   + 
    geom_text(aes(x = -220, y= -50,
                  label = "VISUALIZATION:" ), 
              size = 4.2, hjust = 'left', nudge_x = -25,
              color = '#537efc', fontface = "bold", family = "Segoe UI")  +
    geom_text(aes(x = -220, y= -55,
                  label = "Babatunde Adebayo" ), 
              size = 3.7, hjust = 'left', nudge_x = -25,
              color = '#a2a1ad', fontface = "bold", family = "Segoe UI")  +
    geom_text(aes(x = -220, y= -58,
                  label = "https://github.com/bteinstein" ), 
              size = 3.2, hjust = 'left', nudge_x = -25,
              color = '#a2a1ad', fontface = "bold", family = "Segoe UI") 
  
  #### Final Plot  ####
    
  z3 =   z2 + 
    guides(fill = guide_legend(
      nrow = 3,
      label.position = "right",
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(100, units = "mm"),
      draw.ulim = FALSE,
      title.position = 'top',
      title.hjust = 0.5
    )) + 
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        
        legend.position = c(0.73, 0.07),  
        legend.background = element_blank() ,
        legend.key.size = unit(11,'pt') ,
        legend.key.width  = unit(11,'pt'),
        legend.text =  element_text(size = rel(1.05), face = "bold", color = "#756e6e"),
        legend.key = element_rect(fill = "black", color = 'black'),
    
        panel.background = element_rect(fill = c_blc, colour = c_blc),
        panel.grid = element_blank(),
        
        plot.background = element_rect(fill = c_blc, color = c_blc),
        plot.margin  = unit(c(t = 2, r = 2, b = 2, l = 2), "pt")
        
      )

    ### Save
    png(filename = paste('Map/png/plt_', i, '.png', sep=''), 
        pointsize=10, width=1100, height=600, res=100)
    print(z3)
    dev.off()

}


############# ANIMATION ################
# Read the individual maps into a data structure for use with 'magick'
imglayers <- sapply(1:length(date_range), function(dt) {
  image_read(paste('Map/png/plt_', dt, '.png', sep=''))
})

# Generate an animated GIF with the individual maps and write to a file
imganim <- image_animate(image_join(imglayers), fps = 1, 
                         dispose = "previous",optimize = T)
image_write(imganim, 'Map/gif/global_infection.gif')

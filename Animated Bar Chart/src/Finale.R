library(tidyverse)
library("gganimate")
library(magick)
library("ggflags")
library('ggdark') # devtools::install_github("nsgrantham/ggdark")
library(ggthemes)
library(ggrepel)
library(extrafont)
# font_import()
loadfonts(device = "win")
# font_import(paths = "./font/",prompt = F)
# Data
source('D:/Project/Visualization and Animation/COVID-19-Viz/Animated Bar Chart/src/set_data.R')

#######################################################
# Custom Theme
custom_theme <- dark_mode(theme_fivethirtyeight()) +
  theme(legend.position = 'bottom', 
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, family = 'Comic Sans MS'),
        legend.title = element_text(size = 8),
        legend.box.spacing = unit(1, "mm"),
        title = element_text(size = 18, family = 'Comic Sans MS', face = 'bold'),
        panel.grid.major.y = element_line(colour = '#611405', size = 0.001 ),
        axis.title = element_text(face = "bold", size = 9)) #


######################################################
# Flag plot
plot_flags <-  ggplot(data = df_f %>% select(-order), 
                      aes(x = Date, y= cc, size = log(cc), country = c_code)) +
  geom_flag(position = position_jitter(width = 0.1)) +
  scale_country(name = "Countries") +
  scale_size(range = c(1, 7), guide = FALSE)  + 
  geom_text_repel(data = df_f %>% filter(order <= 3) ,
                  aes(x = Date, y = cc*1.05, label = Country),
                  size = 3, color = 'white', fontface = "bold")   +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
  labs(x = "", y ="") +
  dark_mode(theme_fivethirtyeight()) +
  theme(legend.position = 'bottom', 
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, family = 'Comic Sans MS'),
        legend.title = element_text(size = 8),
        legend.box.spacing = unit(1, "mm"),
        title = element_text(size = 18, family = 'Comic Sans MS', face = 'bold'),
        panel.grid.major.y = element_line(colour = '#611405', size = 0.001 ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(face = "bold", size = 9),
        plot.margin = unit(c(1, 0, 0, 0), "lines"),
        panel.spacing  = unit(c(0, 0, 0, 0), "lines") )+ 
  guides(country = guide_legend(nrow = 2)) 

anm_flag <- plot_flags +  transition_states(Date) 

animate(anm_flag, nframe = 4)

######################################################
# Plot Lines
plot_lines <- ggplot(data = cum_cc, aes(dt, cc)) +
  geom_line(aes(group = 1), size = 1, color = '#ba2406') + 
  geom_area(fill = '#ba2406', alpha = 0.25) +
  geom_point(size = 3, color = "white") +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "K")) + 
  labs(x = "", y = "") +
  custom_theme

anm_line = plot_lines + 
  transition_reveal(dt) +
  view_follow(fixed_y = TRUE)

#####################################################
# Plot header
# Plot Lines
plot_header <- ggplot(data = cum_cc, aes(dt, cc)) +  
  geom_text(aes(x = mean(dt),
                y = max(cc)*0.5,
                label = paste0(dt,"\n")),
            size = 9, color = '#c37878', family = 'Comic Sans MS') +
  geom_text(aes(x = mean(dt),
                y = max(cc)*0.5,
                label = paste0("Total Confirmed Cases: ",cc)),
            size = 5, color = '#e27b7b', family = 'Comic Sans MS') +
  
  scale_y_continuous(limits = c(max(cum_cc$cc)/3, max(cum_cc$cc)/1.2),
                     labels = scales::unit_format(scale = 1e-3, suffix = "K"))  +
  labs(title = "COVID-19 - Daily Confirmed Cases",
       subtitle = "" , 
       caption = "", 
       x="",y="") +
  custom_theme  + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(1, 0, 0, 0), "lines"),
    panel.spacing  = unit(c(0, 0, 0, 0), "lines")
  )

anm_header = plot_header + 
  transition_reveal(dt)

animate(anm_header, nframes = 4)
#####################################################
# Animation Set up
nfrm <- 200 # nfrm <- 100
wd <- 1200; 

anim_flag <- animate( anm_flag , nframes = nfrm,
                      width = wd, height = 440, 
                      end_pause = 10)
anim_line <- animate(anm_line, nframes = nfrm,
                     width = wd, height = 200, 
                     end_pause = 10)
anim_header <- animate(anm_header ,nframes = nfrm,
                       width = wd, height = 160, 
                       end_pause = 10)

anim_save(anim_flag, filename = "./output/f2.gif")
anim_save(anim_line, filename = "./output/l2.gif")
anim_save(anim_header, filename = "./output/h2.gif")

########################
############################
# Combination
head_mgif <- image_read(anim_header)
flag_mgif <- image_read(anim_flag)
line_mgif <- image_read(anim_line)

new_gif <- image_append(c(head_mgif[1], line_mgif[1], flag_mgif[1]), stack = T) # col = 1, i.e. coombine vertically

for(i in 2:nfrm){
  combined <- image_append(c(head_mgif[i], line_mgif[i], flag_mgif[i]), stack = T)
  new_gif <- c(new_gif, combined)
}

save(new_gif, file = "project")
anim_save(filename = "./output/anim5.gif", new_gif)

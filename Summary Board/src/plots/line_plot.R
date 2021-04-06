#
library(tidyverse)
library("gganimate")
library(ggdark)
library(ggthemes)
library(extrafont)
loadfonts(device = "win", quiet = T)


# Comic Sans MS




# Load Data Clean dataset (up to date)
# source('./Summary Board/src/data_format.R')

#
head(data_dsummary)

######### Theme ############
custom_theme_line = 
  # dark_mode(theme_fivethirtyeight()) +
  # dark_mode(theme_fivethirtyeight()) +
  theme(
        # title = element_text(size = 18, family = 'Gentium Basic', face = 'bold'),
        axis.text =  element_text(face = 'bold', size = rel(1), color = "#ccd2f5"),
        legend.position = c(0.1, 0.80),  # legend.position = 'bottom',
        legend.background = element_blank() ,
        legend.direction = "vertical",
        legend.key.size = unit(15,'pt') ,
        legend.key.width  = unit(20,'pt'),
        legend.text =  element_text(size = rel(1), face = "bold", color = "white"),
        legend.key = element_rect(fill = "black", color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#611405', size = 0.001 ),
        panel.background = element_rect(fill = 'black'), 
        plot.background = element_rect(fill="black", colour = '#5d0101', size = 2),
        # panel.border = element_rect(colour = "white", fill=NA, size=2),
        plot.margin  = unit(c(t = 2, r = 2, b = 2, l = 2), "pt")
        ) 

{
######### LABs ###########
l_labs <- labs( # title = "COVID-19 - Daily Cases Summary", 
  x="",y="")
######### SCALES ###############
sc_color = scale_color_manual(name ="", values = c('#59b2ff',"yellow","green",'red'), 
                              breaks = c("Infected","Active","Recovered","Deceased")) 
# Fill
sc_fill<- scale_fill_discrete(guide = FALSE)

#######################################
# the static plot
plot_lines <-
  data_dsummary %>% 
  ggplot(aes(x = date, y= value)) +
  geom_point(aes(color = status), size =1.5) +
  geom_line(aes(color = status), size = 0.09) +
  geom_area(data = data_dsummary %>% filter(status == 'Infected'),
                                           aes(),fill = '#5e68f8', alpha = 0.05) +  
  geom_area(data = data_dsummary %>% filter(status == 'Active'),
                                           aes(),fill = 'yellow', alpha = 0.09) +   
  geom_area(data = data_dsummary %>% filter(status == 'Recovered'),
                                           aes(),fill = 'green', alpha = 0.1) +  
  geom_area(data = data_dsummary %>% filter(status == 'Deceased'),
            aes(),fill = 'red', alpha = 0.12) +
  scale_y_continuous(labels = scales::unit_format(accuracy = 0.1, scale = 1e-6, suffix = "M")) +
  scale_color_manual(name ="", values = c('#59b2ff',"yellow","green",'red'), 
                     breaks = c("Infected","Active","Recovered","Deceased")) +
 scale_fill_discrete(guide = FALSE)+ 
  l_labs + # labels
  custom_theme_line 

plot_lines
}


rm(custom_theme_line)

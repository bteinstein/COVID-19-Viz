library(tidyverse)
library("gganimate")
library(ggdark)
library(ggthemes)
library(extrafont)
loadfonts(device = "win", quiet = T)

# source('./Summary Board/src/data_format.R')


######### Theme ############
custom_theme_header = 
  # dark_mode(theme_fivethirtyeight()) +
  theme(
    title = element_text(size = 20, family = 'Gentium Basic', face = 'bold'),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = '#410a0a'),
    panel.border = element_rect(colour = "#680101", fill=NA, size=2),
    plot.background = element_rect(fill="black", colour = '#5d0101', size = 2),
    plot.margin  = unit(c(t = 10, r = 10, b = 10, l = 10), "pt"))
   


#
{
t_size = 12
  
  header_date <-  ggplot(data = data_daily, aes(x = date)) +
  geom_text(aes(x = 1, y = 1, label = date ), size = 22, family = 'Gentium Basic', color = "#ecedef")  + 
  custom_theme_header 

  header_ic <-  ggplot(data = data_daily, aes(x = date)) +
  geom_text(aes(x = 1, y = 1, label = Infected ), size = t_size, color = "#67b8fd") + 
  custom_theme_header +
  labs(title = "Infected") + 
    theme(title = element_text(colour = '#59b2ff'),
	    panel.border = element_rect(colour = "#0060b3", fill=NA, size=1),
	    panel.background = element_rect(fill = '#003461'))

  header_ac <-  ggplot(data = data_daily, aes(x = date)) +
  geom_text(aes(x = 1, y = 1, label = Active ), size = t_size, color = "#f6ee57") + 
  custom_theme_header +
  labs(title = "Active")+ 
    theme(title = element_text(colour = 'yellow'),
	    panel.border = element_rect(colour = "#e7dc11", fill=NA, size=1),
	    panel.background = element_rect(fill = '#aba314'))

  header_rc <-  ggplot(data = data_daily, aes(x = date)) +
  geom_text(aes(x = 1, y = 1, label = Recovered ), size = t_size, color = "#2af64f") + 
  custom_theme_header +
  labs(title = "Recovered") + 
    theme(title = element_text(colour = 'green'),
	    panel.border = element_rect(colour = "#72ff8c", fill=NA, size=2),
	    panel.background = element_rect(fill = '#0b8922'))

  header_dc <-  ggplot(data = data_daily, aes(x = date)) +
    #geom_text(aes(x = 1, y = 1, label = Deceased ), size = t_size, color = "#ecedef") + 
    geom_text(aes(x = 1, y = 1, label = Deceased ), size = t_size, color = "#ff3838") + 
    custom_theme_header +
    labs(title = "Deceased") + 
    theme(title = element_text(colour = 'red'),
	    panel.border = element_rect(colour = "#ff7070", fill=NA, size=1))
}




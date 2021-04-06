library(tidyverse)
library(extrafont)
loadfonts(device = 'win',quiet = T)
#font_import()

banner_plot <- 
  data_daily %>% 
  ggplot(aes(x=date , y=1)) +
    labs(title = "COVID-19 WORLD PANDEMIC UPDATE",
         subtitle = "Update as at Sunday 12th of April 2020") +
  # geom_text(aes(y=.3, label = "TITLE HERE"), size = 14, 
  #           color = "white") +
  geom_text(aes(x=1,y=1, label = "Daily Cummulative Confirmed Cases\nData Source: John Hopkins CSSE"),
            size = 4, color = "white", fontface = "bold", family = 'Leelawadee UI Semilight')  +
    theme(
      plot.title = element_text(size = rel(3.7), family = 'Bahnschrift', 
                           face = 'bold', colour = "white",hjust = 0.5),
      plot.subtitle = element_text(size = rel(1.7), family = 'Gentium Basic', 
                           face = 'bold', colour = "white",hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = '#480000'),
      panel.border = element_rect(colour = "#b4bdf3", fill=NA, size=1),
      plot.background = element_rect(fill="black", colour = '#5d0101', size = 2),
      plot.margin  = unit(c(t = 13, r = 10, b = 10, l = 10), "pt"))




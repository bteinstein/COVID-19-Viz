library(tidyverse)
library(ggflags)
library("gganimate")
library("ggflags")
library('ggdark') # devtools::install_github("nsgrantham/ggdark")
library(ggthemes)
library(ggrepel)
library(extrafont)
loadfonts(device = "win", quiet = T)


setwd("C:/Users/btein/Workplace/Visualization and Animation/COVID-19-Viz")

source('./Summary Board/src/data_format.R')



custom_theme_flag <-  
  theme(
    plot.title = element_text(size = rel(1.9), family = 'Bahnschrift', face = 'bold',
                              color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = rel(0.5), family = 'Comic Sans MS', 
                                 color = "white", face = 'bold'),
    plot.caption = element_text(size = rel(1.3), family = 'Comic Sans MS', 
                                color = "#585858", face = 'bold', hjust = 0.01),
    axis.text =  element_text(face = 'bold', size = rel(.8), color = "#ccd2f5"),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = '#5d0101', size = 0.001 ),
    panel.background = element_rect(fill = 'black'), 
    plot.background = element_rect(fill="black", colour = '#5d0101', size = 2),
    plot.margin  = unit(c(t = 15, r = 2, b = 2, l = 2), "pt")
  )


# plot
{
  plot_flags <-  
    daily_top_four_cc_country %>% 
    ggplot(aes(x = date, y = c_confirmed, size = log(c_confirmed+1), country = c_code)) +
    geom_flag(position = position_jitter(width = 0.18)) +
    geom_flag() +
    scale_country(name = "Countries", guide = FALSE) +
    scale_size(range = c(5, 8), guide = FALSE)  + 
    geom_text_repel(aes(x = date, y = c_confirmed*1.1, label = country),
                    size = 4, color = '#ccd2f5', fontface = "bold",
                    position = position_jitter(width = 0.1)) +
    scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
    labs(title = "Top 4 Countries by Number of Confirmed Cases",
         caption = "Data Viz: Babatunde Adebayo (github.com/bteinstein)\n") +
    custom_theme_flag + 
    guides(country = guide_legend(nrow = 2)) 
  
  #plot_flags
  }


rm(custom_theme_flag)

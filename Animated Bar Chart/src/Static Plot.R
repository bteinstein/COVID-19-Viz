library("gganimate")
library("tidyverse")

# library(showtext)
# font_add_google(name = 'Oswald', family = "Oswald")

# Load clean data
confirmed_cases <- rio::import('./data/cln_confirm_c_df.csv')

# Dropping "Diamond Princess" "MS Zaandam"  


# Converting Wide to Long
lng_confirmed_cases <- pivot_longer(data = confirmed_cases %>% 
                                      filter(!Country %in% c("Diamond Princess", "MS Zaandam" ) ), # Dropping "Diamond Princess" "MS Zaandam"  
                                    cols = c(-Country, -iso2c, -long, -lat, -income, -region),
                                    names_to = "Days",
                                    values_to = "Confirmed Cases" ) %>% 
                       mutate(Date = as.Date(Days, "%m/%d/%y"))




###########################################################################################
min_y = min(lng_confirmed_cases$`Confirmed Cases`)
max_y = max(lng_confirmed_cases$`Confirmed Cases`)
min_x = min(lng_confirmed_cases$Days)
max_x = max(lng_confirmed_cases$Days)

p = ggplot(lng_confirmed_cases, 
           aes(x = Date, y = `Confirmed Cases`
           )) + 
  
  geom_point(data = lng_confirmed_cases %>% 
               group_by(Date) %>% 
               arrange(Date, desc(`Confirmed Cases`)) %>% 
               mutate(order = 1:n()) %>% 
               filter(order < 10),
             aes(x = Date,  y = `Confirmed Cases`, color = region,
                 size = `Confirmed Cases`,  alpha = 0.5)) +   
  
  scale_size( guide = FALSE)  +
  
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
               date_labels = "%b-%d") + 
  scale_y_continuous(limits = c(0, 300000),
                     breaks = seq(0,300000, by=50000),
                     labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
  
  viridis::scale_color_viridis(discrete = TRUE, name = "Region", 
                               option = "viridis", guide = FALSE) + 
  geom_text(aes(x = as.Date("2020-02-28"), y = 1.3e05,label = Date),
            size = 14, color = 'lightgrey') +
  theme_classic()  + 
  theme(legend.position = "none")

# Animation
p1 = p + transition_time(Date)

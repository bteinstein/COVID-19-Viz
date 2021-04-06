library(tidyverse)
library("gganimate")
library("ggflags")
library('ggdark') # devtools::install_github("nsgrantham/ggdark")
library(ggthemes)
library(ggrepel)

# Load clean data
confirmed_cases <- rio::import('./data/cln_confirm_c_df.csv')

# Converting Wide to Long
lng_confirmed_cases <- pivot_longer(data = confirmed_cases %>% 
                                      filter(!Country %in% c("Diamond Princess", "MS Zaandam" ) ), # Dropping "Diamond Princess" "MS Zaandam"  
                                    cols = c(-Country, -iso2c, -long, -lat, -income, -region),
                                    names_to = "Days",
                                    values_to = "Confirmed Cases" ) %>% 
  mutate(Date = as.Date(Days, "%m/%d/%y"))

# Feature cleaning
p_data <- lng_confirmed_cases %>% filter(`Confirmed Cases` > 1)

df = p_data %>% 
  group_by(Date) %>% 
  arrange(Date, desc(`Confirmed Cases`)) %>% 
  mutate(order = 1:n()) %>% 
  filter(order <= 50, Date %in% unique(lng_confirmed_cases$Date)) %>% 
  mutate(Date2 = Date, 
         c_code = tolower(iso2c))

cum_cc <- lng_confirmed_cases %>% 
  group_by(Date) %>% 
  summarise(cc = sum(`Confirmed Cases`)) %>% 
  rename(Date2 = Date)

# Some verification
head(p_data %>% count(Date))
tail(p_data %>% count(Date))


x = df %>% 
    ggplot(aes(x = Date, y = `Confirmed Cases`, 
                  size = log(`Confirmed Cases` + 1)) ) +
  geom_line(data = cum_cc, aes(Date2, cc,group = 1), size = 1, color = '#ba2406') +
  geom_flag(data = df, aes(country = c_code), position = position_jitter(width = 0.1)) +
  scale_country(name = "Countries", labels = df$iso2c) +
  scale_size(range = c(1, 5), guide = FALSE)  + 
  geom_text_repel(data = df %>% filter(order <= 3) ,
          aes(y = `Confirmed Cases`*1.05, label = Country),
          size = 3, color = 'white', fontface = "bold")   +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
  geom_text(aes(x = mean(df$Date), 
                y = max(df$`Confirmed Cases`)/2,label = Date),
            size = 10, color = 'lightgrey') + 
  labs(title = "COVID-19 - Daily Confirmed Cases",
      subtitle = "") + 
  dark_mode(theme_fivethirtyeight()) +
  theme(legend.position = 'bottom', 
        legend.direction = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.box.spacing = unit(1, "mm"),
        title = element_text(size = 16),
        panel.grid.major.y = element_line(colour = '#611405', size = 0.001 ),
        axis.title = element_text(face = "bold", size = 9))



# animation:
# p1 = p + dark_mode(theme_fivethirtyeight()) + transition_time(Date) + enter_grow() + exit_fade()
# p2 = p + dark_mode(theme_fivethirtyeight()) + transition_time(Date) + ease_aes(default = "linear")
# 
# q1 = q + dark_mode(theme_fivethirtyeight()) + transition_time(Date) +  enter_grow() + exit_fade()
# q2 = q + dark_mode(theme_fivethirtyeight()) + transition_time(Date) + ease_aes(default = "linear")

x1 = x  + transition_reveal(Date2) + transition_time(Date)  + enter_grow() + exit_fade()
x2 = x  + transition_reveal(Date2) + transition_time(Date) + ease_aes(default = "linear")

# animate(p1, nframe =100, fps = 1, width = 900, height = 800, renderer = av_renderer(file = "mov.mp4"))
# animate(p1, duration = 30, width = 900, height = 800, renderer = av_renderer(file = "./output/A.mp4"))
# animate(p1,  nframe =100, fps = 4, width = 900, height = 800, 
#         renderer = av_renderer(file = "./output/glow_fade_lite.mp4"))
# animate(q1,  nframe =100, fps = 4, width = 900, height = 800, 
#         renderer = av_renderer(file = "./output/glow_fade.mp4"))
# animate(p2,  nframe =100, fps = 4, width = 900, height = 800, 
#         renderer = av_renderer(file = "./output/linear_lite.mp4"))
# animate(q2,  nframe =100, fps = 4, width = 900, height = 800, 
#         renderer = av_renderer(file = "./output/linear.mp4"))

filname = gsub(pattern = ":",replacement = "",as.character(Sys.time()))

animate(x1,  nframe = 100, fps = 2,width = 900, height = 700 , 
        renderer = av_renderer(file = paste0("./output/__glow_fade",filname,".mp4")))
animate(x2,  nframe =150, fps = 2, width = 900, height = 700 , 
        renderer = av_renderer(file =paste0("./output/__linear_lite",filname,".mp4")))







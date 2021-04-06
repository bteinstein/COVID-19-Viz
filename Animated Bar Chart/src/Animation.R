p = ggplot(lng_confirmed_cases, 
           aes(x = Date, y = `Confirmed Cases`
           )) + 
  geom_flag(data = lng_confirmed_cases %>% 
               group_by(Date) %>% 
               arrange(Date, desc(`Confirmed Cases`)) %>% 
               mutate(order = 1:n()) %>% 
               filter(order < 10),
             aes(x = Date,  y = `Confirmed Cases`, country = tolower(iso2c),
                 size = `Confirmed Cases`))  +
  scale_country() +
  scale_size(range = c(1, 20), guide = FALSE)  +
  
  scale_x_date(breaks = 
                 function(x) seq.Date(from = min(x), to = max(x), by = "7 days"),
               date_labels = "%b-%d") + 
  scale_y_continuous(limits = c(0, 300000),
                     breaks = seq(0,300000, by=50000),
                     labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
  geom_text(aes(x = as.Date("2020-02-28"), y = 1.3e05,label = Date),
            size = 14, color = 'lightgrey') +
  theme_classic()  + 
  theme(legend.position = "none")


# animation:
p1 = p + transition_time(Date)
anim = animate(p1, nframe =100, fps = 2, width = 900, height = 800, duration = 60)
anim2 = animate(p1, nframe =100, width = 900, height = 800, duration = 30)

animate(p1, nframe =100, fps = 1, width = 900, height = 800, renderer = av_renderer(file = "mov.mp4"))


anim_save(filename = "mov2.mp4", animation = anim)
anim_save(filename = "mov2.gif", animation = anim)
anim_save(filename = "mov3.mp4", animation = anim2)
anim_save(filename = "mov3.gif", animation = anim2)
library(magick)
image_write(anim, 'test.gif')

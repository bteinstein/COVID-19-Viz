

lp <- ggplot(data = df) +
  # flag
  geom_flag(data = df %>% mutate(dt_flag = Date2), 
            aes(x = dt_flag, y = `Confirmed Cases`,
                           country = c_code, 
                           size = log(`Confirmed Cases` + 1)), 
            position = position_jitter(width = 0.1)) +
  # Add Line
  geom_line(data = cum_cc %>% mutate(dt_line = Date2), 
            aes(dt_line, cc,group = 1), size = 1, color = '#ba2406') +
  # Add Arrow
  geom_point(data = cum_cc %>% mutate(d_pnt = Date2), 
             aes(x = d_pnt, y = cc), size = 2) +
  # Scaling
  scale_country(name = "Countries", labels = df$iso2c) +
  scale_size(range = c(1, 5), guide = FALSE)  +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-3, suffix = "K")) +
  # Accessories
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
        axis.title = element_text(face = "bold", size = 9)) +
  guides(col = guide_legend(nrow = 5))
        
  lp
  

# 
anim1 = lp + 
  # Arrow head
  transition_states(d_pnt) + 
  # Flag 
  transition_time(Date) 

anim2 = lp  +
  # Line
  transition_reveal(Date2) +
  view_follow(fixed_y = TRUE)


anim3 = lp +
  # Line
  transition_reveal(Date2) + 
  # Arrow head
  transition_states(d_pnt) + 
  # Flag 
  transition_time(Date)  +
  view_follow(fixed_y = TRUE)

anim4 = lp   +
  # Flag 
    transition_time(dt_flag)  +
  # Line
  transition_reveal(dt_line) + 
  # Arrow head
  transition_states(d_pnt) + 
  view_follow(fixed_y = TRUE)

anim1
anim2
anim3

filname = gsub(pattern = ":",replacement = "",as.character(Sys.time()))
animate(anim4, nframe = 100, duration = 10 
        end_pause = 8, device = "svg"
        renderer = av_renderer(file =paste0("./output/__new",filname,".mp4")))

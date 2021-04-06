lp1 <- ggplot(data = df, aes(x = Date2, y = `Confirmed Cases`,
                             country = c_code, 
                             size = log(`Confirmed Cases` + 1))) +
  
  # Add Line
  # geom_line(data = cum_cc, aes(Date2, cc,group = 1), size = 1, color = '#ba2406') + 
  # flag
  geom_flag( 
            position = position_jitter(width = 0.1)) +
  # Add Arrow
  # geom_point(data = cum_cc %>% mutate(d_pnt = Date2), aes(x = d_pnt, y = cc), size = 2) +
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

a = lp1 + 
  # Arrow head
  # transition_states(d_pnt) + 
  # Flag 
  # transition_time(Date) +
  # Line
  transition_reveal(Date2) +
  # Some  entery and exit animation
  # enter_fade() + 
  # exit_shrink() + 
  # exit_fade() +
  # Follow and zooming
  view_follow(fixed_y = TRUE)

animate(a, nframe = 40, duration = 20, device = 'svg', end_pause = 8)

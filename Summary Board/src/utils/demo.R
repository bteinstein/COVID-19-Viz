# Some basis
df = tibble(x = letters,
            y = rnorm(length(letters)))


df %>% 
  ggplot(aes(x,y,color=x)) + 
  geom_point() +
  labs(title ="Some Random Title",
       subtitle = "Hello") + 
  theme_bw() + 
  theme(
    axis.title = element_blank(),
    # axis.text =  element_blank(),
    axis.text =  element_text(face = 'bold', size = rel(1.5), color = "#ccd2f5"),
    axis.ticks =  element_blank(),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "pt"),
    plot.background = element_rect(fill='black'),
    panel.background = element_rect(fill = 'black'),
    panel.grid = element_blank(),
    legend.position = c(0.1,0.5),
    # legend.box.background = el,
    legend.background = element_blank(),
    legend.key = element_rect(fill="black", color = 'black')
  )





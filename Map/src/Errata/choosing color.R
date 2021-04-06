p + theme_dark()

{
n = 11
# cl =  colorRampPalette(blues9)(n)
cl =  colorRampPalette(brewer.pal(4,"Purples"))(n)
tibble( x = 1:n, y = 1:n, f = LETTERS[1:n]) %>% 
  ggplot(aes(x,y,  color = f)) +
  geom_point(size = 9) + 
  scale_color_manual(element_blank(),
                    values = cl, drop=FALSE) +
  theme_dark()
}


cl[26]


blues9

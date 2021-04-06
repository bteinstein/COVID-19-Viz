library(dplyr)
library(ggplot2)
library(magick)
library(gganimate)


A<-rnorm(100,50,10)
B<-rnorm(100,50,10)
DV <- c(A,B)
IV <- rep(c("A","B"),each=100)
sims <- rep(rep(1:10,each=10),2)
df<-data.frame(sims,IV,DV)

means_df <- df %>%
  group_by(sims,IV) %>%
  summarize(means=mean(DV),
            sem = sd(DV)/sqrt(length(DV)))

stats_df <- df %>%
  group_by(sims) %>%
  summarize(ts = t.test(DV~IV,var.equal=TRUE)$statistic)

a <- ggplot(means_df, aes(x = IV,y = means, fill = IV)) +
  geom_bar(stat = "identity") +
  geom_point(aes(x = IV, y = DV), data = df, alpha = .25) +
  geom_errorbar(aes(ymin = means - sem, ymax = means + sem), width = .2) +
  theme_classic() +
  transition_states(
    states = sims,
    transition_length = 2,
    state_length = 1
  ) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

a_gif <- animate(a, width = 240, height = 240)

b <- ggplot(stats_df, aes(x = ts))+
  geom_vline(aes(xintercept = ts, frame = sims))+
  geom_line(aes(x=x,y=y),
            data = data.frame(x = seq(-5,5, .1),
                              y = dt(seq(-5,5, .1), df = 18))) +
  theme_classic() +
  ylab("density") +
  xlab("t value") +
  transition_states(
    states = sims,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

b_gif <- animate(b, width = 240, height = 240)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = T)

for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = T)
  new_gif <- c(new_gif, combined)
}

new_gif

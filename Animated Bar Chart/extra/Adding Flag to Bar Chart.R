f <- system.file("extdata/medals.txt", package="ggimage")
medals <- read.table(f, header=TRUE)
p <- ggplot(medals, aes(Country, count)) + geom_col(aes(fill = medal), width = .8)

p + geom_flag(y = -2, aes(image = code)) +
  coord_flip() + expand_limits(y = -2)  +
  scale_fill_manual(values = c("Gold" = "gold", "Bronze" = "#cd7f32", "Silver" = "#C0C0C0"))

# trying to change the location of the flag
p1 <- ggplot(medals[1:3, ], aes(Country, count)) + geom_col(aes(fill = medal), width = .8)

p1 + geom_flag( aes(image = code),size = 0.2) + coord_flip() #+ expand_limits(y = -2) 

?geom_flag

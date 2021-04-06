library(tidyverse)
library(viridis)

if (require("maps")) {
  states <- map_data("state")
  arrests <- USArrests
  names(arrests) <- tolower(names(arrests))
  arrests$region <- tolower(rownames(USArrests))
  
  choro <- merge(states, arrests, sort = FALSE, by = "region")
  choro <- choro[order(choro$order), ]
  ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = assault)) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)
}

# available maps
# name of map provided by the maps package. 
# These include maps::county(), 
# maps::france(), maps::italy(), maps::nz(), 
# maps::state(), maps::usa(), maps::world(), maps::world2().




if (require("maps")) {
  
  worlds_maps <- map_data("world")
  choro <- worlds_maps
  choro <- choro[order(choro$order), ]
  wmap <- ggplot(world_map_d, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = 1)) #+
    # coord_map("gilbert")
    # coord_map("albers",  at0 = 0, lat1 = 0)
    # theme_minimal()
  
  wmap  + 
    coord_map("albers",  at0 = 0, lat1 = 0)+
    xlim(-180, 191) + ylim(-60, 83)
}



library(ggplot2)
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis(option="magma", direction = 1) + theme_bw()


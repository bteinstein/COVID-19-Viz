{
  rm(list = ls())
  library(tidyverse)
  library(gganimate)
  library(magick)
  library(ggdark)
  library(ggthemes)
  library(extrafont)
  loadfonts(device = "win", quiet = T)
  
# setwd("C:/Users/btein/Workplace/Visualization and Animation/COVID-19-Viz")
  
  # sourcing
  source('./Summary Board/src/data_format.R')
  source('./Summary Board/src/plots/flag_plot.R')
  source('./Summary Board/src/plots/header.R')
  source('./Summary Board/src/plots/banner.R')
  source('./Summary Board/src/plots/line_plot.R')
}


## some animation parameter
{
  nfrm = 180
  fps = 13
  ed_ps = 15
  wd = 1000
  ht_b = 150
}

# header
{
  anm_bn <- animate( banner_plot + transition_states(date), 
                     width = wd, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anm_dt <- animate( header_date + transition_states(date)+ enter_grow() + exit_fade() , 
                     width = 350, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anm_ic <- animate( header_ic + transition_states(date) + enter_grow() + exit_fade(), 
                     width = 200, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anm_ac <- animate( header_ac + transition_states(date) + enter_grow() + exit_fade(), 
                     width = 150, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anm_rc <- animate( header_rc + transition_states(date) + enter_grow() + exit_fade(), 
                     width = 150, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anm_dc <- animate( header_dc + transition_states(date) + enter_grow() + exit_fade(), 
                     width = 150, height = ht_b ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anim_save(anm_bn, filename = "./Summary Board/output/init/anm_bn.gif")
  anim_save(anm_dt, filename = "./Summary Board/output/init/anm_dt.gif")
  anim_save(anm_ic, filename = "./Summary Board/output/init/anm_ic.gif")
  anim_save(anm_ac, filename = "./Summary Board/output/init/anm_ac.gif")
  anim_save(anm_rc, filename = "./Summary Board/output/init/anm_rc.gif")
  anim_save(anm_dc, filename = "./Summary Board/output/init/anm_dc.gif")
}

# Line plot
{
  anm_ln <- animate( plot_lines + transition_reveal(date) + 
                       enter_grow() + exit_fade() +  
                       view_follow(fixed_y = TRUE), 
                     width = wd, height = 250 ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anim_save(anm_ln, filename = "./Summary Board/output/init/anm_ln.gif")
}

# Flag plot
{
  anm_fg <- animate( plot_flags + transition_states(date) + 
                       enter_grow() + exit_fade() , 
                     width = wd, height = 350 ,
                     nframes = nfrm, fps = fps, end_pause = ed_ps)
  
  anim_save(anm_fg, filename = "./Summary Board/output/init/anm_fg.gif")
} 

############## Clean ################
{
  rm(daily_top_three_cc_country, custom_theme_header, 
     data_daily, data_dsummary,sc_color,sc_fill, l_labs)
  
  rm(header_date, header_ic, header_ac, header_rc, header_dc,
     plot_lines, plot_flags, banner_plot)
  
}

######## Merge #############
{
  gf_bn <- image_read(anm_bn)
  gf_dt <- image_read(anm_dt)
  gf_ic <- image_read(anm_ic)
  gf_ac <- image_read(anm_ac)
  gf_rc <- image_read(anm_rc)
  gf_dc <- image_read(anm_dc)
  gf_ln <- image_read(anm_ln)
  gf_fg <- image_read(anm_fg)
  
  rm(anm_bn, anm_dt, anm_ic, anm_ac,  anm_rc, anm_dc,
     anm_ln, anm_fg)
  
  rm(fps, ed_ps, t_size, wd, ht_b)
}

######## Combination ##########
base_gif <- image_append(
  c(
    gf_bn[1],
    image_append(c(gf_dt[1], gf_ic[1],gf_ac[1],gf_rc[1],gf_dc[1]), stack = F),
    gf_ln[1],
    gf_fg[1]
  )
  ,stack = T
)


# anim_save(filename = "./Summary Board/output/v1.gif", base_gif)

for(i in 2:nfrm){
  cmb <- image_append(
    c(
      gf_bn[i],
      image_append(c(gf_dt[i], gf_ic[i],gf_ac[i],gf_rc[i],gf_dc[i]), stack = F),
      gf_ln[i],
      gf_fg[i]
    )
    ,stack = T
  )
  
  base_gif <- c(base_gif, cmb)
}


#image_write_gif(image = base_gif, path = "./Summary Board/output/covid-19_v1_12th.gif" )
anim_save(filename = "./Summary Board/output/covid-19_v2_12th.gif", base_gif)


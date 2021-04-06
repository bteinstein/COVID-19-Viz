library(wbstats)
# https://www.flaticon.com/packs/countrys-flags# 
# Icons made by <a href="https://www.flaticon.com/authors/freepik" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon"> www.flaticon.com</a>

cnts <- wbstats::wbcountries() #%>%  dplyr::select(country, iso2c)

View(cnts)



df <- data.frame(
     stringsAsFactors = FALSE,
          check.names = FALSE,
               Country = c("USA","Italy","Spain",
                           "China","Germany","France","Iran","UK",
                           "Switzerland","Turkey","Belgium","Netherlands"),
              Code = c("US","IT","ES","CN",'DE','FR','IR','GB','CH','TR','BE','NL')
           Total.Cases = c(215344L,110574L,
                           104118L,81589L,77981L,56989L,47593L,29474L,17781L,
                           15679L,13964L,13614L),
            New.Cases = c(341L, NA, NA, 35L, NA, NA, NA, NA, 13L, NA, NA, NA),
          Total.Deaths = c(5112L,13155L,9387L,
                           3318L,931L,4032L,3036L,2352L,488L,277L,828L,
                           1173L),
           New.Deaths = c(10L, NA, NA, 6L, NA, NA, NA, NA, NA, NA, NA, NA),
       Total.Recovered = c(8878L,16847L,22647L,
                           76408L,19175L,10935L,15473L,135L,2967L,333L,
                           2132L,250L),
          Active.Cases = c(201354L,80572L,72084L,
                           1863L,57875L,42022L,29084L,26987L,14326L,
                           15069L,11004L,12191L),
   `Serious,.Critical` = c(5005L,4035L,5872L,
                           429L,3408L,6017L,3871L,163L,348L,979L,1088L,
                           1053L),
   `Tot Cases/.1M.pop` = c(651L,1829L,2227L,57L,
                           931L,873L,567L,434L,2055L,186L,1205L,795L),
      `Deaths/1Mpop` = c(15L,218L,201L,2L,
                           11L,62L,36L,35L,56L,3L,71L,68L)
 )


library(ggplot2)
library(dplyr)
library(ggimage)

# country_code <- readr::read_csv('./Animated Bar Chart/data/country_code.csv')
df_p <-  df %>% left_join( cnts %>%  dplyr::select(country, iso2c), by=c('Country' = 'country'))

# Which country is not found
df_p[,c("Country","iso2c")]
code_not_found <- df_p$Country[which(is.na(df_p$iso2c))]

df_p$iso2c[df_p$Country %in% code_not_found] <- c('US',"IR",'UK')

# Function to scale the flag
scale_flag <- function(x, min_x, min_y, a = 0.05, b = 0.12){
  
  if (missing(min_x)|missing(min_y)) {
    min_x = min(x); max_x = max(x)
  }
 
  
  res = ( ((b-a)*(x-min_x)) / (max_x - min_x) ) + a
  
  return(res)
}

scale_flag(df$Total.Cases)

# Demo plot
df %>% ggplot(aes(x = 1, y=`Total.Cases`)) + 
  # geom_point(aes(size = log10(Total.Cases)/100))
  geom_flag(aes(x = 1, y=`Total.Cases`, image = Code, size = I(scale_flag(`Total.Cases`))))

df[1:3,] %>% ggplot(aes(x = 1, y=`Total.Cases`)) + 
  # geom_point(aes(size = log10(Total.Cases)/100))
  geom_flag(aes(x = 1, y=`Total.Cases`, image = Code, size = I(c(0.15,0.08,0.02))), alpha = 0) + 
  theme_classic()

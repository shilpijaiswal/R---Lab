
#' Shilpi Jaiswal
#' Winter 2016
#' R - Lab Finals

library("dplyr")  #for dplyr
library("RSQLite") #for sqllite
library("nycflights13") #for data
library(ggplot2)

my.db <- nycflights13_sqlite()



weather.df  <- weather %>% collect()
planes.df  <- planes %>% collect()
flights.df <- tbl(my.db, "flights") %>% 
                 collect() %>%
                 
                  mutate(
                       
                     cancelled = ifelse(is.na(arr_time), 1, 0), # 151 cancellations
                     delayed =  ifelse(dep_delay > 0, 1, 0),
                     date = paste(year, month, day, sep = "-"),
                     date = as.Date(date, "%Y-%m-%d")
                  )

weather.df <- weather.df %>%
             mutate(
                  
                  date = paste(year, month, day, sep = "-"),
                  date = as.Date(date, "%Y-%m-%d")
   
             )

flt.plane <- flights.df %>% 
              left_join(planes.df, by = "tailnum")

flt.plane.w <- flt.plane %>% inner_join(weather.df, by = "date")  %>% 
      select(-year.x, -month.x, -day.x, -precip, -hour.x, -hour.y, -manufacturer,-type, -speed)



 # probit_w <- glm(cancelled ~ temp+dewp+humid+wind_speed+pressure+visib, flt.plane.w, family=binomial(link="probit") )
 # summary(probit_w)

ggplot(
  
  flt.plane.w, aes( x = month.x, y = ,
    group = ,
    colour = 
  )
) +
  stat_smooth(span = 3) 


plot.w.cancel <-  ggplot(flt.plane.w, aes( x = wind_speed, y = dep_delay)) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_point(aes(colour = visib), alpha = 0.3)
  

plot.w.cancel

  
  plot2b <- ggplot(flt.plane.w, aes( x = wind , y = dep_delay)) + 
    scale_x_log10() + 
    scale_y_log10() + 
    geom_point(aes(colour = clarity), alpha = 0.3) + 
    aes(size = carat)  + scale_size(range = c(4, 15))
  
  plot2b

plot1b <- ggplot(flt.plane.w, aes(x = dewp, y = ..density.., fill = visib)) + 
  geom_histogram(binwidth = 1) +
  facet_grid(cancelled ~ .)

plot1b <- ggplot(flt.plane.w, aes(x = , y = ..density.., fill = visib)) + 
  geom_histogram(binwidth = 1) +
  facet_grid(delayed ~ .)

flt.plane.w; distinct(select(flt.plane.w, visib))

plot1b <- ggplot(diamonds, aes(x = dep_delay, y = ..density..)) + 
  geom_histogram(binwidth = 1) +
  facet_grid(visib ~ .)

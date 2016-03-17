
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

# Analysis on weather

plot1a <- ggplot(flt.plane.w, aes(x = dewp, y = ..density.., fill = visib)) + 
  geom_histogram(binwidth = 1) +
  facet_grid(cancelled ~ .)

plot2a <- ggplot(flt.plane.w, aes(x = dewp, y = ..density.., fill = visib)) + 
  geom_histogram(binwidth = 1) +
  facet_grid(delayed ~ .)

flt.plane.w; distinct(select(flt.plane.w, visib))

# Analysis on aspect of time

time_analysis <- flt.plane.w %>% group_by(year.y, month.y, day.y) %>%
  mutate(
    
    rw.median = median(dep_delay, na.rm = T),
    count = n()
  ) %>% 
  select(year.y, month.y,day.y, date, rw.median, delayed) %>%
  arrange(year.y, month.y, day.y)


plot2b <- ggplot(time_analysis, aes( x = date, y = rw.median)) + 
  geom_line(aes(color = month.y), size = 1.05)
plot2b

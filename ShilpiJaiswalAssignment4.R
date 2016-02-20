#' Shilpi Jaiswal
#' Winter 2016
#' ShilpiJaiswalAssignment3.R
#'

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1 
print("Shilpi")
print("Jaiswal")
print(1505120)



library(dplyr)
library("nycflights13")
library(tidyr)

flights <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv", stringsAsFactors=FALSE)
planes <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv", stringsAsFactors=FALSE)
weather <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv", stringsAsFactors=FALSE)
airports <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv", stringsAsFactors=FALSE )

weather$date <- as.Date(weather$date, "%Y-%m-%d")

flights$date <- substr(flights$date, 1, 10)
flights$date <- as.Date(flights$date, format = "%Y-%m-%d")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3a

require(dplyr)

airports %>% tbl_df()

flights.2a <- flights %>% tbl_df() %>%
  filter(
    dest == "SFO" | dest == "OAK"
  )%>%
  left_join(
    airports, by = c("dest" = "iata")
  ) 
print(nrow(flights.2a))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3b
flights.2b <- flights %>%
  filter(
    arr_delay > 60
  )

print(nrow(flights.2b))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 3c

flights.2c <- flights%>%
  filter(
    (arr_delay > 0 & dep_delay > 0 ) & ( arr_delay > 2*dep_delay )
  )

print(nrow(flights.2c))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 4

?dplyr::select

flights%>%
  select(
    ends_with("delay")
  )

flights%>%
  select (
    contains("delay")
  )

flights%>%
  select(
    matches("delay") 
  )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 5

#5a
flights.5a <- flights %>% tbl_df() %>%
  arrange(
    desc(dep_delay)
  )%>%
  select(
    date, dep:dest
  ) 
print(head(flights.5a,5))


#5b


flights.5b <- flights %>%
  #filter(
  #  (dep_delay > 0 & arr_delay < 0)
  #) %>%
  arrange(
    #desc(dep_delay), arr_delay
    desc(dep_delay - arr_delay)
  ) %>%
  select(
    date, dep:dest
  )
print(head(flights.5b,5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 6

flights <- flights %>%
  mutate(
    speed = (dist / time) * 60,
    delta   = (dep_delay - arr_delay)
  )

# 6a                
print(head(arrange(flights.6, desc(speed)), 5))

# 6b
print(head(arrange(flights.6, desc(delta)), 5))

# 6c
print(head(arrange(flights.6, delta), 5))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 7

# 7a
flights.7a <- flights %>%
                group_by(carrier) %>%
                summarise(
                  cancelled.flights = sum(cancelled, na.rm = T),
                  total.flights = n(),
                  percent.cancelled = sum(cancelled, na.rm = T)/n(),
                  delta.min = min(delta, na.rm = T),
                  delta.q1 = quantile(delta, 0.25, na.rm = T),
                  delta.mean = mean(delta, na.rm = T),
                  delta.median = median(delta, na.rm = T),
                  delta.q3 = quantile(delta, 0.75, na.rm = T),
                  delta.q90 = quantile(delta, 0.9, na.rm = T),
                  delta.max = max(delta, na.rm = T)
                )

flights.7a <- arrange(flights.7a, desc(percent.cancelled))
summary(flights.7a)

print("The below code filters out NA values in the dep_delay column in flights table, groups it by date and finds mean value of 
delay  for a given date along with the number of delayed flights on a given day. And finaly filters all observations with
number of delayed flights more than 10")

day_delay <- flights %>%
                filter(!is.na(dep_delay))%>%
                group_by(date)%>%
                summarise(
                  delay = mean(dep_delay),
                  n=n()
                ) %>%
                filter(n > 10) %>% print(head())

day_delay <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
      ),
      date
    ),
    delay = mean(dep_delay),
    n = n()
  ),
  n > 10
)
print(head(day_delay, 10))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 8

arrange(day_delay, date)
delay.8a <- day_delay %>%
               mutate(delay_today = delay,
                  delay_yesterday = lag(delay, 1),
                  delay_inc = delay_today - delay_yesterday)

delay.8a <- delay.8a  %>% 
               arrange(-delay_inc)

head(delay.8a, n=5)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 9


# 9a
dest_delay <- flights %>% filter(!is.na(dep_delay)) %>%
                  group_by(dest) %>%
                  summarize(
                     arr_delay = mean(arr_delay),
                     n = n()
                  )

airports <- airports %>%
               select(dest =iata, name=airport, city, state, lat, long)

airports %>% tbl_df()

df.9a <- dest_delay %>% tbl_df() %>%
            left_join(
               airports, by = c("dest" = "dest")
            ) %>% 
            arrange(-arr_delay)

print(head(df.9a, n=5))


# 9b

df.9b <- dest_delay %>% tbl_df() %>%
            inner_join(
               airports, by = c("dest" = "dest")
             )
  
print(nrow(df.9a))
print(nrow(df.9b))
print("The number of observations are close, but do not match exactly.")

# 9c

df.9c <- dest_delay %>% tbl_df() %>%
            right_join(
              airports, by = c("dest" = "dest")
            )
print(nrow(df.9c))
print("There are NAs in in the arr_delay column, because there are more observations in airports than in dest_delay")
# The right join returns all rows from the airports, and any rows with matching keys from the dest_delay table.
# thus it has 3376 rows which is same as no. of rows in airports table. Hence so many NA values.

# 9d

df.9d <- dest_delay %>% tbl_df() %>%
           right_join(
             airports, by = c("dest" = "dest")
           )
nrow(df.9d)
print("Again there are NAs in arr_delay, because airports has a different number of observations")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 10

hourly_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
                  group_by(date, hour)%>%
                  summarise(
                  delay = mean(dep_delay)
                  )

hourly_delay$date <- as.Date(hourly_delay$date)

df.10a <-  hourly_delay %>% tbl_df() %>%
              left_join(
                  weather, by=c("date"="date")
              )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 11
require(tidyr)
require(dplyr)
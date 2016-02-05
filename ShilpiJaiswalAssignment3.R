#' Shilpi Jaiswal
#' Winter 2016
#' ShilpiJaiswalAssignment3.R
#' 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 0 
print("Shilpi Jaiswal")
print(1505120)
print("shjaiswa@ucsc.edu")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
#loaded this way, it's a data frae
class(df.ex)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 2
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 3
df.ex.3a <- df.ex %>%
  dplyr::arrange(
    year, month 
  )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Question 4
df.ex.4a <- df.ex %>%
  dplyr::select(
    year:age 
  )
df.ex.4b <- df.ex %>%
  dplyr::select(
    year, month, starts_with("i") 
  )

df.ex %>%
  dplyr::select(state)%>%
  dplyr::distinct(state)


 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
 # Question 5 


# Standardizing function
 stndz <- function(x){ 
   (x - mean(x, na.rm = T))  /  sd(x, na.rm = T) 
 } 
 
 # Normalizing function
 nrmlz <- function(x){ 
   (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) 
 }
 
 # 5a
 df.ex.5a<- df.ex %>%
   dplyr::mutate(
     rw.stndz = stndz(rw),
     rw_nrmlz = nrmlz(rw)
   )
 

 # 5b 
 df.ex.5b <- df.ex %>% 
   dplyr::group_by(year, month) %>% 
   dplyr::mutate( 
     rw.stndz = stndz(rw), 
     rw_nrmlz = nrmlz(rw), 
     count    = n() 
   )


 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
 # Question 6 
 df.ex.6 <- df.ex %>% 
   dplyr::group_by(year, month, state) %>% 
   dplyr::summarise( 
     rw_min = min(rw, na.rm = T), 
     rw_1stq = quantile(rw, 0.25, na.rm = T), 
     rw_mean = mean(rw, na.rm = T), 
     rw_median = median(rw, na.rm = T), 
     rw_3rdq = quantile(rw, 0.75, na.rm = T), 
     rw_max = max(rw, na.rm = T), 
     count = n() 
   ) 
print(nrow(df.ex.6))

print(df.ex.6 %>% 
        ungroup() %>% 
        dplyr::arrange(desc(rw_mean)) %>% 
        dplyr::select(year, month, state) %>%
               head(1)
      )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# Question 7

df.ex.7a <- df.ex %>% 
 dplyr::arrange(
   year, month, desc(as.character(state))
 ) 

# Checking if the states are in decending order in df.ex.7a
print (df.ex.7a %>% 
  dplyr::select(state)%>%
  dplyr::distinct(state)
   )


#' Shilpi Jaiswal
#' Winter 2016
#' ShilpiJaiswalAssignment3.R
#' 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 0 
print("Shilpi")
print("Jaiswal")
print(1505120)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1
library(ggplot2)

# 1 a)  Scatter plots of prices

plot1a <- ggplot(diamonds, aes( x = x*y*z, y = price)) + 
                          scale_x_log10() + 
                          scale_y_log10() + 
                          geom_point(aes(colour = clarity), alpha = 0.3) + 
                          aes(size = carat)  + scale_size(range = c(4, 15))
plot1a

# 1 b)  Stacked Histograms

plot1b <- ggplot(diamonds, aes(x=carat, y = ..density.., fill = clarity)) + 
            geom_histogram(binwidth = 0.2) +
            facet_grid(cut ~ .)
plot1b

# 1 c)  Violin plots of price by cut

plot1c <-  ggplot(diamonds, aes( x = cut, y = price)) + 
           geom_violin(size = 1.001) +
           geom_jitter( alpha = 1/60, size = 3.5)

plot1c

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2 a)

library(foreign)
library(dplyr)

# Monthly median real wage by education

org  <- read.dta("http://people.ucsc.edu/~aspearot/Econ_217/org_example.dta")
org <- subset(org, !is.na(rw))

org.date <-  org %>%
             group_by(year, month) %>%
             mutate(
               
                    rw.median = median(rw, na.rm = T),
                    date = paste(year, month, "01", sep = "-"),
                    date = as.Date(date, format = "%Y-%m-%d"),
                    quartile1 = quantile(rw, 0.25, na.rm = T, type = 7),
                    quartile3 = quantile(rw, 0.75, na.rm = T, type = 7),
                    decile1 = quantile(rw, 0.1, na.rm = T, type = 5 ),
                    decile3 = quantile(rw, 0.9, na.rm = T, type = 5),
                    count = n()
            ) %>% select(year, month, date, rw.median, quartile1, quartile3, decile1, decile3, educ)

plot2a <- ggplot(org.date, aes( x = date, y = rw.median)) + 
            geom_line(aes(y = rw.median), size = 1.07) +
            geom_ribbon(aes(ymin = quartile1, ymax = quartile3), alpha = 0.4) + 
            geom_ribbon(aes(ymin = decile1, ymax = decile3), alpha = 0.2) +
            ylim(0, 50)
plot2a

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 2 b)

# Monthly median real wage by education level

org.educ <- org %>%
            group_by(year, month, educ) %>%
            mutate(
    
               rw.median = median(rw, na.rm = T),
               date = paste(year, month, "01", sep = "-"),
               date = as.Date(date, format = "%Y-%m-%d"),
               count = n()
            ) %>% 
            select(year, month, date, rw.median, educ) %>%
            arrange(year,month)


plot2b <- ggplot(org.educ, aes( x = date, y = rw.median)) + 
          geom_line(aes(color = educ), size = 1.05)
plot2b
=======
  group_by(year, month, educ) %>%
  mutate(
    
    rw.median = median(rw, na.rm = T),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d"),
    count = n()
  ) %>% 
  select(year, month, date, rw.median, educ) %>%
  arrange(year,month)


plot2b <- ggplot(org.educ, aes( x = date, y = rw.median)) + 
  geom_line(aes(color = educ), size = 1.05)
plot2b
>>>>>>> f4950a16f152cb949a69eec97ba62f3bb82adc29

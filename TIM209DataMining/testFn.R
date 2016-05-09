
aapl.01.02.exp15P <- subset(aapl.01.02, aapl.01.02$daysToExp == 15 & aapl.01.02$type =="P", drop = FALSE)

S0 <- 110

S095 = S0*0.95

strike <- as.numeric(aapl.01.02.exp15P$strike)

put.01.02 <- aapl.01.02.exp15P[which(abs(strike-S095) == min(abs(strike-S095))),]

listDF <- list( aapl.01.05, aapl.01.06, aapl.01.07, aapl.01.08, aapl.01.09, aapl.01.10, aapl.01.11, aapl.01.12, 
                aapl.01.13, aapl.01.14, aapl.01.15, aapl.01.16, aapl.01.17, aapl.01.18, aapl.01.19, aapl.01.20, 
                aapl.01.21, aapl.01.22, aapl.01.23, aapl.01.24, aapl.01.25, aapl.01.26, aapl.01.27, aapl.01.28, 
                aapl.01.29, aapl.01.30, aapl.01.31, aapl.02.01, aapl.02.02, aapl.02.03, aapl.02.04, aapl.02.05, 
                aapl.02.06, aapl.02.07, aapl.02.08, aapl.02.09, aapl.02.10, aapl.02.11, aapl.02.12, aapl.02.13, 
                aapl.02.14, aapl.02.15, aapl.02.16, aapl.02.17, aapl.02.18, aapl.02.19, aapl.02.20, aapl.02.21, 
                aapl.02.22, aapl.02.23, aapl.02.24, aapl.02.25, aapl.02.26, aapl.02.27, aapl.02.28, aapl.02.29, 
                aapl.02.30, aapl.02.31
              )
# OptionDF <- put.01.02 
search_1 <- function(x){
               #print(class(x))
               return(x[x$ticker == "BBG003D0G938 Equity",])
            }

put.01.02  <- rbind(put.01.02, row.next.day)
aapl.01.02Ticker <- search_1(aapl.01.07)

x <- subset(aapl.01.07 , aapl.01.07$ticker == "BBG003D0G938 Equity")
 Y <- lapply(listDF, search_1, "BBG003D0G938 Equity")
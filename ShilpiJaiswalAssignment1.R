# Problem 5

rowvector <- c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA) 
length(rowvector) 


# Problem 6

X = matrix( c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)

transposeX <- t(X)
eigenvalX <- eigen(X)

eigenvalX$val 
eigenvalX$vec 


Y = matrix( c(1, 2, 3, 3, 2, 1, 2, 3, 0), nrow = 3, ncol = 3, byrow = TRUE)
solve(Y)

# Problem 7

carat <- c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut <- c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity <- c("SI1","I1", "VI1", "VS1", "IF", "VVS2", "NA" ) 
price <- c(850, 450, 450, NA, 750, 980, 420 )
diamond <- data.frame(carat, cut, clarity, price)       # diamond is a data frame

#a
mean(price, na.rm = TRUE)

#b
faircut <- subset(diamond, cut == "fair")
mean(faircut$price, na.rm = TRUE)

#c
faircut1 <- subset(diamond, cut != "fair" )
median(faircut1$price, na.rm = TRUE)

#d
faircut2 <- subset(diamond, cut == "Ideal" & carat > 2 | cut == "very good" & carat > 2)
median(faircut2$price, na.rm = TRUE)
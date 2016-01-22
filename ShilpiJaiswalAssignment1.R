


# R - Lab Assignment 1

install.packages("gridExtra")

# # Problem 0

firstname <- "Shilpi"
lastname <- "Jaiswal,"
studentID <- "1505120"
print(paste(firstname,lastname, studentID)) # Name and Student ID
print("Lab assignment 1")

# # Problem 1

library(foreign)

# Stata file
df.dta <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")

# CSV file
df.csv <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")

# Tab delineated file file
df.td <- read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")

# R Data file
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))

# # Problem 2

print("Size of DTA file is 188 KB ")
print("Size of CSV file is 139 KB ")
print("Size of TXT file is 139 KB ")
print("Size of RDATA file is 45.3 KB ")

print("The RDATA file is the smallest. This variability might be due to the difference in how these file types allocate memory. 
      When we open data in notepad we see there is lot of space. In csv each observations are new line.")


# # Problem 3
print(paste("The class of data strcture is: ", class(NHIS_2007_RData)))
print(paste("The typeof of data strcture is: ", typeof(NHIS_2007_RData)))
print(paste("The length of file is: ", length(NHIS_2007_RData)))
print(paste("The dimension of file is: ", dim(NHIS_2007_RData)))


print(paste(nrow(NHIS_2007_RData), ": number of rows"))
print(paste(ncol(NHIS_2007_RData), ": number of columns"))
print(paste(summary(NHIS_2007_RData), ": Summary of table"))

print(paste(summary(NHIS_2007_RData$rw), ": Summary of variable rw"))

# # Problem 4 : Load org.dta

df <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

print(summary(df))

print(str(df))
print(summary(df$rw))

print("The number of observtions and variables in org data set is, 1119754 obs. and  30 variables ")

# Column variable rw

print("Min of variable rw = 1.8")
print("Mean of variable rw = 19.8")
print("Median of variable rw = 15.9")
print("Third quartile value of variable rw = 24.4")
print("The number of NA's are 521279 ")
# # Problem 5

v <- c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA) 
length(v) 
print("The length of v:")
print(length(v)) # print length of v
print("In R the null values is considered as no data so the length command
      ignores the NULL, on the other hand NA is considered to be unavailable 
      data and is counted by the length command")

print("Mean of the vector is:")
print(mean(v, na.rm = TRUE))

# # Problem 6

#creating matrix X
X = matrix( c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)
print("Matrix X:")
X

# Finding transpose of V
print("The transpose of X is:")
transposeX <- t(X)
transposeX

# Finding eigenvalue and eigenvector of X
eigenvalX <- eigen(X)
print("The eigenvalue of X is")
print(eigenvalX)

print("The eigenvector of X is:")
A <- eigenvalX$vec 
A

# Creating matrix Y
Y = matrix( c(1, 2, 3, 3, 2, 1, 2, 3, 0), nrow = 3, ncol = 3, byrow = TRUE)
print("Matrix Y:")
Y

print("Inverse of Matrix Y:")
Y_inverse <- solve(Y)
Y_inverse

print("Product of Y and Y_inverse:")
product <- Y %*% Y_inverse 
product
print("The product of a matrix with its inverse gives the Identity Matrix")

# # Problem 7

carat <- c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut <- c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity <- c("SI1","I1", "VI1", "VS1", "IF", "VVS2", "NA" ) 
price <- c(850, 450, 450, NA, 750, 980, 420 )
diamond <- data.frame(carat, cut, clarity, price)       # diamond is a data frame
print(summary(diamond))

library(gridExtra)
grid.table(diamond)

#a
print("Mean price of diamonds ")
print(mean(price, na.rm = TRUE))

#b
faircut <- subset(diamond, cut == "fair")
print("Mean price of fair cut diamonds ")
print(mean(faircut$price, na.rm = TRUE))

#c
faircut1 <- subset(diamond, cut != "fair" )
print("Median price of diamonds other than fair cut")
print(median(faircut1$price, na.rm = TRUE))

#d
faircut2 <- subset(diamond, cut == "Ideal" & carat > 2 | cut == "very good" & carat > 2)
print("Median price of diamond of cut 'good', 'very good' and 'Ideal'")
print(median(faircut2$price, na.rm = TRUE))


# End of file

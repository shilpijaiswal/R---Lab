

# ---

# title: Assignment 2 script. 

# author: "Shilpi Jaiswal"

# date: "Winter 2016"

# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_2.pdf

# ---

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Question 0


ShilpiJaiswalAssignment2 <- list(
  
  firstName = "Shilpi",
  
  lastName  = "Jaiswal",
  
  email     = "shjaiswa@ucsc.edu",
  
  studentID = 1505120
  
)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#Problem 1##
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"))

ShilpiJaiswalAssignment2$s1a <- nrow(diamonds)

ShilpiJaiswalAssignment2$s1b <- ncol(diamonds)

ShilpiJaiswalAssignment2$s1c <- names(diamonds)

ShilpiJaiswalAssignment2$s1d <- summary(diamonds$price)

###############################################################################

#Problem 2##

NHIS_2007 <- read.table("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt", sep = "\t", header = TRUE)


ShilpiJaiswalAssignment2$s2a <- nrow(NHIS_2007)

ShilpiJaiswalAssignment2$s2b <- ncol(NHIS_2007)

ShilpiJaiswalAssignment2$s2c <- names(NHIS_2007)

ShilpiJaiswalAssignment2$s2d <- mean(NHIS_2007$weight,na.rm=TRUE)

ShilpiJaiswalAssignment2$s2e <- median(NHIS_2007$weight, na.rm=TRUE)

hist(NHIS_2007$weight) 
table(NHIS_2007$weight)

NHIS_2007$newweight <- ifelse(NHIS_2007$weight >= 996 & NHIS_2007$weight <= 999, NA, NHIS_2007$weight)
hist(NHIS_2007$newweight) 
table(NHIS_2007$newweight)

ShilpiJaiswalAssignment2$s2f <- mean(NHIS_2007$newweight,na.rm=TRUE)

ShilpiJaiswalAssignment2$s2g <- median(NHIS_2007$newweight,na.rm=TRUE)

ShilpiJaiswalAssignment2$s2h <- summary( subset(NHIS_2007, SEX == 2)$newweight) 

ShilpiJaiswalAssignment2$s2i <- summary( subset(NHIS_2007, SEX == 1)$newweight) 


###########################################################################

#Problem 3##
vec <- c(letters,LETTERS)

ShilpiJaiswalAssignment2$s3a <- vec[seq(2,26,2)]

ShilpiJaiswalAssignment2$s3b <- paste(vec[c(45,8,9)], collapse="")

arr <- array( c(letters,LETTERS), dim = c(3,3,3), dimnames = NULL)

ShilpiJaiswalAssignment2$s3c <- arr[1:3,1,2]

ShilpiJaiswalAssignment2$s3d <- arr[2,2,1:3]

ShilpiJaiswalAssignment2$s3e <- paste(arr[1,1,3],arr[2,3,1],arr[3,3,1], sep = "")

#Problem 4##

library(foreign)
orgdata<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

sort(unique(orgdata$year))
sort(unique(orgdata$month))
sort(unique(orgdata$educ))
#Sorting data.

ShilpiJaiswalAssignment2$s4 <- aggregate( orgdata$rw, list(year=orgdata$year, month=orgdata$month, educ=orgdata$educ), mean, na.rm=TRUE )

print(ShilpiJaiswalAssignment2$s4)

# Save Rdata file
save( ShilpiJaiswalAssignment2, file = "ShilpiJaiswalAssignment2.Rdata"
  
)



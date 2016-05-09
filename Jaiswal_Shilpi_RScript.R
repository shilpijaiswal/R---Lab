
# Problem 1
library(class)
df <- data.frame(X1 = c(0,2,0,0,-1,1), X2 = c(3,0,1,1,0,1), X3 = c(0,0,3,2,1,1))
Y = c("Red","Red","Red","Green","Green","Red")

train <- df
test <- c(0,0,0)

cl <- factor(Y)
#a 

dist <- rbind(test, df)
eucl_dist <- dist(dist, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

#b
knn(train, test, cl, k = 1)

#c
knn(train, test, cl, k = 3)

# Problem 2
set.seed(1200)
x <- rnorm(50)
y <- 2 + 3*x + rnorm(50)

reg <- lm(y~x)

par(mfrow = c(1,2))

plot(x, main="Changing title and lables", xlab = "Index of Observations", ylab = "Observations")
plot(x, y)
abline(reg, col= "green", lwd = 2)
points(mean(x), mean(y), pch = "X")
arrows(1, -4.5, mean(x), mean(y), col = "red")
text(1, -4.5, "Center", col = "red")

# Problem 3

#3a

college <- read.csv("C:/Users/jaisw/Downloads/College.csv")
fix(college)

#3b
rownames (college ) <- college [,1]
fix(college)

college <- college [, -1]
fix (college)

#3c

#3c i)
summary(college)

#3c ii)
pairs(college[1:10]) 

#3c iii)
plot(college$Private, college$Outstate, ylab = "Out-of-state tuition", xlab = "Private Colleges")

#3c iv)

Elite <- ifelse(college$Top10perc > 50, "Yes", "No")
Elite <- as.factor (Elite)
college <- data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate, ylab = "Out-of-state tuition", xlab = "Elite Colleges")

#3c v)
par(mfrow=c(2,2)) 
hist(college$Outstate, breaks = 30  , xlab = "Out of State Tuition")
hist(college$Expend, breaks = 25  , xlab = "Instructional expenditure per student")
hist(college$Books, breaks = 20  , xlab = "Estimated book costs")
hist(college$perc.alumni, breaks = 15  , xlab = "Percent of alumni who donate")

#3c vi)
library(ggplot2)

accept.rate <- college$Accept/college$Apps*100
college <- data.frame(college, accept.rate)

p1 <- ggplot(
  data = college, 
  aes(x = accept.rate, 
      y = Outstate)
) + geom_point(aes(color = Private))
p1

p2 <- ggplot(
  data = college, 
  aes(x = Accept, 
      y = Outstate)
) + geom_point(aes(color = Private))
p2

p3 <- ggplot(
  data = college, 
  aes(x = Grad.Rate, 
      y = PhD)
) + geom_point(aes(color = Elite))
p3

plot(college$Elite, college$perc.alumni, xlab = "Elite", ylab = "Percent alumni donating")

d <- ggplot(college, aes(x = S.F.Ratio, y = Grad.Rate)) + geom_point(aes(colour = factor(PhD) ))

d
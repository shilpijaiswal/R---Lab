
library(ISLR)
library(MASS)
library(stargazer)

lm.fit1 <- lm(Sales ~ Price+Urban+US, data=Carseats)

summary(lm.fit1)

stargazer(lm.fit1, type = "text", title="Sales figures", digits=3, out="table1.text") 



lm.fit2 <- lm(Sales ~ Price+US ,data = Carseats)

summary(lm.fit2)

stargazer(lm.fit2, type = "text", title="Sales figures", digits=3, out="table2.text") 

confint(lm.fit2)

plot(predict(lm.fit2), rstudent(lm.fit2), xlab = "Fitted Values", ylab = "Studentized Residuals")
abline(h = 0, lty = 2)

plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))

# Problem 3

# (a) 
set.seed(1) 
X <- rnorm(100)

# (b)
eps <- rnorm(100, 0, 0.25)

# (c)
Y <- -1 +0.5*X + eps
# The length of the vector Y is 100. Bo = -1 and B1 = 0.5 in this linear model.

# (d) Scatterplot displaying relationship between X and Y.
plot(X, Y)
# Comment on the scatterplot what you observe?

# (e) least square line
lm.fitXY <- lm(Y ~ X)
summary(lm.fit3)

stargazer(lm.fitXY, type = "text", title="Linear Model", digits=3, out="table3.text") 
# Bo_hat = -1 and B1_hat = 0.5 in this linear model.

# (f)
#population <- lm(Y~X+offset(eps))

plot(X, Y)
abline(lm.fitXY, col= "red", lwd = 3)
abline(lm(Y~X+eps, drop.unused.levels=T), col= "green", lwd = 3)
legend("topleft", legend = c("Fitted line","Population Line"),lty = c(1,1), col = c("red", "green"))

# (g)

lm.fitXsqY <- lm(Y~X+I(X^2))
summary(lm.fitXsqY)

stargazer(lm.fitXsqY, type = "text", title="Polynomial Model", digits=3, out="table4.text") 

# (h)
set.seed(1) 
X2 <- rnorm(100)

eps2 <- rnorm(100, 0, 0.09)

Y2 <- -1 +0.5*X2 + eps2

lm.fitX2Y2 <- lm(Y2 ~ X2)
summary(lm.fitX2Y2)
stargazer(lm.fitX2Y2, type = "text", title="Linear Model", digits=3, out="table5.text") 

plot(X2, Y2)
abline(lm.fitX2Y2, col= "blue", lwd = 4)
abline(lm(Y2~X2+eps2, drop.unused.levels=T), col= "pink", lwd = 3)
legend("topleft", legend = c("Fitted line(less noise)","Population Line "),lty = c(1,1), col = c("blue", "pink"))

# (i)
set.seed(1) 
X3 <- rnorm(100)

eps3 <- rnorm(100, 0, 0.5)

Y3 <- -1 +0.5*X3 + eps3

lm.fitX3Y3 <- lm(Y3 ~ X3)
summary(lm.fitX3Y3)
stargazer(lm.fitX3Y3, type = "text", title="Linear Model", digits=3, out="table5.text") 

plot(X3, Y3)
abline(lm.fitX3Y3, col= "blue", lwd = 4)
abline(lm(Y3~X3+eps3, drop.unused.levels=T), col= "green", lwd = 3)
legend("topleft", legend = c("Fitted line(more noise)","Population Line "),lty = c(1,1), col = c("blue", "green"))

# (j)
confint(lm.fitX3Y3)
confint(lm.fitX2Y2)
confint(lm.fitXY)
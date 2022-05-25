rm(list=ls())  #clear load space
setwd('/Users/lysi2/Documents/UNI_Imperial/Applied_Stats/Assignments/A2/Code')
library(tidyverse)
library(faraway)
library('MASS')
library(car)
library(margins)




#Multicollinearity
data("longley")
head(longley)
mylm1 <- lm(Employed ~ .,data=longley)
#very large
vif(mylm1)
cor(longley$GNP.deflator, longley$Population) #high correlation

#Box-Cox Transformation

data(savings)
head(savings)
mylm <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
#output: log likelihood for boxcox transformations
bc <- boxcox(mylm, plotit = TRUE)
boxcox(mylm, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))
#we roughly see where the lamdba corresponding to max likelihood is
lambda <- bc$x
lik <- bc$y
together <- cbind(lambda, lik)
max_lam <- together[order(-lik),][1]
#and now fit a new linear model with a value close to the new lambda
#eg. lamda = 1/3
my_new_lm <- lm(sr^1/3 ~ pop15 + pop75 + dpi + ddpi, data = savings)


#marginal effects
head(mtcars)
lm4 = lm(mpg ~ disp + hp + wt, data=mtcars)
lm4$coefficients
head(marginal_effects(lm4))
lm5 = lm(mpg ~ disp + hp + wt + disp:hp, data=mtcars)
head(marginal_effects(lm5))

#dummy-encoding
data(Prestige)
head(Prestige)
contrasts(Prestige$type) <- "contr.sum"
contrasts(Prestige$type) <- "contr.helmert"
contrasts(Prestige$type) <- "contr.treatment"
lm.treatment <- lm(prestige ~ type, data = Prestige)
contrasts(Prestige$type) <- "contr.sum"
lm.sum <- lm(prestige ~ type, data = Prestige)
contrasts(Prestige$type) <- "contr.helmert"
lm.helmert <- lm(prestige ~ type, data = Prestige)


#one factor models
data(coagulation); head(coagulation)


plot(coag ~ diet, data = coagulation, ylab = "coag time")
with(coagulation, stripchart(coag ~ diet, 
          vertical = TRUE, method = "stack",xlab = "diet", ylab = "coag time"))

mylm <- lm(coag ~ diet, data = coagulation)
summary(mylm)
mylm2 <- lm(coag ~ diet - 1, data = coagulation)
summary(mylm2)

modnull <- lm(coag ~ 1, data = coagulation)
anova(modnull,mylm2)

qqnorm(residuals(mylm))
plot(jitter(fitted(mylm)), residuals(mylm), xlab = "Fitted",ylab = "Residuals")


#interaction plots
lm(mpg ~ wt + drat:wt, data = mtcars)
## number of samples
n <- 500
## variables
x1 <- rbinom(n, size = 1, prob = 0.5)
x2 <- runif(n, min = -5, max = 5)
## true intercept and beta values
a <- 0.88
b1 <- 1.5
b2 <- -0.34
b3 <- -4
e <- rnorm(n, mean = 0, sd = 5)
## simulate data
y <- a + (b1 * x1) + (b2 * x2) + (b3 * x1 * x2) + e
sim.dat <- data.frame(y, x1, x2)


lm1 <- lm(y ~ x1 * x2, dat = sim.dat)
summary(lm1)



mycoef <- coef(lm1)
sim1 <- sim.dat[sim.dat$x1 == 0, ]
sim2 <- sim.dat[sim.dat$x1 == 1, ]
plot(sim1$x2, sim1$y, pch = 16, xlab = expression(x[2]), ylab = "y")
abline(a = mycoef[1], b = mycoef[3], lty = 2, lwd = 3, col = "red")
points(sim2$x2, sim2$y, pch = 17, col = "pink")
abline(a = mycoef[1] + mycoef[2], b = mycoef[3] + mycoef[4], lty = 2,lwd = 3, col = "blue")


lm2 <- lm(y ~ x1:x2, data = sim.dat)
summary(lm2)

plot(sim1$x2, sim1$y, pch = 16, xlab = expression(x[2]), ylab = "y")
points(sim2$x2, sim2$y, pch = 17, col = "pink")
abline(a = coef(lm2)[1], b = coef(lm2)[2], lwd = 2)
e <- rnorm(n, mean = 0, sd = 0.5)
y <- a + (b1 * x1) + (b2 * x2) + e
sim.dat2 <- data.frame(y, x1, x2)
lm3 <- lm(y ~ x1 * x2, dat = sim.dat2); summary(lm3)

# interaction plots again
data("ToothGrowth")
head(ToothGrowth)
lmto = lm(len ~ supp * factor(dose), data=ToothGrowth)
summary(lmto)
# the lines are not parallel, indicating the potential presence of an interaction effect.
interaction.plot(ToothGrowth$supp, factor(ToothGrowth$dose), ToothGrowth$len)
interaction.plot(factor(ToothGrowth$dose), ToothGrowth$supp, ToothGrowth$len)

l1 <- lm(len ~ supp + factor(dose) + supp:factor(dose) , data=ToothGrowth)


#GLMs

x = rep(c(-1,0,1),c(2,4,3))
y = c(2,4,6,7,8,10,11,12,14)
plot(y~x)

M = 5 #Number of iterations
X = cbind(1,x) #Specifying design matrix 
beta = c(10,5) #Initial guess
for (i in 1:M){
  eta = X%*%beta #Estimated linear predictor 
  mu = eta #Estimated mean response
  z = y #Form the adjusted variate
  w = 1/mu #weights
  lmz = lm(z~x, weights=w) #regress z on x with weights w 
  beta = lmz$coefficients #new beta
  # print(beta) #print out the beta estimate every iteration
  print(mu)
}



J= t(X)%*%diag(1/as.numeric(mu))%*%X # Fisher information matrix 
# this is cov(beta)
invJ = solve(J) 
#standard error
sqrt(diag(invJ))

#now using glm function
myglm <- glm(y~x,family=poisson(link="identity"))
summary(myglm)

#this is the same as the invJ
vcov(myglm)

plot(y~x)
abline(beta)
points(x= 0.25, y= 8,col='red')

x_star <- matrix(data = c(1,0.25), byrow = T, nrow = 2)

eta_star <- t(x_star)%*%beta
eta_star_R <- predict(myglm, data.frame(x=x_star))

z_alpha <- function(alpha) c(qnorm((alpha)/2), -qnorm((alpha)/2))


#careful, if link function is not identity you need to transform.
CI <-c(eta_star + z_alpha(0.05)[1]*sqrt(t(x_star)%*%invJ%*%x_star),
       eta_star + z_alpha(0.05)[2]*sqrt(t(x_star)%*%invJ%*%x_star))
CI










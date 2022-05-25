rm(list=ls())  #clear load space
setwd('/Users/lysi2/Documents/UNI_Imperial/Applied_Stats/Assignments/A3/Code')
library(tidyverse)
library(faraway)
library('MASS')
library(car)
library(margins)

source('glmxy.R')

x <- dfrm$x
y <- dfrm$y


myglm <- glm(y~x,family = poisson(link='log'))
summary(myglm)
print(myglm$coefficients)

plot(x,y)
xs <- seq(0,1,by=0.01)
lines(xs,y=exp(myglm$coefficients[1] +myglm$coefficients[2]*xs),col='red')

#c

true_beta <- c(-0.5,-2.5)
est_beta <- c(myglm$coefficients[1],myglm$coefficients[2] )
xs <- seq(0,2.5,by=0.01)
plot(exp(myglm$coefficients[1] +myglm$coefficients[2]*x), y,
     cex=0.4, xlab='inverse link function of the estimated linear predictors')
abline(0,1,col='red', lwd=2)
lines(xs, 2/(exp(-(true_beta[1] +
    true_beta[2]*(log(xs)-est_beta[1])/est_beta[2]))-1), col='blue', lwd=2, lty=2)

#d
beta <- myglm$coefficients
x_star <- matrix(data = c(1,0.5), byrow = T, nrow = 2)
invJ <- vcov(myglm)

eta_star <- t(x_star)%*%beta
eta_star_R <- predict(myglm, data.frame(x=x_star))

z_alpha <- function(alpha) c(qnorm((alpha)/2), -qnorm((alpha)/2))

CI <-c(exp(eta_star + z_alpha(0.01)[1]*sqrt(t(x_star)%*%invJ%*%x_star)),
       exp(eta_star + z_alpha(0.01)[2]*sqrt(t(x_star)%*%invJ%*%x_star)))
CI

#e 

plot(x, residuals(myglm, type='deviance'), cex=0.4, ylab = 'Deviance Residuals')
mean(residuals(myglm, type='deviance')<0)

#f 

lm1 <- lm(x ~ 0+as.factor(y))
summary(lm1)

lm2 <- lmer(x ~ 0 + (1|as.factor(y)), REML = FALSE)
summary(lm2)

c1 <- as.vector(lm1$coefficients)
c2 <- ranef(lm2)

plot(as.vector(unlist(c2)),c1, xlim=c(-0.7, 0.7), ylim =c(-0.5,0.5),
     ylab='Coefficients linear model', xlab='Coefficients linear mixed model')
abline(0,1)


plot(y,x,col = rgb(red = 0, green = 0, blue = 1, alpha = 0.1), 
     pch = 16, xlab="x", ylab="y")

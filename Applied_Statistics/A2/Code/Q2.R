rm(list=ls())  #clear load space
setwd('/Users/lysi2/Documents/UNI_Imperial/Applied_Stats/Assignments/A2/Code')
library(tidyverse)
library(faraway)
library('MASS')
library(car)
library(margins)

source('glmxy.R')

r=2
x <- dfrm$x
y <- dfrm$y


beta <- c(-0.1,-0.5) #initial guess
for (i in 1:10){
  eta <- cbind(1,x)%*%beta #estimated linear predictor
  mu <- r * exp(eta)/(1-exp(eta))  #estimated mean response
  z <- eta +(y-mu)*r/(mu*(mu+r)) #form the adjusted variate
  w <- mu*(mu+r)/r #weights
  lmod <- lm(z~x, weights=w) #regress z on x with weights w
  beta <- as.numeric(lmod$coeff) #new beta
  print(beta) #print out the beta estimate every iteration 
}


y1 <- function(x) x
y2 <- function(x, r) x*(x+r)/r
x <- seq(0,100,0.01)
plot(x,y1(x), type='l')
lines(x,y2(x,r=0.1), col='red')
lines(x,y2(x,r=1), col='red')
lines(x,y2(x,r=10), col='red')
lines(x,y2(x,r=50), col='red')
lines(x,y2(x,r=1000), col='red')


rm(list=ls())  #clear loadspace
setwd("/Users/lysi2/Documents/UNI_Imperial/CompStats/Assignments/A2/Code")

library(markovchain)
library(MASS)

set.seed(222)

a=4
b=1

# A1 <- function(y) rnorm(1, mean=(a+b)*y/(1+2*y), sd=1/(1+2*y))
A1 <- function(y) ifelse(y>=0, rnorm(1, mean=(a+b)*y/(1+2*y), 
                                     sd=1/(1+2*y)),0)

A2 <- function(x) rgamma(1, shape=3/2, rate=1/2*(1+(a-x)^2+(b-x)^2))

n <- 2000
xall <- matrix(NA,ncol=2, nrow=n)
x <- c(0,0)
xall[1,] <- x
for (t in seq(2,n)){
  x[1] <- A1(x[2])
  x[2] <- A2(x[1])
  xall[t,]<-x 
}

count = 0
for (i in 1:dim(xall)[1]){
  if ((xall[i,1] <= 1 & xall[i,1] >=-1 ) & (xall[i,2] <= 0.5 & xall[i,2] >=0)){
    count = count + 1
  }
}

count/n

mean(xall[,1]^2)


plot(xall[,1], xall[,2], col=rgb(red=0, green=0, blue=1, alpha=0.14), 
     xlab='x', ylab= 'y', ylim=c(-2.5,2.5))








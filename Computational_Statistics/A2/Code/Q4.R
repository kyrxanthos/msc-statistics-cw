rm(list=ls())  #clear loadspace
setwd("/Users/lysi2/Documents/UNI_Imperial/CompStats/Assignments/A2/Code")

library(markovchain)
library(MASS)

set.seed(222)
dat <- read.csv('qu4.csv')
x <- dat$observations[dat$group==1]
y <- dat$observations[dat$group==2]

n <- length(x)
m <- length(y)

T <- function(Z) abs(var(Z[1:n]) - var(Z[(n+1):(n+m)]))
Z <- c(x,y)
t <- T(Z) 
Zperm <- sample(Z,length(Z))
Trep <- replicate(1e4, T(sample(Z,length(Z)))>=t)
pval <- mean(Trep)
pval

pval+sd(Trep)/sqrt(length(Trep))*qnorm(c(0.025,0.975)) ##Conf Int



#b
k=5
genbootsample <- function(data){
  theta_hat <- mean(data)/k
  rgamma(length(data), shape=k, scale=theta_hat)
}



m1 <- replicate(1000,genbootsample(x))
m2 <- replicate(1000,genbootsample(y))

plot(ecdf(m1), main='')
lines(ecdf(m2),col='red')
legend('bottomright', legend=c('Group 1 Distribution', 
              'Group 2 Distribution'), col=c(1,2), lty=1:2, cex=0.7)



# bootsd <- function(data,T,nrep){
#   replicate(nrep,T(genbootsample(data)))
# }

# T <- function(z) abs(var(z[1:n])-var(z[-(1:m)]))


B <- 1e4
# Trep <- replicate(B, {bx <- sample(Z, n,replace=TRUE);
#                       by <- sample(Z, m,replace=TRUE);
#                       T(c(bx,by))})

# Trep <- replicate(B, {bx <- genbootsample(x);
#                       by <- genbootsample(y);
#                       T(c(bx,by))})

#this works!
Trep <- replicate(B, {T(genbootsample(c(x,y)))})

pval_boot <- mean(c(Trep, t)>=t)
pval_boot










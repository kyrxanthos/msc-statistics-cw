rm(list=ls())  #clear loadspace
setwd("/Users/lysi2/Documents/UNI_Imperial/CompStats/Assignments/A2/Code")

library(markovchain)
library(MASS)

set.seed(222)

my_f <- function(x) ifelse((x>=0 & x<=5), 1/(1+abs(x-2)^3), 0)

MH <- function(f,q,rq,n,init){
  xall <- rep(NA,n)
  x <- init
  xall[1] <- x
  n_acc <- 0 #acceptance rate
  for (t in seq(2,n)){
    y <- rq(x)
    # if (runif(1)<=(f(y)*q(y,x))/(f(x)*q(x,y))){
    if (runif(1)<=(f(y))/(f(x))){
      x <- y
      n_acc <- n_acc+1
    }
    xall[t] <- x
  }
  cat('Acceptance rate: ', n_acc/n)
  return(xall)
}


sigma=1
N=5000
q <- function(x,y) dnorm(y,mean=x,sd=sigma)
rq <- function(x) x+rnorm(1, mean=0, sd=sigma)
f <- my_f
set.seed(123)
xall <- MH(f,q,rq,N,1)

sigma=0.05
xall_1 <- MH(f,q,rq,N,1)
sigma=1
xall_2 <- MH(f,q,rq,N,1)
sigma=5
xall_3 <- MH(f,q,rq,N,1)

plot(xall_1,type='l')
lines(xall_2, col='red')
lines(xall_3, col='blue')
ex2 <- mean(xall^3)
ex2

k <- 1/integrate(f,-Inf,Inf)$value

f3 <- function(x) ifelse((x>=0 & x<=5), k*x^3/(1+abs(x-2)^3), 0)


integrate(f3,0,5)$value

mean(xall<1)


f3 <- function(x) ifelse((x>=0 & x<=5), k/(1+abs(x-2)^3), 0)


integrate(f3,0,1)$value

plot(xall, type='l')


#b
# N=1e4
set.seed(222)
my_chains <- matrix(rep(NA,N*4), nrow = N, ncol = 4)
count <- 0
for (init in c(1,2,3,4)){
  count = count +1
  sigma=0.05
  my_chains[,count] <- MH(f,q,rq,5000,init)
  
}



plot(my_chains[,1],type='l', ylab='X', xlab='Iteration')
for (i in 2:4){
  print(i)
  lines(my_chains[,i], col=i)
}
legend('bottomright', legend=c(-2,-1,1,2), col=c(1,2,3,4), lty=1:2, cex=0.5)

x <- seq (0 ,5 ,0.01)
cdf <- sapply(x, function(a) k*integrate(f, lower=0, upper=a)$value)

plot(x,cdf, type='l', lwd=2)
lines(ecdf(my_chains[,1]), col=2)
lines(ecdf(my_chains[,2]), col=3)
lines(ecdf(my_chains[,3]), col=4)
lines(ecdf(my_chains[,4]), col=5)
legend('bottomright', legend=c('True',-2,-1,1,2), col=c(1,2,3,4,5), lty=1:2, cex=1)





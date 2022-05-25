rm(list=ls())  #clear loadspace
setwd("/Users/lysi2/Documents/UNI_Imperial/CompStats/Assignments/A2/Code")

library(markovchain)
library(MASS)

set.seed(222)
X <- c(-0.71, -1.30, -0.13, -2.03, 1.62, 2.38, 0.48, 0.51, -0.69, -2.32, -2.02,
       1.23, -0.25, 0.76, 0.65)

#two sided interval
two_sided_boot <- function(X,b,alpha){
  bootsamples <- replicate(b,{
    bX <- sample(X,replace=TRUE)
    T(bX)-T(X)
  })
  cstar <- quantile(bootsamples,c(alpha/2,1-alpha/2))
  return(c(T(X)-cstar[2], T(X)-cstar[1]))
}

stud_two_sided_boot <- function(X,b, alpha){
  bootsamples <- replicate(b,{
    bX <- sample(X,replace=TRUE)
    (T(bX)-T(X))/(sdT(bX))
  })
  cstar <- quantile(bootsamples,c(alpha/2,1-alpha/2))
  
  return(c(T(X)-sdT(X)*cstar[2],
           T(X)-sdT(X)*cstar[1]))
  
}

T <- function(x) mean(cos(x))
sdT <- function(x) sd(cos(x))/sqrt(length(x))



b=1000
alpha = 0.05

set.seed(1222)
two_sided_boot(X,b,alpha)
stud_two_sided_boot(X,b,alpha)


r <- function(n){rt(n,4)}
##work out correct value
mean(cos(r(1e6)))#with Monte Carlo

ff <- function(x) cos(x)*gamma(5/3)/(sqrt(4*pi)*gamma(2))*(1+(x^2)/4)^(-5/2)

trueres <- integrate(ff,-Inf,Inf)$value
# trueres <- mean(cos(r(1e6)))

n <- 15
CI_bootnp <- replicate(500,two_sided_boot(r(n),1e3, alpha))
mean((CI_bootnp[1,]<=trueres) &(CI_bootnp[2,]>=trueres))

CI_bootnp_stud<- replicate(500,stud_two_sided_boot(r(n),1e3,alpha))
mean((CI_bootnp_stud[1,]<=trueres) &(CI_bootnp_stud[2,]>=trueres))


# C
T <- function(x) median(cos(x))
sdT <- function(x) sd(median(cos(x)))


b=1000
alpha = 0.05

set.seed(1222)
two_sided_boot(X,b,alpha)


stud_two_sided_boot(X,b,alpha)


library(boot)
b <- boot(data = X,statistic = function(x,i) median(cos(x[i])),R = 1000)
b <- boot(data = X,statistic = function(x,i) mean(cos(x[i])),R = 1000)
boot.ci(b)

B=10
T <- c()
for (i in range(1:b)){
  bX <- sample(X,replace=TRUE)
  for (j in range(1:100)){
    print(cos(bX[j]))
    T[j] <- median(cos(bX[j]))
    print(T[j])
    
  }
}







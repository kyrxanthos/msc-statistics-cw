rm(list = ls())
set.seed(222)
setwd("~/Documents/UNI_Imperial/CompStats/Assignments/A1")
library(nor1mix)

f <- function(x) ifelse(x>= -3/2 & x<= 3/2, 
                        (1+cos(x)^3)*exp(-0.5*x^2), 0)
x <- seq(-5,5,by=0.001)
plot(x,f(x), type = 'l')
lines(x, dnorm(x), col ='red')
# abline(v =3/2)
# abline(v=-3/2)

int <- integrate(f,-3/2,3/2)

k <- 1/int$value

new_f <- function(x) ifelse(x>= -3/2 & x<= 3/2, 
                            k*(1+cos(x)^3)*exp(-0.5*x^2), 0)
g <- function(x) dnorm(x)

x <- seq(-5,5,by=0.001)
plot(x,new_f(x), type = 'l')
lines(x, dnorm(x), col ='red')

C <- 2*sqrt(2)*k/pi


rf <- function(){
  
  while(1){
    x <- rnorm(1)
    if (runif(1) < new_f(x)/(C*g(x))){
      return(x)
    }
  }
}

x_samples <- replicate(1e3, rf())
x <- seq (-3/2 ,3/2 ,0.01)


fx <- Vectorize(new_f)
dx <- 0.01

# pdf('cdf.pdf', width=9, heigh=5)
plot(x, cumsum(fx(x) * dx), type = "l", ylab='F(x)')
lines(ecdf(x_samples), col ='red')
# dev.off()

#part c

x_samples <- replicate(1e4, rf())

mc <- mean(x_samples^2)

mc_sd <- sd(x_samples^2)/sqrt(1e4)
CI <- mc + mc_sd* qnorm(c(0.05, 0.95))

N =round(diff(CI)^2*10^12,0)


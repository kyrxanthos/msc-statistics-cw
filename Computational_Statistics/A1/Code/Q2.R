rm(list = ls())
set.seed(222)

sigma <- 1
sigmas <- c(0.01,0.1,0.5,1)

I_sigma <- function(x, sigma=0.01){
  1/2*x*(1/(pi*(1+x^2)) + 1/(sigma*sqrt(2*pi)) * exp(-(x-5)^2/(2*sigma^2)))
}

x <- seq(-2,8,0.01)

# pdf('Isigma.pdf', width=9, heigh=5)
plot(x, I_sigma(x,0.1), type='l', ylab='I(sigma)')
lines(x, I_sigma(x,0.1), type='l', col ='red')
lines(x, I_sigma(x,0.5), type='l', col ='blue')
lines(x, I_sigma(x,1), type='l', col ='green')
legend(-2, 6, legend=c(0.1, 0.5, 1),
       col=c("red", "blue", "green"), lty=1:1, cex=0.8)
# dev.off()

for(sigma in sigmas){
  print(integrate(I_sigma, -Inf,Inf, sigma))
  }

sigma=1
integrate(I_sigma,-Inf,-1,sigma, subdivisions=2000)$value+
  integrate(I_sigma,-1,1,sigma)$value+
  integrate(I_sigma,1,3,sigma)$value+
  integrate(I_sigma, 3,6,sigma)$value+
  integrate(I_sigma, 6,Inf,sigma, subdivisions=2000)$value

x <- seq(4.8,5.1,0.01)
plot(x, I_sigma(x,0.01), type='l', ylab='I(sigma)')

integrate(I_sigma, -Inf, 4.8, sigma=0.01 ,subdivisions=2000)$value+
  integrate(I_sigma, 4.8, 5.1, sigma=0.01)$value+
  integrate(I_sigma, 5.1, Inf, sigma=0.01, subdivisions=2000)$value


#part b


f_i <- function(x, sigma=1){
  1/2*(1/(pi*(1+x^2)) + 1/(sigma*sqrt(2*pi)) * exp(-(x-5)^2/(2*sigma^2)))
}
g_i <- function(x) dnorm(x,0,4)

phi <- function(x) x

y <- rnorm(1e4,0,4)
w <- y*f_i(y)/g_i(y)
mean(w)

my_ISamp <- function(phi,f,g,y){
  IS <- mean(phi(y)*f(y)/g(y))
  IS_sd <- sd(phi(y)*f(y)/g(y))/sqrt(length(y))
  res <- c(IS, IS_sd)
  res
  
}

my_ISamp(phi,f_i,g_i,y)

n <- 1e4 
X <- rcauchy(n) + rnorm(n,0,5)

mean(X)
x <- seq(-20,20,0.01)
plot(x,dcauchy(x), type ='l', ylim=c(0,0.5))
lines(x, dnorm(x,0,5), col='red')
lines(x, dcauchy(x) + dnorm(x,0,5), col='green')
lines(x, dcauchy(x, 0,0.5), col='blue')


#monte carlo
#need to sample first
#this was not done.

rf <- function(){
  
  while(1){
    x <- rnorm(1)
    if (runif(1) < new_f(x)/(C*g(x))){
      return(x)
    }
  }
}

x_samples <- replicate(1e4, rf())




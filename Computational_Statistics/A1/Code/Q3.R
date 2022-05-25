rm(list = ls())
set.seed(222)
# library(cubature)


f <- function(x,y){(1+(x-2*y)^2/4)^(-5/2) * (1+((x+2*y)^2)/3)^-2
}

I <- function(x) sapply(x, 
      function(t_i) integrate(function(y) f(t_i,y),-Inf,Inf)$value)

k = 1 / integrate(I, -Inf, Inf)$value
k

f <- function(x,y){k *(1+(x-2*y)^2/4)^(-5/2) * (1+((x+2*y)^2)/3)^-2
}

set.seed(222)
n <- 10^4

x <- rnorm(n)
y <- rnorm(n)
w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))

is <- mean((x-y)^2*w)

is_sd <- sd((x-y)^2*w)/sqrt(n)
is
is_sd


set.seed(222)
N <- 10^4
it <- 15
est <- rep(0,it); mw <- rep(0, it); se <- rep(0,it)
c=1
for (n in rep(N,it)){
  x <- rnorm(n)
  y <- rnorm(n)
  w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))
  est[c] <- mean((x-y)^2*w)
  mw[c] <- max(w)
  se[c] <- sd((x-y)^2*w)/sqrt(n)
  c = c +1
}

df <- data.frame(Estimate = est, SE = se, Max_Weight = mw) 
library(xtable)
print(xtable(df),include.rownames=FALSE) #Remove rownames
##############################################################


set.seed(124321)
N <- 10^6
x <- rnorm(N)
y <- rnorm(N)
w <- mapply(f, x, y)/(dnorm(x)*dnorm(y))

In <- cumsum((x-y)^2*w)/1:N

alln <- round(10^seq(1,6,length.out=1000)) 
# exact value for reference line
I2 <- function(x) sapply(x, 
          function(t_i) integrate(function(y) f(t_i,y),-Inf,Inf)$value)
Iexact <- integrate(I, -Inf, Inf)$value
Iexact
plot(alln, In[alln], xlab='n', ylab=expression(I[n]), 
     ylim=Iexact+c(-1,1), log='x')
lines(c(1,N), c(Iexact,Iexact))


##############################################################
set.seed(222)
N <- 10^4
it <- 15
est <- rep(0,it) ; mw <- rep(0, it) ; se <- rep(0,it)
c=1
for (n in rep(N,it)){
  x <- rcauchy(n)
  y <- rcauchy(n)
  w <- mapply(f, x, y)/(dcauchy(x)*dcauchy(y))
  est[c] <- mean((x-y)^2*w)
  mw[c] <- max(w)
  se[c] <- sd((x-y)^2*w)/sqrt(n)
  c = c +1
}

df <- data.frame(Estimate = est, SE = se, Max_Weight = mw) 
library(xtable)
print(xtable(df),include.rownames=FALSE) #Remove rownames

################################################
set.seed(124321)
N <- 10^6
x <- rcauchy(N)
y <- rcauchy(N)
w <- mapply(f, x, y)/(dcauchy(x)*dcauchy(y))

In <- cumsum((x-y)^2*w)/1:N

alln <- round(10^seq(1,6,length.out=1000)) 
# exact value for reference line
I2 <- function(x) sapply(x, 
                         function(t_i) integrate(function(y) f(t_i,y),-Inf,Inf)$value)
Iexact <- integrate(I, -Inf, Inf)$value
Iexact
plot(alln, In[alln], xlab='n', ylab=expression(I[n]), 
     ylim=Iexact+c(-1,1), log='x')
lines(c(1,N), c(Iexact,Iexact))



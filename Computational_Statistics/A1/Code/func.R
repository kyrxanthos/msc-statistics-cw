rm(list = ls())
set.seed(222)
setwd("~/Documents/UNI_Imperial/CompStats/Assignments/A1")
library(xtable)
print(xtable(airquality[11:15,]),include.rownames=FALSE) #Remove rownames

my_ISamp <- function(phi,f,g,y){
  IS <- mean(phi(y)*f(y)/g(y))
  IS_sd <- sd(phi(y)*f(y)/g(y))/sqrt(length(y))
  res <- c(IS, IS_sd)
  res
  
}

#example 1
k <- 0.5563
C <- 6*k
y <- rexp(1e5, rate=1/3)
f_q4 <- function(x) ifelse(x>=0.5, k*(cos(x+1)+1)*exp(-x/3),0)
g_q4 <- function(x) dexp(x, rate=1/3)
my_ISamp(phi=function(x) x, f_q4, g_q4, y)


#example 2
Y <- rnorm(n=1000,0,1)
phi <- function(Y) Y>0.1
f <- function(Y) dt(Y,5)
g <- function(Y) dnorm(Y)

my_ISamp(phi, f, g, Y)



### Input #1: x - the points along the interval of integration
### Input #2: f - the function
### Output: the integral as approximated by rectangular integration (a.k.a. midpoint rule)
rectangular.integration = function(x, f)
{
  # check if the variable of integration is numeric
  if (!is.numeric(x))
  {
    stop('The first argument is not numeric.')
  }
  # check if f is a function
  if (!is.function(f))
  {
    stop('The second argument is not a function.')
  }
  ### finish checks
  # obtain length of variable of integration and integrand
  n.points = length(x)
  # midpoints
  midpoints = 0.5*(x[2:n.points] + x[1:(n.points-1)])
  # function evaluated at midpoints
  f.midpoints = f(midpoints)
  # calculate the widths of the intervals between adjacent pairs of points along the variable of integration
  interval.widths = x[2:n.points] - x[1:(n.points-1)]
  # implement rectangular integration
  # calculate the sum of all areas of rectangles that are used to approximate the integral
  rectangular.integral = sum(interval.widths * f.midpoints)
  # print the definite integral
  return(rectangular.integral)
}
# points along support set for Beta(2, 5)
beta.support = seq(0, 1, by = 0.005)
# calculate integral of Beta(2, 5) PDF over its support set
rectangular.integration(beta.support, dbeta.2.5)

f <- function(x)(max(x^2)-min(x^2))*prod(dnorm(x))

rectangular.integration(beta.support, f)






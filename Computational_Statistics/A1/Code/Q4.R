rm(list = ls())
set.seed(222)

n <- 1e4

X <- rnorm(n)
mc <- mean(X*(X>=1))
mc_sd <- sd(X*(X>=1)) / sqrt(n)
mc
mc_sd

#b)

Y <- 1+ rexp(n)
IC <- mean(Y*(Y>=1) *dnorm(Y)/dexp(Y-1))
IC_sd <- sd(Y*(Y>=1) *dnorm(Y)/dexp(Y-1)) / sqrt(n)
IC
IC_sd

a <- -cov(X*(X>=1), (X>=1)) / var(X>=1)
E_t <- mean(X*(X>=1) -a*((X>=1)- (1-pnorm(1))))
Et_sd <- sd(X*(X>=1) -a*((X>=1)- (1-pnorm(1)))) / sqrt(n)
E_t
Et_sd


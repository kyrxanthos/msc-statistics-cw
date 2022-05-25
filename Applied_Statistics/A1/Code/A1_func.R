setwd("/Users/lysi2/Documents/UNI_Imperial/Applied_Stats/Assignments/A1/Code")

#to print tables:
#  library(xtable) 
# xtable(my_table, ,include.rownames=FALSE)

#95% Confidence intervals:
qnorm(c(0.025,0.975))

# calculates f(x) = 1/(√(2 π) σ) e^-((x - μ)^2/(2 σ^2))
x <- seq(from = 55, to = 95, by = 0.25)
my_var <- dnorm(x, mean = 75, sd =5)
plot(x,my_var, type = 'l')

#P(X < 27.4) 
dens <- pnorm(27.4, mean=50, sd=20)
dens
# finds P(X<27.4)= 0.1292381
qnorm(0.1292381,mean=50,sd=20)


plot_residuals<-function(x,y,title){
  fit = lm(y ~ x)
  e = fit$residuals
  r = e/(norm(e, type="2")/sqrt(n-2)) 
  plot(fit$fitted.values,r,xlab=expression(hat(y)),main=title)
}

n = 50
set.seed(0)
x = sort(runif(n))
e = rnorm(n,sd=.2)
y = 1 + 10*x + e
pdf('trial.pdf', width=7, heigh=5)
plot_residuals(x,y,"Good fit")
dev.off()

plot_cook<-function(x,y,outlier=F){ 
  fit = lm(y ~ x)
  colrs = rep("black",length(y)) 
  if(outlier){colrs[length(y)]="blue"} 
  plot(y ~ x,col=colrs)
  abline(fit)
  if(outlier){abline(lm(y ~ x, subset=-length(y)),lty=2,col="blue")} 
  plot(fit,5,col=colrs)
}

n = 50
x = runif(n)
y = 1 + 3*x + rnorm(length(x),sd=.2)
plot_cook(x,y,T)

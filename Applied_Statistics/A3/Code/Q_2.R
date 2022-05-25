rm(list=ls())  #clear load space
library(lme4)
my_data <- data.frame(source("xyz.R"))
colnames(my_data) <- c('x','y','z')

my_lm <- lm(y~x, data=my_data)
summary(my_lm)
plot(y ~ x, data=my_data, pch=as.character(z))
abline(my_lm$coefficients[1], my_lm$coefficients[2], col='red', lwd=2)


boxplot(y~z, data=my_data)

means <- aggregate(y ~  z, my_data, mean)


my_lmer <- lmer(y ~ 1+x+(1|z), data = my_data, REML=TRUE)
summary(my_lmer)

#d
my_data_2 <- subset(my_data, my_data$z=='B' | my_data$z=='F' | my_data$z=='H')
my_lmer2 <- lmer(y ~ 1+x+(1|z), data = my_data_2,  REML=FALSE)
logLik(my_lmer2)


#e
simple_lm <- lm(y~ 1 + x, data=my_data_2)
logLik(simple_lm)
d <- as.numeric(2*(logLik(my_lmer2)-logLik(simple_lm)))
d


#f

ds <- numeric(1000)
for (i in 1:1000) {
  y_new <- unlist(simulate(simple_lm))
  nullmod <- lm(y_new~ 1 + x, data=my_data_2)
  altmod <-  suppressMessages(lmer(y_new~ 1+x+(1|z), 
                                    data = my_data_2,  REML=FALSE))
  ds[i] <- as.numeric(2 * (logLik(altmod) - logLik(nullmod))) 
}

phat <- mean(ds>d)
phat
sqrt(phat*(1-phat)/1000)


plot(y ~ x, data=my_data_2, pch=as.character(z))
abline(simple_lm$coefficients[1], simple_lm$coefficients[2], col='red', lwd=2)



rm(list=ls())  #clear load space
setwd('/Users/lysi2/Documents/UNI_Imperial/Applied_Stats/Assignments/Group')
library(tidyverse)
library(faraway)
library('MASS')
library(car)
library(margins)
library(sf)


dat <- read.csv("data.csv")
colnames(dat) <- c("date", "age", "distance", "stores", "lat", "lon", "price")
head(dat)

dat$store_density = cut(dat$stores,c(-1,3,7,10), labels = c('low', 'medium', 'high'))
length(dat$store_density[dat$store_density == 'low'])
length(dat$store_density[dat$store_density == 'medium'])
length(dat$store_density[dat$store_density == 'high'])


sum(is.na(dat)) 
pairs(dat)


model0 <- lm(price ~ age + distance + stores + lat + lon + date, data=dat)
summary(model0)

#stores is a bit wierd
confint(model0, interval='prediction')

dat_new <- dat[(cooks.distance(model0)<=0.02)&(hatvalues(model0)<=0.05),]

model1 <- lm(price ~ 1+age + log(distance) + stores + lat + lon +date, data=dat_new)
summary(model1)


par(mfrow = c(2,3)) 
for(k in 1:6){
  plot(model0,k)
}


par(mfrow = c(1,2)) 
ggplot(my_sf) +geom_sf(aes(color = price))
ggplot(my_sf) +
  geom_sf(aes(color = stores))

distance=function(x,y,c){
  sqrt((x-c[1])^2+(y-c[2])^2)
}

dat_new$dcc=distance(dat_new$lat,dat_new$lon,c(24.975, 121.54))

model2 <- lm(price^0.5 ~ 1+age + log(distance) + stores + lat + lon +date +dcc, data=dat_new)
summary(model2)


model3 <- glm(stores~date+age+log(distance)+lat+lon+price,data = dat_new,family = "poisson")
summary(model3)





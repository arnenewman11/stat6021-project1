---
  title: "STAT 6021 Project 1"
author: "Xin Huang"
date: "10/11/2020"
output: pdf_document
---
  
#Start by loading in the dataset and exploring it
```{r}
setwd("C:/Users/hx-cn/Dropbox/Education/UVA- Data Science/STAT 6021/Project 1")
data <- read.csv("diamonds4.csv", header=TRUE)
attach(data)
names(data)
str(data)
```

"""
# Explore the data and distrbution
plot(x=data$cut, y=data$price, ylim = c(0,50000))
"""

#Fit linear model using only numeric variable- all data
plot(price~carat)
lm0<-lm(price~carat)

plot(price~carat, main="Pirce~Carat")
abline(lm0,lty=2, col="red") 


#gen residual plot for no powered model
plot(lm0$fitted.value, lm0$residuals)
abline(h=0, col='red')

# boxcox: should transform y to ln(y)
library(MASS)
boxcox(lm0,lambda = seq(-1,1.5,1))

# According to scatter, transform
ln.price<-log(price)
lm.ln<-lm(ln.price~carat)
summary(lm.ln)

#gen residual plot for exp model
plot(lm.ln$fitted.value, lm.ln$residuals)
abline(h=0, col='red')

plot(ln.price~carat)
abline(lm.ln,lty=2, col="red") 

# Transform x
ln.carat<-log(carat)
lm.ln2<-lm(ln.price~ln.carat)

plot(ln.price~ln.carat)
abline(lm.ln2,lty=2, col="red") 

# Transform X only 
ln.carat<-log(carat)
lm.ln3<-lm(price~ln.carat)
boxcox(lm.ln3,lambda = seq(-1,1.5,1))


#plot all scatter plot together
par(mfrow=c(2,2))
#lm0
plot(price~carat, main="Pirce~Carat")
abline(lm0,lty=2, col="red") 
#ln
plot(ln.price~carat, main="ln(Pirce)~Carat")
abline(lm.ln,lty=2, col="red", ) 
#ln2
plot(ln.price~ln.carat, main="ln(Pirce)~ln(Carat)")
abline(lm.ln2,lty=2, col="red") 

summary (lm.ln2)

# Running the model Through all Assumptions
par(mfrow=c(2,2))
plot(ln.price~ln.carat, main="ln(Pirce)~ln(Carat)")
abline(lm.ln2,lty=2, col="red") 
#1. residual plot
plot(lm.ln2$fitted.value, lm.ln2$residuals, main="Residual Plots")
abline(h=0, col="red") 
#2.
acf(lm.ln2$residuals, main="ACF of Residuals")

qqnorm(lm.ln2$residuals)
qqline(lm.ln2$residuals, col="red")

boxcox(lm.ln2, lambda = seq(-1,1.5,1))

# Generate statistical summary
summary(lm.ln2)




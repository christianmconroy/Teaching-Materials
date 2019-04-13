# Week 9 In-Class and At-Home Exercise Solutions#

# In-Class Activity #1 #
setwd("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")
hsb2 <- read.csv("hsb2.csv", sep=",", na.strings=c(""))
head(hsb2)
attach(hsb2)
#1
library(Hmisc)
install.packages('rms')
library(rms)
dd <- datadist(hsb2)
options(datadist='dd')
model1 <- lm(write ~ female, data=hsb2)
summary(model1, female=2)
names(hsb2)
# Unlike Stata, in R, dummies are by default labelled 1 or 2. In this case, R is automatically making the female (1) the reference group. I used a special package to allow me to insert the baselevel I want using the summary(model2, female=2) code. Let me know if you've used otherways to do the same thing!

#2
model2 <- lm(write ~ female + schtyp + prog + id, data=hsb2)
summary(model2, female=2)

install.packages('AER')
library(AER)

model3 <- lm(write ~ female + schtyp + prog + id, data=hsb2)
coeftest(model3, vcov = vcovHC(model3, type = 'HC1'))

# In the above model3, we estimate the regession using robust standard errors. We use coeftest to get information on the variance of the estimates and the voc=vcovHV part of the command to ask for the heteroscedasticity-consistent standard errors . This way, we can account for heteroscedasticity without affecting the values of the Beta estimates. We'll use this from now on. Again, we're maintaining male as the reference category above. 

#3
hsb2$schtyp <- as.numeric(hsb2$schtyp)
class(hsb2$schtyp)
model4 <- lm(write ~ read + math + science + socst, data=subset(hsb2, schtyp==2))
summary(model4)
coeftest(model4, vcov = vcovHC(model4, type = 'HC1'))

#4 
# Create missing Data
hsb2$write[hsb2$write>=62] <- NA
hsb2$read[hsb2$read>=62] <- NA
hsb2$math[hsb2$math>=62] <- NA
hsb2$science[hsb2$science>=62] <- NA

model5 <- lm(write ~ read + math + science + socst, data=subset(hsb2, schtyp==2))
summary(model5)
coeftest(model5, vcov = vcovHC(model5, type = 'HC1')) 

# Because our sample size is reduced with missing data, we obviously have a much lower degrees of freedom now (99 in this case)s
# Reload non-missing again:
hsb2 <- read.csv("hsb2.csv", sep=",", na.strings=c(""))

#5
plot(write, math, xlab="Math", ylab="Write", pch=19, col="blue")
abline(lm(write~math), col="red")

#6
attach(hsb2)
plot(female, write, xlab="Female", ylab="Write", pch=19, col="blue")
abline(lm(write~female), col="red")

# The line is not very useful. Unlike Stata though, R recognizes that nature of the data and gives a box plot insteas of a scatter. Remember that the syntax for pllot and lm are flipped in R!!!! Plot is x,y and lm is y~x.

# At-Home Activity 
install.packages('haven')
library(haven)

auto <- read_dta('auto.dta')
head(auto)
attach(auto)

#1
png(filename="autoweightlength.png")
plot(length, weight, xlab="Length", ylab="Weight", main="Automobile Weight by Length", pch=19, col="blue")
abline(lm(weight~length), col="red")
dev.off()
# Remember that the syntax for pllot and lm are flipped in R!!!! Plot is x,y and lm is y~x. 
#2
model6 <- lm(weight ~ length)
summary(model6)
coeftest(model6, vcov = vcovHC(model6, type = 'HC1')) 
# Using robust standard errors, we see a coefficient of 33.02 for length with 74 observations. 

#3
newdata <- matrix(data = 
                    c(200), 
                  byrow=T, nrow = 1)

newdata <- as.data.frame(newdata)

colnames(newdata) <- c("length")

predict(model6, newdata)
# Predicted weight (rounded) is 3418

#4
model7 <- lm(weight ~ length, data=subset(auto, foreign==0))
summary(model7)
coeftest(model7, vcov = vcovHC(model7, type = 'HC1')) 
# Coefficient is 31.9 with 52 observations (Can tell by 52-2=50 degrees of freedom.)

#5 
tapply(weight, foreign, mean)
# Domstic cares have a higher average value of weight in this data (3317)


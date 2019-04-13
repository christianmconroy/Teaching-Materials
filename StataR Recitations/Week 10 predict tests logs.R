# Week 10 In-Class and At-Home Activities#

# Warm Up Problems #
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")
install.packages('haven')
library(haven)

nlsw <- read_dta('nlsw88.dta')

#1
install.packages('sandwich')
library(sandwich)
model5 <- lm(wage ~ ttl_exp, data=nlsw)
coeftest(model5, vcov = vcovHC(model5, type = 'HC1'))
# .3314 - Significant at 99% level

#2
# We can add a new observation with 14 as ttl_exp and predict from that:
newdata <- matrix(data = 
                    c(14), 
                  byrow=T, nrow = 1)

newdata <- as.data.frame(newdata)

colnames(newdata) <- c("ttl_exp")

predict(model5, newdata)

# Or we can use the observation already in the dataset that already has 14 years of total experience. We reach the same answer.  
nlsw$predictwage <- predict(model5,nlsw)
nlsw$predictwage[which(nlsw$ttl_exp==14)]

# Someone with 14 total years of experience makes $8.25

#3 
nlsw$predictwage[which(nlsw$ttl_exp==14.25)]

# Someone with 14 total years of experience makes $8.34

#4
model6 <- lm(wage ~ ttl_exp + union + c_city + collgrad, data=nlsw)
coeftest(model6, vcov = vcovHC(model6, type = 'HC1')) 
# Controlling for these factors in the model, as your workforce experience increases, there is a correlation of .295 on wages. 

# End of Semester Project #
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/Recitation10")

vehicles <- read_dta('vehicles.dta')
head(vehicles)

vehicles$priceadj <- ifelse(vehicles$domestic == 1, vehicles$price, ifelse(vehicles$domestic == 0, eval(vehicles$price * 1.068), NA))

vehicles$priceadj2 <- ifelse(vehicles$priceadj < 4000,4000, ifelse(vehicles$priceadj > 15000, 15000, ifelse(vehicles$priceadj >= 4000 & vehicles$priceadj <= 15000, vehicles$priceadj, NA)))

summary(vehicles$priceadj2)
# The average we get is the same as that mentioned in the replication description ($6311).
class(vehicles$make)
vehicles$Datsun <- as.integer(grepl(pattern = "Datsun", x = vehicles$make))
table(vehicles$Datsun)

vehicles$dropvars <- ifelse(vehicles$Datsun == 1 | is.na(vehicles$rep78), 1, 0)
table(vehicles$dropvars)

t.test(priceadj2~dropvars, var.equal = T, data=vehicles)
# P-value is 0.8 so we're comfortable saying that the drop and keep groups are not statistically different in terms of average price. 

vehicles <- subset(vehicles, vehicles$dropvars!=1)
str(vehicles)
sum(is.na(vehicles$rep78))
# We've dropped the 9 and now have only 65 observations. 

vehicles$logmile <- log(vehicles$mpg)
vehicles$logweight <- log(vehicles$weight)
vehicles$loglength <- log(vehicles$length)
vehicles$weightlength <- vehicles$weight/vehicles$length

vehicles$highrep <-cut(vehicles$rep78, c(0,3,5), labels=c(0:1))
levels(vehicles$highrep)
vehicles$highrep <- as.numeric(vehicles$highrep)
vehicles$highrep <- factor(vehicles$highrep, levels = c(1,2), labels = c("Low", "High"))
class(vehicles$highrep)

vehicles$price_cat <- ifelse(vehicles$priceadj2 <= 4000,1, ifelse(vehicles$priceadj2 > 4000 & vehicles$priceadj2 <=5000, 2, ifelse(vehicles$priceadj2 > 5000 & vehicles$priceadj2 <= 10000, 3, ifelse(vehicles$priceadj2 > 10000, 4, NA))))
table(vehicles$price_cat)

attach(vehicles)

model6 <- lm(mpg ~ length + weight + priceadj2, data=vehicles)
summary(model6)
coeftest(model6, vcov = vcovHC(model6, type = 'HC1'))


vehicleslowrep <- subset(vehicles, vehicles$highrep=="Low")
model7 <- lm(mpg ~ length + weight + priceadj2,data=vehicleslowrep)
summary(model7)
coeftest(model7, vcov = vcovHC(model7, type = 'HC1'))

vehiclesnoextpricecat <- subset(vehicles, vehicles$price_cat != 1 & vehicles$price_cat != 4)
model8 <- lm(mpg ~ length + weight + priceadj2,data=vehiclesnoextpricecat)
summary(model8)
coeftest(model8, vcov = vcovHC(model8, type = 'HC1'))

# At Home Activity 

#1
cor(vehicles[,c('mpg', 'weight', 'length')])

#2
attach(vehicles)
vehicles$weightperinch <- weight/length
plot(weightperinch, mpg, xlab="Weightperinch", ylab="MPG", pch=19, col="blue")
abline(lm(mpg~weightperinch), col="red")

#3
model9 <- lm(mpg ~ length + weight,data=vehicles)
summary(model9)
coeftest(model9, vcov = vcovHC(model9, type = 'HC1'))

#4 
model10 <- lm(logmile ~ loglength+ logweight,data=vehicles)
summary(model10)
coeftest(model10, vcov = vcovHC(model9, type = 'HC1'))

#5
t1 <- table(highrep, price_cat)
t1

print(chisq.test(highrep, price_cat, correct = F), digits=5)



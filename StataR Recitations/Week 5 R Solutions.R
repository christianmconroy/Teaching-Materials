# Week 5 In-Class and At-Home Exercise Solutions #
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")

# Activity 1 #
# Load the data set #
mydata<- read.csv("auto-week5.csv", sep=",", stringsAsFactors = T)
attach(mydata)

# Generate new variables and summarize the results #
mydata$x1 <- 1
mydata$x2 <- 2
mydata$y <- x1 + x2

d <- data.frame(y, x1, x2)
summary(d)

# Change value of x1 and rerun the do-file.*/ 
mydata$x1 <- 2
mydata$y <- with(mydata, x1 + x2)
# R doesn't deal with the same restricting as Stata here, so can just run again with the new number for x. 
# Have to use with for the y computation though because with allows us to modify from a copy of the original data. 

save(mydata,file="Automodified.Rda")

# Activity 2

install.packages("webuse")
library(webuse)
webuse("census")
attach(census)

census$pop17 <- poplt5 + pop5_17
summary(pop17)

census$poppro <- pop17/pop 
sorted <- census[order(census$poppro),]
sorted[,c(1,15)]
# In this data, it looks like California has the lowest (data slightly different than hw  problem set data). 

census$marpro <- marriage/pop 
sorted2 <- census[order(-census$marpro),]
sorted2[,c(1,16)]
# Nevada has the highest. 

rural <- pop - popurban
summary(rural)

logpop <- log(pop)

hist(logpop, freq = F, xlab="logpop", ylab = "", main = "Log of Population", col="burlywood2")
hist(pop, freq = F, xlab="pop", ylab = "", main = "Population", col="burlywood2")
# Decided to focus on density here, but could also choose to go with freuqnecy counts by doing freq = T

save(mydata,file="censusmodified.Rda")

# At Home Activity
nlsw <- read.csv("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/nlsw88.csv", sep=",")
attach(nlsw)

nlsw$agesq <- age^2
ages <- data.frame(age, agesq)
summary(ages)
# * age: 39.15 , agesq: 1,542

nlsw$weekwage <- wage * hours
summary(weekwage)
# * weekwage: 298.978

nlsw$logweekwage <- log(weekwage)
library(pastecs)
stat.desc(nlsw$logweekwage)
# * Mean = 5.42   Std. Dev. = 0.78

logpop <- log(pop)
hist(weekwage, freq = F, xlab="weekwage", ylab = "", main = "Distribution of Weekly Wages", col="burlywood2")
hist(nlsw$logweekwage, freq = F, xlab="logweekwage", ylab = "", main = "Distribution of Log of Weekly Wages", col="burlywood2")
# Decided to focus on density here, but could also choose to go with freuqnecy counts by doing freq = T

nlsw$rwage <- round(wage)
table(rwage)
# * $4 per hour is most common with 302 people reporting it.




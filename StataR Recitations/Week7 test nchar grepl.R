# Week 7 In-Class and At-Home Activities #

# Activity 1 #

nlsw <- read.csv("/Users/chris/Downloads/nlsw88.csv", sep=",", na.strings=c(""))
head(nlsw)
attach(nlsw)

# 1. Test the null hypothesis that the true average vale of wage is 7.6.

t.test(wage, mu=7.60)

#2. Report the 90% confidence interval for the average value of hours*/

t.test(wage, conf.level=0.90)

#3. Test the hypothesis that wages are qual for married and non-married respondents. Do not assume the groups have equal variances.

t.test(wage~married)

# Default in R assumes unequal. 

# 4. Test the hypothesis that wages are equal for union and non-union workers. Assume that the two groups have equal variances.

t.test(wage~union, var.equal=T)

# Activity 2 #
autodata <- read.csv("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/RExportcsv.csv", sep=",", na.strings=c(""))
attach(autodata)

# 1. What is the longest car? What is the car with the longest name?

autodata <- autodata[order(-autodata$length),]
head(autodata$length)
autodata[1,]
# Longest car is Lincoln Continental at 233
autodata$make <- as.vector(autodata$make)
class(autodata$make)
autodata$make_chars <- nchar(autodata$make)
autodata <- autodata[order(-autodata$make_chars),]
autodata[1,]

autodata$chev <- ifelse(make == "^Chev", 1, 0)
table(autodata$chev)

# /*2. What percentage of domestic cars are Chevrolets?*/

autodata$Chev <- as.integer(grepl(pattern = "Chev", x = autodata$make))
prop.table(table(autodata$Chev))
# 8.11%

# 3. Add the word EXCELLENT to the cars you think are excellent*/ 
#   generate excellent = "Excellent"

autodata$Chev <- gsub(".*Chev.*", "Chev Excellent", autodata$make)


#   *** Week 7 Problem Set ***
#   Answer the questions below using the two system data sets: 
#   bpwide.dta and census.dta.
# 
setwd("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/")
bpwide <- read.csv("bpwide.csv", sep=",", na.strings=c(""))
head(bpwide)
attach(bpwide)

# Questions 1-4: bpwide
# 
# 1. Calculate 95% confidence intervals for bp_before and bp_after.
t.test(bp_before)
t.test(bp_after)

# bp_before: (154.3912, 158.5088) | bp_after: (148.7956, 153.9210)
# 
# 2. Test the hypothesis that the mean of bp_after is equal to 150. 
# What is the value of the test statistic?
# What is the p-value of the two-sided test?

t.test(bp_after, mu=150)

# t statistic: 1.0495 | Two-sided p-value: 0.2961

# 3. Test the hypothesis that the mean of bp_before is equal for males and females.
# Assume that the variance of the two groups is equal (pooled test)
# What is the value of the test statistic?
# What is the p-value of the two-sided test?

t.test(wage~married, var.equal = T)

# t statistic: 1.9307 | two-sided p-value: 0.05365

# 4. Test the hypothesis that the proportion of females in this population is .6
# What is the value of the test statistic?
# What is the p-value of the two-sided test?
table(bpwide$sex)

prop.test(x = 60, n = 120, p = 0.6, correct = F)
# In the default, there is a slight different in the numbers between r and stata because r incorporates a continuity correction. We made the Yates continuity correction false here so that our results match up perfectly with what we did in Stata. 
 
# Questions 5-6: census.dta
census <- read.csv("census.csv", sep=",", na.strings=c(""))
head(census)
attach(census)

# 5. Create a new variable giving the length of each state name.
# How long is the longest state name (as written in this data set)?
# Which state or states have the longest names (including spaces)?

census$state <- as.vector(census$state)
class(census$state)
census$state_chars <- nchar(census$state)
census <- census[order(-census$state_chars),]
census[1:3,]
# Massachussets and New Hampshire 

# 6. Create a new indicator variable that is equal to 1 for all of the "New"states (New Hampshire, New Jersey, New Mexico, and New York). What is the average population of these four states?

census$statenew <- as.integer(grepl(pattern = "New", x = census$state))
table(census$statenew)
tapply(census$pop, census$statenew, mean)
# Average population is 6786600 people. 

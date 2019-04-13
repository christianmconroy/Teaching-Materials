# In class activity 1 #

mydata<- read.csv("/Users/cmc454/Documents/nlsw88.csv", sep=",")
attach(mydata)

# Number 1 #
summary(mydata[c("age", "race", "grade", "collgrad", "union")], basic = T)

# Number 2 #
table(mydata$industry)
# Professional Services is the most common industry for workers in this sample. #

# Number 3 #
# Keep in mind that R DOES recognize the words for value labels in the analysis # 
IndWages <- tapply(wage, industry, mean)
IndWages[["Professional Services"]]

# Number 4 #

mydata$transpdummy <- as.numeric(industry == "Transport/Comm/Utility")
summary(mydata[mydata$transpdummy == 1, c("wage")], basic = T)
# The avg hourly wage is 11.44 #

# What is the average hourly wage in the transport industry? Use the newly created binary
# variable with your if statement.

# In class activity 2 # 

mydata2<- read.csv("/Users/cmc454/Documents/census.csv", sep=",")
attach(mydata2)

# Number 1-3 #
table(region)
# There are 4 regions used: NE Cntrl, NE, South, and West. South has the most states in the data set. #

# Number 4 #
tapply(medage, region, mean)
# NE has the highest avg median age #

# Number 5 #
sort(table(region), decreasing=T)

# Number 6 #
# Write a command to list the states in the largest region.
SouthSt <- subset(mydata2, region == "South")
as.list(SouthSt$state)

# Number 7 #
str(mydata2)
# State, State 2, and region are technically factors (the equivalent of value labels in Stata) here based on how we have 
# loaded them in. If we had written stringsAsFactors = F upon importing the data, then all three would be strings. 

# At home activity # 
mydata3<- read.csv("/Users/cmc454/Documents/gnp96.csv", sep=",")
attach(mydata3)

# Number 1 #
str(mydata3)
# The data has two variables #
sum(mydata3$gnp96 < 6000)
# 76 quarters had a real GNP of less than $6000
mydata3[order(mydata3$gnp96),]
9678.4-3631.6
# The difference is 6046.80

# Number 2 #
mydata<- read.csv("/Users/cmc454/Documents/nlsw88.csv", sep=",")
attach(mydata)

table(mydata$race)
# 1637 individuals reported their race as white. 

table(mydata$industry)
# Professional Services is the most common industry for workers in this sample. #

mydatawag <-subset(mydata, wage>20)
table(mydatawag$industry)
# Financial/Ins/Real Estate is the most common industry for workers who make an hourly wage greater than $20. 

# Number 4 #
tapply(wage, industry, mean)

# The avg wage of manufacturing is 7.51

# Number 3 # 

prop.table(table(mydata$union))
# 20.52% are union workers. #

table(mydata$union)
# 1417 individuals are not in a union #

tapply(wage, union, mean)
8.674294 - 7.204669 
# The difference is 1.47 # 




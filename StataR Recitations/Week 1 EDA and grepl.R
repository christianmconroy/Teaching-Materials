# In class activity 1 #

mydata<- read.csv("/Users/christianmconroy/Desktop/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/RExportcsv.csv", sep=",")
attach(mydata)

# In the case of R, the "browse" and "list" functions are essentially the same
mydata


mydata[order(mydata$price),]
nrow(mydata)

str(mydata)
str(mydata$mpg); str(mydata$make)  

str(mydata$Weight)
# Just Like Stata, R is also case sensitive! This is why the above returns a null
str(mydata$Wei)
# Unlike Stata, R does not accept abbreviations!

# There is no wildcard operator in R. The closest equivalent is grep() or grep1(). See below for an example of how to identify variables starting with m. Can then create vector or subset and analyze from there. 
grepl("^[m_]", colnames(mydata)) 

summary(mydata$mpg)

summary(mydata$make)
# In R, the summary provides a frequency count in the case of strings. 

# In class activity 2 #
mydata<- read.csv("/Users/christianmconroy/Desktop/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/RExportcsv.csv", sep=",")
attach(mydata)
str(mydata)
d <- data.frame(price,mpg, weight, length)
summary(d)
mydata[order(mydata$mpg),]
41-12
# Difference is 29 

# To summarize price, mpg, weight, and length for cars costing less than $4000.
summary(mydata[mydata$price < 4000, c("price", "mpg", "weight", "length")], basic = T)

# To summarize price, mpg, weight, and length for cars costing less than $4000 that are NOT foreign
summary(mydata[mydata$price < 4000 & mydata$foreign == 'Domestic', c("price", "mpg", "weight", "length")], basic = T)

summary(mydata$price)
mydata$price
make[13]
# Look at the summary, find the row in the matrix, and search make for the corresponding row. The make is Cad. Seville and the price is $15906. 

# After Class Review Exercies 
mydata<- read.csv("/Users/christianmconroy/Desktop/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/lifexpcsv.csv", sep=",")
attach(mydata)
str(mydata)
# Variables: 6; Strings: 2; Numeric: 4; Observations: 68 
sum(is.na(popgrowth)) 
sum(is.na(lexp)) 
sum(is.na(gnppc)) 
sum(is.na(safewater)) 
# 5 missing values for gnppc and 28 missing values for safewater. 
summary(lexp)
79-54
#Difference is 25





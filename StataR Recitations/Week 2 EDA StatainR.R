# In-Class Instruction#
setwd("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")

install.packages('webuse')
library(webuse)
webuse("nlsw88")
attach(nlsw88)

head(nlsw88)
# The <dbl> notation signifies that those variables are value labels, meaning that the numbers correspond to a specific category or level. 
# To identify the "codebook" of what numbers correspond to what categories/factors, we can use the head function on the variable ion question. 

head(race)

# What if we want to see how complete our data set is or how many missing values exist within variables? 

# For the whole data set: 
sum(is.na(nlsw88)) 
# For specific variables
is.na(wage)
sum(is.na(wage)) 
sum(is.na(tenure))
sum(is.na(race))
sum(is.na(married))
sum(is.na(occupation))
sum(is.na(industry))




# X first and then Y
plot(wage[race == 1], tenure[race == 1])

table(industry)
str(nlsw)

# Two ways to make a histogram to represent frequency counts 

barplot(table(industry),main="Frequency Count for Industry",ylab="Freqency",las=2)

library(ggplot2)
ggp <- ggplot(data.frame(industry), aes(x=industry)) + geom_bar() + theme(axis.text.x=element_text(angle=90, hjust=1))
ggp

nlsw$t    ranspdummy <- as.factor(industry == "Transport/Comm/Utility")
summary(nlsw[nlsw$transpdummy == T, c("wage")], basic = T)

summary(nlsw$transpdummy)


nlsw$transpdummy <- as.factor(nlsw$transpdummy)
summary(nlsw$transpdummy)



summary(lm(wage ~ factor(industry), data = nlsw))


  

nlsw$prof <-0
nlsw$prof[which(nlsw$industry ==11)] <-1

summary(nlsw$prof)

for(level in unique(nlsw$industry)){
  nlsw[paste("dummy", level, sep = "_")] <- ifelse(nlsw$industry == level, 1, 0)
}


levels(industry)
nlsw$industry.f <- factor(nlsw$industry, exclude = NA)

summary(lm(wage ~ industry.f, data = nlsw))


# In-Class Activity 1#

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

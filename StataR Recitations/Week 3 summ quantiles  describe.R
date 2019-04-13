# In Class Activity 1 #
mydata<- read.csv("/Users/chris/Downloads/auto.csv", sep=",", stringsAsFactors = T)
attach(mydata)

# Number 1#
summary(mpg)
# The median value of the mpg variable is 20

# Number 2#
Domesticauto <- subset(mydata, foreign == "Domestic")
quantile(Domesticauto$mpg, c(.95))
# The 95th percentile value of the mpg of domestic cars is 28.45

# Number 3 #
install.packages('psych')
library(psych)
describeBy(mydata[,c('price', 'weight', 'mpg', 'rep78')], mydata$foreign)

# In Class Activity 2 #
summarized.foreign = mpg[,list(obs = length(mpg), mean=mean(mpg), sd=sd(mpg), min=min(mpg), max=max(mpg)), by="foreign"]
summarized.foreign

mydata <- mydata[order(-mydata$mpg),]
# Can also go into data table and click on filtering icon next to variable name. 

# At home activity
mydata2<- read.csv("/Users/chris/Downloads/nlsw88.csv", sep=",")
attach(mydata2)

# There is not one function that gives the exact output of Stata's summarize, detail. It is best to use a combination of the following. 
describe(wage)
quantile(wage, c(.01, .05, .10, .25, .5, .75, .90, .95, .99))
head(sort(wage, decreasing = T),5)
head(sort(wage, decreasing = F),5)
# Second highest hourly wage is $40.20

mydata2_u <-subset(mydata2, union=='union')
head(sort(mydata2_u$wage, decreasing = T),5)
#Second highest hourly wage is 28.46

prop.table(table(union))
# The function by default shows the missing (16.28%)

# Unless you have written a codebook file, there is no automatic codebook in R similar to that in Stata. The two below will get you as close as possible. 
install.packages('Hmisc')
library(Hmisc)
describe(mydata2)
str(mydata2)


length(unique(grade))
# There are 16 unique values for grade (R lists 17 because it includes the missing value category)

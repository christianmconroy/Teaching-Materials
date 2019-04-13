######### WEEK 3 R RECITATION #############

#### Loading in the data ######
install.packages('webuse')
library(webuse)
# We use the combination of install.packages and library to call in the numerous functions from the comprehensive R archive network (CRAN) and operationalize the functions of packages within the console. This also imports in a help file that we can use to figure out the usage, syntax, arguments, details, and examples of functions within the imported packages. 

# In this case, we are installing webuse, a package that allows us to import Stata datasets from Stata's server and load it as a named object in this environment. We can get a list of all the stata datasets we have access to via names(webuselist)
names(webuselist)
# The R equivalent of internal R datasets can be accessed using.
data()
data(mtcars)

# We're going to be using the nlsw88 dataset we've previously worked with in the Stata Recitations. 
webuse("nlsw88")
attach(nlsw88)

###### Looking at the data #######
head(nlsw88)
# The <dbl> notation signifies that those variables are value labels, meaning that the numbers correspond to a specific category or level. This is almost the opposite of Stata where words mask the underlying numbers.  

# To see how many levels a factor variable has, we can use length:

length(unique(industry))
# It lists 13 values as it includes a level for NA. To remove that level, we can do: 

# nlsw88 <- as.data.frame(na.omit(nlsw88$industry))

# To identify the "codebook" of what numbers correspond to what categories/factors, we can use the head function on the variable ion question. 

head(industry)

# What if we want to see how complete our data set is or how many missing values exist within variables? 

# For the whole data set: 
is.na(nlsw88)
sum(is.na(nlsw88)) 
# For specific variables
is.na(wage)
sum(is.na(wage)) 
sum(is.na(tenure))
sum(is.na(race))
sum(is.na(married))
sum(is.na(occupation))
sum(is.na(industry))

############## What if we're concerned with finding out which industry makes the most money?  ##############

# What does the distribution of wages in our dataset look like? 
hist(wage, xlab="Wages", ylab = "", main = "Wages Distribution")
# The default here is to plot frequencies. If you identify freq = FALSE, then it will provide the probability densities (i.e. the probability that a variable takes on a value given the distribution)


## First, we can summarize the mean wages for each industry.##
# To get the mean wages for all of our industries, we can use tapply. Can go back to head(industry) to reference which numbers correspond to which industries
# If we just want to target the mean wage (and quartiles) of one industry, we can use the summary function.
head(industry)
summary(wage[industry==7])

# If we want to get the mean wage for all of our industries, we can use tapply. 
tapply(wage, industry, mean)

# If we want to plot the wage of the industry with the highest average, we use the plot function.
plot(wage[industry == 2], pch=19, col="blue")
# pch tells R which shape to use for the plotted points: http://www.endmemo.com/program/R/pchsymbols.php

# As we see in the above, the reason mining has such a high wage is that there are only four observations in our dataset that are in the mining industry and one of them appears to be a significant outlier, making over $40. 

# What about the next highest? 

plot(wage[industry == 5], pch=19, col="blue")
# The transport industry also has some high wages, but at least there is a greater amount of data thatr we're dealing with here. 

# Maybe the reason that the transport industry wages are so high is that wages are correlated with tenure and those in our transport sample happen to be very experienced. Let's check that out. 

# The syntax for plotting is plot(x, y, ...). 
plot(tenure, wage, xlab="Tenure", ylab="Wage", pch=19, col="blue")
# Can use the zoom function of the plot tab to get a clearer picture of the plot. 

# I'm still not sure what the trend looks like, so I will add a fit line.
# Keep in mind that while plot uses x,y, regressions use y~x. 
plot(tenure, wage, xlab="Tenure", ylab="Wage", pch=19, col="blue")
abline(lm(wage~tenure), col="red")

# I can also look specifically at how this trend plays out in the transportation industry. 
plot(tenure[industry == 5], wage[industry == 5], xlab="Tenure", ylab="Wage", pch=19, col="blue")
abline(lm(wage[industry == 5] ~ tenure[industry == 5]), col="red")

# The relationship does not look particularly strong, but I can just run the regression to check the overall relationship in the data. 
tenurecorr <- lm(wage ~ tenure)
summary(tenurecorr)

# While the model is not perfect and likely suffers from exogeneity and Omitted Variable Biad (We would need to include more variables), it looks like from the results of our regression that there might be a correlation between tenure and wages. To assess whether there is a higher tenure level for transport compared to others, we'd also have to look at whether there is a statistically significant difference in means between transportation and other industries. 

# To do so, we'd first create a dummy variable from the transport level of the industry factor variable:

nlsw88$transpdummy <- as.factor(industry == 5)
head(transpdummy)

# Then we would run a t test: 

t.test(tenure~transpdummy)

# The null here is that one equals the other and that there is no difference. When you perform a t-test, you're usually trying to find evidence of a significant difference between population means (2-sample t) or between the population mean and a hypothesized value (1-sample t). The t-value measures the size of the difference relative to the variation in your sample data. Put another way, T is simply the calculated difference represented in units of standard error. The greater the magnitude of T (it can be either positive or negative), the greater the evidence against the null hypothesis that there is no significant difference. The closer T is to 0, the more likely there isn't a significant difference.


tenurecorr2 <- lm(wage ~ factor(industry))
summary(tenurecorr2)

# In this case, R selects the first industry, agriculture, as the reference category. 

# In-Class Activity 1#

names(nlsw88)

# Number 1 #
table(collgrad)
table(union)
# table(nlsw88[c("collgrad", "union")])
# table(cbind("collgrad", "union"))
summary(nlsw88[c("age", "wage", "tenure")])

# Number 2 #
head(industry)
table(industry)
# Professional Services is the most common industry for workers in this sample. #

# Number 3 #
tapply(wage, industry, mean)
# OR
summary(nlsw88$wage[industry==11])
# The average hourly wage is $7.88

# Number 4 #
plot(ttl_exp, wage, xlab="Tenure", ylab="Wage", pch=19, col="blue")
abline(lm(wage~ttl_exp), col="red")
# It looks like what you would expect. 

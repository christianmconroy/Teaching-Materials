# Week 6 In-Class and At-Home Exercises
nlsw <- read.csv("/Users/chris/Downloads/nlsw88.csv", sep=",")

head(nlsw)

attach(nlsw)

nlsw <- na.omit(nlsw)

# Activity 1 #

# 1. What is the average wage of nonunion white workers in the Prof. Services industry? Sales & Laborers? Does it vary by marriage status*/ 
library(psych)
describeBy(nlsw$wage[which(union=="nonunion" & race=="white" & industry=="Professional Services")], nlsw$married[which(union=="nonunion" & race=="white" & industry=="Professional Services")])

#  Those who are single meake an average of $8.51 and those who are married make an average of $7.63.

nlswsub <- subset(nlsw, union=="nonunion" & race=="white")
describeBy(nlswsub$wage[which(occupation== "Laborers" | occupation == "Sales")], nlswsub$married[which(occupation== "Laborers" | occupation == "Sales")])

# In this case, we interpret the language of "in the" to mean that we first are talking about people who are nonwhite and nonuion and then whether they are in sales or labor as an occupation. This is in contrast to the first part, where we talked about people who met three conditions: They are white, they are nonunion, and they are in the Professional services industry. Slightly different interpretation of the wording. 

# 2. Among those who earn the second highest wage in the sample, how many are single?
nlsw <- nlsw[order(-nlsw$wage),]
head(nlsw$wage)

table(nlsw$married[which(nlsw$wage > 40.198 & nlsw$wage < 40.199)])
describe(nlsw)

# 9 married and 7 single.

# Activity 2 #
# Generate an indicator variable called wage_indicator*/ 

nlsw$wage_indicator <- ifelse(wage <=4.25, 0, ifelse(wage >=9.598, 1, 3))

# 2. What is the average hourly wage for rich guys who work in Manufacturing, 
head(industry)
nlswrich <- subset(nlsw, wage_indicator == 1)
summary(nlswrich$wage[which(industry == "Manufacturing" | industry == "Transport/Comm/Utility" | industry == "Wholesale/Retail Trade")])
# Average hourly wage is $15.18. 

# At Home Activity #
# 1. What is the average wage for each race category, white, black, and other
tapply(wage, race, mean)
# Black: $6.84 White: $8.08 Other: $8.55

# *  2. Create an indicator variable called once_married, for people who were once 
# *     married, but are not currently married
# *     How many people are once_married?
nlsw$once_married <- ifelse(married=="single" & never_married==0, 1, ifelse(married=="married" | never_married==1, 0, 3))
table(nlsw$once_married)

# 570 people are once married. 
# Keep in mind the nature of how the data was originally recorded and input. In this case, never_married was imported as an interger and married was imported as a factor. Therefore, we had to use the words "married" and "single". We could convert married to a numeric for consistency like so:
attach(nlsw)
nlsw$married2 <- as.numeric(nlsw$married)
nlsw$once_married2 <- ifelse(married2==2 & never_married==0, 1, ifelse(married2==1 | never_married==1, 0, 3))
table(nlsw$once_married2)

# *  3. Report average wages for never_married and once_married?
# *     Which group has higher average wages?
summary(nlsw[nlsw$never_married == 1, c("wage")], basic = T)
summary(nlsw[nlsw$once_married == 1, c("wage")], basic = T)

# Never_married is $8.51 and Once_married is $7.91, so it looks like never married is better. 

# 4. Create an indicator called tenure20 for people with 20 or more years tenure.

table(tenure)
nlsw$tenure20 <- ifelse(tenure>=20, 1, 0)
table(nlsw$tenure20)
# 48 people

# *  5. Create an indicator called midwage for people whose wage is within $5.00 per hour of the mean. of the mean. What percentage of people reported hourly wage within one st. dev. of the mean? 
summary(wage)
nlsw$midwage <- ifelse(wage >= 7.77-5 & wage <= 7.77+5, 1, 0)
prop.table(table(nlsw$midwage))
# 85.57% of people 

# 6. Create an indicator called occ1 for people in the following occupations:Clerical/unskilled, Laborers, Farm laborers, Household workers. How many people have occ1 equal to zero? How many people have occ1 equal to one?
attach(nlsw)

nlsw$occ1 <- ifelse(occupation=="Clerical/unskilled" | occupation=="Farm laborers" | occupation=="Laborers" | occupation=="Household workers" , 1, 0)
table(nlsw$occ1)
# ooc1 == 0: 1830
# occ1 == 1: 395

# Answer slightly different than Stata set as we removed NAs at the start of this. If we left NAs in, we'd get 399 for 1. 
# Week 6 Spring 2018 - R Solutions #
# ******************************************************************************** PMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")
# Merging, missing data, string replace
# *Q1
schools <- read.csv("userssharedsdfschoolimprovement2010grants.csv", header = T, na.strings =c("", "NA")) 

# Load in package for reading .dta files 
library(haven)

# *Q2
unique(schools$State)
# only 50 unique values, including DC. Which state is missing? Hawaii (short HI)

# Merge the data sets
stateabbs <- read_dta("state_name_abbreviation.dta")
colnames(stateabbs)[2] <- "State"

schools$State <- as.character(schools$State)

library(dplyr)
schools_full <- full_join(schools, stateabbs, by='State')
# We use full join above to ensure that it is not dropping Hawaii. If we want it to automatically drop the unmatched, we could've just used left join. 

# See what wasn't matched. 
rowSums(is.na(schools_full))
schools_full[832,]

# Delete the unmatched row
schools_full <- schools_full[-832,]

schools_full$X2010.11.Award.Amount <- as.numeric(gsub("\\$", "", schools_full$X2010.11.Award.Amount))

# *Q3
schools_full$X2010.11.Award.Amount <- as.numeric(gsub("\\$", "", schools_full$X2010.11.Award.Amount))

# *Q4
schools_full$Model.Selected <- as.factor(schools_full$Model.Selected)

table(schools_full$Model.Selected)

# *Q5
d <- data.frame(schools_full$X2010.11.Award.Amount, schools_full$Model.Selected)
str(d)
summary(d)
library(mice)
md.pattern(d)
# Not as useful in R. 

# 
# *Q6
# * Dealing with missing data
summary(d, na.rm = T)

# egen numbermissing = rowmiss(grantamt model)
schools_full$na_count <- rowSums(is.na(schools_full[c("X2010.11.Award.Amount", "Model.Selected")]))
table(schools_full$na_count)

schools_full$nomiss <- ifelse(schools_full$na_count == 0, 0, ifelse(schools_full$na_count > 0, 1, NA))
table(schools_full$nomiss)

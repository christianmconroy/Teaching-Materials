# In Class Activity 1 
autodata <- read.csv("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/RExportcsv.csv", sep=",")
attach(autodata)

# In Class Acticity 1 #
pdf(file="InClass1.pdf")
plot(mpg, price, xlab="Mileage (mpg) ", ylab="Price in Dollars", pch=19, col="blue")
abline(lm(price~mpg), col="red")

# In Class Acticity 2 #
FDgrouping <- split(autodata$gear_ratio, autodata$foreign)
Splitgear <- sapply(FDgrouping, mean)
barplot(Splitgear, col="red", ylab= "Mean of gear_ratio")

# At home activity #
#1
plot(autodata$weight, autodata$mpg, xlab="Weight (lbs)", ylab= "", main="Vehicle MPG by Weight", pch=19, col="blue")
title(sub="Problem Set Image: Do Not Reproduce This Text")

#2
nlsw<- read.csv("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations/nlsw88.csv", sep=",")
attach(nlsw)
indmeans <- tapply(wage, industry, mean, rm.na=T)
IndNames <- c(levels(industry))
par(mai=c(1,2,1,1))
barplot(indmeans, main="Wage by Occupation", xlab="Ave. wage", names.arg = IndNames, cex.names = 1, las=1, horiz=T, col="blue")
title(sub="Problem Set Image: Do Not Reproduce This Text")

#3
hist(tenure, freq = F, xlab="job tenure(years)", ylab = "", main = "Distribution of Job Tenure", col="burlywood2")
title(sub="Problem Set Image: Do Not Reproduce This Text")

#4
install.packages('dplyr')
library(dplyr)
install.packages('ggplot2')
library(ggplot)
nlsw_df <- nlsw %>% as_tibble 

for (col in c("union", "married", "collgrad")) {
  nlsw_df[[col]] <- factor(nlsw_df[[col]])
}


nlsw_df <- subset(nlsw_df, union!="",
                  select=idcode:tenure)
table(nlsw_df$union)


means <- nlsw_df %>% 
  na.omit() %>%
  droplevels() %>% 
  group_by(union, married, collgrad) %>% 
  summarise(
    mean_wage = mean(wage)
  )

ggplot(data = means) +
  geom_bar(aes(x = collgrad, y = mean_wage, fill = union), stat = "identity", position = "dodge") +
  facet_wrap(~ married)

# Week 8 In Class and At Home Solutions #

# In-Class Activity 1 #

#1
install.packages('haven')
library(haven)
citytemp <- read_dta('citytemp.dta')
attach(citytemp)

#1
citytemp$hightempJan <- ifelse(citytemp$tempjan > 40, 1, ifelse(citytemp$tempjan <= 40, 0, NA))
citytemp$hightempJan <- as.factor(citytemp$hightempJan)
class(citytemp$hightempJan)
#2
table(hightempJan)
# 353 cities have high January temperatures. 

#3
install.packages('ggplot2')

citytemp <- na.omit(citytemp)
a <- ggplot(data=citytemp, aes(x=hightempJan, y = heatdd, group = hightempJan, fill = hightempJan)) + geom_bar(stat = "identity", width = 0.5, position = "dodge") + scale_x_discrete(labels=c("Low", "High")) + guides(fill=F) + labs(y = "Heating Degree Days", x = "January Temp", title="Heating Degree Days by January Temperature")
b <- ggplot(data=citytemp, aes(x=hightempJan, y = cooldd, group = hightempJan, fill = hightempJan)) + geom_bar(stat = "identity", width = 0.5, position = "dodge") + scale_x_discrete(labels=c("Low", "High")) + guides(fill=F) + labs(y = "Cooling Degree Days", x = "January Temp", title="Cooling Degree Days by January Temperature")
grid.arrange(a, b, ncol = 2)

# While we can usually use par or layout to put multiple plots on one page, there is a different process for ggplot. Here we use grad.arrange to look at two different continuous variables. There are likely other ways to do this too. 

#4 
region
table(citytemp$region)
division
table(citytemp$division)
# Thwre are 256 cities in the West region and 61 in the Mountain division. As you see above, you can just type and run the name of the variable to get the value labels. 

# In-Class Activity 2#
#1 
t.test(hsb2$write, mu=7.60) 

#2 
attach(hsb2)
t.test(write~female)

#3
library(MASS)
chisq.test(female, schtyp, correct = F)

# In-Class Activity 3#
#1
plot(read, write, xlab="Read", ylab="Write", pch=19, col="blue")
abline(lm(write~read), col="red")

#2
cor.test(read, write)

#3
hsb2$id_odd <- ifelse(as.integer(hsb2$id)%%2, 1, 0)

# %% refers to integer-divide x by y and return the remainder. So we're testing for whether the number can be divided by 2 into an integer here or not. 

#4
table(hsb2$id_odd)
# There are 100 odd numbers where id_odd equals 1. 
hsb2$write[hsb2$id_odd==1] <- NA

#5
cor.test(hsb2$read, hsb2$write)
# The corrleation now is still .609. It is significant still. There are 100 observations now

# In Class Activity 4#
nlsw <- read_dta('nlsw88.dta')

#1 
label(nlsw) <- "NLS Mature and Young Women, 1988" 
save(nlsw,file="nlsw.Rda")

# While the first code runs, it is not very useful. There isn't really any dataset labeling that is used in R. 

#2
names(nlsw)
nlsw$weekly_wage <- eval(wage*hours)
label(nlsw$weekly_wage) <- "Weekly Wages" 
describe(nlsw$weekly_wage)

#3
nlsw$employ_type <-cut(nlsw$hours, c(0,40,60,100), labels=c(1:3))
levels(nlsw$employ_type)
class(nlsw$employ_type)

nlsw$employ_type <- factor(nlsw$employ_type, levels = c(1,2,3), labels = c("Part-time", "Full-Time", "Other"))
describe(nlsw$employ_type)

# At-Home Activity 
#1 
auto <- read_dta("auto.dta")
auto$mpgcat <-cut(auto$mpg, c(0,18,23,100), labels=c(1:3))
table(auto$mpgcat)

summary(auto$mpg)
# The highest value of mpg in the data is 41. 
auto$mpgcat[which.max(auto$mpg)]
# OR
auto$mpgcat[auto$mpg==41]
# The associated mpgcat is 3, so we did it right. 

#2 
label(auto$mpgcat) <- "MPG Category" 
auto$mpgcat <- factor(auto$mpgcat, levels = c(1,2,3), labels = c("low mpg", "mid mpg", "high mpg"))
table(auto$mpgcat)

#3 
attach(auto)
ggplot(auto) + geom_bar(aes(label, price, fill = as.factor(mpgcat)), 
           position = "dodge", stat = "summary", fun.y = "mean")

# Mid MPG has the highest average price. 
tapply(price, mpgcat, mean) # To check to make sure we get the right values. 

library(dplyr)
automeans <- auto %>%
  group_by(mpgcat) %>%
  summarise(mean_P = mean(price))

ggplot(automeans, aes(mpgcat, mean_P)) + geom_col() + labs(y = "Avg. Price", x = "MPG Cat", title="Average Price by Mileage")
# Cars in the low mpg category have the highest average price.

#4 
table(rep78, mpgcat, useNA = "ifany")
# 2 have mid-range mpg and are missing rep78

#5 
t1 <- table(rep78, mpgcat)
t1
prop.table(t1)
# Most commob combination is rep78 == 3 and mpgcat == 2
# 23.19% of records has this combination of rep78 and mpgcat.

# 6
print(chisq.test(rep78, mpgcat, correct = F), digits=5)

#   Pearson chi2(8) =  18.8   
# P-value = 0.016

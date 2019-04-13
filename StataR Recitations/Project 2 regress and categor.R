 # * Spring 2018 McCourt STATA Recitation
# 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations") 

sports <- read.csv("Schools.csv", header = T)
# // 2.3
# * creating dummies
# Medium
sports$medSchool <- ifelse(sports$EFTotalCount >=1000 & sports$EFTotalCount < 4999, 1, ifelse(sports$EFTotalCount <1000 | sports$EFTotalCount >=4999, 0, NA))
# Large
sports$largeSchool <- ifelse(sports$EFTotalCount >= 5000, 1, ifelse(sports$EFTotalCount < 5000, 0, NA))

# *verify dummies
table(sports$medSchool)
table(sports$largeSchool)

sapply((split(sports$EFTotalCount, sports$medSchool)), mean)
sapply((split(sports$EFTotalCount, sports$LargeSchool)), mean)

reg1 <- lm(TOTAL_EXPENSE_ALL ~ medSchool + largeSchool, data = sports)
summary(reg1)

# // 2.4 factor variables
sports$schoolsize <- ifelse(sports$EFTotalCount >=0 & sports$EFTotalCount < 1000, 1, ifelse(sports$EFTotalCount >=1000 & sports$EFTotalCount <4999, 2, ifelse(sports$EFTotalCount >=5000, 3, NA)))
table(sports$schoolsize)

# *check categorical against dummies

table(sports$schoolsize, sports$largeSchool, sports$medSchool)

reg2 <- lm(TOTAL_EXPENSE_ALL ~ factor(schoolsize), data = sports)
summary(reg2)

# // 2.5
sports$sectorid <- as.factor(sports$sector_name)
table(sports$sectorid)
levels(sports$sectorid)

reg3 <- lm(TOTAL_REVENUE_ALL ~ factor(sectorid), data = sports)
summary(reg3)
# 
# // 2.6
sports$schoolsize2 <- ifelse(sports$EFTotalCount >=0 & sports$EFTotalCount <= 999, 1, ifelse(sports$EFTotalCount >=1000 & sports$EFTotalCount <=4999, 2, ifelse(sports$EFTotalCount >=5000, 3, NA)))
# There is not as big of a difference between dummy generation and a recode function in R as there is in Stata. Easty to do all with ifelse statements. 

sports$numparticipants <- rowSums(sports[c("PARTIC_MEN", "PARTIC_WOMEN")], na.rm=T)
summary(sports$numparticipants)
# * so 17 26 41 seem like cut offs

sports$categorical_participants <- ifelse(sports$numparticipants >=0 & sports$numparticipants <= 17, 1, ifelse(sports$numparticipants >=18 & sports$numparticipants <=26, 2, ifelse(sports$numparticipants >=27 & sports$numparticipants >=41, 3, ifelse(sports$numparticipants <=42, 4, NA))))

sports$categorical_participants <- factor(sports$categorical_participants, levels = c(1,2,3,4), labels = c("Small Team", "Mid-Small Team", "Mid-Large Team", "Large Team"))

table(sports$categorical_participants)

reg4 <- lm(TOTAL_EXPENSE_ALL ~ factor(categorical_participants), data = sports)
summary(reg4)
# 
# ************** WEEK 2 OF PROJECT 2 ****************************************
#   
#   // Q1
# Lookfor total
names(sports)
# * We see that we're looking at TOTAL_REVENUE_ALL and TOTAL_EXPENSE_ALL

sports$profit <- eval(sports$TOTAL_REVENUE_ALL - sports$TOTAL_EXPENSE_ALL)

# // Q2
table(sports$Sports)
sports$sportid <- as.factor(sports$Sports)
levels(sports$sportid)

# Alternative way to recode - the recode function in the car package
sports$sportcat <- NA
sports$sportcat <- as.numeric(sports$sportid)

install.packages('car')
library(car)

sports$sportcat <- recode(sports$sportcat, "c(4,24)=1; 5=2; 12=3; 23=4; else=5")

sports$sportcat <- factor(sports$sportcat, levels = c(1,2,3,4,5), labels = c("Baseball/Softball", "Basketball", "Football", "Soccer", "Other"))
table(sports$sportcat)

# Graph the two means by group 
install.packages('reshape2')
library(reshape2)
dfm <- melt(sports[,c('sportcat','TOTAL_REVENUE_ALL','TOTAL_EXPENSE_ALL')],id.vars = 1)

ggplot(dfm,aes(x = sportcat,y = value)) + 
  geom_bar(aes(fill = variable),stat = "summary",position = "dodge", fun.y = "mean")

# 
# // Q3
table(sports$SUM_FTHDCOACH_FEM)
table(sports$SUM_PTHDCOACH_FEM)

sports$femhdcoach <- ifelse(sports$SUM_FTHDCOACH_FEM>0  | sports$SUM_PTHDCOACH_FEM>0, 1, ifelse(sports$SUM_FTHDCOACH_FEM==0  | sports$SUM_PTHDCOACH_FEM==0, 0, NA))
table(sports$femhdcoach)

# // Q4
ggplot(sports,aes(x = sportcat,y = profit)) + 
  geom_bar(stat = "summary",position = "dodge", fun.y = "mean") + facet_wrap(~femhdcoach)

# // Q5
reg5 <- lm(profit ~ factor(sportcat) + factor(femhdcoach) + sportcat * femhdcoach, data = sports)
summary(reg5)

#   * If want to get significance. 
reg52 <- lm(profit ~ factor(sportcat) + factor(femhdcoach), data = sports)
anova(reg52, reg5)
# Not statistically significant. 

# // Q6
ggplot(sports, aes(x=numparticipants, y=profit)) +geom_point(aes(col=sportcat))

# // Q7
reg6 <- lm(profit ~ numparticipants + factor(sportcat) + numparticipants * sportcat, data = sports)
summary(reg6)
# 
# // Q8
sports$prprof <- fitted(reg6)

qplot(x = numparticipants, y = profit, color = sportcat, data = sports) + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

# ********************* END PROJECT 2 *****************************************

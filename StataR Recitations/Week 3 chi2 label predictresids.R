## Stata Reciation 1-3 R Translation ##

# *1.2
# * Change working directory
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations") 

install.packages('haven')
library(haven)

NAMCS2010 <- read_dta('NAMCS2010_recitation.dta')
View(NAMCS2010)

# *1.3
# * Change variable name to lower case
names(NAMCS2010) <- tolower(names(NAMCS2010))

# * Get mean and median of age
summary(NAMCS2010$age)

# * Histogram 
hist(NAMCS2010$age)

# * Patient race
NAMCS2010$raceun <- as.factor(NAMCS2010$raceun)
table(NAMCS2010$raceun)
str(NAMCS2010)
library(MASS)
chisq.test(female, schtyp, correct = F)

# * Relationship between sex and race
NAMCS2010$sex <- as.factor(NAMCS2010$sex)
table(NAMCS2010$sex, NAMCS2010$raceun)
chisq.test(NAMCS2010$sex, NAMCS2010$raceun, correct=F)
# Ans: the null hypothesis that the two attributes are independent of each other is rejected.

# *1.4
NAMCS2010 <- subset(NAMCS2010, NAMCS2010$age>=16)
# As required in week 3, we've changed this from >- to 18 to >=16. 

# *1.5
# Working with Stata Variable Labels in R is a bit tricky, so I made my own function here to search through all variable names. Keep in mind that R is case sensitive. 
# lookfor weight
vlabelpull <- function(varname) {
  l <- attr(varname, "label")
  l
}

l2 <- lapply(NAMCS2010, vlabelpull)
l2 <- as.data.frame(l2)
grep("Weight", l2)
l2[c(58, 59, 99, 152)]

# lookfor height
grep("Height", l2)
l2[c(57, 59, 98)]
# 
# *1.6
# * Numeric with lable
head(NAMCS2010$wtlb)

# * tabulate command without value labels.
table(NAMCS2010$wtlb)
table(NAMCS2010$htin)

# *  get the label values 
attr(NAMCS2010$wtlb, "label")
attr(NAMCS2010$htin, "label")

# 
# * Recode "Blank" as missing

NAMCS2010$wtlb[NAMCS2010$wtlb == -9] <- NA
NAMCS2010$htin[NAMCS2010$htin == -9] <- NA

# * Max and min
summary(cbind(NAMCS2010$wtlb, NAMCS2010$htin))

# *1.7
# * Get codebook of usetobac
str(NAMCS2010$usetobac)

# * Generate dummy variable
NAMCS2010$current_tobac <- ifelse(NAMCS2010$usetobac == 2, 1, ifelse(NAMCS2010$usetobac == 1, 0, NA))

# * two-way tabulation with missing value
table(NAMCS2010$current_tobac, NAMCS2010$usetobac, exclude = NULL)
# 
# * Generate overweight dummy variable
NAMCS2010$overwt <- ifelse(NAMCS2010$bmi >= 27, 1, ifelse(NAMCS2010$bmi < 27, 0, NA))

# * two-way tabulation with missing value
table(NAMCS2010$bmi, NAMCS2010$overwt, exclude = NULL)

# * twoway tab
table(NAMCS2010$overwt, NAMCS2010$current_tobac)

# * count observations
length(which(NAMCS2010$overwt==1 & NAMCS2010$current_tobac == 1))

# *1.8s
# * regression of systolic blood pressure on age, height, weight, and bmi.
reg1 <- lm(bpsys ~ age + htin + wtlb + bmi, data = NAMCS2010)
summary(reg1)
#
# * test coefficients
library(car)
linearHypothesis(reg1, "wtlb=0")

# ********************* WEEK 2 **************************
#   *1 - confirm sample
# *make sure we have 3885 observations (3965 after 18 to 16 switch)
nrow(NAMCS2010)
# 
# *2 - recode missing
# *check for negative values by lookin gat the negative
summary(cbind(NAMCS2010$bpsys, NAMCS2010$htin, NAMCS2010$wtlb, NAMCS2010$bpdias))
# *remove missing values from bpdias
NAMCS2010$bpdias[NAMCS2010$bpdias == -9] <- NA

# *3
reg2 <- lm(bpsys ~ overwt + current_tobac, data=NAMCS2010)
summary(reg2)

# *4
linearHypothesis(reg2, "current_tobac=0")
# *so same p-value

linearHypothesis(reg2, "current_tobac=7")
# *reject null

# *5
reg3 <- lm(bpsys ~ current_tobac + overwt, data=NAMCS2010)
summary(reg3)

# 
# test current_tobac overwt
linearHypothesis(reg3, c("current_tobac = 0", "overwt = 0"))
# * ^This tests that coefficients are equal to 0
# test current_tobac = overwt
linearHypothesis(reg3, "current_tobac = overwt")
linearHypothesis(reg3, "current_tobac - overwt = 0")
# * ^These test that coefficients are equal.

# *6
NAMCS2010$bpave <- eval((NAMCS2010$bpsys + NAMCS2010$bpdias)/2)
# ^This is the equivalent of gen bpave = . ; replace bpave = (bpsys + bpdias)/2
summary(NAMCS2010$bpave)

x <- cbind(NAMCS2010$bpsys, NAMCS2010$bpdias)
NAMCS2010$bpave2 <- rowMeans(x)
# ^ This is the equivalent of egen bpave2 = rowmean(bpsys bpdias)
summary(NAMCS2010$bpave2)


# In R, there is no similar difference as the gen vs. egen. The two above essentially do the same thing. 

# *7 - introduction to bysort
summary(NAMCS2010[NAMCS2010$sex == 1, c("htin", "wtlb", "bpsys", "bpdias")], basic = T)
summary(NAMCS2010[NAMCS2010$sex == 2, c("htin", "wtlb", "bpsys", "bpdias")], basic = T)

# * Alternative Method
install.packages('dplyr')
library(dplyr)

NAMCS2010 %>% 
  group_by(sex) %>% 
  summarize(htinmean=mean(htin, na.rm=T), wtlbmean=mean(wtlb, na.rm=T), bpsysmean=mean(bpsys, na.rm=T), bpdiasmean=mean(bpdias, na.rm=T))
  
# 
# *** summarizing bpdias and bpsys 
NAMCS2010 %>% 
  group_by(current_tobac, overwt) %>% 
  summarize(htinmean=mean(htin, na.rm=T), wtlbmean=mean(wtlb, na.rm=T), bpsysmean=mean(bpsys, na.rm=T), bpdiasmean=mean(bpdias, na.rm=T))

# *8
# *example
NAMCS2010$mfaveht <- ifelse(NAMCS2010$sex == 1, mean(NAMCS2010$htin[NAMCS2010$sex==1], na.rm = T), ifelse(NAMCS2010$sex == 2, mean(NAMCS2010$htin[NAMCS2010$sex==2], na.rm = T), NA))

# There may be a neater way to do this with dplyr, but the above works fine. 

NAMCS2010$mftall <- ifelse(NAMCS2010$htin > NAMCS2010$mfaveht, 1, ifelse(NAMCS2010$htin <= NAMCS2010$mfaveht, 0, NA))

# *creating age variable
NAMCS2010 <- NAMCS2010 %>%
  group_by(age) %>%
  mutate(avgwtlb = mean(age, na.rm = TRUE))

NAMCS2010$ageheavy <- ifelse(NAMCS2010$wtlb > NAMCS2010$avgwtlb, 1, ifelse(NAMCS2010$wtlb <= NAMCS2010$avgwtlb, 0, NA))

# ********************* WEEK 3 **************************
#   * 1.21 Create a dummy variable for male
str(NAMCS2010$sex)
# No explicit label here, but we know from the stata file that 2=male
NAMCS2010$male <- ifelse(NAMCS2010$sex == 2, 1, ifelse(NAMCS2010$sex == 1, 0, NA))

# * Create dummy variable for overweighted male
NAMCS2010$male <- NAMCS2010$male * NAMCS2010$overwt
# 
# * 1.22 update sample
# * Change command 1.4 at the top!
nrow(NAMCS2010)

# * 1.24 Linear combinations of coefficients
# regress bpsys current_tobac male wtlb age
reg4 <- lm(bpsys ~ current_tobac + male + wtlb + age, data=NAMCS2010)
summary(reg4)
# 
# * change in predicted systolic blood pressure for a male tobacco user
# display "The combined effect is: " _b[current_tobac] + _b[male]
r <- paste("The combined effect is", eval((summary(reg4)$coefficients[2]) + (summary(reg4)$coefficients[3])), sep = " ")
r
# 
# * Test combined change
linearHypothesis(reg4, "current_tobac + male = 0")

# * Not statistically signifidant (p = .13)
# 
# * Confidence interval of the combination
library(foreign)
library(multcomp)

confcm <- summary(glht(reg4, linfct = c("current_tobac + male = 0")))
confint(confcm)

# * Combined change of wtlb and age
linearHypothesis(reg4, "wtlb + age = 0")
#
# * Combined change of being male and having additional 10lbs weight
linearHypothesis(reg4, "10*wtlb + male = 0")

# * Difference in weight cause a tobacco user and a non-user to have
# * equal predicted value of bpsys

# * increase bpsys tobacco user:
 summary(reg4)$coefficients[2]

# * increase bpsys by pound:
 summary(reg4)$coefficients[4]

# * thus to increase bpsys through weight gain by the same amount:
eval((summary(reg4)$coefficients[2])/ (summary(reg4)$coefficients[4]))

# * 1.26 Predict values
# 
# * Very manual approach - For one observation. 
reg5 <- lm(bmi ~ wtlb, data = NAMCS2010, na.action = na.exclude)
# regress bmi wtlb
(summary(reg5)$coefficients[1]) + (summary(reg5)$coefficients[2])*150
# disp _b[_cons] + _b[wtlb]*150 
# 
# * Manual approach
# regress bmi wtlb
# gen pr_bmi_manual = _b[_cons] + _b[wtlb] * wtlb
NAMCS2010$prbmimanual <- (summary(reg5)$coefficients[1]) + (summary(reg5)$coefficients[2])*NAMCS2010$wtlb

# * use predict postestimation command 
NAMCS2010$prbmi <- fitted(reg5)

ggplot(NAMCS2010, aes(wtlb)) + geom_point(aes(y=bmi), color="blue") + geom_point(aes(y=prbmi), color = "red")


# * Generate residuals
NAMCS2010$rebmi <- resid(reg5)

ggplot(NAMCS2010, aes(wtlb)) + geom_point(aes(y=bmi), color="blue") + geom_point(aes(y=prbmi), color = "red") + geom_point(aes(y=rebmi), color = "green")

summary(cbind(NAMCS2010$bmi, NAMCS2010$prbmi, NAMCS2010$rebmi))

# * Descriptive stats by male and current)tobac
NAMCS2010 %>% 
  group_by(male, current_tobac) %>% 
  summarize(bmimean=mean(bmi, na.rm=T), prbmimean=mean(prbmi, na.rm=T), prbmiremean=mean(rebmi, na.rm=T))

# * 1.28 Outreg2 Table
library(stargazer)

reg6 <- lm(bpsys ~ current_tobac + overwt + nummed + mftall + ageheavy, data = NAMCS2010)

stargazer(reg6, type = 'text')

reg7 <- lm(bpdias ~ current_tobac + overwt + nummed + mftall + ageheavy, data = NAMCS2010)

stargazer(reg6, reg7, type = 'text')

# Easy to just copy and past the above into excel, word, or other formats. 

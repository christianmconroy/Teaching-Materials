### Week 8 Solutions ###

# ProbLogit, plotting probit/logit predictions,   

# Set WD
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")
# Load Required Packages 
library(haven)
library(AER)
library(ggplot2)
library(reshape2)
install.packages('margins')
library(plyr)
library(MASS)

# Question 1
card <- read_dta('card.dta')
head(card)

# Question 2 
reg1 <- lm(libcrd14 ~ educ, data = card, na.action = na.exclude)
# Can use the code below to get heteroscedasticity-consistent errors (Like Robust in Stata)
coeftest(reg1, vcov = vcovHC(reg1, type = 'HC1'))

card$lpm_libcrd14 <- fitted(reg1)
summary(card$lpm_libcrd14)
# Problem with a negative probability

ggplot(data=card, aes(x=educ, y=lpm_libcrd14)) +
  geom_line() +  labs(x="Education", y = "LPM Predictions")

# Question 3
probit1 <- glm(libcrd14 ~ educ, data = card, family = binomial(link = "probit"), na.action = na.exclude)

card$probit_libcrd14 <- predict(probit1, type="response")

ggplot(data=card, aes(x=educ, y=probit_libcrd14)) +
  geom_line() +  labs(x="Education", y = "Probit Predictions")

# Question 4
logit1 <- glm(libcrd14 ~ educ, data = card, family = binomial(link = "logit"), na.action = na.exclude)

card$logit_libcrd14 <- predict(logit1, type="response")

ggplot(data=card, aes(x=educ, y=logit_libcrd14)) +
  geom_line() +  labs(x="Education", y = "Logit Predictions")

# Question 5

ggplot(card, aes(educ)) + 
  geom_line(aes(y = lpm_libcrd14, colour = "lpm_libcrd14")) + 
  geom_line(aes(y = probit_libcrd14, colour = "probit_libcrd14")) + 
  geom_line(aes(y = logit_libcrd14, colour = "logit_libcrd14"))

ggplot(card, aes(educ)) + 
  geom_line(aes(y = lpm_libcrd14, colour = "lpm_libcrd14")) + 
  geom_line(aes(y = probit_libcrd14, colour = "probit_libcrd14")) + 
  geom_line(aes(y = logit_libcrd14, colour = "logit_libcrd14")) + geom_hline(yintercept = 0) + geom_hline(yintercept = 1)

# Question 6
summary(card$libcrd14)
summary(card$logit_libcrd14)

logit1 <- glm(libcrd14 ~ educ, data = card, family = binomial(link = "logit"), na.action = na.exclude)

# No equivalent of average response using margins in R so far as I know. 
summary(card$logit_libcrd14)

# Question 7
probit2 <- glm(libcrd14 ~ educ + IQ, data = card, family = binomial(link = "probit"), na.action = na.exclude)
summary(probit2)
pnorm(-2.074502 + 0.109520*12 +  0.012719*102.4498)
pnorm(probit2$coef[1] + probit2$coef[2]*12 +  probit2$coef[3]*102.4498)

# Question 8 and 9
newdata <- data.frame("educ" = seq(8,16,2), "IQ" = rep(102.4498, each = 5))
newdatapred <- as.data.frame(predict(probit2,newdata=newdata, type="response", se.fit=T))
forplot <- cbind(newdatapred, newdata)

ggplot(forplot, aes(x=educ, y=fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.1) +
  geom_line() +
  geom_point()

newdata2 <- data.frame("educ" = rep(12, each = 11), "IQ" = seq(50,150, 10))
newdatapred2 <- as.data.frame(predict(probit2,newdata=newdata2, type="response", se.fit=T))
forplot2 <- cbind(newdatapred2, newdata2)

ggplot(forplot2, aes(x=IQ, y=fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.1) +
  geom_line() +
  geom_point()

# Question 10 
card$preduc12 <- pnorm(probit2$coef[1] + probit2$coef[2]*12 + probit2$coef[3]*card$IQ)
summary(card$preduc12)

# Approach to simulate Stata's at means option
marginsframe <- data.frame("educ" = rep(12, each = length(card$IQ)), "IQ" = card$IQ)
newdatapred3 <- as.data.frame(predict(probit2,newdata=marginsframe, type="response", se.fit=T))
summary(newdatapred3$fit)

# at
atfunction <- function(i) {
  newdata <- data.frame("educ" = rep(i, each = length(card$IQ)), "IQ" = card$IQ)
  newdatapred <- as.data.frame(predict(probit2, newdata = newdata, type = "response", se.fit=T))
  fullframe <- cbind(newdatapred, newdata$educ)
}
f <- seq(8,16,2)
data <- lapply(f, atfunction)

# Why will this not take the actual means?! I see that I have data!
g <- 1:5
datalist <- list()
for(i in g){
  ave <- mean(unlist(data[[i]][1]), na.rm = TRUE)
  se <- mean(unlist(data[[i]][2]), na.rm = TRUE)
  educ <- mean(unlist(data[[i]][4]), na.rm = TRUE)
  stats <- cbind(ave, se, educ)
  stats$i <- i
  datalist[[i]] <- stats
}

fullstats <- as.data.frame(do.call(rbind, datalist))
fullstats$V1 <- as.numeric(fullstats$V1)
fullstats$V2 <- as.numeric(fullstats$V2)
fullstats$V3 <- as.numeric(fullstats$V3)

# This is the fairly manual way to get the margins and ses. Do not know of a more streamlined way at the moment. 

ggplot(fullstats, aes(x=V3, y=V1)) + 
  geom_errorbar(aes(ymin=V1-V2, ymax=V1+V2), width=.1) +
  geom_line() +
  geom_point() + labs(x="Education", y="Probit Predictions")


mean(unlist(data[[2]][1]), na.rm = TRUE)
mean(unlist(data[[2]][2]), na.rm = TRUE)



newdata3 <- data.frame("educ" = rep(12, each = 11), "IQ" = seq(50,150, 10))
newdatapred4 <- as.data.frame(predict(probit2,newdata=newdata2, type="response", se.fit=T))
forplot3 <- cbind(newdatapred2, newdata2)

ggplot(forplot, aes(x=IQ, y=fit)) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=.1) +
  geom_line() +
  geom_point()

# Question 11
summary(margins(probit2, type = "response"))
summary(margins(probit2, at = list(educ = 12, IQ = 102.4498)))

# Question 12
logit2 <- glm(libcrd14 ~ educ + IQ, data = card, family = binomial(link = "logit"), na.action = na.exclude)

exp(cbind(coef(logit2), confint(logit2)))  

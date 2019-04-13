## Week 7 Solutions ##
# * March 18 2018
# Locals and Loops

# *Q1
mynumber <- 4
print(mynumber)
# Because of the object-oriented language here, for the purposes of this exercise, macros in terms of how they are used in Stata are not really necessary here. 

nextnumber <- as.data.frame(5)
df <- as.vector(paste("x", nextnumber, sep = "_"))
colnames(nextnumber) <- c(df)
# Because we can just create data here unlike in Stata, this part is a bit different. 

mylocalexample <- "hello, world"
print(paste("the text of the local macro, mylocalexample, is: ||", mylocalexample, "||", sep = " "))

nextnumber <- as.data.frame(4)
print(paste("The local macro named mynumber is equal to:", nextnumber, sep = " "))
print(paste("The command I am trying to run is: generate x_4 =", nextnumber, sep = " "))
df <- as.vector(paste("x", nextnumber, sep = "_"))
colnames(nextnumber) <- c(df)

nextnumber <- as.data.frame(5)
print(paste("The local macro named mynumber is equal to:", nextnumber, sep = " "))
print(paste("The command I am trying to run is: generate x_4 =", nextnumber, sep = " "))
df <- c(df, as.vector(paste("x", nextnumber, sep = "_")))

nextnumber <- as.data.frame(6)
print(paste("The local macro named mynumber is equal to:", nextnumber, sep = " "))
print(paste("The command I am trying to run is: generate x_6 =", nextnumber, sep = " "))
df <- c(df, as.vector(paste("x", nextnumber, sep = "_")))

nextnumber <- as.data.frame(7)
print(paste("The local macro named mynumber is equal to:", nextnumber, sep = " "))
print(paste("The command I am trying to run is: generate x_7 =", nextnumber, sep = " "))
df <- c(df, as.vector(paste("x", nextnumber, sep = "_")))

# * Q3
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/StataRecitations")
library(haven)
nhanes2 <- read_dta("http://www.stata-press.com/data/r12/nhanes2.dta")
ctrlvars <- cbind(nhanes2$age, nhanes2$bpsystol, nhanes2$bpdiast, nhanes2$tcresult)
ctrlvars_names <- c('age, bpsystol, bpdiast, tcresult')
print(paste("controlvars:", ctrlvars_names, sep= " "))
summary(lm(weight ~ ctrlvars, data=nhanes2))

# Because R variable names represent the entire vector, printing ctrlvars above would give us the full variable column instead of just the variable names. Hence, we make a separate object for just the variable name strings. 


# * Q4
nextnumber <- 1
df <- as.vector(paste("x", nextnumber, sep = "_"))
nhanes2$df <- nextnumber
colnames(nhanes2)[59] <- df

nextnumber <- 2
df <- as.vector(paste("x", nextnumber, sep = "_"))
nhanes2$df2 <- nextnumber
colnames(nhanes2)[60] <- df

nextnumber <- 3
df <- as.vector(paste("x", nextnumber, sep = "_"))
nhanes2$df3 <- nextnumber
colnames(nhanes2)[61] <- df

nextnumber <- 4
df <- as.vector(paste("x", nextnumber, sep = "_"))
nhanes2$df4 <- nextnumber
colnames(nhanes2)[62] <- df

nextnumber <- 5
df <- as.vector(paste("x", nextnumber, sep = "_"))
nhanes2$df5 <- nextnumber
colnames(nhanes2)[63] <- df


# start of loop

for(mynumber in 1:10) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  print(paste("My favorite number is", mynumber, sep = " "))
}

# end of loop 

# start of loop
c <- c("carrots", "pasta", "soup", "salad")
for(food in c) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  print(paste("Today, I want to eat", food, sep = " "))
}
#  end of loop

# start of loop

for(num in 1:10){
  d <- as.data.frame(assign("x", num)) 
  colnames(d) <- c(paste("x", num, sep = "_"))
  nhanes2 <- cbind(nhanes2, d)
}
# end of loop

# * Q5 

?Control

# start of loop
f <- c("dog", "cat", "chicken", "mouse")
for(mylname in f) {
  print(paste("The next animal is: --", mylname, "--", sep = " "))
}
# end of loop
  
# * Q6
g <- data.frame(nhanes2$hgb, nhanes2$iron)
summary(g)

# start of loop
# First we install and load a package that allows us to neatly present the results of each regression in a table side by side.
# install.packages('memisc')
library(memisc)

ylist <- c("hgb", "hct", "tibc", "iron")
for(i in 1:length(ylist)) {
  model <- paste("model",i, sep="")
  m <- lm(as.formula(paste(ylist[i],"~ weight + height")), data=nhanes2)
  assign(model, m) # We use assign here to assign each variable to each model (1,2,3,4)
  print(mtable(model1, model2, model3, model4)) # Each model corresponds to each variable that we are looping through. 
}
# end of loop

# * Q7
# Load in capability to adjust variable labels
library(Hmisc)
# // start of loop

# NO CLUE WHAT TO DO BELOW. 
ylist <- c("hgb", "hct", "tibc", "iron")
for (i in ylist) {
  nhanes2[paste(i,"ln", sep="_")] <- log(nhanes2[i])
}

varnames <- Cs(age, race, sex, weight)
for(i in ylist) {
  modelformula <- paste(i, " ~ weight + height")
  l <- assign(x = paste("m", i, sep = "."), value = lm(as.formula(modelformula), data = nhanes2))
  print(summary(l))
}

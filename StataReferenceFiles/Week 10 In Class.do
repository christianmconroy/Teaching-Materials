*Week 10 In Class Do

* Warmup Problems *

sysuse nlsw88, clear

* #1
reg wage ttl_exp, robust
* .3314 - Significant at 99% level

* #2
* Manual Way - Can incorporate outside values as well obviously. 
disp _b[_cons] + _b[ttl_exp]*14
* Predicted wage is $8.25

* Automated Way
* Can see this and other options using help regression postestimation
* If we have the value we are looking to predict in our data set
predict predval, xb
sum predval if ttl_exp == 14

* #3
list if ttl_exp == 14.25
* Actual wages are $12.38, $13.28, and $13.08

* #4
reg wage ttl_exp union c_city collgrad, robust
/* Controlling for these factors in the model, as your workforce experience 
increases, there is a correlation of .2951299 on wages. */

/* For #3 above, if they ask how to compare the actual and the predicted, we can 
get the residuals: */

predict predvalres, residuals
list predvalres if ttl_exp == 14.25
* 4.05, 4.95, and 4.75 difference between actual and predicted. 

***** Big Project ******
/* Setup */ 
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations\Reciation10
cd
use vehicles.dta, clear 
/* the help cd menu tells you whether backslash or forward slash is appropriate 
depending on the system you are using. Use file -> Change Working Directory 
if you can't figure it out. */ 

/* This will give you an original dataset and a new dataset. Save separate files
if you need intermediate data sets */ 
save vehicles_final.dta, replace

* Load only a subset
clear
* Can look at variables without actually loading in data. Super useful. 
describe using vehicles.dta
use make price weight mpg using vehicles.dta, clear

/* The scenario replication analysis */ 
use vehicles.dta, clear 

gen priceadj = 0

* Adjust for the 1977 inflation rate
replace priceadj = price*1.068 if domestic==0
replace priceadj = price if domestic ==1

*Adjust for high vs. low price mistakes
recode priceadj (15000/max = 15000) (min/4000 = 4000), gen(priceadj2)
sum priceadj2

/* Order slightly tweaked to account for need to test those will drop against those
we won't */ 
gen datsun = regexm(make, "^Datsun")
gen dropvars = 0
replace dropvars =1 if rep78 == . | datsun == 1
ttest priceadj2, by(dropvars)
disp 6544.01-6279.298

/* Drop the Datsun inaccuracies and missing for repair records */ 
drop if rep78 == .
drop if datsun == 1
describe

* Generate new variables
gen logmile = log(mpg)
gen logweight = log(weight)
gen loglength = log(length)
gen weightlength = weight/length

* Splitting by Repair Records 
recode rep78 (1/3 = 0) (4/5 = 1), gen(highrep)
label define highreplab 0 "Low" 1 "High"
label values highrep highreplab
tab highrep

* Categorizing by Price 
recode priceadj2 (min/4000 = 1) (4000.001/5000 = 2) (5000.001/10000 = 3) (10000.001/max = 4), gen(price_cat)
tab price_cat

gen price33 = 0
replace price33 = 1 if priceadj2 <= 4000
replace price33 = 2 if priceadj2 > 4000 & price <=5000
replace price33 = 3 if priceadj2 > 5000 & price <=10000
replace price33 = 4 if priceadj2 > 10000
tab price33

* Final Analysis 
reg mpg length weight priceadj2, robust

reg mpg length weight priceadj2 if highrep == 0, robust

reg mpg length weight priceadj2 if price_cat != 1 & price_cat != 4, robust





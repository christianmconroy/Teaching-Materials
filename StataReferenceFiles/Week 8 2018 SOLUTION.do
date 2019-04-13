set more off

*Q1
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

use card.dta, clear

codebook libcrd14

*Q2
* Linear Probability Model just uses regress
reg libcrd14 educ , robust

* We want linear predication, the xb option
predict lpm_libcrd14, xb
* Label and graph new variable
label variable lpm_libcrd14 "LPM"
* Summarize to see that there are negative values (Problem 1)
sum lpm_libcrd14
*Remember, if we make a connected line graph, the points are connected in 
* whichever order they appear in the data set.
* To get a nice graph, sort the data by the X-variable before graphing
sort educ
*or add it into the command
twoway line lpm_libcrd14 educ, name(LPM, replace) sort(educ)
* note: name lets you have multiple graph windows open(need to name each one something different)

*Q3
* Probit command is very similar to regress
help probit
* You can also find full examples and a review of probit in the pdf-manual entry
probit libcrd14 educ
* Again, calculate predicted probabilities. For probit, use the pr option of predict
help probit postestimation
predict probit_libcrd14, pr
* Label and graph new variable
label variable probit_libcrd14 "Probit"

*Q4
* Logit command is also very similar:
help logit
logit libcrd14 educ
* To get predicted values, use predict with the pr option, just like probit
help logit postestimation
predict logit_libcrd14, pr
* Label and graph new variable
label variable logit_libcrd14 "Logit"


*Q5
* Now graph all predicted values together
twoway line lpm_libcrd14 probit_libcrd14 logit_libcrd14 educ 



* Add horizontal lines at predicted probabilities of zero and one
twoway line lpm_libcrd14 probit_libcrd14 logit_libcrd14 educ , yline(0) yline(1)
* Notice the logit and probit predicted values will never cross the 0 or 1 line.
twoway line lpm_libcrd14 probit_libcrd14 logit_libcrd14 educ , yline(0) yline(1) scheme(s1mono)
*or try out 
twoway line lpm_libcrd14 probit_libcrd14 logit_libcrd14 educ , yline(0) yline(1) scheme(sj)

*or this one is probably the best!!!!
twoway (line lpm_libcrd14 educ) (lowess probit_libcrd14 educ) (lowess logit_libcrd14 educ), ///
yline(0) yline(1) 


*Q6
*mean of just the dependent variable (So we'd expect more to have card)
sum libcrd14 
* mean of the predicted probabilities of logit
sum logit_libcrd14
* 3010 observations
*make sure we have the logit active
logit libcrd14 educ
margins
* margins is same thing as margins, atmeans
* In contrast to just getting the average, there are 2,997 observations here. 
*no it is not - different number of missing data
sum logit_libcrd14 if libcrd14 != . 
* The mean and the margin are the same though when we change this up. 
* note: if x values are missing predict will not predict them

*Q7
probit libcrd14 educ IQ
probit, coefl

* display approach
* Super manual
display normprob(-2.074503 + .1095198*12 + .0127187*102.4498)
* Slightly less manual
display normprob(_b[_cons] + _b[educ]*12 + _b[IQ]*102.4498)
* Automatic Approach
margins , atmeans at(educ=12)
/* When we only specific one of the variables in atmeans, it takes the average of the others. 
This is clear in the output */ 

*Q8 - Don't do the marginsplot part of the code until #9
margins , atmeans at(educ=(8 10 12 14 16) )
* The below is the same as the above but just says go up by increments of 2 between 8 and 16. 
margins , atmeans at(educ=(8(2)16))
marginsplot

margins, at(IQ=(50(10)150) educ=12)
marginsplot

*Q9
marginsplot

*Q10
gen preduc12 = normprob(_b[_cons] + _b[educ]*12 + _b[IQ]*IQ)
sum preduc12 if e(sample)
margins , at(educ=12)

/* Slight difference in observation without e(sample). 
This goes back to the discrepency from earlier */ 
sum preduc12 
sum preduc12 if libcrd14!=. 

margins , at(educ=(8(2)16))
* Or, same thing

margins , asobserved at(educ=(8(2)16))
marginsplot

* Q11
* Overall observed - Might be switched. 
margins, dydx(*)
* Below: Marginal effect evaluated at the the average for all variables.
margins, dydx(*) atmeans

/* If you use -atmeans-, you get MEMs, Marginal Effects at the Means. 
If you don't use -atmeans- you are basically using -asobserved-, 
which gives you AMEs, Average Marginal Effects. */ 

* Q12
logit libcrd14 educ IQ 
logit libcrd14 educ IQ, or

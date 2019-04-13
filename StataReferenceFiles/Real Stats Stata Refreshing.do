* Real Stats Stata Refreshing *
*************************** Time Series **************************** 
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

use "bikeshare.dta", clear

* 1. Detecting autocorrelation 

* Estimate basic regression model 
reg trips lowtemp weekend

* Save residuals using resid subcommand
predict Err, resid

* Plot residuals over time
scatter Err date
/* If the plot is relatively smooth, positive autocorrelation is likely to exist.
Randomness means autocorrelation doesn't likely exist. Extreme spikiness means 
negative autocorrelation is likely to exist. */

* Tell Stata which variable indicates time. This is key. 
tsset date

* This is where we do the auxilliary regression
reg Err L.Err
/* If p is statistically significantly different from zero, we have evidence 
of autocorrelation. Errors being correlated of course does not mean that your 
estimator is coefficient, but it does mean that the standard equation for variance 
is inaccurate. If the variance estimated by OLS is estimated to be too low, our 
confidence intervals can be too small and we might end up rejecting the null 
hypothesis when we should not. 

Autoregressive for us just means that the error depends on the error from the 
previous period*/ 

* 2. Correcting for autocorrelation 

* Tell Stata which variables order the data chronologically. 
tsset date

* The prais command is the command used to estimate p-transformed models. 
* Corc twostep tells Stata to handle the first observations 

prais trips lowtemp weekend, corc twostep
reg trips lowtemp weekend

/* Running the p-transformed model produces coefficient estiatyes that are unbiased
and consistent and also produces accurate standard errors. Confidence intervals 
will be larger, and it will be harder to reject null hypotheses. */ 

* Running the dynamic model 

reg trips L.trips lowtemp weekend

/* In a dynamic model, the value of the dependent variablwe directly depends on the value
of the dependent variable in the previous term. */ 
* The ways dynamic models differ from the OLS models 
/* The interpretation of the coefficients changes in that an increase in X has 
not only immediate effects but also long-term effects because the boost to Y 
will carry forward via the lagged dependent variable. */
/* Correlated errors can cause a lot of problems in dynamic models. */
/* Including a lagged dependent variable when lambda = 0 can cause the 
estimated coefficient on X to be vastly understated: The lagged dependent variable 
will have wrongly soaked up much of the explanatory power of the independent variable. */ 

* Dickey-Fuller Test is the main test for unit roots

****************** Dummy Dependent Variables ***********************
* Load in the Titanic data
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations\Week5RRecitation
import delimited titanic.csv , clear bindquotes(strict) varnames(1)

* Example below assumes we are dealing with continuous X1
* Estimate probit model after making female and child age variables 
gen female = (sex == "female")
gen child = (age<17)

probit survived age child 

/* This is in contrast to a normal LPM where coefficients are interpreted as the change 
in probability of observing Y=1 for a one-unit change in X. A one unit increase in X is 
associated with a B1 increase in the probability of observing Y=1. For example, an increase
in one point on the 100-point GPA scale is associated with a 3.2 percent increase in the probability
of admission into this law school. The problems of course are that a) probabilities end up
being lower than 0 or greater than 1 and b) there is a risk of misspecifying the relationship.
 */ 
 
 /* The probit/logit models introduce latent variables that allow us to estimate 
 S curves. The relationship is non-linear of course. Latent variables: For a probit/logit
 model, an unobserved continuous variable reflecting the propensity of an individual 
 observation of Y to equal 1. */ 
 
* In the probit model, the error term is normally distributed. This is an assumption. 

/* The estimation process for probit and logit is called Maximum likelihood estimation (MLE), 
which is used to generate the coefficients. We use z tests instead of t tests for MLE and 
probit/logit. The overall fit of a probit/logit model is reported with a log likelihood 
statistic, which is the log of the probability of observing the Y outcomes we did with 
the given X data and the Bs. This is used in hypothesis tests involving multiple coefficients. */

* Interp Strategy 1: Simulations

/* Interpretation is not easy because the effect of X varies across values of X, so 
the value of other independent variables matters (can't just say holding all else equal).
One approach is to use simulations. If X is continuous, we summarize the effect of X1 
on the probability Y=1 by calculating the average increase that would occur in fitted 
probabilities if we were to increase X1 by one standard deviation to the value of X1
for each observation. 1. Estimate Bs. 2. Add one SD to the values of X1 for each observation
and calculate new values. 3. Calculate the average difference to get simulated effect. */
  
* Generate predicted probabilities for all observations 
predict P1 if e(sample)
* Missing generated because some do not have age listed. 
* There is obviously a manual way to do this too on pg 435 of real stats

gen X1Plus = age + 1 

gen P2 = normal(_b[_cons] + _b[age]*X1Plus + _b[child]*child) if e(sample)

gen PDiff = P2 - P1

sum PDiff if e(sample)

* If X1 is a dummy variable (Observed value, discrete differences approach)
probit survived child age

* Generate predicted probabilities for all observations with X1 = 0
gen P3 = normal(_b[_cons] + _b[child]*0 + _b[age]*age) if e(sample)

gen P4 = normal(_b[_cons] + _b[child]*1 + _b[age]*age) if e(sample)

gen PDiff2 = P4 - P3

sum PDiff2 if e(sample)

/* We can sat stuff like "The esimtates imply that increasing GPA by one 
SD is associated with an average increase of 15 percentage points in predicted probability 
of being admitted to law school */ 

* Interp Strategy 2: Margins test
* The marginal effects approach calculates the effect of changing X1 by a miniscule amount
probit survived age child 
margins, dydx(age)

* Likelihood Ratio Test
* To test the null hypothesis that the coefficients on both X2 and X3 are zero:
* First run the restricted model 
probit survived age
estimates store RESTRICTED

* Then run the unrestricted and do the lrtest command
probit survived age child pclass
lrtest RESTRICTED
* This gives us an LR statistic and a p value 

/* If the null hypothesis is true, the log likelihood should be pretty much 
the same for the restricted and unrestricted versions of the model. */ 

* The logit model is on this page too. 

******************************* Regression Discontinuity **********************
* RD is about analyzing discontinuities at where the treatment applies 
 
/* To start with RD, create a dummy treatment variable and an X1 - C variable 
and use the syntax for multivariate OLS. X1 - C is the distance to the cutoff 
variable */ 
/* Key assumption is that error term does not jump at the point of discontinuity 
too. However, even if the error term is correlated with the assignment variable, the 
estimated effect of the treatment is still valid. */ 
/* First create a scalar variable (This is a variable with a single value and it 
will represent the eligibility threshold/cutoff here) */ 
scalar cutoff = 10

* T below just represents treatment. 
gen T = 0

* So we make our groups based on below or above the theshold 
/* We'll pretend in the case of the Titanic data that there is some threshold for 
treatment based on age */ 
replace T = 1 if age > cutoff

/* The assign part deals with the fact that we only look at the parts right near
the threshold. Those above receive and those below don't receive. */ 
gen Assign = age - cutoff

* Then we just do a basic RD (Pretend that fare means something here)
reg fare T Assign

* Stata concepts again on pg. 370 on pg. 391









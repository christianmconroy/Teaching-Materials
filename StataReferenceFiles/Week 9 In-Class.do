/* Week 9 Recitation */ 

/* Review */ 
sysuse nlsw88, clear

* One-sample t-test
ttest wage == 20

* Unpaired t-test equal variance

ttest wage, by(married)

* Unpaired t-test unequal variance 

ttest wage, by(married) unequal

* What's an example of a paired t-test

* mpg1 == mpg2

* Proportion test

prtest collgrad, by(married)

* Chi^2 test? 

tab collgrad union, chi2

/* In-class code */ 

sysuse auto, clear
twoway (scatter mpg weight) (lfit mpg weight)

* Which one is the dependent variable here? mpg. 

regress mpg weight, robust
ereturn list


* To calculate p-statistic for weight
disp 2*ttail(e(df_r),abs(-11.60))

reg mpg weight if foreign == 0

reg mpg weigh length
/* Compare with what we did with just mpg and weight to briefly introduce them to 
OVB */

sum mpg weight length

sum mpg weight length

reg mpg weight length

replace weight = . in 7
reg mpg weight length
replace weight = . in 25
reg mpg weight length
replace weight = . in 60
reg mpg weight length

replace length = . if mpg == .
reg mpg weight length

/* In-class activity 1 */ 
use https://stats.idre.ucla.edu/stat/stata/notes/hsb2, clear

*1
reg write female

*2
reg write female schtyp prog id, robust

*3
reg write read math science if schtyp == 1, robust

*4
* Manually eliminate data with data editor and run a regression. 

*5
twoway (scatter write math) (lfit write math)

*6
twoway (scatter write female) (lfit write female)

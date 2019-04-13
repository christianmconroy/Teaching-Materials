* Tell Stata not pause for -more- messages (Don't need in latest edition)
set more off

*1.2
* Change working directory
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations
cd

* Mac style -> Forward slash
* cd "/Users/hoya99/MSPP/Stata_Recitation/quant2/Data/"

* Windows style -> Back slash
* cd "C:\Users\hoya99\MSPP\Stata_Recitation\quant2\Data"

/* Open the dataset -> Can manually run do with icon above or highlight (can do control l)
 and do control d */ 
use "NAMCS2010_recitation.dta", clear

* Explore the data set (Question 1)
browse
describe
codebook

*1.3
* Change variable name to lower case (Pretty frustrating otherwise)
rename *, lower

* Get mean and median of age
sum age, detail
* With just mean and no median
sum age

* Histogram 
histogram age

* Patient race
tab raceun

* Relationship between sex and race
tab2 sex raceun, chi2
* Ans: the null hypothesis that the two attributes are 
* independent of each other is rejected.

*1.4
* Drop patients if their age is less than 18 years old.
codebook age
drop if age<18
keep if age>17 & age !=.
* No values actually missing here though. 

*1.5
* Height and weight?
lookfor weight
lookfor height

*1.6
* Numeric with lable
codebook wtlb
codebook htin

* tabulate command without value labels.
tab wtlb
tab wtlb, nolabel
tab htin
tab htin, nolabel
*  get the label values with describe
des wtlb
label list WTLBF
des htin
label list HTINF

* Recode "Blank" as missing
codebook wtlb
replace wtlb=. if wtlb==-9
replace htin=. if htin==-9

* Max and min (To summarize here)
sum wtlb htin

*1.7
* Get codebook of usetobac
codebook usetobac

* Generate dummy variable
gen current_tobac=.
replace current_tobac=1 if usetobac==2
replace current_tobac=0 if usetobac==1

* two-way tabulation with missing value
tab2 usetobac current_tobac,m

* Generate overweight dummy variable
gen overwt=.
replace overwt=1 if bmi>=27 & bmi!=.
replace overwt=0 if bmi<27

* two-way tabulation with missing value
tab bmi overwt,m

* twoway tab
tab overwt current_tobac

* count observations
count if overwt == 1 & current_tobac == 1

*1.8

* regression of systolic blood pressure on age, height, weight, and bmi.
reg bpsys age htin wtlb bmi
reg bpsys age htin wtlb bmi, robust
* you should have 1464 observations in the regression

* test coefficients
test wtlb

* Can optionally put at end of do-file to save with date
save "NAMCS2010_adjusted_`c(current_date)'.dta", replace
/* Back tick `' forces Stata to evaluate the valueto return */ 

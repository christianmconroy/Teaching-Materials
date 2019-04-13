* Tell Stata not pause for -more- messages
set more off

*1.2
* Change working directory
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

* Mac style
* cd "/Users/hoya99/MSPP/Stata_Recitation/quant2/Data/"

* Windows style
* cd "C:\Users\hoya99\MSPP\Stata_Recitation\quant2\Data"

use "NAMCS2010_recitation.dta", clear

*1.3
* Change variable name to lower case
rename *, lower

* Get mean and median of age
sum age, detail

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
drop if age<18
keep if age>17 & age !=.

*1.5
* Height and weight?
lookfor weight
lookfor height

*1.6
* Numeric with lable
codebook wtlb

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

* Max and min
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
* you should have 1464 observations in the regression

* test coefficients
test wtlb

********************* WEEK 2 **************************


*1 - confirm sample
*make sure we have 3885 observations
count

*2 - recode missing
*check for negative values by lookin gat the negative 
sum bpsys bpdias htin wtlb, detail
*remove missing values from bpdias
recode bpdias (-9 = .)
*or 
replace bpdias = . if bpdias == -9

*3
reg bpsys overwt current_tobac

*4
test current_tobac==0 
*so same p-value

test current_tobac==7 
*reject null


*5
reg bpdias current_tobac overwt, robust

test current_tobac overwt
* ^this tests that coefficients are equal to 0
test current_tobac = overwt
* ^this tests that coefficients are equal
test current_tobac = overwt = 0
* ^ this test appears equivalent to the first test
test current_tobac - overwt = 0
* This test is same as second


*6
gen bpave = .
replace bpave = (bpsys + bpdias)/2
* Or just
gen bpave = (bpsys + bpdias)/2
sum bpave


egen bpave2 = rowmean(bpsys bpdias)
sum bpave2

* Select specific rows and columns to view - How is it dealing with missing
* Below gives us those where only one side is missing
br bpsys bpdias bpave bpave2 if (bpsys != . | bpdias != .) & missing(bpsys, bpdias)


*7 - introduction to bysort (Manually)
summarize htin wtlb bpsys bpdias if sex==1
summarize htin wtlb bpsys bpdias if sex==2
* Alternative Method
bysort sex: summarize htin wtlb bpsys bpdias

*** summarizing bpdias and bpsys 
bysort current_tobac overwt: summ bpdias bpsys

*8
*example
bysort sex: egen mfaveht = mean(htin)	
tab mfaveht
gen mftall = .
replace mftall = 1 if htin > mfaveht
replace mftall = 0 if htin <= mfaveht
replace mftall = . if htin==. | mfaveht==.

browse sex htin mfaveht mftall

*creating age variable
bysort age: egen avgwtlb  = mean(wtlb)
gen ageheavy = .
replace ageheavy = 1 if wtlb > avgwtlb
replace ageheavy = 0 if wtlb <= avgwtlb
replace ageheavy = . if wtlb==. | avgwtlb==.

* Working with Dates (EXTRA)
import excel "Stata Class File_long.xlsx", 	/// Imports the excel file
			sheet("Sheet1") /// Tells stata which sheet to import
			firstrow 		/// Tells stata first row is variable names
			case(lower) 	/// Makes variable names lowercase (Stata is case sensitive)
			allstring 		/// Forces all to be string, some numeric won't import well
			clear
			
	list arrdate in 1/10
	gen arr_d=date(arrdate, "MDY") 			/*Need to input format of the date of birth, 
											"MDY" (Month Day Year) or 
											"DMY" (Day Month Year)[So it's days since 1/1/1960 up here] */
		label var arr_d "Arrest Date" 		/*Adds a label to the variable*/
		list arr_d arrdate in 1/10
		format arr_d %td  					/*dates in stata are numeric values that are 
											the number of days (or other denomination) 
											since 1/1/1960, they need to be formatted.
											% td takes it from days since to an actual date format*/
		list arr_d arrdate in 1/10
		
	

/*Once it's in stata date format it's easy to transform*/
	gen arr_m=month(arr_d)				/*generates month of arrest variable*/
	gen arr_y=year(arr_d)				/*generates year of arrest variable*/
	gen arr_my=mofd(arr_d) 				/*generates variable equal to the month/year of 
										arrest, note numerically this is the 
										number of months since 1/1/1960, need to 
										format*/
list arr_m arr_y arr_my arr_d in 1/10
	format arr_my %tm 					/*formats it as year month*/
	list arr_m arr_y arr_my arr_d in 1/10
	gen arr_jan25=(arr_d==td(25jan2014)) 	/*Shortcut for creating dummies is to put 
											the if statement after the equal sign in 
											parenthesis, downside is only for zero/one 
											variables, problem for missings*/
											/*Note td() calls the label otherwise 
											you would have to calculate days from 
											1/1/1960*/		

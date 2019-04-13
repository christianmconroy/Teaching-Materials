* Spring 2018 McCourt STATA Recitation

clear

cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations


// 2.2 Import, verify, and reorder
import excel using Schools.xlsx, clear firstrow
*so each observation is a sport at a particular school
order institution_name Sports
* The above moves the two columns to the front

set more off

// 2.3
* creating dummies
gen medSchool = 0
gen largeSchool = 0
replace medSchool = 1 if EFTotalCount >=1000 & EFTotalCount <4999
replace largeSchool = 1 if EFTotalCount >=5000
replace medSchool = . if EFTotalCount ==.
replace largeSchool = . if EFTotalCount ==.
*verify dummies
tab  medSchool , missing
bysort medSchool: summarize EFTotalCount
tab  largeSchool, missing
bysort largeSchool: summarize EFTotalCount

reg TOTAL_EXPENSE_ALL medSchool largeSchool

// 2.4 factor variables
gen schoolsize = .
replace schoolsize = 1 if EFTotalCount >=0 & EFTotalCount < 1000
replace schoolsize = 2 if EFTotalCount >=1000 & EFTotalCount <4999
replace schoolsize = 3 if EFTotalCount >=5000 & EFTotalCount !=.

* Alternative approach 
* recode EFTotalCount (min/999 = 1) (1000/4999 = 2) (5000/max = 3), gen(schoolsize)
* We show them below though, so don't really need to reemphasize. 

*check categorical against dummies
tab2 schoolsize largeSchool medSchool

reg TOTAL_EXPENSE_ALL i.schoolsize
help fvvarlist
* Above explains further, including ib language. 
/* Can either make 1 the reference category because Stata chooses the smallest 
value or use ib notation with the number of in parantheses category after 
like below (equivalent of making dummies manually and choosing to put one in 
over others): 
reg TOTAL_EXPENSE_ALL ib3.schoolcat
*/ 

// 2.5
* reg TOTAL_REVENUE_ALL i.sector_name - can only use i.notation with numeric vars
* The below changes sector_name form string to factor in sectorid
encode sector_name, gen(sectorid)
tab sectorid
tab sectorid, nolabel
regress TOTAL_REVENUE_ALL i.sectorid
* Private for-profit two-year is the reference category

// 2.6
recode EFTotalCount (0/999 = 1 "Small") (1000/4999 = 2 "Medium") (5000/max = 3 "Large") , gen(schoolsize2)

* Manual label adding: 
/* label define sccatlab 1 small 2 medium 3 large
label values schoolcat sccatlab
codebook schoolcat */ 

egen numparticipants = rowtotal(PARTIC_MEN PARTIC_WOMEN)
sum numparticipants, detail
* so 17 26 41 seem like cut offs
recode numparticipants  (0/17 = 1 "Small Team") (18/26 = 2 "Mid-Small Team") (27/41 = 3 "Mid-Large Team") (42/max = 4 "Large Team"), gen(categorical_participants)
tab categorical_participants, missing
	
reg TOTAL_EXPENSE_ALL i.categorical_participants



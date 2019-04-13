* Spring 2018 McCourt STATA Recitation

clear

cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

// 2.2 Import, verify, and reorder
import excel using Schools.xlsx, clear firstrow
*so each observation is a sport at a particular school
order institution_name Sports

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

*check categorical against dummies
tab2 schoolsize largeSchool medSchool

reg TOTAL_EXPENSE_ALL i.schoolsize
help fvvarlist

// 2.5
* reg TOTAL_REVENUE_ALL i.sector_name - can only use i.notation with numeric vars
encode sector_name, gen(sectorid)
tab sectorid
tab sectorid, nolabel
regress TOTAL_REVENUE_ALL i.sectorid

// 2.6
recode EFTotalCount (0/999 = 1 "Small") (1000/4999 = 2 "Medium") (5000/max = 3 "Large") , gen(schoolsize2)

egen numparticipants = rowtotal(PARTIC_MEN PARTIC_WOMEN)
sum numparticipants, detail
* so 17 26 41 seem like cut offs
recode numparticipants  (0/17 = 1 "Small Team") (18/26 = 2 "Mid-Small Team") (27/41 = 3 "Mid-Large Team") (42/max = 4 "Large Team"), gen(categorical_participants)
tab categorical_participants, missing
	
reg TOTAL_EXPENSE_ALL i.categorical_participants

************** WEEK 2 OF PROJECT 2 ****************************************

// Q1
lookfor total
* We see that we're looking at TOTAL_REVENUE_ALL and TOTAL_EXPENSE_ALL
gen profit = TOTAL_REVENUE_ALL - TOTAL_EXPENSE_ALL


// Q2
tab Sports
encode Sports, gen(sportid)
* see help recode
codebook sportid, tab(1000)
* OR
label list sportid
/* Open the help menu to see how to code two categories that are not numericaly 
in order into one category in our new variable (i.e. baseball and softball) */ 
recode sportid  (4 24 = 1 "Baseball/Softball") ///
						(5 = 2 Basketball) ///
						(12 = 3 Football) ///
						(23 = 4 Soccer) ///
						(nonmissing = 5 Other) ///
						(missing = .) ///
					, gen(sportcat)

tab sportcat,m
graph bar (mean) TOTAL_REVENUE_ALL (mean) TOTAL_EXPENSE_ALL, over(sportcat)

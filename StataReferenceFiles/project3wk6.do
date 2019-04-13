* Project 3, Week 6 Spring 2018
********************************************************************************
cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

* Download data from http://catalog.data.gov/dataset/school-improvement-2010-grants
* Or, go to data.gov and search for "School Improvement 2010 Grants"


*Q1

* if you don't use the bindquote option:
import delimited userssharedsdfschoolimprovement2010grants.csv , clear varnames(1)

* with bindquote option:
import delimited userssharedsdfschoolimprovement2010grants.csv , clear bindquotes(strict) varnames(1)
/* - Could just do the import delimited extvarlist using filename [, import_delimited_options]
option if you want to only import a subset
- Can also use to export a DTA that you might've changed to a csv file.
- If there are embedded spaces, remember to use quotes around the file name */

* Opening up the other dataset just to look at it*

* Make surw we go back to the usershared... dataset for next part


*Q2
codebook state
* only 50 unique values, including DC. Which state is missing? Hawaii (short HI)

help merge

* use the merge command. The state_name_abbreviation data set has to be in the 
* correct folder.
merge m:1 state using state_name_abbreviation.dta

br if _merge == 2

drop if _merge == 2

* we could also drop the whole variable _merge now since we don't need it anymore.
* if you were to merge with another data set later on and don't drop _merge before,
* this might lead to confusion.

drop _merge

*Q3
destring v5 , gen(grantamt) ignore("$")

*Q4
encode modelselected, gen(model)
tab model

*Q5
summarize grantamt model
misstable summarize grantamt model

*Q6
* Dealing with missing data
* The example
summarize grantamt model if grantamt != . | model != . 

egen numbermissing = rowmiss(grantamt model)
table(numbermissing)

browse if numbermissing > 0

gen  nomiss = 0
replace nomiss = 1 if numbermissing==0

summarize grantamt if nomiss==1
tab model if nomiss==1




 


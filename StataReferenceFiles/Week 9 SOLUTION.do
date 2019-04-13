// Week 9 Spring 2018 McCourt School of Public Policy Stata Recitation
//
// SOLUTION
//
//

set more off

clear

cd C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations

*Q2
* global macros

* create two folders: one called "Week 9 data", and one called "output"
* MAKE SURE THEY ARE REPLACING THE ... WITH THEIR FILE PATHWAY!

global rawdatapath "C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations\Week 9 data"

global outpath "C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\StataRecitations\output"

*global rawdata "...\Week 9 data"

di "$rawdatapath"

*set directory

cd "$rawdatapath"

*Q3 

*show all excel files in folder

local allfiles : dir . files "*.xlsx"
display `allfiles'

*import all xlsx files and save as dta file
*note: the excel file cannot be open in excel when stata is trying to read it

foreach file in `allfiles' {
	** We start ar A12 because that's where the variable names start. Above is filler. 
	** We specify firstrow to tell Stata that A12 is variable names. 
    import excel using "`file'", sheet("BLS Data Series") cellrange(A12:D511) firstrow clear
    
    **to save as a stata file with the same name, create a local with the .xls extension removed
	** subinstr allows us to do byte based substitution 
    local noextension=subinstr("`file'",".xlsx","",.)
	
	** rename the variable Value to employment
	rename Value employment
    
    save "`noextension'", replace
	
}

*let's look at one of the files:

use seriesreport-20170914163828_f73e7a.dta, clear

* seriesID contains state ID
* year
* month
* employment number

*Q4

* open and append all dta files and save as one file

help append

* a data set needs to be open to be able to append further data

use seriesreport-20170914163828_f73e7a.dta, clear

* next, append another stata file

append using seriesreport-20170914163906_400133.dta

* save the file in your output folder, name it fullfile

save "$outpath\fullfile.dta", replace

* browse
* the data from the second file has been added to the data of the first file
br


* this is cumbersome, so here is a loop to append all files at once


*clear data in memory (THIS WILL NOT CLEAR MACROS)
clear

*define local with filenames of all *.dta files
local statfiles : dir . files "*.dta"


**create a blankvariable so you can save a blank dataset to start appending to
gen blankvar=.
save "$outpath\fullfile.dta", replace

foreach data in `statfiles' {
    use "$outpath\fullfile.dta", clear
    append using `data'
    save "$outpath\fullfile.dta", replace
}

drop blankvar

save "$outpath\fullfile.dta", replace

*how many states are in this data set?

codebook SeriesID
* 51 unique values

*how many observations by state?
count if SeriesID == "LASST130000000000005"
* 499 = 41 years * 12 months plus 7 months in 2017
di 41*12+7

*Q5

* merge this data set with state names

use "$outpath\fullfile.dta", clear

merge m:1 SeriesID using "$outpath\names.dta"
 
drop _merge

br

* We now have a new column with the "Area"

*Q6

* create one time variable out of Year and Month

* first need to destring Period and remove initial M
* then use yearmonth function ym(year,month)
 
destring Period, ignore("M") generate(month)
 
gen time = ym(Year,month)
* sort by State and time
sort Area time

br time

format time %tm // to display time in readable format

br

save "$outpath\data_and_time.dta", replace

*Q7

* now we can use Stata's time series tools to generate the variable of interest: employment growth
* growth rates are approximately equal to the difference of logged variables

* one of the problems is that Stata does not accept strings as observation ID. We have to create a numeric ID

egen ID = group(SeriesID)

* give stata the observation (state) ID and time variable
xtset ID time
 
* gen log employment so difference operator can be used to get percent change
 
gen log_emp = log(employment)
 
* generate employment growth variable as a first difference using the D notation, 
* 51 missing values, for the first obs of 50 states and DC
 
gen emp_growth = D1.log_emp

save "$outpath\final_data.dta", replace

*Q8

* graphing the data: do we have variation in employment growth?

preserve

collapse (mean) emp_growth (first) Area , by(ID)

twoway scatter emp_growth ID, mlabel(Area)

restore

*Q9

* in case restore did not work
use "$outpath\final_data.dta", clear

* let's run a regression of current employment growth on lagged employment

reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth

* for all states, employment growth in percent today = 0.7* % growth last month + 0.11 * % change two months ago ...

* what about other states? Note the double quotation marks when the local is used

local sun_belt "Arizona","Nevada","California","Florida"

local farm_states "Idaho","North Dakota","Iowa","South Dakota"

local oil_states "Louisianna","Texas","Wyoming","Oklahoma"


reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`sun_belt'")
* about same as national economy

reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`farm_states'")
* impact of previous emp_growth lower than in national economy

reg emp_growth L1.emp_growth L2.emp_growth L3.emp_growth L4.emp_growth if inlist(Area,"`oil_states'")
* really strong short-term impact, second lag already insignificant



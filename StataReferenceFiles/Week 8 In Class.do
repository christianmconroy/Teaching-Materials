sysuse citytemp, clear

/* In-Class Activity 1: Review Problem */ 

*1
gen hightempJan = 0
replace hightempJan = 1 if tempjan==.
replace hightempJan =1 if tempjan > 40

*2
tab hightempJan
*355

*3

label define htjlabel 0 "Low Temp" 1 "High Temp"
label values hightempJan htjlabel

graph bar (mean) cooldd (mean) heatdd, over(hightempJan) ///
ytitle(Avg. Cooling Days) title(Mean Cooling and Heating Days) ///
subtitle(By High vs. Low January Temp.) 

*4
tab2 region division
* 61

/* In-class code */ 

sysuse nlsw88, clear
tab2 race married
tab2 age married
tab2 married age
tab2 union married
tab2 union married, m
sum married union

tab2 race union, column
tab race if union == 0

tab2 race union
tab2 race union, column
tab2 race union, row
tab2 race union, cell
tab2 race union, row column cell

tab2 race union, chi2

/* In-class activity 2 */ 
use https://stats.idre.ucla.edu/stat/stata/notes/hsb2, clear

*1
ttest write==50
* Reject

*2 
ttest write, by(female)
* I assumed equal variance here 
* Reject

* 3
tab2 schtyp female, chi2
* Fail to reject

/* Resume in-class notes */ 
sysuse nlsw88, clear
twoway (scatter wage tenure) (lfit wage tenure)

corr wage tenure
corr wage tenure age hours
pwcorr wage tenure age hours, obs

/* In-class activity 3 */ 	
use https://stats.idre.ucla.edu/stat/stata/notes/hsb2, clear
*1
twoway (scatter write read) (lfit write read)

*2
pwcorr write read, sig
* Yes

*3
gen id_odd = mod(id, 2)

*4
* There are 100. 
replace write = . if id_odd ==1 

*5
pwcorr write read, sig obs
* Still stat sig. 100 obs now. 

* In-class notes continued
sysuse nlsw88, clear
gen weekwage = wage * hours
label variable weekwage "Ave. Weekly Pay"

label data "Modified dataset for recitation 8"
save "nlsw88_recitation8.dta"

clear
use "nlsw88_recitation8.dta"

label list occlbl

gen tenure20 = 0
replace tenure20 = 1 if tenure >=20
replace tenure20 = . if tenure == .

list tenure tenure20 in 45/55

label variable tenure20 "Tenure of 20 or more years"
label define tenure20lbl 0 "less than 20 years" 1 "20 or more years" 

label values tenure20 tenure20lbl

tab tenure20

/* Activity 4 */ 
sysuse nlsw88, clear
*1 
label data "NLS Mature and Young Women, 1988"
save "NLS.dta"

*2
gen weekly_wage = wage * hours
label variable weekly_wage "Ave. Weekly Pay"

*3 
sum hours
hist hours

generate employ_type = .
	replace employ_type = 0 if hours == 0 
	replace employ_type = 1 if hours < 40 & hours > 0
	replace employ_type = 2 if hours == 40
	replace employ_type = 3 if hours > 40 & hours <= 80
codebook employ_type
	
label define emptypelbl 0 "Unemployed" 1 "Part-time" 2 "Full-Time" 3 "Other"
label values employ_type emptypelbl

codebook employ_type

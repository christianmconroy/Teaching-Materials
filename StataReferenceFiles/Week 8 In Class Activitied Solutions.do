/* Week 8 In Class Activities */

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

/* Activity 4 */ 
*1 
label data "NLS Mature and Young Women, 1988"
save "NLS.dta"

*2
gen weekly_wage = wage * hours
label variable weekly_wage "Ave. Weekly Pay"

*3 
generate partt = "Part-Time"
generate fullt = "Full-Time"
generate other = "Other"

gen employ_type = 0
replace employ_type = partt if inrange(hours, 0, 39)

generate employ_type =  partt if inlist(hours, 0,39)
replace employ_type =  fullt if hours >= 40
replace employ_type = other if employ_type == ""

encode employ_type, gen(employ_typen)
label variable employ_typen "Employment Types"
label define employtlbl 0 "Part-Time" 1 "Full-Time" 
label values employ_typen employtlbl




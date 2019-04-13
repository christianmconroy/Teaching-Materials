* PPOL 503 PS4
* Christian Conroy

* Question 2, Part i *

cd "C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\Semester3Fall2017MPP\QuantIII"
use "JoyData.dta", clear

* For weighting: reg DependentVariable IndependentVariables [aweight=PopulationSize], robust

label variable famplanpercapita "Family Planning Spending Per Capita"
label variable marriagerate "Marriage Rate"
label variable unemploymentrate "Unemployment Rate"
label variable percentfemale "Percent Female"
label variable povertyrate "Poverty Rate"
label variable hsgrad "HS Grad"
label variable blackfemalecount "Black Women"
label variable welfrincome "Welfare Income"
label variable medhhinc "Median Household Income"
label variable abortionrate "Abortion Rate"
label variable stateID "State ID"
label variable femchildbearpopsize "Number of Women of Childbearing Age" 
label variable year "Year"
	 



* 2a
reg abortionrate famplanpercapita [aweight=femchildbearpopsize], robust

outreg2 using myreg.doc, replace ctitle(Abortion Rate) nocons addtext ///
(State and Year Fixed Effects, no) label nonotes addnote///
(*Indicates coefficient is significant at the p<.01 level., **Indicates ///
coefficient is significant at the p<.001 level.)

* 2b
reg abortionrate famplanpercapita marriagerate unemploymentrate percentfemale ///
povertyrate hsgrad blackfemalecount welfrincome medhhinc ///
[aweight=femchildbearpopsize], robust
outreg2 using myreg.doc, append ctitle(Abortion Rate) nocons ///
addtext(State and Year Fixed Effects, no) label

* 2c
xtset stateID year
xtreg abortionrate famplanpercapita marriagerate unemploymentrate percentfemale ///
povertyrate hsgrad blackfemalecount welfrincome medhhinc i.year ///
[aweight=femchildbearpopsize], fe vce (robust)
outreg2 using myreg.doc, append ctitle(Fixed Effects) keep(abortionrate ///
famplanpercapita marriagerate unemploymentrate percentfemale povertyrate hsgrad ///
blackfemalecount welfrincome medhhinc) nocons ///
addtext(State and Year Fixed Effects, yes) label

* 2e
corr famplanpercapita welfrincome [aweight=femchildbearpopsize]
corr abortionrate welfrincome [aweight=femchildbearpopsize]

*2f
tab year, gen(y)
corr famplanpercapita y1 [aweight=femchildbearpopsize]
corr famplanpercapita y2 [aweight=femchildbearpopsize]
corr famplanpercapita y3 [aweight=femchildbearpopsize]
corr abortionrate y1 [aweight=femchildbearpopsize]
corr abortionrate y2 [aweight=femchildbearpopsize]
corr abortionrate y3 [aweight=femchildbearpopsize]










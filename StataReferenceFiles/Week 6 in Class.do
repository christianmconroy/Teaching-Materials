

****** MODES ********

/* Way #1 */ 
tab grade

/* Way #2 */ 
egen grademode = mode(grade)
sum grademode

/* Way #3 */ 
help modes
* Install the package - The one with tabulate modes 
modes grade


/* In-class code */ 
capture log close
	
log using "Recitation 6.log", replace

sysuse auto, clear

help operators

sum weight length mpg if foreign == 1
sum weight length mpg if foreign != 1

sum weight length mpg if foreign == 0 & weight >= 3317

list weight length mpg if foreign == 0 & (weight <= 3317 | length <=196)
sum weight length mpg if foreign == 0 & (weight <= 3317 | length <=196)

list weight length mpg if foreign == 0 & weight <= 3317 | length <=196
sum weight length mpg if foreign == 0 & weight <= 3317 | length <=196

sum weight length mpg if (foreign == 0 & weight <= 3317) | (foreign == 0 & length <= 196)

codebook rep78
list rep78 if rep78 ==.
list rep78 if rep78 > 999999
list rep78 if rep78 > 4 & rep78 != .

sum gear_ratio
list make gear_ratio if gear_ratio == 3.89
list make gear_ratio if gear_ratio > 3.88999 & gear_ratio < 3.89001
describe

gen midmpg = 0

gen midmpg = 0
replace midmpg = . if mpg == .
replace midmpg = 1 if inlist(mpg, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
tab mpg midmpg, missing

capture log close

/* Bonus Stata Fun to Start the Class!*/
sysuse auto, clear

***** Fun with Factors and Dummies *******

/* To add a new column with numerical levels for string */ 
egen du = group(make)

* Gen is for simple mathematical transformations of other variables or numbers.
* Egen is for functions on groups of observations.

/* To add a new column with value labels for string */ 
encode make, gen(du2)
codebook du2, tab(1000)

/* To add variables that are dummies for all levels of your categorical variable */
sysuse nlsw88, clear
codebook race
tab race, gen(racedum)

/* To run regression that automatically gives dummies and chooses a first level 
as reference */ 
reg wage i.race, robust
